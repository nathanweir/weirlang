# Package System

## Decision summary

| Area | Decision |
|------|----------|
| Manifest format | S-expression syntax in `weir.pkg` |
| Manifest parser | Dedicated small S-expr reader (not the full Weir parser) |
| Module naming | `lib.weir` maps to bare package name; other files get `package.filename` |
| Dependencies | Local path-based only (registry deferred) |
| Import resolution | Full type-checked wiring via `check_with_imports` (not source concatenation) |
| Multi-module codegen | AST merging — all modules flattened into one for codegen |
| Visibility | All symbols public for now (`pub` filtering deferred) |
| CLI integration | `weir build/run/dev` discovers `weir.pkg` when no file arg given |

## Manifest format

Package metadata lives in a `weir.pkg` file at the package root. The format is S-expressions — dogfooding the language syntax. A dedicated small parser handles it (not the full Weir parser, since manifests have a fixed, simple structure).

### Fields

| Field | Required | Description |
|-------|----------|-------------|
| `name` | yes | Package name (string). Becomes the root module name for imports. |
| `version` | yes | Semver string (informational for now). |
| `sources` | no | Source files — string or list of strings. Paths relative to `weir.pkg`. |
| `main` | no | Entry point file (has `defn main`). For applications only. |
| `deps` | no | Dependencies. Each is `(name (path "relative/or/absolute"))`. |
| `native` | no | Native C code. `sources` = C files to compile, `link` = libraries to link (`-l` flags). |

### Library package

```lisp
;; weir-gl/weir.pkg
(package
  (name "weir-gl")
  (version "0.1.0")

  (sources "lib.weir")

  (native
    (sources "gl_helper.c")
    (link "glfw" "GL" "m")))
```

### Application package

```lisp
;; demos/tetris/weir.pkg
(package
  (name "tetris-gl")
  (version "0.1.0")

  (deps
    (weir-gl (path "../../../weir-gl")))

  (main "tetris.weir"))
```

## Module naming convention

File paths within a package determine module names:

| Package name | Source file | Module name |
|-------------|------------|-------------|
| `weir-gl` | `lib.weir` | `weir-gl` |
| `foo` | `bar.weir` | `foo.bar` |
| `foo` | `lib.weir` | `foo` |
| `foo` | `utils.weir` | `foo.utils` |

**Convention:** `lib.weir` is special — it maps to the bare package name (no `.filename` suffix). This is the package's root module and the default entry point for imports.

## Dependency resolution

### Process

1. Parse the `weir.pkg` file
2. Recursively resolve dependencies (follow `path` references, read their `weir.pkg`)
3. Topologically sort the dependency graph (error on cycles)
4. Return a `ResolvedProject` with all source files, module names, native code, and link flags

### Data structures

**Files:** `crates/weir-pkg/src/lib.rs`

```rust
pub struct PackageManifest {
    pub name: String,
    pub version: String,
    pub sources: Vec<PathBuf>,       // relative to package root
    pub main: Option<PathBuf>,       // entry point (applications)
    pub deps: Vec<DepSpec>,
    pub native_sources: Vec<PathBuf>,
    pub link_libs: Vec<String>,
}

pub struct DepSpec {
    pub name: String,
    pub path: PathBuf,
}

pub struct ResolvedModule {
    pub module_name: SmolStr,        // e.g. "weir-gl" or "weir-gl.utils"
    pub source_path: PathBuf,        // absolute path to .weir file
    pub package_name: String,        // which package this belongs to
}

pub struct ResolvedProject {
    pub modules: Vec<ResolvedModule>,   // topologically sorted (deps first)
    pub entry_module: SmolStr,          // the main module name
    pub native_sources: Vec<PathBuf>,   // all C files from all packages
    pub link_libs: Vec<String>,         // all -l flags from all packages
}
```

Dependencies are **local path-based only**. No registry, no git URLs, no lock files. This is a personal ecosystem — packages live on the developer's local machine.

## Import resolution

Full type-checked wiring, not source concatenation. Modules are type-checked in topological order (dependencies first), and each module's exports are available to its dependents with real types.

### Existing infrastructure

The import system is already partially built:

- **Parser:** `Import` AST node with `module_path: SmolStr`, `ImportKind` enum (`Names(Vec<SmolStr>)`, `Alias(SmolStr)`, `All`)
- **Type checker:** `check_with_externals(module, externals: &HashSet<SmolStr>)` treats names in `externals` as known (assigns fresh type vars)
- **Interpreter/codegen:** Currently ignore `Item::Import` entirely

### New type checker API

**Files:** `crates/weir-typeck/src/lib.rs`, `crates/weir-typeck/src/checker.rs`, `crates/weir-typeck/src/result.rs`

```rust
/// External symbol with its known type (from another module's type check).
pub struct ExternalSymbol {
    pub name: SmolStr,
    pub ty: Ty,
    pub kind: ExternalKind,  // Function, Type, Constructor, etc.
}

pub fn check_with_imports(
    module: &Module,
    imported_symbols: &[ExternalSymbol],
) -> TypeCheckResult;
```

### Resolution process

1. Type-check modules in topological order (dependencies first)
2. After type-checking module A, extract its public symbols + types into an `ExternalSymbol` list
3. When type-checking module B that imports from A, pass A's symbols as `imported_symbols`
4. The type checker adds imported symbols to scope before checking B's items
5. Import validation: if B says `(import A (foo bar))`, verify `foo` and `bar` exist in A's exports — error if not

### Changes to `TypeChecker`

In `collect_definitions()`, process `Item::Import` nodes:

- Look up the module name in the provided imports
- `ImportKind::Names(names)`: add each named symbol to scope with its known type
- `ImportKind::All`: add all symbols from the module
- `ImportKind::Alias(alias)`: deferred — parse but error at type check time initially
- Error on: unknown module, unknown symbol in module, duplicate imports

### Multi-module parsing

```rust
pub struct ParsedModule {
    pub module_name: SmolStr,
    pub ast: weir_ast::Module,
    pub source: String,                // needed for macro expansion + error reporting
    pub exports: HashSet<SmolStr>,     // public symbol names (all for now, pub filtering later)
}

pub struct ModuleIndex {
    pub modules: IndexMap<SmolStr, ParsedModule>,  // topological order preserved
    pub entry: SmolStr,
}
```

For the first pass, **all top-level definitions are considered public** (no `pub` filtering).

Source assembly for each module in topological order:

1. Read source file
2. Prepend prelude
3. Expand macros
4. Parse into AST
5. Extract top-level symbol names (`defn`, `deftype`, `defstruct`, `extern "C"` declarations)
6. Record in `ModuleIndex`

## Multi-module codegen strategy

After all modules are type-checked with proper import resolution, **merge their ASTs into a single `Module` for codegen**. This avoids a major codegen refactor:

- All function definitions from all modules are compiled together
- Symbol names must be unique across modules (fine for the personal ecosystem scale)
- `extern "C"` declarations from library packages are included
- The `main` function comes from the entry module
- The type checker handles scoping/validation; codegen just sees a flat list of definitions

**Files:** `crates/weir-codegen/src/lib.rs`, `crates/weir-codegen/src/aot.rs`

### AOT builds

- Collect all native C sources from all packages
- Collect all link libraries
- Pass to `build_executable` via the existing `cc_args` and `link_flags` parameters

### JIT / dev mode

- Compile all native sources into a shared library
- `dlopen` before JIT compilation
- Extern symbols resolve via `dlsym` as today

## CLI integration

### Manifest discovery

When invoked without a file argument, the CLI looks for `weir.pkg` in the current directory:

```bash
# In demos/tetris/:
weir build                           # reads weir.pkg, resolves deps, builds
weir build -o ../../tmp/tetris_gl    # custom output path

weir run                             # JIT via weir.pkg
weir dev                             # dev mode via weir.pkg
```

### Backwards compatibility

Existing single-file mode still works. When a file arg is provided, skip manifest discovery entirely:

```bash
weir build demos/tetris.weir -o tmp/tetris   # no weir.pkg needed
weir run examples/hello.weir                 # no weir.pkg needed
```

**Files:** `crates/weir-cli/src/main.rs`, `crates/weir-cli/Cargo.toml`

### What the package system replaces

The manual invocations that `weir.pkg` makes declarative:

```bash
# AOT build (current manual approach):
weir build demos/tetris_gl.weir -o tmp/tetris_gl --cc-arg libs/gl_helper.c -l glfw -l GL -l m

# JIT run (current manual approach):
cc -shared -fPIC -o tmp/libgl_helper.so libs/gl_helper.c -lglfw -lGL -lm
weir run demos/tetris_gl.weir --load tmp/libgl_helper.so
```

Becomes:

```bash
cd demos/tetris && weir build
```

## Concrete examples

### weir-gl library package

Located at `/home/nathan/dev/weir-gl/` (external to the language repo).

```
weir-gl/
  weir.pkg
  lib.weir          # extern "C" declarations for all gl_* functions
  gl_helper.c       # native C helper (OpenGL/GLFW wrappers)
```

`lib.weir`:
```lisp
(extern "C"
  (defn gl_init ((w : i64) (h : i64) (title : String)) : i64)
  (defn gl_should_close () : i64)
  ;; ... all gl_* declarations
  (defn gl_cleanup () : Unit))
```

### tetris application package

Located at `demos/tetris/` (inside the language repo — useful for verification).

```
demos/tetris/
  weir.pkg
  tetris.weir       # game code importing weir-gl
```

`tetris.weir`:
```lisp
(import weir-gl (gl_init gl_should_close gl_begin_frame gl_end_frame
                 gl_draw_rect gl_draw_text gl_get_key_pressed
                 gl_time_ms gl_screenshot gl_cleanup))

;; ... game code (moved from demos/tetris_gl.weir, minus extern block)
```

## Deferred items

| Item | Notes |
|------|-------|
| `pub` visibility | All symbols public for now. Add `pub` filtering in a follow-up. |
| Qualified access | `(import m :as v)` + `(v.func ...)` syntax. Will parse but error initially. |
| Circular import detection | Cross-package cycles handled by topological sort. Within-package cycles are a follow-up. |
| Platform conditionals | e.g., `(native (link (linux "GL") (macos (frameworks "OpenGL"))))` |
| Package registry | Git URLs, hosted registry, version resolution |
| Lock files | Not needed for local-only path deps |
| Per-module incremental compilation | All modules compile together for now. Caching is a performance optimization for later. |
| Portability | Cross-platform builds (may require OS-specific builds). Deferred as a separate concern. |

## Implementation order

1. `crates/weir-pkg/` — manifest parsing + dependency resolution
2. Multi-module parsing — read all sources, build `ModuleIndex`
3. Type checker: `check_with_imports` — process `Import` items, add imported symbols to scope with real types
4. AST merging for codegen — concatenate validated modules for compilation
5. CLI integration — `weir build/run/dev` with manifest discovery
6. Extract weir-gl — create external package at `/home/nathan/dev/weir-gl`
7. Create `demos/tetris/` — project with `weir.pkg` consuming weir-gl
8. Justfile + test — verify end-to-end, existing tests still pass

## Verification criteria

1. `cargo test --workspace` — all existing tests still pass
2. `cd demos/tetris && weir build -o ../../tmp/tetris_gl` — builds via package system
3. `./tmp/tetris_gl` — game runs, screenshot generated
4. `cd demos/tetris && weir dev --load ...` — dev mode works with live reload
5. Modify `weir-gl/lib.weir` to add a typo in a function name — type error in tetris referencing the bad import
6. Remove an import from `tetris.weir` — type error for undefined symbol
7. Single-file mode still works: `weir run examples/hello.weir`
