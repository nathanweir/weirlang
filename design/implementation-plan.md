# Implementation Plan: Struct Codegen, TCO & CFFI

**Status:** Active
**Prerequisite:** Phases 0-10 complete (see `archive/implementation-plan-phases-0-10.md`)

---

## Phase 11: Struct Codegen ✅

**Completed 2026-02-19.** 613 tests passing (was 606). Struct destructure syntax changed from `{x y}` to `{:x :y}` (keyword-based) during implementation.

Structs work in the type checker and interpreter but were completely absent from the Cranelift JIT/AOT backend. Any program using `defstruct` would type-check and interpret correctly but crash during codegen.

### Current state

| Layer | Status |
|-------|--------|
| Parser / AST | Done — `Item::Defstruct`, `ExprKind::FieldAccess`, `PatternKind::StructDestructure` |
| Type checker | Done — `struct_defs` HashMap, `check_struct_construction()`, `check_field_access()`, struct destructure in patterns |
| Interpreter | Done — `Value::StructConstructor`, `Value::StructInstance`, construction (positional + named), field access, pattern destructure |
| Codegen | **Not started** — `ty_to_cl_with()` returns `None` for struct types, no construction/access/destructure codegen |

### Representation

Structs are heap-allocated, GC-managed, fixed-layout objects — the same strategy already used for closures and tagged ADT payloads:

- Each struct instance is a GC object: `weir_gc_alloc(shape, size)` returns an `i64` pointer
- Fields are stored as consecutive i64 slots at offsets `0, 8, 16, ...`
- Non-i64 fields (f32, i8, bool) are widened to i64 for uniform slot size (matching the existing pattern for vector elements and ADT payloads)
- The `ShapeDesc` uses `SHAPE_FIXED` with a `pointer_mask` marking which slots contain heap pointers

### Steps

#### 11.1 — Pass struct definitions to codegen

`TypeCheckResult` currently has no struct info. Add a public struct metadata type and field:

```rust
// In weir-typeck/src/result.rs
pub struct StructInfo {
    pub fields: Vec<(SmolStr, Ty)>,  // field name + resolved type
}

// In TypeCheckResult:
pub struct_defs: HashMap<SmolStr, StructInfo>,
```

Populate this in `TypeChecker::finish()` by resolving each `StructDef`'s field `TypeExprId`s to concrete `Ty`s.

**Files:** `crates/weir-typeck/src/result.rs`, `crates/weir-typeck/src/checker.rs`

#### 11.2 — Recognize struct types in codegen

In `Compiler::new()`, collect struct names from `type_info.struct_defs` into a `struct_defs: HashMap<SmolStr, StructInfo>` field on `Compiler` (and threaded into `FnCompileCtx`).

In `ty_to_cl_with()`, add a `struct_names: Option<&HashSet<SmolStr>>` parameter (or check in `ty_cl()` directly) so that `Ty::Con("Point", _)` returns `Some(types::I64)` — structs are pointer-sized.

In `is_heap_pointer()`, return `true` for struct-typed `Ty::Con`.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.3 — Struct construction codegen

When `compile_call()` sees a callee name in `struct_defs`:

1. Compile each argument expression
2. Widen each to i64
3. Build a `SHAPE_FIXED` shape descriptor with `num_slots = field_count` and `pointer_mask` bits set for heap-pointer fields
4. Call `weir_gc_alloc(shape, field_count * 8)`
5. Store each field at `ptr + i * 8` using `builder.ins().store(...)`
6. Push the result onto the GC shadow stack
7. Return the pointer as `i64`

This mirrors the existing closure allocation code pattern.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.4 — Field access codegen

When `compile_expr()` encounters a `Call` where the callee is `ExprKind::FieldAccess(field_name)`:

1. Look up the object's type from `expr_types`
2. Find the struct definition and the field's index
3. Compile the object expression to get the pointer
4. `builder.ins().load(field_type, MemFlags::new(), ptr, offset)` where offset = `index * 8`
5. Narrow from i64 if the field type is smaller

Also handle standalone `ExprKind::FieldAccess` as a first-class accessor (creates a closure that takes an object and returns the field). This can be deferred — error gracefully for now.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.5 — Struct pattern destructure codegen

In `compile_match()` / pattern compilation, handle `PatternKind::StructDestructure`:

1. For each field binding, compute the field's offset in the struct layout
2. Load the value from `ptr + offset`
3. Bind it to the pattern variable

No tag check needed (structs aren't sum types), so the destructure is unconditional.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.6 — AOT runtime support

The AOT C runtime needs no new functions — struct allocation uses the existing `weir_gc_alloc`. However, verify that the AOT path correctly handles `Defstruct` items (currently skipped).

**Files:** `crates/weir-codegen/src/aot.rs` (verify, likely no changes needed)

#### 11.7 — Tests

- Unit tests: struct construction, field access, nested structs, structs with heap-pointer fields (strings, vectors), struct in match patterns
- Integration: run `demo-raytracer.weir` rewritten with `Vec3` struct via JIT and AOT
- GC stress: create many short-lived structs to verify shape descriptors and shadow stack interaction

**Test count target:** 15-20 new tests

---

## Phase 12: Tail-Call Optimization ✅

**Completed 2026-02-19.** 616 tests passing (was 613). Self-recursive tail calls are compiled as loops (codegen) and trampolined (interpreter). Deep recursion (1M+ depth) works without increased stack size.

Without TCO, all iteration in Weir is recursion, and deep recursion overflows the stack. The ray tracer requires `RUST_MIN_STACK=67108864` for a 160x90 image. TCO is essential for the language to be practical.

### Current state

- [x] Tail-position analysis in `weir-ast/src/tco.rs`
- [x] Codegen: self-tail-calls compiled as jumps to loop header block
- [x] Interpreter: trampoline via `tail_call_args` sentinel in `InterpError`
- [x] 3 CLI tests (interp, JIT, AOT) with 1M-depth recursion

### Design decision: implicit self-tail-calls first

Start with the simplest, highest-value case: **self-recursive tail calls are compiled as loops**. This covers the vast majority of Weir iteration patterns (see `render-col`, `render-rows`, `find-hit`, `find-t` in the ray tracer). Mutual tail calls and general tail-call elimination can follow later.

A call `(f args...)` is in tail position if it is the last expression evaluated before the enclosing `defn` returns. In Weir, tail positions are:

- The body of a `defn`
- The then/else branches of an `if` in tail position
- The last expression in a `let` body in tail position
- The matching branch of a `cond`/`match` in tail position

### Steps

#### 12.1 — Tail-position analysis pass

Add a function `mark_tail_positions(body: ExprId, arena: &ExprArena) -> HashSet<ExprId>` that walks the AST and collects expression IDs that are in tail position. This runs once per function before codegen.

Rules:
- `body` itself is in tail position
- If an `If` is in tail position, both its then and else branches are
- If a `Let` is in tail position, its body is
- If a `Cond` is in tail position, each branch result is
- If a `Match` is in tail position, each arm body is
- A `Call` in tail position where the callee is the enclosing function → **self-tail-call**

**Files:** new function in `crates/weir-codegen/src/lib.rs` (or a small `tco.rs` module)

#### 12.2 — Codegen: self-tail-calls as jumps

For a function that has self-tail-calls:

1. Create a "loop header" block at the top of the function body
2. Function parameters become block parameters of the loop header
3. At each self-tail-call site: compile arguments, then `builder.ins().jump(loop_header, args)` instead of `call`
4. Non-tail self-calls still use regular `call`

This transforms:
```
(defn count-down ((n : i64)) : i64
  (if (= n 0) 0 (count-down (- n 1))))
```
Into effectively:
```
loop:
  if n == 0: return 0
  n = n - 1
  goto loop
```

**Files:** `crates/weir-codegen/src/lib.rs`

#### 12.3 — Interpreter: trampoline for self-tail-calls

Convert the interpreter's recursive `eval_body()` call chain into an iterative trampoline for self-tail-calls:

1. Use the same `mark_tail_positions` analysis
2. When evaluating a self-tail-call in tail position, instead of recursing, return a `TailCall { args }` sentinel
3. The outermost `call_value()` wraps evaluation in a loop: if result is `TailCall`, rebind parameters and re-evaluate; otherwise return

This eliminates stack growth for tail-recursive functions in the interpreter.

**Files:** `crates/weir-interp/src/lib.rs`

#### 12.4 — Verify with deep recursion

Test that previously stack-overflowing programs now work:
- `(count-down 1000000)` — trivial tail recursion
- `demo-raytracer.weir` at higher resolutions without `RUST_MIN_STACK`
- Mutual recursion (not optimized yet) — verify it still works, just not optimized

**Test count target:** 8-10 new tests

---

## Phase 13: CFFI — extern "C" Support ✅

**Completed 2026-02-19.** 623 tests passing (was 616). `extern "C"` declarations now work end-to-end across the type checker, JIT codegen, AOT codegen, and interpreter. Calling extern functions requires an `unsafe` block.

Without working CFFI, Weir cannot call external C libraries (OpenGL, SDL, GLFW, etc.) and is limited to only the built-in runtime functions. CFFI is essential for the language to be useful for real programs.

### Current state

- [x] `Ty::Ptr` raw pointer type for FFI
- [x] Type checker collects extern "C" signatures, enforces `unsafe` requirement
- [x] Codegen: extern functions declared with `Linkage::Import`, JIT resolves via `dlsym`
- [x] AOT: `--link` / `-l` flag passes library link flags to `cc`
- [x] Interpreter: extern calls via `libffi` + `dlsym`
- [x] `ExprKind::Unsafe` compiles as passthrough (safety enforced at type-check time)
- [x] 4 CLI tests (interp, JIT, AOT, unsafe enforcement) + 3 typeck unit tests

### Key design decisions

- **`Ty::Ptr`** is represented as `i64` in codegen (pointer-sized), not GC-managed
- **`unsafe` enforcement** happens at the very start of `check_call`, before builtin resolution — this ensures extern functions that shadow builtins (e.g. `abs`) still require `unsafe`
- **JIT symbol resolution** uses `dlsym(RTLD_DEFAULT, ...)` to find symbols in already-loaded shared libraries
- **Interpreter** uses the `libffi` crate's `Cif` API for dynamic C function invocation
- **AOT** relies on the system linker (`cc`) with `-l` flags to resolve extern symbols

### Files changed

| File | Changes |
|------|---------|
| `crates/weir-typeck/src/types.rs` | `Ty::Ptr` variant + Display |
| `crates/weir-typeck/src/checker.rs` | Resolve `Ptr` type, collect extern C sigs, `extern_fns` set, unsafe enforcement, `ExprKind::Unsafe` depth tracking |
| `crates/weir-typeck/src/result.rs` | `extern_fns: HashSet<SmolStr>` in `TypeCheckResult` |
| `crates/weir-typeck/src/lib.rs` | Populate `extern_fns` from checker |
| `crates/weir-typeck/src/tests.rs` | 3 extern C unit tests |
| `crates/weir-codegen/src/lib.rs` | `Ty::Ptr` handling, extern function declaration (`Linkage::Import`), JIT dlsym, `ExprKind::Unsafe` passthrough |
| `crates/weir-codegen/src/aot.rs` | `link_flags` parameter for `build_executable` |
| `crates/weir-codegen/Cargo.toml` | `libc` dependency |
| `crates/weir-interp/src/lib.rs` | `Value::ExternFn`, `process_extern_c`, `call_extern_fn` via libffi |
| `crates/weir-interp/Cargo.toml` | `libffi` and `libc` dependencies |
| `crates/weir-cli/src/main.rs` | `--link` / `-l` flag on build command |
| `crates/weir-cli/tests/cli.rs` | 4 CFFI CLI tests |
| `tests/fixtures/cffi.weir` | Test fixture calling C `abs()` |
| `flake.nix` | `libffi` + `pkg-config` in buildInputs |

---

## Phase 14: General tail calls (future)

Not in immediate scope, but documenting the path forward:

- **Cranelift `return_call`**: For mutual tail calls, Cranelift supports `return_call` and `return_call_indirect`. These reuse the caller's stack frame. Requires matching calling conventions.
- **Interpreter mutual trampolining**: Extend the `TailCall` sentinel to include the target function name/value.
- **`loop`/`recur` construct**: An explicit loop form could complement implicit TCO for cases where the programmer wants guaranteed iteration semantics.

---

## Phase 15: Package System

**Status: Not started.**

Implement the package system designed in [packages.md](packages.md). This replaces manual `--cc-arg` / `-l` / `--load` flags with declarative `weir.pkg` manifests, adds multi-module compilation with real import resolution, and enables library packages (starting with weir-gl).

### Current state

| Layer | Status |
|-------|--------|
| Manifest format | Designed — S-expression `weir.pkg` |
| Parser / AST | Partial — `Import` AST node exists, `parse_import()` works |
| Type checker | Partial — `check_with_externals()` exists but ignores `Item::Import` |
| Codegen | Ignores imports — single-module only |
| CLI | File-arg only — no manifest discovery |

### Steps

#### 15.1 — `crates/weir-pkg/`: manifest parsing + dependency resolution

New crate. Parse `weir.pkg` S-expressions into `PackageManifest`. Recursively resolve path-based dependencies. Topologically sort the dependency graph (error on cycles). Produce a `ResolvedProject` with all source files, module names, native code, and link flags.

**Files:** new `crates/weir-pkg/src/lib.rs`, `crates/weir-pkg/Cargo.toml`

#### 15.2 — Multi-module parsing

Read all source files from the resolved project. Build a `ModuleIndex` (topologically ordered). For each module: prepend prelude, expand macros, parse into AST, extract top-level symbol names.

**Files:** `crates/weir-cli/src/main.rs` (or a new orchestration module)

#### 15.3 — Type checker: `check_with_imports`

New entry point that accepts imported symbols with real types. Process `Item::Import` nodes in `collect_definitions()` — look up module, add named symbols to scope. Type-check modules in topological order, threading each module's exports to its dependents.

**Files:** `crates/weir-typeck/src/lib.rs`, `crates/weir-typeck/src/checker.rs`, `crates/weir-typeck/src/result.rs`

#### 15.4 — AST merging for codegen

After all modules are type-checked, merge their ASTs into a single `Module` for codegen. Collect all native C sources and link libraries across packages. No structural changes to codegen internals needed.

**Files:** `crates/weir-codegen/src/lib.rs`, `crates/weir-codegen/src/aot.rs`

#### 15.5 — CLI integration

When `weir build/run/dev` is invoked without a file argument, discover `weir.pkg` in the current directory. Resolve the project, compile native code, and proceed with multi-module compilation. Single-file mode (with file arg) unchanged.

**Files:** `crates/weir-cli/src/main.rs`, `crates/weir-cli/Cargo.toml`

#### 15.6 — Extract weir-gl + create demos/tetris package

Create `/home/nathan/dev/weir-gl/` as a standalone library package. Create `demos/tetris/` with a `weir.pkg` that depends on weir-gl. Verify end-to-end build and run.

**Files:** external `weir-gl/` repo, `demos/tetris/weir.pkg`, `demos/tetris/tetris.weir`

#### 15.7 — Tests

- `cargo test --workspace` — all existing tests still pass
- Package manifest parsing unit tests
- Multi-module type checking tests (import resolution, unknown symbol errors)
- End-to-end: `cd demos/tetris && weir build -o ../../tmp/tetris_gl` builds and runs
- Backwards compatibility: `weir run examples/hello.weir` still works

### Acceptance criteria

- [ ] `weir.pkg` manifest parsed correctly for both library and application packages
- [ ] Dependencies resolved recursively with topological sort
- [ ] Imports type-checked with real types from dependency modules
- [ ] Multi-module programs compile and run via JIT and AOT
- [ ] `weir build` / `weir run` work with manifest discovery (no file arg)
- [ ] Single-file mode still works when file arg is provided
- [ ] weir-gl library package builds and is consumable by tetris
- [ ] All existing tests pass

---

## Verification plan

After each phase, all existing tests must continue to pass:

```
nix develop --command cargo test --workspace
```

Current baseline: 623 tests passing.

### Phase 11 acceptance criteria
- [x] `(defstruct Vec3 (x : f64) (y : f64) (z : f64))` compiles and runs via JIT and AOT
- [x] Field access `(.x v)` works in codegen
- [x] Struct destructuring in `match` works in codegen
- [x] GC correctly traces struct fields (no use-after-free with heap-pointer fields)
- [ ] Ray tracer rewritten with Vec3 structs works on all 3 paths (deferred to after TCO — deep recursion with structs needs larger stacks)

### Phase 12 acceptance criteria
- [x] Self-tail-recursive functions don't grow the stack (codegen)
- [x] Self-tail-recursive functions don't grow the stack (interpreter)
- [x] `render-col` / `render-rows` in ray tracer work without `RUST_MIN_STACK`
- [x] Higher resolution renders (320x180, 640x360) succeed
- [x] Non-tail recursion still works correctly (doesn't get incorrectly optimized)

### Phase 13 acceptance criteria
- [x] `extern "C"` declarations type-check and register function signatures
- [x] Calling extern functions without `unsafe` block produces type error
- [x] `abs(-42)` works via interpreter (libffi), JIT (dlsym), and AOT (linker)
- [x] `--link` / `-l` flag passes library names to AOT linker
- [x] `Ty::Ptr` resolves in type checker and maps to `i64` in codegen
- [x] `ExprKind::Unsafe` compiles correctly in codegen (passthrough to body)

---

## File change summary

| File | Phase 11 | Phase 12 | Phase 13 |
|------|----------|----------|----------|
| `crates/weir-typeck/src/types.rs` | — | — | `Ty::Ptr` variant |
| `crates/weir-typeck/src/result.rs` | Add `StructInfo`, populate `struct_defs` | — | `extern_fns` field |
| `crates/weir-typeck/src/checker.rs` | Export struct info in `finish()` | — | Extern C collection, unsafe enforcement |
| `crates/weir-codegen/src/lib.rs` | Struct type recognition, construction, field access, destructure | Tail-position analysis, loop-header codegen | Extern decls, JIT dlsym, Unsafe passthrough |
| `crates/weir-codegen/src/aot.rs` | Verify struct path works | — | Link flags parameter |
| `crates/weir-interp/src/lib.rs` | — | Trampoline for self-tail-calls | ExternFn value, libffi calls |
| `crates/weir-cli/src/main.rs` | — | — | `--link` / `-l` flag |
| `tests/fixtures/cffi.weir` | — | — | New fixture |
| `flake.nix` | — | — | libffi + pkg-config |
| `design/implementation-plan.md` | This document | This document | This document |
