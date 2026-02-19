# Weir Implementation Roadmap

## Context

Weir is a statically-typed Lisp with live reloading, designed for game development. The design phase is complete — 7 design documents cover philosophy, execution model, memory management, type system, macro system, concurrency, and syntax. No implementation code exists yet. This plan covers the path from zero to a working language.

## Guiding principles

- **End-to-end first, depth later.** Get a minimal program running through the full pipeline as early as possible, then deepen each component.
- **Tree-walking interpreter before Cranelift.** Validate language semantics quickly without codegen complexity. The interpreter becomes a test oracle for the compiled version.
- **Test from day one.** Snapshot tests (via `insta`) for every compiler phase. End-to-end tests that run `.weir` files and check output.
- **Design for incrementality.** Live reloading is a core feature. Use ID-based AST nodes, intern strings, and keep compiler phases as pure functions from the start.

## Technology choices

| Component | Crate/tool | Why |
|---|---|---|
| Lexer | `logos` | Fast, proc-macro based lexer generation |
| Parser | Hand-written recursive descent | S-expressions are simple; hand-written gives best error messages and full control |
| Syntax tree | `rowan` | Lossless CST for LSP support (incremental reparsing, error recovery). Invest upfront to avoid parser rewrite later |
| String interning | `string-interner` or `smol_str` | Efficient identifier handling, fast equality |
| AST node IDs | `la-arena` | Typed index arenas for ID-based AST (enables incremental compilation) |
| Error reporting | `miette` | Rich diagnostics with source spans, help text |
| Snapshot testing | `insta` | Standard for Rust compiler testing; `cargo-insta` for review workflow |
| Property testing | `proptest` | Fuzz the parser, test type system invariants |
| JIT codegen | `cranelift-codegen`, `cranelift-jit`, `cranelift-frontend` | Dev-mode native compilation |
| LSP server | `tower-lsp` + `lsp-types` | Async LSP framework |
| Arena allocator | `bumpalo` | For Weir's `with-arena` blocks |
| Incremental computation | `salsa` (evaluate later) | Could underpin both LSP and live-reload cascade; assess after Phase 3 |

## Project structure

```
weir/
  flake.nix              # Nix dev environment (Rust toolchain, just, etc.)
  .envrc                 # direnv integration
  Justfile               # Dev commands (build, test, run, etc.)
  weir.toml              # Future: project config for Weir projects
  Cargo.toml             # Workspace root
  crates/
    weir-lexer/          # Tokenizer
    weir-parser/         # Parser → CST/AST
    weir-ast/            # AST type definitions, IDs, interning
    weir-typeck/         # Type checker
    weir-ir/             # Lowered IR
    weir-interp/         # Tree-walking interpreter (early phases + test oracle)
    weir-codegen/        # Cranelift JIT backend
    weir-runtime/        # GC, function table, live reload machinery
    weir-macros/         # Macro expander
    weir-lsp/            # Language server
    weir-cli/            # CLI entry point (weir run, weir build, weir repl)
  tests/
    fixtures/            # .weir source files for testing
    parser/              # Parser snapshot tests
    typechecker/         # Type checker snapshot tests
    integration/         # End-to-end run-and-check tests
  tree-sitter-weir/      # Tree-sitter grammar (for editor tooling)
```

Cargo workspace with small, focused crates. Each compiler phase has clear inputs and outputs. Avoids the "god module" pitfall (Carp's biggest technical debt).

## Test strategy

### Snapshot tests (`insta`)
Every compiler phase gets snapshot tests:
- **Parser:** source → AST pretty-print
- **Type checker:** source → typed AST / type error messages
- **IR:** source → lowered IR dump
- **Codegen:** source → CLIF IR dump (optional, for debugging)

### End-to-end tests
`.weir` files in `tests/fixtures/` with expected output:
```
tests/fixtures/
  arithmetic.weir        # (+ 1 2) → 3
  functions.weir         # defn, function calls
  type-errors/           # files that SHOULD fail type checking
    mismatched-return.weir
    unknown-variable.weir
```

Test runner compiles and executes each fixture, compares output. Also verifies that type-error files produce the expected error.

### Property tests (`proptest`)
- Any input to the lexer/parser either produces a valid result or a clean error (never panics)
- Type inference is idempotent
- Interpreter and compiled code produce the same results (once both exist)

### CI
Run `just test` on every commit. Tests must pass before merging.

## Phases

### Phase 0: Project scaffold
- [x] Create `flake.nix` with Rust toolchain (stable), `just`, and dev tools
- [x] Create `.envrc` for direnv
- [x] Create `Justfile` with recipes: `build`, `test`, `run`, `clean`, `check`, `fmt`, `clippy`
- [x] Create Cargo workspace with initial crates: `weir-lexer`, `weir-parser`, `weir-ast`, `weir-cli`
- [x] Set up `insta` for snapshot testing
- [ ] Create initial tree-sitter grammar skeleton (`tree-sitter-weir/`) — *skipped for now; not needed until Phase 6 (editor tooling)*
- [x] Verify: `just build` compiles, `just test` runs (even if no real tests yet)

### Phase 1: Lexer + Parser
- [x] **Lexer** (`weir-lexer`): tokenize Weir source into token stream
  - Tokens: `(`, `)`, `[`, `]`, `{`, `}`, integers, floats, strings, symbols, keywords (`:name`), type variables (`'a`), comments (`;`), `?`, `.field`
  - Use `logos` for token definitions
  - Track source spans for error reporting
- [x] **AST** (`weir-ast`): define AST node types
  - Top-level: `Defn`, `Deftype`, `Defstruct`, `Defclass`, `Instance`, `Import`, `Declare`, `ExternC`
  - Expressions: `Lit`, `Var`, `Call`, `Let`, `If`, `Cond`, `When`, `Unless`, `Match`, `Fn`, `Do`, `Set!`, `Ann`, `FieldAccess`, `VectorLit`, `MapLit`, `Thread`, `Unsafe`
  - Patterns: `Wildcard`, `Var`, `Constructor`, `Literal`, `StructDestructure`
  - Types: `Named`, `Fn`, `Applied`, `TypeVar`, `Constrained`
  - Use `la-arena` for ID-based nodes, `smol_str` for identifiers
- [x] **Parser** (`weir-parser`): hand-written recursive descent
  - Parse S-expressions into AST
  - Error recovery: on parse error, skip to next top-level form
  - Track source spans on all nodes
- [x] **Snapshot tests**: parse fixtures, snapshot AST pretty-print (18 snapshot tests)
- [ ] **Property tests**: parser never panics on arbitrary input — *deferred; will add with `proptest`*
- [x] **CLI** (`weir-cli`): `weir parse <file>` — dump parsed AST
- [x] Verify: can parse the example code from design/syntax.md (via `syntax-showcase.weir` fixture)

### Phase 2: Tree-walking interpreter
- [x] **Interpreter** (`weir-interp`): evaluate AST directly
  - Values: integers, floats, booleans, strings, Unit, closures, ADT constructors/instances, arrays, maps
  - Evaluation: function calls, let bindings, if/cond/when/unless, match (with exhaustiveness warning, not yet enforced), basic arithmetic/comparison/boolean ops
  - Environment: lexical scoping, `mut` bindings with `set!`
  - Built-in functions: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `println`, `print`, `str` (plus `mod`, `<=`, `>=`, `!=`, `not`, `and`, `or`, `len`, `nth`, `append`, `type-of`)
- [x] **End-to-end tests**: `.weir` fixtures → expected output (5 fixture files with snapshot tests)
- [x] **CLI**: `weir run <file>` — parse and interpret
- [x] Verify: can run non-trivial programs (fibonacci, factorial, closures, ADTs, structs, pattern matching)
- [x] **Milestone 0 demo**: `tests/fixtures/milestone0-demo.weir` — functions, let bindings, mutation, arithmetic, ADTs, pattern matching, closures

### Phase 3: Type checker
- [x] **Type checker** (`weir-typeck`):
  - Phase 1: Check explicitly annotated code (no inference yet)
    - Verify function arg/return types match
    - Verify let binding types match
    - Type-check arithmetic, comparisons, function calls
  - Phase 2: Local type inference
    - Unification-based inference within function bodies
    - Infer let binding types, closure parameter types, intermediate expressions
    - Numeric literal defaults (unadorned int → i32, float → f64)
  - Phase 3: ADT type checking
    - `deftype` and `defstruct` type definitions
    - Constructor type checking
    - Pattern match type checking with exhaustiveness enforcement
  - All numeric types: i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
  - Error reporting via `miette` with source spans
- [x] **Snapshot tests**: type error messages, inferred types
- [x] **CLI**: `weir check <file>` — parse and type-check without running
- [x] Verify: type errors produce clear, located diagnostics

### Phase 4: Lowered IR + Cranelift codegen
- [ ] **IR** (`weir-ir`): lowered intermediate representation — *deferred; AST lowers directly to Cranelift IR for the current numeric/control-flow subset. A dedicated IR will be introduced when pattern matching decision trees require it.*
  - Desugar pattern matching into decision trees
  - Flatten nested expressions
  - Normalize control flow
  - Explicit temporary variables
- [x] **Codegen** (`weir-codegen`): AST → Cranelift IR (direct lowering via `FunctionBuilder`)
  - Use `cranelift-frontend` for SSA construction
  - Use `cranelift-jit` for JIT module
  - Function compilation → native function pointer
  - Calling convention: SystemV ABI
  - All numeric types (i8–i64, u8–u64, f32, f64), booleans, unit, and arithmetic
  - Function calls (including mutual recursion), let bindings, if/cond/when/unless, do blocks, set!
  - `println`/`print` via extern "C" runtime helpers with thread-local output buffer
  - Expression type map (`expr_types`) added to `weir-typeck` for codegen type resolution
- [x] **Test oracle**: interpreter and compiled code produce identical output (7 oracle tests + 4 fixture oracle tests)
- [x] **CLI**: `weir run <file>` compiles and runs natively via Cranelift JIT; `weir interp <file>` runs via interpreter
- [x] Verify: 45 codegen tests pass, all 183 workspace tests pass, clippy clean

### Phase 5: AOT compilation, function table + basic live reload
- [x] **AOT compilation** (`weir-codegen`): produce standalone native binaries
  - Switch from `JITModule` to `ObjectModule` to emit `.o` object files
  - Small runtime library embedded as C source in `weir-codegen` (no separate `weir-runtime` crate yet)
  - Link via system linker (`cc`) to produce executable
  - Same Cranelift IR generation as JIT path — just a different module backend
  - Note: LLVM backend deferred; Cranelift codegen quality is sufficient for gamedev workloads where hot paths are arenas/GC-controlled (Phases 9–10) and rendering is GPU-bound. Revisit if profiling shows codegen is the bottleneck.
- [x] **CLI**: `weir build <file>` — compile to standalone binary
- [x] **Runtime** (embedded in `weir-codegen` via `DevSession`):
  - Function pointer table with indirect dispatch (`AtomicPtr` array with atomic swap)
  - All calls go through the table (one extra indirection per call)
  - File watcher (`notify` crate)
  - On file change: re-parse, re-typecheck, re-compile changed functions, swap pointers
  - Error reporting: if recompilation fails, keep old version, print error
- [x] **CLI**: `weir dev <file>` — compile, run, watch for changes, live reload
- [ ] **Milestone 1 demo**: a program running a loop; edit a function; the running program uses the new version without restart — *not documented yet, but `weir dev` is functional*
- [x] Verify: `weir build` produces working binaries; live reload works for body-only changes (8 AOT tests + 5 dev-session tests)

### Phase 6: Tree-sitter grammar + LSP
- [x] **Tree-sitter grammar** (`tree-sitter-weir/`):
  - Complete grammar covering all Weir syntax
  - Highlighting queries (`.scm` files)
  - Bracket/indent queries
  - Test corpus (tree-sitter's built-in test format, 4 test files)
- [x] **LSP** (`weir-lsp`):
  - Uses `tower-lsp` + `lsp-types`, full-text document sync
  - Analysis pipeline: macro expansion → parse → typecheck → cache per document
  - [x] Diagnostic reporting (parse errors, type errors, macro expansion errors)
  - [x] Document symbols / outline (defn, deftype, defstruct, defclass, instances with nested children)
  - [x] Go-to-definition (same file + cross-file via workspace index)
  - [x] Hover (expression types from typeck, function signatures, docstrings)
  - [x] Completion (user-defined items + builtins + types + keywords, scope-aware locals + cross-file workspace symbols)
  - [x] Find references (same file + cross-file, scope-aware with shadowing)
  - [x] Rename (same file + cross-file, scope-aware with prepare_rename support)
  - [x] Inlay hints (unannotated params, return types, let bindings, lambdas; range-aware)
  - [x] Document formatting (Wadler-Lindig pretty printer, 80-col, comment-preserving)
  - [x] Semantic token highlighting (15 token types, delta encoding)
  - [x] Signature help (parameter hints, active parameter tracking, return types)
  - [x] Cross-file support (workspace discovery, cross-file goto-def/references/rename/completion, external symbol awareness in type checker)
  - [x] Workspace symbol search (case-insensitive substring matching)
  - [ ] Code actions — *not implemented*
- [x] **Zed extension**: create `zed-weir` extension (adapt from `/home/nathan/dev/zed-common-lisp`)
  - `extension.toml` with language/grammar config
  - Point to tree-sitter-weir grammar
  - Syntax highlighting, brackets, outline queries
  - [x] Configure LSP launch (`weir lsp` via extension)
- [x] Verify: open a `.weir` file in Zed, see syntax highlighting, get type errors inline, hover, inlay hints, completion

### Phase 7: Macros
- [x] **Macro expander** (`weir-macros`):
  - `defmacro` syntax with required and `&rest` parameters
  - Quasiquoting (backtick, unquote, splice)
  - Hygienic expansion (alpha-renaming with gensyms)
  - Insert into pipeline: lex → read(SExpr) → **expand** → flatten → parse → typecheck → compile
  - [ ] Expansion short-circuit: compare output structurally, skip downstream if unchanged — *deferred*
- [x] **Built-in macros**: `->`, `->>`
  - `->` / `->>`: threading transformation (replaced hardcoded AST nodes)
  - [ ] `format`: compile-time arity checking, Show constraint verification — *deferred; needs typeclasses (Phase 8)*
- [ ] **Update LSP**: macro expansion preview — *blocked on `weir-lsp` crate*
- [x] **Tests**: 22 unit tests + end-to-end fixture (`tests/fixtures/macros.weir`)
- [x] **CLI**: `weir expand <file>` — show expanded source for debugging
- [x] Verify: user-defined macros work, built-in macros work, hygiene prevents capture, 183 workspace tests pass

### Phase 8: Generics + typeclasses
- [x] **Generics** (in `weir-typeck`):
  - Parametric polymorphism with type variables (`'a`)
  - Type variable binding and unification via `FnScheme` with `instantiate()`
  - Generic function instantiation at call sites
- [x] **Typeclasses**:
  - `defclass` / `instance` syntax and semantics
  - Single-parameter typeclasses with method signatures and constraints
  - Interpreter: runtime dispatch via type tags
  - Codegen: monomorphization (specialization collection + mangled names)
  - Constraint resolution with deferred checking
  - [x] Core typeclasses beyond Eq/Show: `Ord`, `From` — *done in Phase 8b*
  - [x] Coherence checking (Rust-style: only defining module can write instance) — *done in Phase 8b*
  - [ ] `Shareable` typeclass (auto-derived based on type contents) — *deferred; needs concurrency (Phase 10)*
- [x] **HKTs**:
  - Kind system (`TyKind::Star`, `TyKind::Arrow`)
  - `Ty::App` for type application (`('f 'a)`)
  - Kind inference from method signatures
  - Kind checking in instance declarations
  - Functor example working (interpreter)
- [x] Verify: 322 tests pass, generic functions work in interpreter + codegen, typeclass dispatch works, HKTs work, oracle tests confirm interpreter/codegen agreement

### Phase 8b: Independent deferred items
Items deferred from earlier phases that don't depend on the runtime (Phase 9) or concurrency (Phase 10).

- [x] **Core typeclasses: `Ord`, `From`** — `Ord` in prelude with instances for all numeric types + String; `<`/`>`/`<=`/`>=` defer Ord constraints and dispatch through typeclass for custom types. `From` implemented as multi-parameter typeclass.
- [x] **Typeclass coherence checking** — orphan rule + duplicate instance detection implemented.
- [x] **`Result` type + `?` operator** — `Result` in prelude; `?` desugars to early return on `Err` across parser, typechecker, interpreter, and codegen.
- [x] **Property tests (`proptest`)** — parser, typechecker, interpreter, and codegen all have proptest suites: fuzz inputs (never panic), determinism checks, interpreter/codegen agreement on generated arithmetic and programs.

### Phase 9: GC + arenas
- [x] **Tracing GC** (in `weir-runtime`):
  - Replaced leak-everything strategy with mark-and-sweep, stop-the-world GC
  - Object header at negative offset (32 bytes: next, mark, shape, user_size) — existing pointer offsets unchanged
  - Shape descriptors for type-precise scanning: `SHAPE_FIXED` (bitmask) for closures, `SHAPE_VECTOR` (runtime length) for vectors
  - Shadow stack for root scanning — compiler pushes/pops heap-typed locals at function entry/exit, let scopes, and early returns
  - GC-managed heap for closures, vectors, and dynamic strings
  - Root set: shadow stack (pointers to stack slots holding heap pointers)
  - Collection triggered on allocation when byte threshold exceeded
  - Full AOT C runtime with matching GC implementation
  - 524 tests pass (519 original + 5 new GC stress tests)
- [ ] **Arenas** (deferred to Phase 9b):
  - `with-arena` block support
  - Bump allocation via `bumpalo`
  - Compile-time escape analysis in type checker
  - Arena-scoped values cannot escape block, be stored in GC heap, or cross threads
- [ ] Verify arenas: arenas free on block exit, escape analysis catches violations

### Phase 10: Full cascade + concurrency
- [ ] **Dependency tracker**:
  - Track expansion, type, and call dependencies
  - Full cascade: macro change → re-expand → re-typecheck → recompile
  - Type change → recompile dependents + soft restart communication
  - Short-circuit: skip cascade when expansion output unchanged
- [ ] **Concurrency primitives**:
  - `spawn` / `with-tasks` (structured concurrency)
  - `par-map` / `par-for-each` (parallel iteration)
  - `channel` (typed channels)
  - `atom` (compare-and-swap)
  - `Shareable` enforcement at spawn/send boundaries
- [x] **Result + ? operator**: error propagation in codegen — *done in Phase 8b*
- [ ] Verify: cascade works for all change types, concurrency primitives are thread-safe

## Milestone summary

| Milestone | Phase | What you can show |
|---|---|---|
| **M0: "It runs"** | Phase 2 | Parse and interpret Weir programs with functions, let, if, arithmetic | **Done** |
| **M1: "It compiles"** | Phase 4 | Programs compile to native code via Cranelift JIT and run | **Done** |
| **M2: "Standalone binary + live reload"** | Phase 5 | `weir build` produces native binaries; `weir dev` enables live reload | **Done** |
| **M3: "Editor support"** | Phase 6 | Syntax highlighting, inline errors, type hover in Zed | **Done** (LSP: diagnostics, hover, goto-def, references, rename, completion, inlay hints, formatting, semantic tokens, signature help, cross-file support, workspace symbol search; Zed: tree-sitter highlighting + LSP) |
| **M4: "Real language"** | Phase 8 | Generics, typeclasses, macros — write non-trivial programs | **Done** (macros, generics, typeclasses, HKTs all working) |
| **M5: "Production-ready runtime"** | Phase 10 | GC, arenas, concurrency, full cascade — the complete vision | Not started |

## Verification strategy

Every phase ends with a verification step. The test suite should be runnable via `just test` at all times. Specific verification approaches:

1. **Snapshot tests** (`insta`): parser output, type checker output, IR output, error messages
2. **End-to-end tests**: `.weir` fixtures with expected output, run via both interpreter and compiler
3. **Property tests** (`proptest`): parser robustness, type system invariants
4. **Test oracle**: interpreter and compiler must agree on all fixtures (from Phase 4 onward)
5. **Regression tests**: every bug fix gets a test case
6. **CI**: `just test` runs all tests; `just check` runs clippy + fmt + test

## Key pitfalls to avoid

1. **Don't build the runtime before the language.** Start with a tree-walking interpreter. Get semantics right first.
2. **Don't shape the IR around Cranelift.** The IR should represent Weir's semantics. Lowering to CLIF is a separate pass.
3. **Don't skip incrementality.** Use ID-based AST nodes and pure compiler phases from day one — bolting incrementality onto a batch compiler later is painful.
4. **Don't defer macro hygiene.** Implement hygiene from the start, even if simple. Adding it later breaks existing macros.
5. **Don't couple dev mode and release mode.** Share everything up to IR, diverge only at codegen.
6. **Don't start with a complex GC.** Mark-and-sweep, stop-the-world. Generational/incremental are optimizations for later.

## Files to create in Phase 0

- `flake.nix` — Nix dev environment
- `.envrc` — direnv hook
- `Justfile` — task runner
- `Cargo.toml` — workspace root
- `crates/weir-lexer/Cargo.toml` + `src/lib.rs`
- `crates/weir-parser/Cargo.toml` + `src/lib.rs`
- `crates/weir-ast/Cargo.toml` + `src/lib.rs`
- `crates/weir-cli/Cargo.toml` + `src/main.rs`
- `tree-sitter-weir/grammar.js` — initial grammar skeleton

## Reference projects

- `/home/nathan/dev/clef` — CL language server (Common Lisp, SBCL). Reference for LSP architecture, JSON-RPC handling, diagnostic reporting. Uses tree-sitter for parsing.
- `/home/nathan/dev/zed-common-lisp` — Zed extension (Rust → WASM). Reference for extension structure, `extension.toml`, tree-sitter integration, LSP configuration. Uses `flake.nix` + `fenix` for Rust nightly + WASM target.
