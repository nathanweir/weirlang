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
- [ ] **Interpreter** (`weir-interp`): evaluate AST directly
  - Values: integers, floats, booleans, strings, Unit, closures, ADT constructors/instances, arrays, maps
  - Evaluation: function calls, let bindings, if/cond/when/unless, match (with exhaustiveness warning, not yet enforced), basic arithmetic/comparison/boolean ops
  - Environment: lexical scoping, `mut` bindings with `set!`
  - Built-in functions: `+`, `-`, `*`, `/`, `<`, `>`, `=`, `println`, `print`, `str`
- [ ] **End-to-end tests**: `.weir` fixtures → expected output
- [ ] **CLI**: `weir run <file>` — parse and interpret
- [ ] Verify: can run non-trivial programs (fibonacci, factorial, simple data structures)
- [ ] **Milestone 0 demo**: a Weir program that defines functions, uses let bindings, does arithmetic, prints results

### Phase 3: Type checker
- [ ] **Type checker** (`weir-typeck`):
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
- [ ] **Snapshot tests**: type error messages, inferred types
- [ ] **CLI**: `weir check <file>` — parse and type-check without running
- [ ] Verify: type errors produce clear, located diagnostics

### Phase 4: Lowered IR + Cranelift codegen
- [ ] **IR** (`weir-ir`): lowered intermediate representation
  - Desugar pattern matching into decision trees
  - Flatten nested expressions
  - Normalize control flow
  - Explicit temporary variables
- [ ] **Codegen** (`weir-codegen`): translate IR to Cranelift CLIF
  - Use `cranelift-frontend` for SSA construction
  - Use `cranelift-jit` for JIT module
  - Function compilation → native function pointer
  - Calling convention: standard C ABI initially
  - All numeric types and arithmetic
  - Function calls, let bindings, if/cond, basic control flow
- [ ] **Test oracle**: interpreter and compiled code produce identical output for all fixtures
- [ ] **CLI**: `weir run <file>` now compiles and runs natively (interpreter available via `weir interp <file>`)
- [ ] Verify: compiled programs produce correct results, match interpreter

### Phase 5: Function table + basic live reload
- [ ] **Runtime** (`weir-runtime`):
  - Function pointer table with indirect dispatch
  - All calls go through the table (one extra indirection per call)
  - File watcher (`notify` crate, ~50ms debounce)
  - On file change: re-parse, re-typecheck, re-compile changed functions, swap pointers
  - Simple dependency tracking: if a function signature changes, recompile callers
  - Error reporting: if recompilation fails, keep old version, print error
- [ ] **CLI**: `weir dev <file>` — compile, run, watch for changes, live reload
- [ ] **Milestone 1 demo**: a program running a loop; edit a function; the running program uses the new version without restart
- [ ] Verify: live reload works for body-only changes and signature changes

### Phase 6: Tree-sitter grammar + basic LSP
- [ ] **Tree-sitter grammar** (`tree-sitter-weir/`):
  - Complete grammar covering all Weir syntax
  - Highlighting queries (`.scm` files)
  - Bracket/indent queries
  - Test corpus (tree-sitter's built-in test format)
- [ ] **LSP** (`weir-lsp`):
  - Tier 1 features (syntax-only):
    - Diagnostic reporting (parse errors, type errors)
    - Semantic token highlighting
    - Document symbols (outline)
    - Go-to-definition (same file, via name resolution)
    - Hover (show type of symbol)
  - Reference: adapt patterns from `/home/nathan/dev/clef` (CL language server)
  - Use `tower-lsp` + `lsp-types`
- [ ] **Zed extension**: create `zed-weir` extension (adapt from `/home/nathan/dev/zed-common-lisp`)
  - `extension.toml` with language/grammar/LSP config
  - Point to tree-sitter-weir grammar
  - Configure LSP launch
- [ ] Verify: open a `.weir` file in Zed, see syntax highlighting, get type errors inline

### Phase 7: Macros
- [ ] **Macro expander** (`weir-macros`):
  - `defmacro` syntax
  - Quasiquoting (backtick, unquote, splice)
  - Hygienic expansion (alpha-renaming with gensyms)
  - Insert into pipeline: parse → **expand** → typecheck → compile
  - Expansion short-circuit: compare output structurally, skip downstream if unchanged
- [ ] **Built-in macros**: `format`, `->`, `->>`
  - `format`: compile-time arity checking, Show constraint verification
  - `->` / `->>`: threading transformation
- [ ] **Update LSP**: macro expansion preview
- [ ] **Snapshot tests**: macro expansion output
- [ ] Verify: user-defined macros work, built-in macros work, hygiene prevents capture

### Phase 8: Generics + typeclasses
- [ ] **Generics** (in `weir-typeck`):
  - Parametric polymorphism with type variables
  - Type variable binding and unification
  - Dev mode: type erasure (boxed representation)
  - Generic function instantiation
- [ ] **Typeclasses**:
  - `defclass` / `instance` syntax and semantics
  - Single-parameter typeclasses first
  - Dictionary passing (dev mode) or monomorphization (release mode)
  - Core typeclasses: `Eq`, `Ord`, `Show`, `From`
  - Coherence checking (Rust-style: only defining module can write instance)
  - `Shareable` typeclass (auto-derived based on type contents)
- [ ] **HKTs** (if time permits, can defer):
  - Kind system (`*`, `* -> *`)
  - `Functor`, `Foldable`, etc.
- [ ] Verify: generic functions work, typeclass dispatch works, coherence errors are caught

### Phase 9: GC + arenas
- [ ] **Tracing GC** (in `weir-runtime`):
  - Replace leak-everything strategy (used in earlier phases)
  - Start with mark-and-sweep, stop-the-world
  - GC-managed heap for all Weir values
  - Root set: stack, global function table, live closures
  - Suppressible (for game loop critical sections)
  - Manually triggerable
- [ ] **Arenas**:
  - `with-arena` block support
  - Bump allocation via `bumpalo`
  - Compile-time escape analysis in type checker
  - Arena-scoped values cannot escape block, be stored in GC heap, or cross threads
- [ ] Verify: GC collects unreachable objects, arenas free on block exit, escape analysis catches violations

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
- [ ] **Result + ? operator**: error propagation in codegen
- [ ] Verify: cascade works for all change types, concurrency primitives are thread-safe

## Milestone summary

| Milestone | Phase | What you can show |
|---|---|---|
| **M0: "It runs"** | After Phase 2 | Parse and interpret Weir programs with functions, let, if, arithmetic |
| **M1: "It compiles"** | After Phase 4 | Programs compile to native code via Cranelift and run |
| **M2: "Live reload"** | After Phase 5 | Edit a function → running program uses new version |
| **M3: "Editor support"** | After Phase 6 | Syntax highlighting, inline errors, type hover in Zed |
| **M4: "Real language"** | After Phase 8 | Generics, typeclasses, macros — write non-trivial programs |
| **M5: "Production-ready runtime"** | After Phase 10 | GC, arenas, concurrency, full cascade — the complete vision |

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
