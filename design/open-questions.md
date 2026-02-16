# Open Questions

Remaining design questions organized by topic. See individual design documents for full context and decided items.

## Design documents

- [philosophy.md](philosophy.md) — Core design principles
- [execution-model.md](execution-model.md) — Runtime, compilation, live reloading
- [memory-management.md](memory-management.md) — GC + arenas
- [type-system.md](type-system.md) — Types, inference, generics, typeclasses
- [macro-system.md](macro-system.md) — Macros and their interaction with types/reloading
- [concurrency.md](concurrency.md) — Layered concurrency model with compile-time safety
- [syntax.md](syntax.md) — Syntax decisions, type annotation style, collection literals

## Decision summary

| Area | Key decision |
|---|---|
| Philosophy | Guard rails are a feature; prevent footguns |
| Implementation language | Leaning Rust (Cranelift anchor) |
| Execution model | Long-running runtime, incremental native compilation, no image persistence |
| Live reloading | Function-level redefine-in-place, full redefinition (types/macros), soft restart |
| Reload interface | LSP protocol (primary), file watching with ~50ms debounce (fallback) |
| Memory | Tracing GC default + opt-in arenas with compile-time escape prevention |
| Type inference | Local only (Rust-style): explicit signatures, inferred locals |
| ADTs | Yes, with exhaustive pattern matching |
| Generics | Monomorphize for release, type erasure for dev mode |
| Polymorphism | Haskell-style typeclasses with HKTs, Rust-style coherence |
| Immutability | Immutable by default, explicit opt-in for mutation |
| Macros | Purely syntactic, hygienic, can generate types |
| Concurrency | Layered: structured concurrency + parallel iteration + channels + atoms |
| Thread safety | Compile-time via `Shareable` typeclass; immutable data always shareable |
| Type declarations | Both inline and separate; inline preferred |
| Function types | `(Fn [args] return)`, multi-argument (not curried) |
| Type variables | Quote prefix: `'a`, `'b` (OCaml-style) |
| Keywords | Colon prefix: `:name` (CL-style, no conflict) |
| Collection literals | `[]` for arrays/vectors, `{}` for maps |
| String formatting | `format` macro (compile-time checked) + `str` function |
| Record access | Coalton-style `.field` accessor functions |

---

---

---

## Open: Smaller questions by area

### Execution model
- Compilation unit for incremental recompilation (per-function vs per-module)
- Dev/release behavioral equivalence guarantees
- Cascade communication UX (LSP inline errors? Runtime warnings?)

### Memory management
- Starting GC algorithm (mark-and-sweep vs generational)
- Named arenas (multiple concurrent arenas)
- Arena size configuration
- Arena nesting rules

### Type system
- Type alias support
- Functional dependency syntax in S-expressions
- Kind system design (at least `*` and `* -> *`)
- Numeric typeclass hierarchy
- Interior mutability patterns vs whole-binding mutability

### Macro system
- Macro definition syntax/API (quasiquoting? pattern-based? procedural?)
- Hygiene escape hatch mechanism
- How much language is built via macros vs primitives
- Macro expansion caching and invalidation
- Side effects during expansion policy

### Syntax
- Exact `deftype` syntax: unified form for structs/enums, or separate?
- Named/keyword arguments in function calls
- Module/namespace system (`module`, `import`, qualified names)
- Destructuring in let bindings and function parameters
- Control flow forms (`cond`, `if`, `when`)
- Pipe/threading operator specifics
- Whether `the` (inline type annotation on expressions) is needed
- Block expressions / implicit `progn`

---

## Decision dependencies

```
Philosophy
    │
    ▼
Execution Model ──→ Memory Management ──→ Type System ──→ Macro System
    │                                         │
    └──→ Live Reloading                       └──→ Concurrency Model
```

Most foundational decisions are now made. The remaining open areas (concurrency, syntax, and smaller questions) are less tightly coupled and can be addressed more independently.
