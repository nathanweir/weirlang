# Execution Model

## Summary of decisions

| Decision | Status |
|---|---|
| Implementation language | Leaning Rust (Cranelift as anchor) |
| Runtime model | Long-running process with incremental native compilation |
| Image persistence | No — starts from source every session |
| Live reloading granularity | Function-level redefine-in-place |
| Full redefinition (types, macros) | Yes, with clear developer communication |
| Stale instance handling | Soft restart with communication |
| Dev-mode code generation | Cranelift (optimized for fast compilation) |
| Release-mode code generation | AOT compilation, potentially LLVM for better optimization |
| Reload interface | LSP protocol (primary), file system watching with ~50ms debounce (fallback) |

## Architecture

A long-running runtime process with an incremental native compiler. Closer to Erlang's hot code loading than SBCL's image model.

Components:
- **Runtime process** that stays alive for the duration of a dev session
- **Incremental compiler** (using Cranelift) that compiles individual functions to native code at runtime
- **Function table** with indirect dispatch — all calls go through a table of function pointers; swapping a function means updating one pointer
- **Dependency tracker** that knows which functions call which, which functions use which types, and which code was produced by which macro — so changes cascade correctly
- **Program always starts from source** (no image files to manage), but once running, individual definitions can be redefined live
- **Release builds** use a separate AOT compilation path producing standalone binaries

### Why no image persistence

Full image save/restore (serializing the heap to disk) is a historical artifact of Lisp machines (1960s-80s) where the Lisp environment was the operating system itself. Modern tooling (git, Nix, CI/CD) handles reproducibility better. Smalltalk/Pharo still uses images and it's one of the ecosystem's biggest pain points (version control, merging, corruption). The valuable part of the CL model is the *live interaction*, not the persistence.

### Why Rust (leaning)

The strongest argument for Rust is **Cranelift** — a Rust-native code generation library (from Wasmtime) designed for fast runtime code generation. Cranelift prioritizes compilation speed over maximum optimization, which is exactly right for dev-mode live reloading. For release builds, LLVM (which also has Rust bindings via `inkwell`) could provide better optimization.

The main cost: the runtime core (heap management, function table, executable memory) will require substantial `unsafe` code, since these are fundamentally mutable aliased pointer graphs. The parser, type-checker, and LSP (majority of the codebase) benefit enormously from Rust.

Open to reconsidering if a compelling alternative surfaces, but Cranelift is a strong anchor.

## Live reloading cascade model

The compilation pipeline: `parse → macro expand → type check → compile to native`

Each kind of redefinition triggers a different cascade:

| What changed | What happens |
|---|---|
| Function body (same signature) | Recompile that function only, swap pointer |
| Function signature | Re-typecheck + recompile all callers |
| Type definition | Recompile all functions using the type + soft restart for live instances |
| Macro definition | Re-expand all call sites → may change types/signatures → cascades further |

The dependency graph tracks relationships across layers:
- **Expansion dependencies**: which code was produced by which macro
- **Type dependencies**: which functions use which types
- **Call dependencies**: which functions call which functions

### Short-circuit optimization

When re-expanding a macro call produces the same output as before, the downstream cascade is skipped entirely. The dependency tracker compares expansion outputs structurally. This is critical for performance.

### Error behavior

A failed cascade is a build error, not a crash:
- The running program keeps using the **last-known-good** code
- Errors are reported to the developer via the LSP connection
- The game keeps running — a build error doesn't kill the process

### Reload interface and batching

**Primary: LSP reload protocol.** The editor communicates changes to the runtime via the LSP connection. A workspace-wide rename is a single "workspace edit" in LSP, processed as one batch/cascade.

**Fallback: file system watching.** ~50ms debounce catches multi-file writes. Tunable.

**Cascade-in-progress:** If a new change arrives during a cascade, it's queued. Current cascade finishes, then a new cascade runs for queued changes. No cancellation of in-progress work.

### Soft restart for type redefinition

When a type is redefined and live instances exist with the old layout:
1. All dependent functions are re-typechecked and recompiled (cascade)
2. Pattern match exhaustiveness is re-checked
3. The runtime reports: "type `Player` was redefined, N live instances need reinitialization"
4. The developer triggers a state reset (reload level, respawn entities, etc.)
5. The game loop keeps running; you re-enter it with fresh state

The system **never silently runs with stale data**.

## Implementation challenges

### Runtime-resident compiler
Cranelift handles native code generation. Remaining work: managing executable memory pages (`mmap` with `PROT_EXEC`), handling relocations, coordinating compilation with the running program.

### Indirect function dispatch
One extra pointer dereference per call. Negligible cost — SBCL does this and is fast.

### Code garbage collection
Old compiled code stays in memory after redefinition. Can be deferred initially (just leak; a dev session won't generate enough orphaned code to matter). Worth implementing for long sessions.

## Prior art

- **Erlang/BEAM** — hot code loading, no persistent images. Closest model to our approach.
- **SBCL** — full image model with native compilation. More powerful but more complex.
- **Julia** — JIT via LLVM, REPL-driven, starts from source. Similar vibe, JIT-based.

## Open questions

- What's the compilation unit for incremental recompilation? Per-function is most flexible but more dependency tracker overhead.
- How important is it that dev mode and release mode produce identical behavior?
- How should the cascade recompiler communicate with the developer? (LSP inline errors? Runtime warnings? Dedicated UI?)
- FFI decided: C FFI via `extern "C"` + `unsafe` blocks; safe Rust-implemented wrappers for stdlib. See syntax.md.
## REPL and interactive development

The REPL is not architecturally special — it's another client of the same runtime that the editor/LSP uses. The runtime already accepts code, compiles it via Cranelift, and executes it. The REPL is a text interface to that mechanism; the editor (via LSP) is a richer one.

- The REPL acts as an implicit module. Definitions accumulate during the session.
- `defn` at the REPL requires type signatures, same as in source files.
- Bare expressions are evaluated with full local type inference — `(+ 1 2)` just works. This isn't a REPL-specific relaxation; it's how inference already works for expressions everywhere.
- Definitions entered at the REPL participate in the same live-reloading cascade as file-based code.

### Tooling

Primary interactive development interface is the **editor + LSP**, not the REPL. The REPL is a complement, not the centerpiece (unlike the Emacs/SLIME model). Planned tooling:

- **LSP language server** — primary interface for live reloading, errors, completions
- **Editor extensions** (Zed initially) — first-class editor support from day one
- **DAP debugger** — under consideration for interactive debugging
- **REPL** — available for exploration, testing, and scripting
