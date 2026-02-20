# WASM Output Backend for Weir

## Context

Weir currently compiles to native code via Cranelift (JIT for `run`/`dev`, AOT for `build`). The goal is to add WASM as a general-purpose compilation target so any Weir program can optionally run in the browser. The Tetris demo (`demos/tetris/`) serves as the testbed to validate the end-to-end pipeline, but the WASM backend is a first-class compiler feature available to all Weir projects.

The approach uses direct WASM binary emission via the `wasm-encoder` crate (not Cranelift, which reads WASM but cannot write it). The existing ~60 runtime helpers get split into portable (compiled to WASM) and platform-dependent (provided via JS imports). A new `(target ...)` special form enables compile-time platform switching in Weir source code.

---

## Phase 1: `(target ...)` Special Form & Package System Support

### 1a. Parser: `target` special form

`Target` added to `ExprKind` enum. Parsed as `(target (:native expr) (:wasm expr))`. Each branch is a keyword + expression pair.

### 1b. Macro expander: resolve `target` early

During macro expansion, `target` forms are resolved based on `CompileTarget` enum threaded through the expander. Replace the `Target` node with the matching branch's expression. Non-matching branches are discarded.

### 1c. Thread target through the pipeline

`CompileTarget` enum (`Native`, `Wasm`) added to the expansion context. CLI passes it based on subcommand.

### 1d. Package manifest: `target` section

Per-target native/bridge declarations in `weir.pkg`:

```lisp
(target
  (:native (native (sources "gl_helper.c") (link "glfw" "GL" "m")))
  (:wasm (wasm-bridge "gl_bridge.js")))
```

### 1e. Top-level `target` for definitions

`target` at the top level wrapping `defn`, `extern`, etc. Resolved during expansion.

---

## Phase 2: WASM Codegen Crate

### Type mapping

| Weir type | WASM |
|-----------|------|
| `i8`, `u8`, `Bool` | `I32` (WASM has no sub-word types) |
| `i16`, `u16` | `I32` |
| `i32`, `u32` | `I32` |
| `i64`, `u64` | `I64` |
| `f32` | `F32` |
| `f64` | `F64` |
| `String`, `Vector`, `Fn`, structs, ADTs | `I32` (linear memory offset) |
| `Ptr` | `I32` |
| `Unit` | empty result |

### Memory layout

```
Linear memory (initial 256 pages = 16MB, growable):
  [0x0000 - 0x0FFF]  Reserved (null page)
  [0x1000 - 0xFFFF]  Static data (string literals, shape descriptors)
  [0x10000 - 0x1FFFF] Shadow stack (64KB, grows upward)
  [0x20000 - ...]     GC heap (bump allocator, managed by runtime)
```

### Closures

Closures store a function **table index** (i32) instead of a function pointer. The compiler maintains a WASM `table` section.

### Shadow stack

Stack slots for GC root tracking use a shadow stack region in linear memory instead of taking addresses of locals (impossible in WASM).

---

## Phase 3: WASM Runtime

Portable runtime functions (GC, vectors, strings, arenas, math, random) become internal WASM functions. JS-bridged functions (print, time, IO, math/libm) are imported from the `"env"` module.

---

## Phase 4: WebGL Bridge

`gl_bridge.js` implements the same 11-function GL API as `gl_helper.c` using WebGL 2. Includes batch renderer, 8x8 bitmap font, key state management.

---

## Phase 5: Game Loop Architecture

Native blocking loop replaced with `weir_frame` export + `requestAnimationFrame` JS driver. `wasm-set-state`/`wasm-get-state` builtins manage state across frames.

---

## Phase 6: CLI Integration

`weir wasm [file] -o out/` subcommand. Outputs: `app.wasm`, `runtime.js`, `gl_bridge.js` (if GL used), `index.html`, `loader.js`.

---

## Phase 7: HTML Harness & Deployment

Static site output deployable to Cloudflare Pages. Each project's `web/` folder is self-contained.

---

## Implementation Order

| Step | Description | Crate(s) | Depends on |
|------|-------------|----------|------------|
| 1 | `CompileTarget` enum + thread through pipeline | weir-cli, weir-expand | — |
| 2 | `(target ...)` parser support | weir-parser | — |
| 3 | `(target ...)` expansion (resolve at expand time) | weir-expand | 1, 2 |
| 4 | Package manifest `target` section | weir-pkg | 1 |
| 5 | `crates/weir-wasm` scaffold + type mapping | weir-wasm (new) | — |
| 6 | WASM codegen: basic expressions | weir-wasm | 5 |
| 7 | WASM codegen: runtime imports + GC in linear memory | weir-wasm | 6 |
| 8 | WASM codegen: closures via function table | weir-wasm | 6 |
| 9 | WASM codegen: strings, vectors, structs, ADTs, match | weir-wasm | 7 |
| 10 | Shadow stack in linear memory | weir-wasm | 7 |
| 11 | JS runtime bridge (`runtime.js`) | weir-wasm | 7 |
| 12 | `weir wasm` CLI subcommand | weir-cli | 6, 11 |
| 13 | End-to-end: compile + run simple Weir program in browser | all | 12 |
| 14 | WebGL bridge (`gl_bridge.js`) | weir-opengl | 11 |
| 15 | Game loop adaptation (weir_frame export, target branches) | demos/tetris | 3, 14 |
| 16 | Tetris running in browser | all | 15 |
| 17 | HTML harness generation + deployment | weir-wasm, weir-cli | 16 |
