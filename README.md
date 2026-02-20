# Weir

> **Warning:** Heads-up, this is a fun personal project to drive Claude Code very hard and make a domain-specific programming language. It's a fun personal project, but should probably not be used by anyone.

Weir is a statically typed, Lisp-family programming language targeting native code via [Cranelift](https://cranelift.dev/). It's designed for fast iteration in small-scale game development, combining S-expression syntax with a modern type system and function-level live reloading.

## Key Features

- **S-expression syntax** with type annotations, collection literals (`[]`, `{}`), and threading macros (`->`, `->>`)
- **Declarative static types** — explicit function signatures, locally inferred locals, algebraic data types, exhaustive pattern matching
- **Typeclasses** with higher-kinded types, multi-parameter support, and coherence rules
- **Hygienic macros** — purely syntactic, can generate type definitions and typeclass instances
- **Native compilation** — Cranelift JIT for dev mode, AOT compilation for release binaries
- **Live reloading** — function-level hot-swap in dev mode with cascade recompilation
- **Tracing GC** with opt-in arena allocation for hot paths
- **Package system** — `weir.pkg` manifests, path-based dependencies, native C source compilation
- **C FFI** — `extern "C"` declarations with `unsafe` blocks

## Current Status

Working today:

- **Compiler pipeline**: lexer, parser, macro expander, type checker, Cranelift codegen
- **Three execution modes**: JIT (`weir run`), AOT (`weir build`), tree-walking interpreter (`weir interp`)
- **Dev mode**: `weir dev` with file watching and function-level live reload
- **Package system**: multi-package projects with dependency resolution and native code support
- **OpenGL Tetris demo**: full game built with Weir + GLFW/OpenGL via C FFI (`just tetris`)
- **LSP server**: diagnostics, hover, completion, go-to-definition, document symbols
- **Zed extension**: syntax highlighting and LSP integration
- **647 tests** across the workspace

## Getting Started

Requires [Nix](https://nixos.org/) with flakes enabled:

```bash
git clone <repo-url> && cd weirlang
nix develop                        # activate dev environment
just test                          # run all tests
```

Run a single file:
```bash
weir run examples/hello.weir       # JIT compile and run
weir build examples/hello.weir     # AOT compile to binary
weir dev examples/hello.weir       # dev mode with live reload
```

Run a package project:
```bash
cd demos/tetris && weir run        # discovers weir.pkg, resolves deps, runs via JIT
```

## Project Structure

```
crates/
  weir-lexer/       Tokenizer
  weir-parser/      S-expression parser → AST
  weir-macros/      Macro expander
  weir-typeck/      Type checker (local inference, typeclasses, ADTs)
  weir-codegen/     Cranelift JIT/AOT backend + dev mode live reload
  weir-interp/      Tree-walking interpreter
  weir-runtime/     GC, arenas, runtime support
  weir-pkg/         Package system (manifest parsing, dependency resolution)
  weir-cli/         CLI frontend (clap)
  weir-lsp/         Language Server Protocol implementation
  weir-ast/         AST types, pretty printer, TCO analysis
design/             Language design documents
std/                Standard library (prelude)
demos/              Demo projects (tetris)
docs/               Documentation website (Astro + Starlight)
```

## Documentation

See [`docs/`](docs/) for the full documentation site, or run `just docs-dev` to browse locally.

## License

Zero-Clause BSD
=============

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
