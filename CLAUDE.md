# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Weirlang is a programming language project in the early design phase. The goal is to build a complete language ecosystem: compiler/interpreter, LSP (language server protocol), and a Zed editor extension.

## Design Workflow

This project uses a voice-memo-driven design process:
1. Voice memo transcripts are recorded in `.transcripts/` (gitignored)
2. Transcripts are processed into structured design documents in `design/`
3. Design documents are iterated on with the developer before implementation begins

When processing a new transcript, ingest it into the existing design documents in `design/`, updating or creating documents as needed. Always surface open questions and contradictions.

## Language Design Goals

- **Lisp-family syntax** (S-expressions) inspired by Common Lisp, Carp, and Coalton
- **Declarative static typing** — local inference (explicit function signatures, inferred locals), Haskell-style typeclasses with HKTs, immutable by default
- **Live reloading** — function-level redefine-in-place via long-running runtime with incremental native compilation (Cranelift). Full redefinition of types/macros with cascade recompilation and soft restart
- **Memory** — tracing GC by default + opt-in arena allocation for hot paths, with compile-time arena escape prevention
- **Macros** — purely syntactic, hygienic, can generate type definitions
- **Guard rails philosophy** — prevent footguns, safe defaults, explicit opt-in for advanced/unsafe features
- **Modern tooling** — first-class LSP and editor support from day one using existing user-written Common Lisp language server (/home/nathan/dev/clef) and Zed editor extension (/home/nathan/dev/zed-common-lisp) as reference
- **Implementation** — leaning Rust (Cranelift for dev-mode codegen, potentially LLVM for release builds)

## Development Environment

- **Dependencies**: Managed via `flake.nix` with `direnv` — entering the project directory automatically activates the Nix environment
- **Task runner**: Uses `just` (Justfile) for dev commands — run `just` to list available recipes

## Repository Structure

- `design/` — Design documents:
  - `philosophy.md` — Core design principles
  - `execution-model.md` — Runtime, compilation, live reloading, cascade model
  - `memory-management.md` — GC + arenas
  - `type-system.md` — Types, inference, generics, typeclasses
  - `macro-system.md` — Macros and their interaction with types/reloading
  - `concurrency.md` — Layered concurrency model with compile-time safety
  - `syntax.md` — Syntax decisions, type annotation style, collection literals
  - `open-questions.md` — Index of all decisions and remaining open questions
- `.transcripts/` — Raw voice memo transcripts (gitignored, not committed)
- `README.md` — Project description and workflow explanation
