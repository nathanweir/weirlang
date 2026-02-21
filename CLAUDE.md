# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Weir is a programming language project transitioning from design to implementation. The goal is to build a complete language ecosystem: compiler/interpreter, LSP (language server protocol), and a Zed editor extension.

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
- **Modern tooling** — first-class LSP (`crates/weir-lsp`) and Zed editor extension (`/home/nathan/dev/zed-weir`) from day one
- **Implementation** — leaning Rust (Cranelift for dev-mode codegen, potentially LLVM for release builds)

## Development Environment

- **Dependencies**: Managed via `flake.nix` with `direnv` — entering the project directory automatically activates the Nix environment
- **Shell commands**: Use `nix develop --command <cmd>` to run commands (e.g. `nix develop --command cargo test --workspace`). Do NOT use `direnv export bash` — it does not work in this context.
- **Task runner**: Uses `just` (Justfile) for dev commands — run `just` to list available recipes

## Temporary Files

Use the project-local `tmp/` directory (gitignored) for any scratch files, test outputs, or temporary artifacts. Do not use the system `/tmp` or other directories outside this project.

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
  - `implementation-plan.md` — Phased implementation roadmap, technology choices, test strategy
  - `lsp-status.md` — LSP feature inventory: what's implemented, known limitations, and candidate next features. **Keep this up to date** when adding, changing, or removing LSP features.
- `.transcripts/` — Raw voice memo transcripts (gitignored, not committed)
- `README.md` — Project description and workflow explanation

## Implementation Plan Maintenance

When completing features or phases, always update `design/implementation-plan.md`:
- Mark completed items with `[x]`
- Update test counts if mentioned
- Add notes about what was implemented if the description was aspirational

## Advguild Port

Active port of the Adventurer's Guild game from Common Lisp to Weir:
- **CL source**: `/home/nathan/dev/advguild` (original Common Lisp implementation)
- **Weir port**: `/home/nathan/dev/advguild` (same repo, Weir files in `src/*.weir`)
- **Port plan**: `design/advguild-port.md` — phased migration roadmap
- **Phase workflow**: implement phase, code review, fix issues, commit

## Documentation Site Maintenance

When implementing language changes (new syntax, new features, changed semantics), update the docs site in `docs/src/content/docs/` as a finishing step:
- `guide/syntax.mdx` — Syntax changes, new forms, new operators
- `guide/data-types.mdx` — Type system changes, new types, struct features
- `guide/functions.mdx` — Function features, mutability, closures
- `stdlib/builtins.mdx` — New builtins or standard library changes
- `concepts/` — Architecture/compilation pipeline changes
