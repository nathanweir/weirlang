# Code Review — Full Project Audit

Review performed 2026-02-18. Covers architecture, code quality, maintainability, testing, and performance.

## Overall Verdict

The architecture is sound. The frontend pipeline (lexer -> parser -> macro expander -> type checker) is clean and well-layered. The `la-arena` + `smol_str` AST foundation, the parser error recovery with proptest/fuzz coverage, and the arena allocator + GC runtime are all genuinely well-engineered. The main concerns are about file size / modularity and a subtle GC correctness edge — nothing structurally broken.

---

## Critical

### 1. `weir-codegen/src/lib.rs` is a ~7,000-line god file

JIT helpers, AOT runtime, compiler, dev session, and capture analysis all in one file. Suggested split: `runtime_helpers.rs`, `compiler.rs`, `dev_session.rs`, `aot.rs`. The `declare_runtime_helpers` method alone (~400 lines of repetitive signature/declaration pairs) could be replaced with a table-driven approach.

### 2. `macros_changed_source` duplicated 3 times in `weir-lsp`

Same token-stream comparison logic in `diagnostics.rs`, `workspace.rs`, and `server.rs`. If the logic drifts in one copy, different LSP features (hover vs go-to-def) will see different span origins.

### 3. GC shadow stack correctness edge

If Cranelift optimizes away a stack slot while a GC-managed pointer lives only in a register, that pointer becomes untracked during a mid-function GC. A known hard problem in JIT+GC design — hasn't manifested in tests yet, but should be documented and stress-tested.

---

## Important

### 4. `weir-typeck/src/lib.rs` is 4,470 lines

Types, unification, constraint resolution, HKT handling, and arena provenance tracking all in one impl. Suggested split: `types.rs`, `unify.rs`, `check.rs`, `constraints.rs`.

### 5. `seen_specs` uses `Vec` with O(n) scan

Instead of `HashSet` for deduplicating generic specializations (`weir-typeck/src/lib.rs:270-309`).

### 6. `collect_definitions` clones the entire items vector

6 times per type-check — fixable by iterating indices (`weir-typeck/src/lib.rs:1033-1038`).

### 7. LSP holds documents mutex across full analysis

Blocks all other requests during type-checking (`weir-lsp/src/server.rs:76-87`). Fix: copy text out, release lock, analyze, re-acquire.

### 8. `ExternC` reuses `Defn` struct

Implies extern declarations can have bodies (`weir-ast/src/lib.rs:184-189`). A separate `ExternDecl` type would be cleaner.

### 9. Struct destructure parser has acknowledged ambiguity

Capitalization heuristic documented in a comment but not enforced in code (`weir-parser/src/lib.rs:1202-1213`).

---

## Minor

### 10. `declare_runtime_helpers` boilerplate

~400 lines that could be ~50 with a table-driven approach.

### 11. `resolve_named_type` accepts both `"Bool"` and `"bool"`

Without canonicalizing — error messages always say `Bool`.

### 12. `Float(f64)` in patterns

Semantically dangerous (float equality), should be disallowed or documented.

### 13. Mangled specialization names could collide

For complex types containing commas/parens in their display name.

### 14. Workspace indexing races with document analysis

Should be documented in `lsp-status.md`.

---

## Positive Callouts

- **Arena allocator + GC shadow stack** — clean `#[repr(C)]` layout, proper threshold tuning, AOT C runtime mirrors Rust faithfully
- **Parser error recovery** — always produces a Module, proptest + fuzz coverage directly target the most dangerous failure modes
- **`la-arena` + `smol_str` for AST** — typed indices, compact identifiers, ready for incremental compilation
- **Overall crate structure** — clean boundaries, well-layered pipeline, good oracle testing (interpreter vs codegen agreement)
