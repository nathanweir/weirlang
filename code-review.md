# Code Review — Full Project Audit

Review performed 2026-02-18. Covers architecture, code quality, maintainability, testing, and performance.

## Overall Verdict

The architecture is sound. The frontend pipeline (lexer -> parser -> macro expander -> type checker) is clean and well-layered. The `la-arena` + `smol_str` AST foundation, the parser error recovery with proptest/fuzz coverage, and the arena allocator + GC runtime are all genuinely well-engineered. The main concerns are about file size / modularity and a subtle GC correctness edge — nothing structurally broken.

---

## Critical

### 1. `weir-codegen/src/lib.rs` is a ~7,000-line god file — FIXED

Split into `lib.rs` (5,853 lines), `jit_helpers.rs`, `aot.rs`, `dev_session.rs`. The `declare_runtime_helpers` method was also replaced with a table-driven approach (see item 10).

### 2. `macros_changed_source` duplicated 3 times in `weir-lsp` — FIXED

Extracted shared `expand.rs` module with `macros_changed_source()` and `expand_source()`. All three call sites now use the shared implementation.

### 3. GC shadow stack correctness edge — DOCUMENTED

Added detailed doc comment to `push_gc_root()`, cross-reference comment in SetBang codegen, and "Known limitation" section in `design/memory-management.md`.

---

## Important

### 4. `weir-typeck/src/lib.rs` is 4,470 lines — FIXED

Split into 7 files: `lib.rs` (155 lines), `checker.rs` (2,722), `tests.rs` (1,299), `defs.rs` (122), `types.rs` (110), `result.rs` (70), `error.rs` (18).

### 5. `seen_specs` uses `Vec` with O(n) scan — FIXED

Changed to `HashSet<SmolStr>` for O(1) deduplication.

### 6. `collect_definitions` clones the entire items vector — NO CHANGE NEEDED

Clone is necessary: `TypeChecker` holds `&'a Module` (immutable borrow), so `std::mem::take` is impossible. Index-based iteration has the same borrow conflict. The clone correctly sidesteps the borrow checker constraint.

### 7. LSP holds documents mutex across full analysis — FIXED

`analyze_and_publish` now copies text/line_index/version out of the lock, releases it, runs analysis, then re-acquires to store results.

### 8. `ExternC` reuses `Defn` struct — DEFERRED

Involves AST restructuring. Low practical impact since extern declarations are parsed without bodies.

### 9. Struct destructure parser has acknowledged ambiguity — DEFERRED

Capitalization heuristic works in practice. Fixing requires either parser backtracking or introducing distinct syntax.

---

## Minor

### 10. `declare_runtime_helpers` boilerplate — FIXED

Replaced ~400 lines of repetitive signature/declaration pairs with a 38-entry table + 10-line loop.

### 11. `resolve_named_type` accepts both `"Bool"` and `"bool"` — FIXED

Removed the `"bool"` alternative. Only canonical `"Bool"` is accepted.

### 12. `Float(f64)` in patterns — DEFERRED

Requires design decision about whether to disallow float patterns or add a warning. Low practical impact.

### 13. Mangled specialization names could collide — NO CHANGE NEEDED

Analysis showed commas never appear in `Ty::Display` output (constructors use spaces, functions use brackets), so the comma-joined mangling scheme is collision-free. Added a safety comment documenting why.

### 14. Workspace indexing races with document analysis — DOCUMENTED

Added to `design/lsp-status.md` Known Limitations section.

---

## Positive Callouts

- **Arena allocator + GC shadow stack** — clean `#[repr(C)]` layout, proper threshold tuning, AOT C runtime mirrors Rust faithfully
- **Parser error recovery** — always produces a Module, proptest + fuzz coverage directly target the most dangerous failure modes
- **`la-arena` + `smol_str` for AST** — typed indices, compact identifiers, ready for incremental compilation
- **Overall crate structure** — clean boundaries, well-layered pipeline, good oracle testing (interpreter vs codegen agreement)
