# Testing Plan

Status inventory and improvement roadmap for the Weir test suite.

## Current State (as of Feb 2025)

| Crate | Tests | Error-path tests | Notes |
|-------|------:|------------------:|-------|
| weir-lexer | 24 | 0 | No malformed-input tests |
| weir-parser | 34 | 13 | 13 error snapshot tests + 3 proptest |
| weir-macros | 26 | 2 | 4 hygiene tests added |
| weir-typeck | 69 | 19 | 3 fixture-based error tests added |
| weir-interp | 67 | 9 | 8 error-path tests added |
| weir-codegen | 70 | 5 | 5 error/DevSession tests added |
| weir-lsp | 150 | ~8 | No diagnostics or protocol tests |
| weir-ast | 0 | 0 | Indirectly tested via parser snapshots |
| weir-cli | 12 | 3 | NEW: assert_cmd integration tests |
| **Total** | **~452** | **~59** | |

### Strengths
- **Oracle testing** (interp vs JIT vs AOT agreement) catches representation/codegen divergences
- **Snapshot testing** (insta) guards parser output and typeck error messages against regressions
- **LSP coverage** is thorough (150 tests across 11 modules)
- **Typeclass/HKT testing** covers instances, orphan rules, kinds
- **Property-based testing** (proptest) catches parser panics on arbitrary input
- **Fuzzing** (cargo-fuzz) provides ongoing crash detection for the parser
- **CLI integration tests** verify end-to-end behavior of all commands
- **Macro hygiene** is now explicitly tested

### Remaining Gaps
- Lexer has no malformed-input or error tests
- LSP lacks diagnostics push and protocol-level tests
- Codegen feature coverage is narrow (no strings, structs, closures, vectors in JIT)
- No benchmarks or performance regression tests

---

## Improvement Plan (priority order)

### 1. Parser error tests ✅ DONE
Added 13 snapshot-based error tests to `weir-parser` covering unclosed parens, unexpected tokens, incomplete defn, empty input, bad patterns, multiple errors, etc.

### 2. Fuzz the parser with cargo-fuzz ✅ DONE
Added `fuzz/` directory with `parse` fuzz target. The property: `parse()` never panics on any input. Run with `cargo +nightly fuzz run parse`.

### 3. Interpreter error-path tests ✅ DONE
Added 8 error-path tests: division by zero (int + float), mod by zero, wrong arg count, set on undefined variable, subtract non-numeric, set on immutable, non-exhaustive match, len wrong args.

### 4. Wire up type-error fixture files ✅ DONE
Added `fixture_check_err` helper and 3 fixture-based tests for `mismatched-return`, `non-exhaustive-match`, and `unknown-variable`.

### 5. Codegen error tests ✅ DONE
Added 5 tests: cannot print ADT type, DevSession parse error, DevSession type error, DevSession reload parse error (with old code recovery), DevSession reload type error (with old code recovery).

### 6. proptest roundtrip tests ✅ DONE
Added `proptest` as a workspace dev dependency. 3 property tests in `weir-parser`: random ASCII, random bytes, and lispy-structured input — all assert parse doesn't panic. Note: pretty-print roundtrip tests were not feasible because `weir_ast::pretty_print` outputs a debug format, not parseable Weir syntax.

### 7. Macro hygiene tests ✅ DONE
Added 4 tests in `weir-macros/src/expander.rs`: let binding doesn't capture outer variable, macro var doesn't leak to caller, nested macros get independent gensyms, macro params are not renamed.

### 8. CLI integration tests with assert_cmd ✅ DONE
Added `assert_cmd`, `predicates`, `tempfile` as workspace dev dependencies. 12 integration tests in `crates/weir-cli/tests/cli.rs` covering: check (success + type error), run (2 fixtures), interp (2 fixtures), parse (AST output + no prelude), build (produces executable), error handling (missing file, no subcommand, syntax error).

---

## Testing Techniques Reference

| Technique | Crates using it | Notes |
|-----------|----------------|-------|
| Snapshot (insta) | parser, typeck, interp, lsp/formatting | Guards against message/output regressions |
| Oracle (interp vs codegen) | codegen | Catches representation divergences |
| Fixture files | interp, codegen, typeck, cli | Real-world-shaped programs |
| Inline source strings | all | Quick unit tests |
| Property-based (proptest) | parser | Catches panics on arbitrary input |
| Fuzzing (cargo-fuzz) | parser | Ongoing crash detection |
| CLI integration (assert_cmd) | weir-cli | End-to-end command tests |
