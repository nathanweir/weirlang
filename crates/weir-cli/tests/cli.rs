use predicates::prelude::*;
use std::fs;

fn weir() -> assert_cmd::Command {
    assert_cmd::cargo::cargo_bin_cmd!("weir").into()
}

fn fixture_path(name: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!(
        "{}/tests/fixtures/{}.weir",
        manifest_dir.replace("/crates/weir-cli", ""),
        name
    )
}

// ── check command ───────────────────────────────────────────

#[test]
fn check_valid_file_exits_zero() {
    weir()
        .args(["check", &fixture_path("codegen-functions")])
        .assert()
        .success()
        .stdout(predicate::str::contains("OK"));
}

#[test]
fn check_type_error_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("bad.weir");
    fs::write(&file, "(defn main () (+ 1 true))").unwrap();

    weir()
        .args(["check", file.to_str().unwrap()])
        .assert()
        .failure()
        .stderr(predicate::str::contains("type error"));
}

// ── run command ─────────────────────────────────────────────

#[test]
fn run_codegen_functions_produces_output() {
    // codegen-functions has type annotations required for JIT
    weir()
        .args(["run", &fixture_path("codegen-functions")])
        .assert()
        .success()
        .stdout(predicate::str::contains("120"));
}

#[test]
fn run_codegen_arithmetic_produces_output() {
    weir()
        .args(["run", &fixture_path("codegen-arithmetic")])
        .assert()
        .success();
}

// ── interp command ──────────────────────────────────────────

#[test]
fn interp_factorial_produces_output() {
    // interp doesn't require type annotations
    weir()
        .args(["interp", &fixture_path("factorial")])
        .assert()
        .success()
        .stdout(predicate::str::contains("120"));
}

#[test]
fn interp_fibonacci_produces_output() {
    weir()
        .args(["interp", &fixture_path("fibonacci")])
        .assert()
        .success()
        .stdout(predicate::str::contains("55"));
}

// ── parse command ───────────────────────────────────────────

#[test]
fn parse_outputs_ast() {
    weir()
        .args(["parse", &fixture_path("factorial")])
        .assert()
        .success()
        .stdout(predicate::str::contains("defn"));
}

#[test]
fn parse_does_not_include_prelude() {
    // Parse should show raw AST without prelude injected
    weir()
        .args(["parse", &fixture_path("factorial")])
        .assert()
        .success()
        .stdout(predicate::str::contains("Result").not());
}

// ── build command ───────────────────────────────────────────

#[test]
fn build_produces_executable() {
    let dir = tempfile::tempdir().unwrap();
    let output = dir.path().join("functions_bin");

    weir()
        .args([
            "build",
            &fixture_path("codegen-functions"),
            "-o",
            output.to_str().unwrap(),
        ])
        .assert()
        .success();

    assert!(output.exists(), "binary should be created");
}

// ── error handling ──────────────────────────────────────────

#[test]
fn missing_file_produces_error() {
    weir()
        .args(["run", "nonexistent.weir"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("could not read"));
}

#[test]
fn no_subcommand_shows_help() {
    weir()
        .assert()
        .failure()
        .stderr(predicate::str::contains("Usage"));
}

#[test]
fn syntax_error_exits_nonzero() {
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("bad.weir");
    fs::write(&file, "(defn main (").unwrap();

    weir()
        .args(["check", file.to_str().unwrap()])
        .assert()
        .failure()
        .stderr(predicate::str::contains("error"));
}
