# List available recipes
default:
    @just --list

# Build all crates
build:
    cargo build

# Run all tests
test:
    cargo test

# Run a .weir file
run file:
    cargo run --bin weir -- run {{file}}

# Remove build artifacts
clean:
    cargo clean

# Run clippy, check formatting, and run tests
check:
    cargo clippy --all-targets -- -D warnings
    cargo fmt --check
    cargo test

# Format all code
fmt:
    cargo fmt

# Run clippy lints
clippy:
    cargo clippy --all-targets -- -D warnings

# Review insta snapshots
review:
    cargo insta review
