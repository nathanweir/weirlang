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
    cargo run --bin weir -- run {{ file }}

# Remove build artifacts
clean:
    cargo clean

# Clean the tmp/ scratch directory

# NOTE: HARDCODED TO THIS SPECIFIC FULL PATH FOR SAFETY; DO NOT REMOVE
clean-tmp:
    rm -rf /home/nathan/dev/weirlang/tmp/*

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

# Generate tree-sitter parser from grammar.js
ts-generate:
    cd tree-sitter-weir && tree-sitter generate

# Run tree-sitter test corpus
ts-test:
    cd tree-sitter-weir && tree-sitter test

# Build the OpenGL Tetris demo via package system
tetris-build:
    cd demos/tetris && cargo run --manifest-path ../../Cargo.toml --bin weir -- build -o ../../tmp/tetris_gl

# Run the OpenGL Tetris demo (AOT)
tetris: tetris-build
    ./tmp/tetris_gl

# Run OpenGL Tetris via JIT (package system)
tetris-run:
    cd demos/tetris && cargo run --manifest-path ../../Cargo.toml --bin weir -- run

# Run OpenGL Tetris in dev mode (live reload — edit demos/tetris/tetris.weir while it runs)
tetris-dev:
    cd demos/tetris && cargo run --manifest-path ../../Cargo.toml --bin weir -- dev

# Convert a BMP screenshot to PNG for viewing (usage: just screenshot tmp/tetris_screenshot.bmp)
screenshot file:
    convert {{ file }} -resize 50% {{ without_extension(file) }}.png && echo "Wrote {{ without_extension(file) }}.png"

# Start docs dev server
docs-dev:
    cd docs && pnpm run dev

# Build docs site
docs-build:
    cd docs && pnpm run build
