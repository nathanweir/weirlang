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

# Build the OpenGL Tetris demo
tetris-build:
    cargo run --bin weir -- build demos/tetris_gl.weir -o tmp/tetris_gl --cc-arg libs/gl_helper.c -l glfw -l GL -l m

# Run the OpenGL Tetris demo
tetris: tetris-build
    ./tmp/tetris_gl

# Convert a BMP screenshot to PNG for viewing (usage: just screenshot tmp/tetris_screenshot.bmp)
screenshot file:
    convert {{ file }} -resize 50% {{ without_extension(file) }}.png && echo "Wrote {{ without_extension(file) }}.png"
