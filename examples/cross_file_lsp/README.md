# Cross-File LSP Demo

This directory demonstrates the Weir LSP's cross-file features. Open it as a workspace in your editor.

## Files

- **math.weir** — Arithmetic helpers (`square`, `cube`, `abs`, `clamp`) and predicates (`positive?`, `negative?`, `zero?`)
- **types.weir** — Shared types (`Option`, `Result`, `Ordering`, `Point`) and a typeclass (`Compare`)
- **main.weir** — Calls functions and uses types from the other files

## What to Try

1. **Go-to-definition**: In `main.weir`, place your cursor on `square` or `clamp` and jump to definition — the LSP navigates to `math.weir`.

2. **Find references**: In `math.weir`, find references on `square` — results include both the definition in `math.weir` and all call sites in `main.weir`.

3. **Rename**: Rename `clamp` from either file — both the definition in `math.weir` and the call in `main.weir` are updated.

4. **Completion**: In `main.weir`, type a new expression and start typing `sq` — `square` appears in completions with `from math.weir` detail text, sorted after local definitions.
