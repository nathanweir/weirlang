# Advguild Port Analysis

Analysis of porting [advguild](file:///home/nathan/dev/advguild) (Common Lisp) to Weir.

## Source game overview

~13,600 lines of Common Lisp across 85 files. Adventure guild management/autobattler:

- **City scene**: 2D tile grid, building placement, production economy with worker pathfinding
- **Combat scene**: ATB (active time battle) with gambit-based AI
- **Dungeon scene**: Procedural grid generation, party exploration, encounters
- **World map scene**: Quest selection, party management
- **Rendering**: OpenGL 4.6, ortho 2D, textured sprites + bitmap font, dynamic VBO
- **Architecture**: Entity-Component-System via `cl-fast-ecs`, frame-based game loop (fixed 60Hz sim, variable render)

### CL dependencies

| Library | Purpose | Weir replacement |
|---|---|---|
| `cl-opengl` | OpenGL bindings | Extend existing `weir-opengl` (used by Tetris demo) |
| `glfw` | Window + input | C FFI wrapper (~200 lines) |
| `pngload` | PNG decoding | Bind `stb_image.h` via FFI |
| `zpng` | PNG encoding (screenshots) | Defer — tool-only, not needed for game |
| `cl-fast-ecs` | ECS macros | Build Weir ECS library (~500-800 lines) |
| `bordeaux-threads` | Threading (REPL dev) | Not needed — Weir has native live reloading |

### CL features used and translation

**Direct translations (no language work needed):**

| CL feature | Weir equivalent |
|---|---|
| `defstruct` | `defstruct` |
| Keyword enums (`:fighter`, `:city`) | ADTs with exhaustive `match` (strictly better) |
| `mapcar`/`find-if`/`remove-if-not` | `map`/`find`/`filter` |
| `format` strings | `str` function / `format` macro |
| `lambda` | `fn` |
| Property lists (sprite data) | Map literals `{}` or dedicated structs |
| `case` dispatch | `match` (exhaustive) |
| Declarative config (`define-item-type`) | Macros or plain constructor calls |

**Translations requiring care but no new features:**

| CL pattern | Weir approach |
|---|---|
| CLOS dispatch (GLFW callbacks) | Callback registration (pass functions) |
| `define-symbol-macro` for globals | Direct struct field access |
| `cl-fast-ecs` `defsystem` macro DSL | Manual iteration initially; Weir macros later |
| Association lists (inventory) | `Map` type |
| 2D arrays (`aref`) | Flat vector with index math |

**Not used (no concern):** `eval`, reader macros, `multiple-value-bind`, conditions/restarts, `apply` on symbols, runtime code generation.

## Recommended language additions

Features that Weir should add — not to mimic CL, but because they make Weir better for games. Ordered by impact.

### 1. Mutable struct field assignment

**The problem:** The game mutates individual struct fields thousands of times per frame. `update` copies the entire struct for every field change. For game code this is both slow and noisy.

**Proposed syntax:**
```lisp
(let ((mut enemy (get-enemy id)))
  (set! (.hp enemy) (- (.hp enemy) dmg))
  (set! (.state enemy) Dead))
```

**Relationship to `ref`:** This does NOT require mutable references. `set!` on `.field` of a `mut` local modifies the local copy only. The caller's data is unaffected — same as `set!` on any `mut` binding today. No aliasing, no arena escape, no concurrency concern. This is purely a convenience over `(set! enemy (update enemy :hp ...))` that also avoids the copy.

**Key insight:** In an ECS, components live in dense arrays owned by the ECS. The ECS provides `get`/`set` functions. The game pattern becomes:
```lisp
(let ((mut cs (ecs-get combat-state entity-id)))
  (set! (.current-hp cs) (- (.current-hp cs) damage))
  (ecs-set combat-state entity-id cs))
```
This is copy-modify-writeback with no aliasing. The field mutation is local convenience; the writeback is explicit.

### 2. Imperative iteration (`for`)

**The problem:** Games have tight loops over grids, entity lists, countdown timers. Recursion adds ceremony; `map`/`fold` don't fit when you're mutating state mid-loop.

**Proposed syntax:**
```lisp
;; Counted loop
(for (i 0 (< i height))
  (for (j 0 (< j width))
    (set-tile! terrain i j :grass)))

;; Collection iteration
(for-each (enemy enemies)
  (set! (.hp enemy) (- (.hp enemy) dot-damage)))
```

Compiles to tight loops. No allocations, no closure overhead.

### 3. Mutable collections (`MutVec`, `MutMap`)

**The problem:** Games need O(1) insert/delete/update on collections that change every frame — building registries, entity lookups, frame-local sprite accumulators. Persistent immutable maps are the wrong tool here.

**Proposed types:**
```lisp
(let ((mut sprites (mut-vec)))
  (mut-push! sprites {:x 0 :y 0 :texture "hero.png"})
  (mut-push! sprites {:x 32 :y 0 :texture "enemy.png"})
  sprites)

(let ((mut buildings (mut-map)))
  (mut-set! buildings grid-pos new-building)
  (mut-remove! buildings old-pos))
```

These are not `Shareable` — they cannot cross thread boundaries. This preserves Weir's concurrency safety.

### 4. Numeric conversion ergonomics

**The problem:** Games constantly mix int and float — grid coords are ints, positions are floats, timers are floats, HP is int. Explicit conversion on every operation makes math unreadable.

**Options (pick one):**
- **Implicit safe widening** in arithmetic context: `i64 + f64` promotes the i64. Only lossless conversions (i32→i64, i32→f64, etc.)
- **Terse conversion syntax**: `(f64 x)` instead of `(i64->f64 x)` — type name as conversion function

### 5. Global mutable bindings (`defglobal mut`)

**The problem:** Games have genuinely global mutable state — current scene, camera, input state. Forcing these through function parameters or monadic patterns adds friction with no safety benefit (they're already single-threaded frame-local state).

**Proposed syntax:**
```lisp
(defglobal mut *current-scene* : Scene :city)
(defglobal mut *camera-x* : f64 0.0)
```

Not `Shareable` — cannot be accessed from spawned tasks. Concurrency safety preserved.

## Estimated effort

| Work item | Lines | Notes |
|---|---|---|
| Language features (above) | — | Compiler work, not game code |
| GLFW bindings | ~200 | Window, input, frame timing |
| OpenGL extensions | ~200 | Texture loading, UV support, tint mode |
| stb_image binding | ~50 | PNG loading |
| ECS library | ~500-800 | Entity storage, component arrays, system runner |
| Bitmap font renderer | ~100 | Character UV lookup |
| Game logic port | ~10,000 | Mechanical translation of game code |
| **Total game + library code** | **~11,000-11,400** | Comparable to CL original |
