# Memory Management

## Decision: Tracing GC by default + opt-in arena allocation for hot paths

### The model

**Default: Tracing GC.** Most code allocates freely with no memory management burden, preserving the Lisp feel. The GC is designed for game dev from the start.

**Opt-in: Arena allocation for hot paths.** The developer explicitly opts into arena allocation via `with-arena` blocks for performance-critical code. Arenas use bump allocation (pointer increment — near-zero cost) and free all contents at once when the block ends.

```lisp
;; Normal code — GC managed, Lisp feel, no memory thinking
(defn load-level ((path : String)) : Level
  (let ((data (read-file path))
        (entities (parse-entities data)))
    (Level entities)))

;; Hot path — arena allocated, no GC pauses
(defn update-physics ((world : World)) : Unit
  (with-arena frame
    (let ((contacts (detect-collisions world))
          (impulses (solve-constraints contacts)))
      (apply-impulses world impulses))))
;; everything allocated in the arena is freed instantly at block end
```

### Why this hybrid

| Concern | How it's addressed |
|---|---|
| Lisp feel | GC by default — 90%+ of code just allocates freely |
| Game dev performance | Arenas for hot paths — zero GC pauses where it matters |
| Safety | GC prevents use-after-free/dangling pointers; arena escapes caught at compile time |
| Live reloading | GC handles stale instances naturally (drop references, they get collected). Arenas are transient and unaffected by reloads |
| Guard rails | Safe by default (GC), explicit opt-in for performance (arenas), compiler prevents misuse |

### Arena escape prevention: compile-time (lexical)

Arena-allocated values cannot escape their `with-arena` block. The compiler enforces this via **lexical escape analysis** — a purely syntactic check, not a full lifetime system:

- **Cannot** return an arena-allocated value from the `with-arena` block
- **Cannot** assign an arena value to an outer mutable variable via `set!`
- **Conservative**: any function call returning a heap type inside an arena is treated as arena-provenance (may produce false positives for functions that return pre-existing GC values)
- **Can** pass arena values to functions called within the block, read from them, use in computations

#### What this is NOT

This is **not** Rust-style lifetime checking. Key differences:

- **No interprocedural analysis.** The compiler does not track what happens to arena values passed as function arguments. If a function receives an arena-allocated vector, the compiler cannot verify the function doesn't stash it somewhere.
- **No lifetime annotations.** Function signatures carry no provenance information.
- **Lexical only.** The analysis checks syntactic position (is this `set!` inside a `with-arena` block?) rather than data flow.

#### Why it works in practice

Despite the theoretical incompleteness, several properties of Weir's current design make interprocedural escape difficult:

- **No mutable references.** Function arguments are passed by value (copying the pointer). The callee cannot modify the caller's bindings.
- **No global mutable state.** There are no top-level mutable variables to stash arena pointers in.
- **Closures capture by value.** A closure copies captured values into its environment at creation time. `set!` inside a closure modifies the copy, not the original.
- **Conservative call tagging.** Any function call returning a heap type inside an arena block gets arena provenance, preventing the return value from escaping — even if the function actually allocated from the GC.

These properties mean the only way to persist an arena pointer past the block boundary is through `set!` to an outer variable (caught) or returning it (caught). Without mutable references or global state, there is no indirect mutation path.

#### Future risk

If Weir adds mutable references, global state, or FFI that allows storing arbitrary pointers, the escape analysis will need to be strengthened — either with lifetime annotations on function parameters or with runtime safety checks (tagging arena pointers).

If data needs to outlive the arena, it should be GC-allocated instead — that's a design signal that the data isn't transient.

#### Known escape vector: `ref` parameters

Once `ref` parameters (mutable references) are implemented, the lexical escape analysis will have a concrete exploit. A function receiving a `ref` to an outer variable can smuggle an arena pointer out of the block, because the `set!` happens inside the callee — outside the `with-arena` block lexically:

```lisp
(defn stash ((ref target : (Vector i64)) (v : (Vector i64))) : Unit
  (set! target v))   ;; lexically NOT inside with-arena — uncaught

(defn main () : Unit
  (let ((mut holder [0]))
    (with-arena a
      (let ((v [42 43 44]))
        (stash holder v)   ;; smuggles arena pointer into holder via ref
        0))
    (println (nth holder 0))))   ;; use-after-free: arena is destroyed
```

**TODO (post-`ref` implementation):** Build and run this example to demonstrate the limitation explicitly. Then evaluate mitigation options:
1. Forbid passing arena-provenance values to `ref` parameters (simplest, may be too restrictive)
2. Add provenance annotations to `ref` parameters (mini-lifetime system)
3. Runtime tagging of arena pointers with a check on `ref` write-back

Similarly, FFI (`extern "C"`) can trivially escape arena pointers since foreign functions are opaque to the compiler. This is expected and acceptable — FFI is inherently `unsafe`.

### GC design considerations

The GC should be game-dev-aware from the start:

- **Incremental/concurrent**: reduce pause times by doing work between frames or on a background thread
- **Generational**: optimize for short-lived allocations (which dominate in game loops)
- **Suppressible**: developer can mark critical sections where GC must not run
- **Manually triggerable**: run full GC during loading screens, scene transitions, menus
- **Game loop aware**: prefer collecting during the gap between frame end and next frame start

### Interaction with the type system

The type system must track arena provenance to enforce escape prevention. This is a form of region typing scoped to arena blocks:

- Arena-scoped values may be represented with implicit annotations (the escape checker tracks provenance without the developer writing region types)
- Generic functions should work over both GC and arena-allocated values — a function taking `(List Contact)` shouldn't care where the list was allocated; only the escape checker needs to know
- A closure that captures an arena value must itself be arena-scoped

### Open questions

- ~~What GC algorithm to start with?~~ **Resolved: mark-and-sweep, stop-the-world.** Generational/incremental are future optimizations.
- Should there be named arenas (multiple concurrent arenas with different lifetimes)? Arenas have names for diagnostics but names are not semantically meaningful yet.
- Should arena size be configurable per-block, or globally set? Currently fixed at 64 KB initial chunk, doubling on growth.
- ~~Can arenas be nested?~~ **Resolved: yes.** Inner arena values cannot escape to the outer arena. Each arena manages its own memory independently.
- Should the escape analysis be strengthened before adding mutable references or FFI? The current lexical analysis is sufficient for the language's current feature set but would need lifetime annotations or runtime checks to remain safe with mutable references.
