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
    (make-level entities)))

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

### Arena escape prevention: compile-time

Arena-allocated values cannot escape their `with-arena` block. The compiler enforces this statically via escape analysis:

- **Cannot** be returned from the `with-arena` block
- **Cannot** be stored in GC-heap data structures
- **Cannot** be captured by closures that outlive the block
- **Can** be passed to functions called within the block, used in computations, etc.

This is much simpler than full Rust-style lifetime checking because the "lifetime" is always tied to a lexical scope. No lifetime annotations, no lifetime elision rules. The compiler tracks: "did this value originate from an arena? If so, it can't escape."

If data needs to outlive the arena, it should be GC-allocated instead — that's a design signal that the data isn't transient. Any use case where you'd "want" arena data to escape is an anti-feature.

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

- What GC algorithm to start with? (Mark-and-sweep is simplest; generational is better for games but more complex)
- Should there be named arenas (multiple concurrent arenas with different lifetimes)?
- Should arena size be configurable per-block, or globally set?
- Can arenas be nested? (Probably yes — inner arena values can reference outer arena values but not vice versa)
