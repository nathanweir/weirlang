# Concurrency Model

## Decision: Layered concurrency with compile-time thread safety

A composable set of concurrency primitives covering the full range of game dev needs, with data race prevention enforced by the type system.

Immutable-by-default does most of the heavy lifting: immutable data is freely shared across threads with zero synchronization. The concurrency primitives only matter for the explicitly-mutable parts of the program.

## The four layers

### Layer 1: Structured Concurrency (foundation)

All concurrent work is scoped — spawned tasks must complete before their parent scope exits. No orphaned tasks, no fire-and-forget. Errors propagate from child to parent.

```lisp
(with-tasks
  (spawn (load-assets level-path))
  (spawn (connect-to-server addr))
  ;; both must complete before with-tasks exits
  )
```

**Why structured:** Unstructured concurrency (detached goroutines, fire-and-forget threads) is a major source of bugs — resource leaks, silent failures, ordering assumptions violated. Structured concurrency prevents all of these by construction. Aligns with the guard rails philosophy.

**Game dev mapping:**
- System update spawns parallel subtasks → all must complete before update phase ends
- Background loading scoped to a scene → cancelled when scene unloads
- Network tasks scoped to a session → clean up when session ends

### Layer 2: Parallel Iteration (data parallelism)

For processing entity arrays, collections, and other bulk operations. Safe when the operation is pure (which immutable-by-default encourages).

```lisp
(par-map entities update-position)
(par-for-each chunks process-chunk)
```

**Game dev mapping:** The ECS pattern — process 10,000 entities across a thread pool. The most common parallelism need in games.

### Layer 3: Channels (inter-component communication)

Typed channels for communication between long-lived subsystems.

```lisp
(let ((render-chan (channel RenderCommand)))
  (spawn (render-loop render-chan))
  (send render-chan (draw-sprite player-sprite pos)))
```

**Game dev mapping:** Game thread ↔ render thread, game thread ↔ audio thread, game thread ↔ network thread.

### Layer 4: Atoms (shared mutable state)

For cases where shared mutable state is genuinely needed. Compare-and-swap semantics — never blocks, never deadlocks.

```lisp
(let ((score (atom 0)))
  (swap! score (fn (s) (+ s 10))))
```

**Why atoms over mutexes:** Atoms are lock-free (compare-and-swap), composable, and never deadlock. Mutexes are error-prone (deadlocks, forgotten unlocks, priority inversion). Guard rails philosophy favors atoms.

## Compile-time thread safety

The type system prevents data races at compile time via a `Shareable` typeclass (analogous to Rust's `Send`):

| Type | Shareable? | Why |
|---|---|---|
| Immutable values | Always | No mutation = no data races |
| Mutable bindings (`mut`) | No | Could be reassigned concurrently |
| `(Atom a)` | Yes, if `a` is `Shareable` | CAS provides safe concurrent access |
| `(Channel a)` | Yes, if `a` is `Shareable` | Channels are designed for cross-thread use |
| Arena-scoped values | Never | Can't even escape their scope, let alone cross threads |

The compiler checks `Shareable` constraints at `spawn` and `send` boundaries. If you try to send a mutable binding across a thread boundary, it's a compile error — not a runtime race condition.

## Interaction with other decisions

**Immutable-by-default:** The foundation that makes this model work. Most data is freely shareable with zero effort.

**Arenas:** Arena-scoped values are naturally thread-local (they can't escape their scope). No special handling needed — the escape analysis already prevents cross-thread sharing.

**GC:** The GC must be thread-safe (concurrent collection, safe allocation from multiple threads). This is a non-trivial implementation requirement but well-understood.

**Live reloading:** The function table indirection handles this naturally — when a function is redefined, the next call uses the new version, but currently-executing code continues with the old version. Spawned tasks pick up new code on their next function call. Same semantics as Erlang.

**Typeclasses:** `Shareable` is a typeclass with compiler-derived instances. Immutable types automatically derive `Shareable`. Mutable containers derive it only if their contents are `Shareable`. This leverages the typeclass system we've already designed.

## Open questions

- Green threads vs OS threads vs hybrid (implementation detail)
- Bounded vs unbounded channels
- Whether to add STM for coordinated multi-atom transactions
- Thread pool configuration and sizing
- Exact syntax for all concurrency primitives
- How does the parallel iterator interact with the ECS pattern? (Should there be first-class ECS support, or is par-map sufficient?)
- Cancellation semantics for structured concurrency (cooperative vs forced)
