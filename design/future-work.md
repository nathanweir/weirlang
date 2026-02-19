# Future Work

Deferred features that have been explicitly evaluated and set aside. Each entry includes the motivation, why it was deferred, what covers the use case today, and conditions for reconsideration.

---

## `ref` parameters (mutable references)

### What it is

A `ref` keyword on function parameters that gives the callee a mutable reference to the caller's data, enabling in-place struct field mutation via `set!`.

### Motivating example

```lisp
;; Function takes a mutable reference — can modify caller's data in place
(defn damage ((ref e : Enemy) (amount : i32)) : Unit
  (set! (.health e) (- (.health e) amount)))

(let ((mut e (spawn-enemy :pos (vec2 0 0) :health 100 :state Idle)))
  (damage e 10)    ;; e.health is now 90
  (.health e))     ;; => 90
```

### Why it was deferred

1. **Introduces aliasing.** Weir currently has no aliasing — function arguments are passed by value (copying the pointer), and callees cannot modify the caller's bindings. `ref` would be the first and only source of aliasing in the language, fundamentally changing the memory model.

2. **Arena escape vector.** With `ref`, a function can smuggle an arena-allocated pointer out of a `with-arena` block by writing it through a mutable reference. The lexical escape analysis cannot catch this because the `set!` happens in the callee, lexically outside the arena block. Mitigating this would require either lifetime annotations (complexity) or runtime tagging (overhead).

3. **Doesn't compose with concurrency.** Mutable references are inherently not `Shareable` — they enable concurrent mutation of shared data. This limits their usefulness in the concurrent/parallel patterns the language is designed around.

4. **Primarily a convenience feature.** The motivating use case is ergonomic in-place struct field mutation for gamedev. The performance argument (avoiding copies) is already addressed by arenas. The ergonomic argument is addressed by functional struct update.

### What covers the use case instead

**Functional struct update** — create a modified copy of a struct with specific fields changed:

```lisp
;; Functional update — returns new Enemy with modified health
(defn damage ((e : Enemy) (amount : i32)) : Enemy
  (update e :health (- (.health e) amount)))

(let ((e (spawn-enemy :pos (vec2 0 0) :health 100 :state Idle)))
  (let ((e2 (damage e 10)))
    (.health e2)))  ;; => 90
```

Combined with arenas, functional update is zero-cost for hot paths (bump allocation is near-free), and the compiler can potentially optimize single-use updates into in-place mutation as a future optimization.

### Conditions for reconsideration

- Profiling shows that functional update + arenas has unacceptable overhead for a real game workload
- A clean aliasing model is found that doesn't compromise arena safety or concurrency (e.g., uniqueness types, linear types)
- The language adds a borrow checker or lifetime system for other reasons, making `ref` safety "free"
