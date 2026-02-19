# Type System

## Summary of decisions

| Decision | Status |
|---|---|
| Static typing | Yes — declarative, mandatory |
| Type inference | Local inference only (Rust-style): explicit function signatures, inferred locals |
| Algebraic data types | Yes — sum types + product types + exhaustive pattern matching |
| Generics | Monomorphization for release, type erasure for dev mode (faster live reloading compilation) |
| Ad-hoc polymorphism | Haskell-style typeclasses (with Rust-style coherence rules) |
| Immutability | Immutable by default, explicit opt-in for mutation |

## Type inference: local only (Rust-style)

Top-level function signatures are always explicitly annotated (parameter types and return type). Within function bodies, local variable types are inferred.

**Why this over full Hindley-Milner:**
- Function boundaries are self-documenting — you can read a function's signature without running inference in your head
- Error messages are localized — a type mistake inside function A can't produce a confusing error in function B
- **Live reloading benefit**: the dependency tracker can instantly detect whether a signature changed by comparing declarations. Body-only changes never cascade to callers. Signature changes are always intentional and visible, making reload behavior predictable
- IDE/LSP support can show inferred types for locals inline, giving the benefits of inference where it's useful without losing the documentation value of explicit signatures

**What's inferred:**
- Local variable types in `let` bindings
- Closure parameter types (when inferrable from context)
- Intermediate expression types

**What's always explicit:**
- Top-level function parameter types and return type
- Type definitions (obviously)
- Typeclass instance declarations

## Algebraic data types

Sum types (tagged unions) + product types (structs/records) + pattern matching.

**Pattern matching is exhaustive by default** — a non-exhaustive match is a compile error. If you want to ignore cases, you explicitly write a wildcard `_` arm. This catches bugs when enum variants are added later (and the cascade recompiler re-checks exhaustiveness during live reloading).

## Generics / parametric polymorphism

**Middle ground: monomorphize for release, type erasure for dev.**

- **Dev mode (live reloading)**: type erasure — one compiled version of each generic function. Faster compilation means faster cascade/reload. Values passed through uniform representation (boxed/indirected).
- **Release mode (AOT)**: monomorphization — specialized version per concrete type. Zero runtime overhead, allows per-type optimization.

The behavioral semantics are identical in both modes; only performance characteristics differ. This aligns with the dev/release split throughout the architecture.

## Ad-hoc polymorphism: typeclasses

Haskell-style typeclasses with Rust-inspired pragmatic constraints.

### Why typeclasses over Rust traits

Typeclasses and traits are very similar — both provide compile-time ad-hoc polymorphism, both support superclass constraints, default implementations, and associated types. The key differences that favor typeclasses for Weirlang:

| | Haskell typeclasses | Rust traits |
|---|---|---|
| **Higher-kinded types** | Natural support. `class Functor f where fmap :: (a -> b) -> f a -> f b` — `f` is a type constructor. | No HKT support. Can't write `trait Functor<F<_>>`. Significant expressiveness gap. |
| **Multi-parameter** | Supported: `class Convert a b where convert :: a -> b`. With functional dependencies for disambiguation. | Only dispatch on `Self`. |
| **Terminology** | More natural in a Lisp context. | "Trait" is a Rust-ism. |
| **Orphan rules** | More relaxed (orphan instances allowed but discouraged). | Strict — only the crate defining the type or trait can write an impl. |

### Design choices for Weirlang typeclasses

- **Higher-kinded types**: supported. Essential for `Functor`, `Monad`, `Foldable`, etc. A typed Lisp without HKTs would be unnecessarily limited.
- **Multi-parameter typeclasses**: supported, with functional dependencies for disambiguation.
- **Coherence rules**: Rust-style strictness. Only the module defining the type *or* the typeclass can define an instance. Prevents conflicting instances, aligns with guard rails philosophy. More restrictive than Haskell, but prevents a real class of confusing bugs.
- **Associated types**: supported. Enables expressive typeclass definitions without type-level functions.
- **Default implementations**: supported. Reduces boilerplate for typeclass instances.
- **Deriving via macros**: `derive`-style macros can generate typeclass instances. The macro system's ability to generate type definitions covers this.

## Immutability by default

Values are immutable by default. Mutation requires explicit opt-in.

**Why:**
- Fewer aliasing bugs — shared data can't be mutated under you
- Better interaction with GC — immutable data can be shared freely
- More functional style, natural for a Lisp
- Enables more compiler optimizations (the compiler knows a value won't change)
- Aligns with guard rails philosophy — accidental mutation is a common source of bugs

**Explicit mutation** via `mut` bindings or mutable data structures:
```lisp
;; Immutable by default
(let ((x 5))
  ;; x cannot be mutated

;; Explicit mutation
(let ((mut x 5))
  (set! x 10))
```

Game dev requires mutation (game state changes every frame), so the opt-in mechanism must be ergonomic, not punitive. The goal is making mutation a conscious choice, not adding friction for its own sake.

## Syntax reference (inspirations)

Weirlang's type syntax draws from both Carp and Coalton, adapted for our specific design decisions.

### Carp syntax patterns

Carp uses `deftype` with bracket-delimited fields for structs and bare constructors for enums:

```clojure
;; Struct (product type)
(deftype Vector2 [x Int, y Int])

;; Sum type (enum)
(deftype (Maybe a)
  (Just [a])
  (Nothing []))

;; Separate sig for type annotations
(sig map (Fn [(Ref (Array a)) (Fn [a] b)] (Array b)))

;; Pattern matching
(match maybe-val
  (Just x) (use x)
  (Nothing) (default-val))
```

Carp's `definterface` + `implements` is simpler than typeclasses but less expressive (no HKTs, no multi-parameter).

### Coalton syntax patterns

Coalton uses `:` prefix for type variables and `declare` for type annotations:

```coalton
;; Sum type
(define-type (Tree :a)
  (Branch (Tree :a) :a (Tree :a))
  (Leaf :a))

;; Struct
(define-struct Point
  (x Integer)
  (y Integer))

;; Type annotation (separate from definition)
(declare map-optional ((:a -> :b) -> (Optional :a) -> (Optional :b)))

;; Typeclass
(define-class (Eq :a)
  (== (:a -> :a -> Boolean)))

;; Typeclass with superclass
(define-class (Eq :a => Ord :a)
  (<=> (:a -> :a -> Ord)))

;; Higher-kinded typeclass
(define-class (Functor :f)
  (map ((:a -> :b) -> :f :a -> :f :b)))

;; Instance
(define-instance (Functor Optional)
  (define (map f x)
    (match x
      ((Some a) (Some (f a)))
      ((None) None))))

;; Instance with constraint
(define-instance (Eq :a => Eq (Optional :a))
  (define (== x y)
    (match (Tuple x y)
      ((Tuple (Some x) (Some y)) (== x y))
      ((Tuple (None) (None)) True)
      (_ False))))
```

Coalton's typeclass system is the closest prior art for what we want — Haskell-style typeclasses in S-expression syntax, with HKT support.

### Weirlang syntax direction

The exact syntax is TBD, but will likely blend elements:
- Coalton's typeclass and type definition patterns (most directly applicable)
- Carp's struct syntax (field names in type definition)
- Rust-style inline annotations (type annotations on function parameters rather than separate `declare`/`sig`)
- `:` for type annotations within definitions (e.g., `(x : i32)`)

## Interaction with other systems

### With live reloading
- Explicit function signatures → clean dependency tracking, predictable cascade
- Typeclass instance changes → cascade to all code using that typeclass with the affected type
- Exhaustive match checking → re-verified on type redefinition

### With macros
- Macros can generate typeclass instances (derive-style)
- Macro cascade → potential typeclass instance changes → further cascade
- Purely syntactic macros + expressive type system reduces need for type-aware macros

### With arenas
- Generic functions work over both GC and arena-allocated values
- Arena escape analysis is separate from parametric polymorphism
- The escape checker tracks provenance; the type checker tracks types

### With memory management
- Immutable data can be freely shared (no aliasing concerns for GC)
- Mutable bindings require explicit `mut`, making mutation points visible to both developer and GC

## Open questions

- Exact syntax for type definitions, annotations, typeclasses, and instances (see syntax reference above for starting points)
- Should type aliases be supported? (Likely yes)
- How do functional dependencies work syntactically in S-expressions?
- What's the kind system? (Need at least `*` and `* -> *` for HKTs)
- Should there be a `Num` typeclass hierarchy or simpler numeric types?
- Mutation model decided: `mut` for reassignable bindings, functional struct update for modified copies. No mutable references — see syntax.md for details and future-work.md for the `ref` evaluation.
- Error handling decided: `Result` type + `?` propagation + `From` typeclass for error conversion. See syntax.md.
- Numeric types decided: full set (i8–i64, u8–u64, f32, f64); defaults i32/f64. Numeric typeclass hierarchy TBD.
