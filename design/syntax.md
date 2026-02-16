# Syntax Design

## Summary of decisions

| Decision | Choice |
|---|---|
| Overall syntax | S-expression / Lisp family |
| Type declarations | Both inline and separate supported; inline preferred |
| Function type syntax | Carp-style `(Fn [args] return)` |
| Type variable syntax | Quote prefix: `'a`, `'b` (OCaml-style) |
| Keyword syntax | Colon prefix: `:name`, `:health` (CL-style, no conflict with type vars) |
| Typeclass constraints | Prefix with `=>`: `(=> (Eq 'a) ...)` |
| Pattern matching | Each case wrapped in parens |
| Record field access | Coalton-style `.field` accessor functions |
| Mutability | `mut` for reassignable bindings, `ref` for mutable references |
| Collection literals | `[]` for arrays/vectors, `{}` for maps |
| Functions | Multi-argument (not curried) |
| String formatting | `format` macro (compile-time checked) + `str` function |
| Comments | `;` line comments, docstrings in definitions |
| Pipe/threading | `->` (thread-first) and `->>` (thread-last); `->` also used for chained field access |
| Module system | File = module, `import` at top-level, compiler resolves order |
| Visibility | Private by default, `pub` modifier per definition (Rust-style) |
| Circular imports | Disallowed (compile error) |
| Qualified access | Dot-separated: `module.function`, `.field` for accessor |
| Import form | Top-level-only special form, not a runtime function |
| Named arguments | Optional at call site, positional in definition; compiler-checked |
| Destructuring | Supported; irrefutable patterns only in `let`/params, refutable in `match` only |
| Control flow | `if` (else required when result used), `cond` (else required), `when`/`unless` |
| Unit type | `Unit` — the "no meaningful value" type, returned by side-effecting functions |
| Type definition forms | Separate: `deftype` for sum types (enums), `defstruct` for product types (structs) |
| Struct construction | Type name is constructor; supports positional and named args (same as functions) |
| Type assertion | `(ann type expr)` — inline type annotation for disambiguation, not casting |
| Block expressions | Implicit sequencing in bodies; explicit `do` block available for expressions |
| Error handling | `Result` type + `?` propagation operator; errors visible in type signatures |
| Lambdas | `(fn (params) body)`, mirrors `defn`; capture by reference (standard for GC'd languages) |
| FFI | C FFI via `extern "C"` + safe Rust-implemented wrappers; raw C calls require `unsafe` block |
| Numeric types | Full set (`i8`–`i64`, `u8`–`u64`, `f32`, `f64`); defaults: `i32` for integers, `f64` for floats |

## Type declarations

Both inline and separate type declarations are supported. Inline is preferred for most functions; separate is available for complex types where inline gets unwieldy.

### Inline (preferred)

```lisp
(defn add ((x : i32) (y : i32)) : i32
  (+ x y))

(defn map ((f : (Fn ['a] 'b)) (xs : (List 'a))) : (List 'b)
  (match xs
    (Nil Nil)
    ((Cons x rest) (Cons (f x) (map f rest)))))
```

### Separate (for complex types)

```lisp
(declare transform
  (=> (Functor 'f)
      (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
(defn transform (func container)
  (map func container))
```

When both are present, they must agree — a mismatch is a compile error.

## Function type syntax

Carp-style `Fn` with brackets separating arguments from return type:

```lisp
(Fn [i32 String] Bool)           ;; takes i32 and String, returns Bool
(Fn [] Unit)                      ;; takes no args, returns Unit
(Fn [(List 'a) (Fn ['a] 'b)] (List 'b))  ;; higher-order
```

Functions are multi-argument (not curried). Passing the wrong number of arguments is an immediate compile error, not a silent partial application. Partial application is available explicitly when needed (e.g., via `partial` or lambda).

## Type variable syntax

Quote-prefixed lowercase: `'a`, `'b`, `'elem`, `'container`.

```lisp
(deftype (Option 'a)
  (Some 'a)
  None)

(deftype (Result 'ok 'err)
  (Ok 'ok)
  (Err 'err))

(declare map (Fn [(Fn ['a] 'b) (List 'a)] (List 'b)))
```

This avoids any conflict with `:keyword` syntax (colon prefix), which is reserved for keywords in value positions:

```lisp
;; Keywords (value context) — always colon prefix
{:name "Alice" :health 100}
(spawn-enemy :pos (vec2 0 0) :health 50)

;; Type variables (type context) — always quote prefix
(declare lookup (Fn [(Map 'k 'v) 'k] (Option 'v)))
```

No ambiguity between the two — different prefix, different context.

## Typeclass constraint syntax

Constraint prefix with `=>`:

```lisp
;; Single constraint
(declare equal? (=> (Eq 'a) (Fn ['a 'a] Bool)))

;; Multiple constraints
(declare serialize (=> (Show 'a) (Eq 'a) (Fn ['a] String)))

;; In typeclass definition
(defclass (=> (Eq 'a) (Ord 'a))
  (<=> : (Fn ['a 'a] Ordering)))

;; Higher-kinded
(defclass (Functor 'f)
  (map : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
```

## Pattern matching

Each case wrapped in its own parenthesized pair:

```lisp
(match x
  ((Some val) (use val))
  (None (default-value)))

(match enemy-state
  ((Patrol start end) (move-between start end))
  ((Chase target-id) (pursue target-id))
  (Idle (stand-still))
  (Dead (remove-entity)))
```

Pattern matching is **exhaustive by default** — a non-exhaustive match is a compile error. Use `_` wildcard to explicitly handle remaining cases.

## Record / struct field access

Coalton-style: `.field` is a first-class accessor function.

```lisp
;; Access a field
(.pos enemy)         ;; returns the pos field of enemy

;; .field is a function — composable with map, filter, etc.
(map .pos enemies)   ;; extract all positions
(filter (fn (e) (> (.health e) 0)) enemies)  ;; filter by health

;; Nested access
(.x (.pos enemy))
```

## Mutability syntax

Two distinct concepts, two distinct keywords:

- **`mut`** — the local binding is reassignable. Value semantics. The caller's data is never affected.
- **`ref`** — a mutable reference. The function can modify the caller's data in place.

`set!` for mutation (Scheme convention — `!` suffix signals mutation).

### `mut` — reassignable bindings

```lisp
;; Immutable (default)
(let ((x 5))
  ;; x cannot be reassigned
  (+ x 1))

;; Mutable — explicit opt-in
(let ((mut x 5))
  (set! x (+ x 1))
  x)  ;; => 6

;; With type annotation (uncommon — locals are inferred)
(let (((mut x : i32) 5))
  (set! x 10)
  x)
```

### `ref` — mutable references

For functions that need to modify the caller's data in place. The caller must pass a `mut` binding.

```lisp
;; Function takes a mutable reference
(defn damage ((ref e : Enemy) (amount : i32)) : Unit
  (set! (.health e) (- (.health e) amount)))

;; Caller must have a mut binding
(let ((mut e (spawn-enemy :pos (vec2 0 0) :health 100 :state Idle)))
  (damage e 10)    ;; e.health is now 90
  (.health e))     ;; => 90
```

Field mutation via `set!` is only valid through a `ref` parameter. Without `ref`, struct fields are read-only:

```lisp
;; Immutable binding — can read fields, not mutate them
(let ((e (spawn-enemy ...)))
  (.health e)                          ;; OK — read
  (set! (.health e) 50))              ;; COMPILE ERROR — e is not a mutable reference

;; mut binding without ref — can reassign the whole binding, not individual fields
(let ((mut e (spawn-enemy ...)))
  (set! e (spawn-enemy ...))           ;; OK — reassign entire binding
  (set! (.health e) 50))              ;; COMPILE ERROR — use ref in a function to mutate fields
```

### Summary

| Keyword | Meaning | Field mutation? | Caller affected? |
|---|---|---|---|
| (none) | Immutable binding | No | No |
| `mut` | Reassignable binding | No | No |
| `ref` (in params) | Mutable reference | Yes, via `set!` | Yes |

For the parser, a binding is one of:
- `(name value)` — immutable, type inferred
- `(mut name value)` — reassignable, type inferred
- `((name : type) value)` — immutable, type annotated
- `((mut name : type) value)` — reassignable, type annotated

A function parameter is one of:
- `(name : type)` — immutable
- `(mut name : type)` — reassignable local copy
- `(ref name : type)` — mutable reference to caller's data

## Collection literals

Square brackets for arrays/vectors, curly braces for maps. Parens remain for S-expressions (code, lists).

```lisp
;; Array / vector literal
[1 2 3 4 5]
["hello" "world"]

;; Map literal (keywords as keys)
{:name "Alice"
 :health 100
 :pos (vec2 0.0 0.0)}

;; Nested
{:player {:name "Alice" :level 5}
 :enemies [(spawn-goblin) (spawn-orc)]}

;; Empty
[]
{}
```

Parser treats `[]` and `{}` as alternative delimiters producing tagged AST nodes (`VectorLiteral`, `MapLiteral`). Small parser change, large ergonomic benefit.

## String formatting

### `format` macro (primary — compile-time checked)

```lisp
(format "Player {} has {} health" name health)
;; Compile-time: checks arity (2 placeholders, 2 args)
;; Compile-time: checks Show constraint on each arg
;; Expands to string concatenation with (show ...) calls

;; Compile error — arity mismatch:
(format "Player {} has {} health" name)
;; Error: format expects 2 arguments, got 1
```

### `str` function (simple concatenation)

```lisp
(str "Hello, " name "!")
;; Just concatenates, each arg converted via Show
```

### Printing (separate from formatting)

```lisp
(println (format "Player {} has {} health" name health))
(print "no newline")
(println "with newline")
```

No CL-style destination argument. Printing and formatting are separate concerns.

## Comments

```lisp
;; Line comment (standard Lisp)

;; Docstring as first expression in definition
(defn add ((x : i32) (y : i32)) : i32
  "Add two integers."
  (+ x y))
```

## Pipe / threading operators

Two threading macros: `->` (thread-first) and `->>` (thread-last).

### `->` (thread-first)

Inserts the previous result as the **first** argument to each subsequent form. Primary mechanism for chained field access and object-like pipelines:

```lisp
;; Chained field access (instead of (.x (.pos enemy)))
(-> enemy .pos .x)

;; Mixed field access and function calls
(-> world
    .entities
    (filter alive?)
    (map .pos))
```

### `->>` (thread-last)

Inserts the previous result as the **last** argument. Natural for collection pipelines where the data argument is conventionally last:

```lisp
(->> (range 100)
     (filter even?)
     (map square)
     (take 10))
;; Expands to: (take (map (filter (range 100) even?) square) 10)
```

`as->` (bind-to-name threading) intentionally omitted — a `let` binding covers those rare cases.

## Type assertion (`ann`)

`ann` (annotate) is an inline type annotation that constrains inference — it tells the compiler which type to pick when multiple are valid. It is **not** casting or coercion: no runtime conversion happens. If the expression can't be the asserted type, it's a compile error.

```lisp
;; Disambiguate numeric literals
(ann i32 42)
(ann f32 (* delta speed))

;; Pin a polymorphic return type
(ann (List Enemy) (filter alive? entities))

;; Compile error — not a conversion:
(ann String 42)  ;; Error: i32 is not String
```

For actual type conversion, use explicit conversion functions or typeclass methods (e.g., a `From`/`Into` pattern).

## Numeric types

Full set of fixed-size numeric types, matching Rust/C for FFI completeness:

| Category | Types |
|---|---|
| Signed integers | `i8`, `i16`, `i32`, `i64` |
| Unsigned integers | `u8`, `u16`, `u32`, `u64` |
| Floating point | `f32`, `f64` |

### Defaults for unadorned literals

- Integer literals (`42`, `-7`) default to `i32`
- Float literals (`3.14`, `-0.5`) default to `f64`
- Use `ann` to disambiguate when needed: `(ann f32 3.14)`, `(ann u8 255)`

```lisp
(let ((x 42))          ;; x : i32 (default)
     ((y 3.14))        ;; y : f64 (default)
     ((z (ann f32 3.14)))  ;; z : f32 (explicit)
  ...)
```

Numeric conversions are explicit — no implicit widening or narrowing. Use conversion functions or a `From`/`Into` typeclass.

## Module system

### File = module

File path determines module identity. No explicit module declaration needed.

```
src/game/physics.weir   →  module game.physics
src/math/vec2.weir      →  module math.vec2
src/main.weir           →  entry point
```

### Prelude

An implicit prelude is automatically in scope for all modules — no import needed. Contents will be built up during implementation, but expected to include:

- Primitive types: all numeric types, `Bool`, `String`, `Unit`
- Core ADTs: `Option` (`Some`, `None`), `Result` (`Ok`, `Err`)
- Core typeclasses: `Eq`, `Ord`, `Show`, `From`, `Shareable`
- Basic operations: arithmetic, comparison, boolean logic
- Collection types: `List`, `Array`, `Map`
- I/O: `println`, `print`

### Imports

`import` is a **top-level-only special form** — not a function, not valid inside expressions. The compiler needs all imports statically to build the dependency DAG, resolve names, and manage live reloading cascades.

```lisp
;; Import specific items
(import game.entities (Enemy spawn-enemy update-enemy))

;; Import with alias for qualified access
(import math.vec2 :as v)
;; then: (v.add a b)

;; Import everything (discouraged, available for REPL/scripting)
(import math.vec2 :all)
```

### Visibility

Private by default, `pub` modifier per definition:

```lisp
(pub defn spawn-enemy (...) ...)    ;; public
(pub deftype Enemy ...)             ;; public
(defn internal-helper (...) ...)    ;; private — not accessible from outside
```

### Qualified access

```lisp
;; Via alias
(import math.vec2 :as v)
(v.add a b)

;; Via direct import
(import math.vec2 (add))
(add a b)

;; Fully qualified (always works)
(math.vec2.add a b)
```

### Dot disambiguation

- **`.name`** (dot at start) → field accessor function
- **`module.name`** (dot in middle) → qualified module access

No ambiguity — modules are known statically from the file system.

### Circular imports: disallowed

Circular imports are a compile error. If two modules need each other, extract shared types into a third module. Prevents initialization-order bugs, encourages clean dependency hierarchies.

### Dependency resolution

The compiler scans all `.weir` files, parses imports, builds a dependency DAG, and compiles in topological order. No manual file ordering.

### Project configuration

```toml
# weir.toml
[project]
name = "my-game"
version = "0.1.0"
entry = "src/main.weir"

[dependencies]
# external packages
```

## Type definition forms

Separate forms for sum types and product types, following Coalton's pattern.

### `deftype` — sum types (enums)

```lisp
;; Simple enum
(deftype (Option 'a)
  (Some 'a)
  None)

(deftype (Result 'ok 'err)
  (Ok 'ok)
  (Err 'err))

;; Enum with mixed data
(deftype EnemyState
  Idle
  (Patrol Vec2 Vec2)
  (Chase EntityId)
  Dead)
```

### `defstruct` — product types (structs)

```lisp
(defstruct Enemy
  (pos : Vec2)
  (health : i32)
  (state : EnemyState))

(defstruct Vec2
  (x : f32)
  (y : f32))
```

No `fields` keyword needed — `defstruct` is unambiguous. Fields always have names and types.

### Struct construction

The type name is automatically a constructor function. Supports both positional and named arguments, consistent with regular function calls:

```lisp
;; Positional (fine for small structs)
(Vec2 1.0 2.0)

;; Named (better for larger structs)
(Enemy :pos (vec2 0 0) :health 100 :state Idle)

;; Mixed (positional first, then named)
(Enemy (vec2 0 0) :health 100 :state Idle)
```

## Bringing it all together

What Weirlang code looks like:

```lisp
;; Enum with data
(deftype EnemyState
  Idle
  (Patrol Vec2 Vec2)
  (Chase EntityId)
  Dead)

;; Typeclass
(defclass (Show 'a)
  (show : (Fn ['a] String)))

;; Typeclass with superclass
(defclass (=> (Eq 'a) (Ord 'a))
  (<=> : (Fn ['a 'a] Ordering)))

;; Higher-kinded typeclass
(defclass (Functor 'f)
  (map : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))

;; Typeclass instance
(instance (Show Enemy)
  (defn show (e)
    (format "Enemy at {} with {} hp" (.pos e) (.health e))))

;; Instance with constraint
(instance (=> (Show 'a) (Show (Option 'a)))
  (defn show (opt)
    (match opt
      ((Some x) (format "Some({})" x))
      (None "None"))))

;; Function with types
(defn spawn-wave ((n : i32) (origin : Vec2)) : (List Enemy)
  (map (fn (i)
         (Enemy
           :pos (vec2 (+ (.x origin) (* (ann f32 i) 32.0)) (.y origin))
           :health 100
           :state Idle))
       (range n)))

;; Mutable game state
(defn update-physics ((mut world : World)) : Unit
  (with-arena frame
    (let ((contacts (detect-collisions world))
          (impulses (solve-constraints contacts)))
      (apply-impulses world impulses))))

;; Using collections
(let ((scores {})
      (enemies []))
  (println (format "Tracking {} enemies" (length enemies))))
```

## Named arguments

Callers can optionally use keyword syntax to name arguments at the call site. The definition is always positional — no extra syntax for the function author.

```lisp
;; Definition — normal positional parameters
(defn spawn-enemy ((pos : Vec2) (health : i32) (state : EnemyState)) : Enemy
  ...)

;; All valid call styles:
(spawn-enemy (vec2 0 0) 50 Idle)                              ;; positional
(spawn-enemy :pos (vec2 0 0) :health 50 :state Idle)          ;; named
(spawn-enemy :health 50 :pos (vec2 0 0) :state Idle)          ;; named, any order
(spawn-enemy (vec2 0 0) :health 50 :state Idle)               ;; mixed (positional first)
```

The compiler verifies at compile time that keyword names match parameter names. A typo like `:heatlh` is a compile error, not a runtime surprise.

**Consequence:** parameter names are part of the public API. Renaming a parameter can break callers who use the keyword form. This is arguably a feature — it encourages meaningful parameter naming and makes renames a visible, intentional API change (caught by the cascade recompiler during live reloading).

## Destructuring

Destructuring is supported in `let` bindings and function parameters, but restricted to **irrefutable patterns** (patterns that always match). Refutable patterns (enum variants that might not match) require `match`.

### Struct destructuring (curly-brace syntax)

```lisp
;; Destructure struct fields into local bindings
(let (({x y} my-vec))
  (+ x y))

;; In function parameters
(defn distance (({x ax y ay} : Vec2) ({x bx y by} : Vec2)) : f32
  (sqrt (+ (square (- bx ax)) (square (- by ay)))))

;; Partial destructuring (only some fields)
(let (({health} enemy))
  (> health 0))
```

### Tuple/product destructuring

```lisp
(let (((a b) my-pair))
  (+ a b))
```

### Irrefutable vs refutable

```lisp
;; OK — struct destructuring always succeeds
(let (({x y} my-vec))
  (+ x y))

;; COMPILE ERROR — Option might be None, use match instead
(let (((Some val) maybe-result))
  (use val))

;; Correct — refutable patterns go in match
(match maybe-result
  ((Some val) (use val))
  (None (default-value)))
```

This is a guard rail: the compiler prevents you from writing destructuring that could fail at runtime. If a pattern can fail, you're forced to handle all cases via `match`.

## Control flow

### `if` — binary conditional expression

```lisp
;; As expression — else branch required
(let ((status (if alive? :active :dead)))
  ...)

;; As statement (result unused) — else optional, implicit Unit
(if (key-pressed? :escape)
  (open-menu))
```

### `cond` — multi-branch conditional expression

`else` branch is required — an unhandled case is a silent bug.

```lisp
(cond
  ((< health 0) (die))
  ((< health 20) (flee))
  ((< health 50) (defend))
  (else (attack)))
```

### `when` / `unless` — single-branch side-effecting forms

Always return `Unit`. No else branch. Makes intent clear: "I only care about this one case."

```lisp
(when (key-pressed? :escape)
  (open-menu)
  (pause-game))

(unless (alive? entity)
  (remove entity))
```

### Unit

`Unit` is the type with exactly one value, used for functions called for their side effects. Unlike `void` in C/Java, `Unit` is a real type that composes normally — a `(List Unit)` is valid, a function returning `Unit` is a regular function.

```lisp
(defn greet ((name : String)) : Unit
  (println (format "Hello, {}!" name)))
```

## Block expressions

Function bodies, `let` bodies, `when`/`unless` bodies, and lambda bodies implicitly sequence multiple expressions. The last expression is the return value. This is standard Lisp behavior.

```lisp
(defn update-enemy ((mut e : Enemy)) : Unit
  (set! (.health e) (- (.health e) 1))
  (set! (.pos e) (move (.pos e)))
  (check-death e))

(let ((x 5))
  (println x)
  (+ x 1))  ;; returns 6
```

An explicit `do` block is available for contexts where you need to sequence multiple expressions inside a single-expression position (e.g., one branch of an `if`):

```lisp
(if condition
  (do (log "taking branch A")
      (branch-a))
  (branch-b))
```

## Lambdas / closures

Lambda syntax mirrors `defn` without the name. Type annotations are optional (usually inferred from context).

```lisp
;; Type-inferred (common)
(fn (x) (+ x 1))

;; Type-annotated
(fn ((x : i32)) : i32 (+ x 1))

;; Multi-arg
(fn ((x : i32) (y : i32)) (+ x y))

;; Multi-expression body (implicit sequencing)
(fn (e)
  (println (.name e))
  (.health e))
```

### Capture semantics

Closures capture by reference — the closure shares the variable with its enclosing scope. This is the standard for GC'd languages (no lifetime concerns). Changes to a `mut` binding in the enclosing scope are visible to the closure and vice versa.

```lisp
(let ((mut x 5))
  (let ((f (fn () x)))
    (set! x 10)
    (f)))           ;; => 10
```

Thread safety is handled by the `Shareable` typeclass, not the capture mechanism: a closure that captures a `mut` binding is not `Shareable` and cannot be sent across threads. This is a compile-time check.

## Error handling

Errors are values, represented by the `Result` type. Functions that can fail declare this in their return type. The `?` operator propagates errors up the call stack.

```lisp
;; Function that can fail — Result in return type
(defn load-level ((path : String)) : (Result Level IOError)
  (let ((data (read-file path)?)            ;; ? propagates IOError on failure
        (entities (parse-entities data)?))
    (Ok (Level entities))))

;; Callers must handle the Result
(match (load-level "level1.weir")
  ((Ok level) (start level))
  ((Err e) (show-error e)))
```

The `?` operator on a `(Result 'a 'err)` expression:
- If `(Ok val)` → unwraps to `val`, execution continues
- If `(Err e)` → immediately returns `(Err e)` from the enclosing function

For functions that can produce multiple error types, a `From` typeclass enables automatic error conversion at `?` sites:

```lisp
;; GameError can be constructed from IOError or ParseError
(instance (From IOError GameError) ...)
(instance (From ParseError GameError) ...)

;; ? automatically converts via From
(defn load-level ((path : String)) : (Result Level GameError)
  (let ((data (read-file path)?)            ;; IOError → GameError via From
        (entities (parse-entities data)?))   ;; ParseError → GameError via From
    (Ok (Level entities))))
```

This keeps errors visible in type signatures — you can never accidentally ignore a fallible operation. Aligns with the guard rails philosophy.

## FFI (Foreign Function Interface)

Two layers: raw C FFI for binding external libraries, and safe Rust-implemented wrappers for common functionality.

### C FFI

Declare external C functions with `extern "C"`. Calling them requires an `unsafe` block — C code can violate all of Weirlang's safety guarantees.

```lisp
;; Declare external C functions
(extern "C"
  (defn SDL_Init ((flags : u32)) : i32)
  (defn SDL_CreateWindow ((title : CString) (x : i32) (y : i32)
                          (w : i32) (h : i32) (flags : u32)) : (Ptr SDL_Window)))

;; Must wrap calls in unsafe
(unsafe
  (SDL_Init SDL_INIT_VIDEO)
  (let ((window (SDL_CreateWindow "My Game" 0 0 800 600 0)))
    ...))
```

### Safe Rust wrappers (primary interface)

The standard library and runtime are implemented in Rust. These provide safe abstractions over I/O, graphics, networking, etc. No `unsafe` needed — the Rust compiler already enforces safety on that side.

```lisp
;; These are safe — implemented in Rust, linked into the runtime
(let ((window (create-window "My Game" 800 600)))
  (println "Window created"))
```

### What `unsafe` means in Weirlang

`unsafe` is narrower than in Rust. It means: "I'm calling code that the Weirlang compiler can't verify." Specifically:
- Calling `extern "C"` functions
- Calling Rust functions explicitly marked `unsafe`

Safe Rust code (the vast majority of the runtime/stdlib) does not require `unsafe` — the Rust compiler already guarantees safety. The expected pattern: C FFI lives behind safe wrapper libraries. Most developers never write `unsafe`.

## Open questions


- Functional update syntax for structs (creating modified copies without `ref`)
- Named arguments + threading macro interaction (where does threaded value go?)

- `pub` modifier syntax (wrapper vs inline modifier — revisit with more examples)
