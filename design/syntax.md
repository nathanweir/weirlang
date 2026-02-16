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
| Mutability | `mut` keyword in binding form |
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
| Type assertion | `(as type expr)` — inline type annotation for disambiguation, not casting |
| Block expressions | Implicit sequencing in bodies; explicit `do` block available for expressions |

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

`mut` keyword in the binding form. `set!` for mutation (Scheme convention — `!` suffix signals mutation).

```lisp
;; Immutable (default)
(let ((x 5))
  ;; x cannot be mutated
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

For the parser: a binding is one of:
- `(name value)` — immutable, type inferred
- `(mut name value)` — mutable, type inferred
- `((name : type) value)` — immutable, type annotated
- `((mut name : type) value)` — mutable, type annotated

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

## Type assertion (`as`)

`as` is an inline type annotation that constrains inference — it tells the compiler which type to pick when multiple are valid. It is **not** casting or coercion: no runtime conversion happens. If the expression can't be the asserted type, it's a compile error.

```lisp
;; Disambiguate numeric literals
(as i32 42)
(as f32 (* delta speed))

;; Pin a polymorphic return type
(as (List Enemy) (filter alive? entities))

;; Compile error — not a conversion:
(as String 42)  ;; Error: i32 is not String
```

For actual type conversion, use explicit conversion functions or typeclass methods (e.g., a `From`/`Into` pattern).

## Module system

### File = module

File path determines module identity. No explicit module declaration needed.

```
src/game/physics.weir   →  module game.physics
src/math/vec2.weir      →  module math.vec2
src/main.weir           →  entry point
```

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
         (make-enemy
           :pos (vec2 (+ (.x origin) (* (the f32 i) 32.0)) (.y origin))
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
