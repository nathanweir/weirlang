# Macro System

## Summary of decisions

| Decision | Status |
|---|---|
| Macro style | Purely syntactic (S-expression → S-expression) |
| Hygiene | Hygienic by default, with explicit escape hatches |
| Type generation | Macros can generate type definitions and typeclass instances |
| Type awareness | No — macros cannot query the type system during expansion |
| Side effects during expansion | Leaning toward disallowing (guard rails philosophy) |

## Purely syntactic macros

Macros transform S-expressions into S-expressions without type knowledge. The compilation pipeline stays strictly linear:

```
parse → macro expand → type check → compile
```

No circular dependencies between expansion and type checking. This makes the cascade model clean: re-expansion feeds into re-typechecking, which feeds into recompilation — each stage has clear inputs and outputs.

**What syntactic macros can do:**
- Transform and generate arbitrary S-expression code
- Generate type definitions (`deftype`, `defstruct`)
- Generate typeclass instances (`derive`-style)
- Generate function definitions
- Pattern-based or quasiquote-based code generation

**What syntactic macros cannot do:**
- Query the type of an expression during expansion
- Adapt output based on types of arguments
- These use cases should be handled by the type system instead (typeclasses, associated types, generics)

**Type-aware macros may be revisited** if concrete use cases in the language itself motivate the complexity. We want to see specific examples before committing to the implementation cost.

## Hygienic by default

Macro-generated code cannot accidentally shadow or capture variables from the call site. This is especially important in a live-reloading context where code is being swapped in and out — accidental capture across reload boundaries produces the worst kind of intermittent bugs.

Explicit escape hatches (mechanism TBD — Scheme's `syntax-parameterize`, an explicit `capture` form, or similar) cover the rare cases where intentional capture is needed.

## Macros can generate type definitions

Essential for first-class macro citizenship. Without this, macros would be second-class citizens unable to define key abstractions.

Use cases:
- `defstruct`-style convenience macros
- `derive`-style automatic typeclass instance generation
- ECS component definition macros
- Domain-specific type generation

The cascade model handles the complexity: macro redefinition → re-expansion → potential type changes → cascade to dependent code → soft restart for live instances if type layout changed.

The dependency tracker distinguishes:
- **Type defined directly by the user** → redefinition is a direct edit
- **Type defined by macro expansion** → the macro "owns" the type; redefining the macro may change the type

## Interaction with types and live reloading

See also: [execution-model.md](execution-model.md) for the cascade model details.

### The cascade in practice

```lisp
;; A macro that generates an ECS system
(defmacro defsystem (name component-type &body body)
  `(defn ,name ((entities : (List ,component-type))) : Unit
     (for-each entities (fn (e) ,@body))))

;; A type definition
(deftype Enemy
  (fields
    (pos : Vec2)
    (health : i32)))

;; A macro invocation
(defsystem update-enemies Enemy
  (set! (. e pos) (add (. e pos) (vec2 1.0 0.0))))
```

**Change `update-enemies` body** → recompile, swap pointer. Instant.

**Change `Enemy` type** → re-typecheck `update-enemies` and all Enemy-using code → soft restart for live instances.

**Change `defsystem` macro** → re-expand all call sites → compare output to previous expansion → short-circuit if unchanged → re-typecheck and recompile if changed.

### Short-circuit optimization

Most macro redefinitions won't change most expansions. The dependency tracker compares expansion outputs structurally and skips the downstream cascade when output hasn't changed. Critical for performance.

### Error reporting for macro-generated code

When macro-generated code has a type error:

1. **Primary location**: the macro call site ("error in expansion of `defsystem` at game.weir:15")
2. **Secondary location**: the specific point in the expansion that failed
3. **Expansion visibility**: developer can inspect the expanded code (like `cargo expand`, integrated into LSP)

During live reloading: keep the old working version running, report errors via LSP, don't crash.

### Forward references

- **Initial load**: process entire files as units, resolve forward references before type-checking
- **Live reloading**: if a new definition references an undefined type, report "type not found" and keep old version
- **Batched changes**: group related changes to avoid transient forward-reference errors

## Open questions

- What is the specific syntax/API for defining macros? (quasiquoting? pattern-based? procedural?)
- What escape hatch mechanism for intentional hygiene-breaking?
- How much of the language is built via macros vs being primitive?
- How does macro expansion interact with incremental compilation caching?
- Should there be a "macro expansion preview" mode in the editor?
- Should macros with side effects during expansion be disallowed entirely, or permitted with restrictions?
