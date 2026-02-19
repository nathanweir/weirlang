# Implementation Plan: Struct Codegen & Tail-Call Optimization

**Status:** Active
**Prerequisite:** Phases 0-10 complete (see `archive/implementation-plan-phases-0-10.md`)

---

## Phase 11: Struct Codegen ✅

**Completed 2026-02-19.** 613 tests passing (was 606). Struct destructure syntax changed from `{x y}` to `{:x :y}` (keyword-based) during implementation.

Structs work in the type checker and interpreter but were completely absent from the Cranelift JIT/AOT backend. Any program using `defstruct` would type-check and interpret correctly but crash during codegen.

### Current state

| Layer | Status |
|-------|--------|
| Parser / AST | Done — `Item::Defstruct`, `ExprKind::FieldAccess`, `PatternKind::StructDestructure` |
| Type checker | Done — `struct_defs` HashMap, `check_struct_construction()`, `check_field_access()`, struct destructure in patterns |
| Interpreter | Done — `Value::StructConstructor`, `Value::StructInstance`, construction (positional + named), field access, pattern destructure |
| Codegen | **Not started** — `ty_to_cl_with()` returns `None` for struct types, no construction/access/destructure codegen |

### Representation

Structs are heap-allocated, GC-managed, fixed-layout objects — the same strategy already used for closures and tagged ADT payloads:

- Each struct instance is a GC object: `weir_gc_alloc(shape, size)` returns an `i64` pointer
- Fields are stored as consecutive i64 slots at offsets `0, 8, 16, ...`
- Non-i64 fields (f32, i8, bool) are widened to i64 for uniform slot size (matching the existing pattern for vector elements and ADT payloads)
- The `ShapeDesc` uses `SHAPE_FIXED` with a `pointer_mask` marking which slots contain heap pointers

### Steps

#### 11.1 — Pass struct definitions to codegen

`TypeCheckResult` currently has no struct info. Add a public struct metadata type and field:

```rust
// In weir-typeck/src/result.rs
pub struct StructInfo {
    pub fields: Vec<(SmolStr, Ty)>,  // field name + resolved type
}

// In TypeCheckResult:
pub struct_defs: HashMap<SmolStr, StructInfo>,
```

Populate this in `TypeChecker::finish()` by resolving each `StructDef`'s field `TypeExprId`s to concrete `Ty`s.

**Files:** `crates/weir-typeck/src/result.rs`, `crates/weir-typeck/src/checker.rs`

#### 11.2 — Recognize struct types in codegen

In `Compiler::new()`, collect struct names from `type_info.struct_defs` into a `struct_defs: HashMap<SmolStr, StructInfo>` field on `Compiler` (and threaded into `FnCompileCtx`).

In `ty_to_cl_with()`, add a `struct_names: Option<&HashSet<SmolStr>>` parameter (or check in `ty_cl()` directly) so that `Ty::Con("Point", _)` returns `Some(types::I64)` — structs are pointer-sized.

In `is_heap_pointer()`, return `true` for struct-typed `Ty::Con`.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.3 — Struct construction codegen

When `compile_call()` sees a callee name in `struct_defs`:

1. Compile each argument expression
2. Widen each to i64
3. Build a `SHAPE_FIXED` shape descriptor with `num_slots = field_count` and `pointer_mask` bits set for heap-pointer fields
4. Call `weir_gc_alloc(shape, field_count * 8)`
5. Store each field at `ptr + i * 8` using `builder.ins().store(...)`
6. Push the result onto the GC shadow stack
7. Return the pointer as `i64`

This mirrors the existing closure allocation code pattern.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.4 — Field access codegen

When `compile_expr()` encounters a `Call` where the callee is `ExprKind::FieldAccess(field_name)`:

1. Look up the object's type from `expr_types`
2. Find the struct definition and the field's index
3. Compile the object expression to get the pointer
4. `builder.ins().load(field_type, MemFlags::new(), ptr, offset)` where offset = `index * 8`
5. Narrow from i64 if the field type is smaller

Also handle standalone `ExprKind::FieldAccess` as a first-class accessor (creates a closure that takes an object and returns the field). This can be deferred — error gracefully for now.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.5 — Struct pattern destructure codegen

In `compile_match()` / pattern compilation, handle `PatternKind::StructDestructure`:

1. For each field binding, compute the field's offset in the struct layout
2. Load the value from `ptr + offset`
3. Bind it to the pattern variable

No tag check needed (structs aren't sum types), so the destructure is unconditional.

**Files:** `crates/weir-codegen/src/lib.rs`

#### 11.6 — AOT runtime support

The AOT C runtime needs no new functions — struct allocation uses the existing `weir_gc_alloc`. However, verify that the AOT path correctly handles `Defstruct` items (currently skipped).

**Files:** `crates/weir-codegen/src/aot.rs` (verify, likely no changes needed)

#### 11.7 — Tests

- Unit tests: struct construction, field access, nested structs, structs with heap-pointer fields (strings, vectors), struct in match patterns
- Integration: run `demo-raytracer.weir` rewritten with `Vec3` struct via JIT and AOT
- GC stress: create many short-lived structs to verify shape descriptors and shadow stack interaction

**Test count target:** 15-20 new tests

---

## Phase 12: Tail-Call Optimization ✅

**Completed 2026-02-19.** 616 tests passing (was 613). Self-recursive tail calls are compiled as loops (codegen) and trampolined (interpreter). Deep recursion (1M+ depth) works without increased stack size.

Without TCO, all iteration in Weir is recursion, and deep recursion overflows the stack. The ray tracer requires `RUST_MIN_STACK=67108864` for a 160x90 image. TCO is essential for the language to be practical.

### Current state

- [x] Tail-position analysis in `weir-ast/src/tco.rs`
- [x] Codegen: self-tail-calls compiled as jumps to loop header block
- [x] Interpreter: trampoline via `tail_call_args` sentinel in `InterpError`
- [x] 3 CLI tests (interp, JIT, AOT) with 1M-depth recursion

### Design decision: implicit self-tail-calls first

Start with the simplest, highest-value case: **self-recursive tail calls are compiled as loops**. This covers the vast majority of Weir iteration patterns (see `render-col`, `render-rows`, `find-hit`, `find-t` in the ray tracer). Mutual tail calls and general tail-call elimination can follow later.

A call `(f args...)` is in tail position if it is the last expression evaluated before the enclosing `defn` returns. In Weir, tail positions are:

- The body of a `defn`
- The then/else branches of an `if` in tail position
- The last expression in a `let` body in tail position
- The matching branch of a `cond`/`match` in tail position

### Steps

#### 12.1 — Tail-position analysis pass

Add a function `mark_tail_positions(body: ExprId, arena: &ExprArena) -> HashSet<ExprId>` that walks the AST and collects expression IDs that are in tail position. This runs once per function before codegen.

Rules:
- `body` itself is in tail position
- If an `If` is in tail position, both its then and else branches are
- If a `Let` is in tail position, its body is
- If a `Cond` is in tail position, each branch result is
- If a `Match` is in tail position, each arm body is
- A `Call` in tail position where the callee is the enclosing function → **self-tail-call**

**Files:** new function in `crates/weir-codegen/src/lib.rs` (or a small `tco.rs` module)

#### 12.2 — Codegen: self-tail-calls as jumps

For a function that has self-tail-calls:

1. Create a "loop header" block at the top of the function body
2. Function parameters become block parameters of the loop header
3. At each self-tail-call site: compile arguments, then `builder.ins().jump(loop_header, args)` instead of `call`
4. Non-tail self-calls still use regular `call`

This transforms:
```
(defn count-down ((n : i64)) : i64
  (if (= n 0) 0 (count-down (- n 1))))
```
Into effectively:
```
loop:
  if n == 0: return 0
  n = n - 1
  goto loop
```

**Files:** `crates/weir-codegen/src/lib.rs`

#### 12.3 — Interpreter: trampoline for self-tail-calls

Convert the interpreter's recursive `eval_body()` call chain into an iterative trampoline for self-tail-calls:

1. Use the same `mark_tail_positions` analysis
2. When evaluating a self-tail-call in tail position, instead of recursing, return a `TailCall { args }` sentinel
3. The outermost `call_value()` wraps evaluation in a loop: if result is `TailCall`, rebind parameters and re-evaluate; otherwise return

This eliminates stack growth for tail-recursive functions in the interpreter.

**Files:** `crates/weir-interp/src/lib.rs`

#### 12.4 — Verify with deep recursion

Test that previously stack-overflowing programs now work:
- `(count-down 1000000)` — trivial tail recursion
- `demo-raytracer.weir` at higher resolutions without `RUST_MIN_STACK`
- Mutual recursion (not optimized yet) — verify it still works, just not optimized

**Test count target:** 8-10 new tests

---

## Phase 13: General tail calls (future)

Not in immediate scope, but documenting the path forward:

- **Cranelift `return_call`**: For mutual tail calls, Cranelift supports `return_call` and `return_call_indirect`. These reuse the caller's stack frame. Requires matching calling conventions.
- **Interpreter mutual trampolining**: Extend the `TailCall` sentinel to include the target function name/value.
- **`loop`/`recur` construct**: An explicit loop form could complement implicit TCO for cases where the programmer wants guaranteed iteration semantics.

---

## Verification plan

After each phase, all existing tests must continue to pass:

```
nix develop --command cargo test --workspace
```

Current baseline: 616 tests passing.

### Phase 11 acceptance criteria
- [x] `(defstruct Vec3 (x : f64) (y : f64) (z : f64))` compiles and runs via JIT and AOT
- [x] Field access `(.x v)` works in codegen
- [x] Struct destructuring in `match` works in codegen
- [x] GC correctly traces struct fields (no use-after-free with heap-pointer fields)
- [ ] Ray tracer rewritten with Vec3 structs works on all 3 paths (deferred to after TCO — deep recursion with structs needs larger stacks)

### Phase 12 acceptance criteria
- [x] Self-tail-recursive functions don't grow the stack (codegen)
- [x] Self-tail-recursive functions don't grow the stack (interpreter)
- [x] `render-col` / `render-rows` in ray tracer work without `RUST_MIN_STACK`
- [x] Higher resolution renders (320x180, 640x360) succeed
- [x] Non-tail recursion still works correctly (doesn't get incorrectly optimized)

---

## File change summary

| File | Phase 11 | Phase 12 |
|------|----------|----------|
| `crates/weir-typeck/src/result.rs` | Add `StructInfo`, populate `struct_defs` | — |
| `crates/weir-typeck/src/checker.rs` | Export struct info in `finish()` | — |
| `crates/weir-codegen/src/lib.rs` | Struct type recognition, construction, field access, destructure | Tail-position analysis, loop-header codegen |
| `crates/weir-codegen/src/aot.rs` | Verify struct path works | — |
| `crates/weir-interp/src/lib.rs` | — | Trampoline for self-tail-calls |
| `crates/weir-lsp/src/completion.rs` | — | — |
| `design/implementation-plan.md` | This document | This document |
