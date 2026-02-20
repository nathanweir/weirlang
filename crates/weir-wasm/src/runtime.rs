use wasm_encoder::{
    BlockType, Function, Instruction, MemArg, ValType,
};

/// Memory layout constants
pub const NULL_PAGE_END: u32 = 0x1000;
pub const STATIC_DATA_START: u32 = 0x1000;
pub const SHADOW_STACK_START: u32 = 0x10000;
pub const SHADOW_STACK_SIZE: u32 = 0x10000; // 64KB
pub const GC_HEAP_START: u32 = 0x20000;
pub const INITIAL_PAGES: u32 = 256; // 16MB

/// Global indices
pub const GLOBAL_HEAP_PTR: u32 = 0;
pub const GLOBAL_SHADOW_SP: u32 = 1;
pub const GLOBAL_WASM_STATE: u32 = 2;

/// Names of runtime functions that are imported from JS.
pub const JS_IMPORTS: &[(&str, &[ValType], &[ValType])] = &[
    // Printing
    ("weir_print_i64", &[ValType::I64], &[]),
    ("weir_print_f64", &[ValType::F64], &[]),
    ("weir_print_str", &[ValType::I32], &[]),
    ("weir_print_bool", &[ValType::I32], &[]),
    ("weir_print_unit", &[], &[]),
    ("weir_print_newline", &[], &[]),
    // Time
    ("weir_time_ms", &[], &[ValType::I64]),
    // Sleep (no-op in browser)
    ("weir_sleep_ms", &[ValType::I64], &[]),
    // File I/O (stub)
    ("weir_read_file", &[ValType::I32], &[ValType::I32]),
    ("weir_write_file", &[ValType::I32, ValType::I32], &[]),
    // Math (from JS Math object)
    ("sin", &[ValType::F64], &[ValType::F64]),
    ("cos", &[ValType::F64], &[ValType::F64]),
    ("tan", &[ValType::F64], &[ValType::F64]),
    ("asin", &[ValType::F64], &[ValType::F64]),
    ("acos", &[ValType::F64], &[ValType::F64]),
    ("atan", &[ValType::F64], &[ValType::F64]),
    ("atan2", &[ValType::F64, ValType::F64], &[ValType::F64]),
    ("exp", &[ValType::F64], &[ValType::F64]),
    ("log", &[ValType::F64], &[ValType::F64]),
    ("pow", &[ValType::F64, ValType::F64], &[ValType::F64]),
    ("round", &[ValType::F64], &[ValType::F64]),
    ("sqrt", &[ValType::F64], &[ValType::F64]),
];

/// Emit a simple bump-allocator `weir_gc_alloc(size: i32, shape: i64) -> i32`.
///
/// Bumps the heap pointer and returns the old value.
/// A real GC would be more sophisticated, but bump allocation
/// is sufficient for initial WASM support.
pub fn emit_gc_alloc(func: &mut Function) {
    // local 0 = size (i32), local 1 = shape (i64)
    // local 2 = ptr (i32) — result

    func.instruction(&Instruction::LocalGet(0)); // size
    func.instruction(&Instruction::I32Const(7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32)); // ~7
    func.instruction(&Instruction::I32And); // align to 8

    func.instruction(&Instruction::LocalSet(0)); // aligned size

    // ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(2));

    // heap_ptr += aligned_size
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // return ptr
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_vec_alloc(len: i64, shape: i64) -> i32`.
///
/// Allocates a vector: [len (8 bytes)] + [len * 8 bytes for elements].
pub fn emit_gc_vec_alloc(func: &mut Function) {
    // local 0 = len (i64), local 1 = shape (i64)
    // local 2 = total_size (i32), local 3 = ptr (i32)

    // total_size = (1 + len) * 8 (length slot + elements)
    func.instruction(&Instruction::LocalGet(0)); // len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(2)); // total_size

    // ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(3));

    // heap_ptr += total_size
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Store length at ptr[0]
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(0)); // len as i64
    func.instruction(&Instruction::I64Store(memarg(0)));

    // return ptr
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_str_alloc(len: i64) -> i32`.
///
/// Allocates len+1 bytes for a null-terminated string.
pub fn emit_gc_str_alloc(func: &mut Function) {
    // local 0 = len (i64)      — parameter
    // local 1 = ptr (i32)      — extra local
    // local 2 = aligned_size (i32) — extra local

    func.instruction(&Instruction::LocalGet(0)); // len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I32WrapI64);

    // Align to 8
    func.instruction(&Instruction::I32Const(7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);

    func.instruction(&Instruction::LocalSet(2)); // aligned_size (i32 into i32 local)

    // ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1));

    // heap_ptr += aligned_size
    func.instruction(&Instruction::LocalGet(2)); // aligned_size (i32)
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // return ptr
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_str_dup(src: i32) -> i32`.
///
/// Copies a null-terminated string to a new allocation.
pub fn emit_gc_str_dup(func: &mut Function) {
    // local 0 = src (i32)
    // local 1 = len (i32), local 2 = dst (i32), local 3 = i (i32)

    // Compute length by scanning for null
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(1)); // len = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if src[len] == 0 then break
    func.instruction(&Instruction::LocalGet(0)); // src
    func.instruction(&Instruction::LocalGet(1)); // len
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1)); // break to block

    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(0)); // continue loop
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // Allocate len+1 bytes, aligned
    // dst = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(2));

    // heap_ptr += aligned size
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Copy bytes: for i in 0..=len
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if i > len then break
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(1));

    // dst[i] = src[i]
    func.instruction(&Instruction::LocalGet(2)); // dst
    func.instruction(&Instruction::LocalGet(3)); // i
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::LocalGet(0)); // src
    func.instruction(&Instruction::LocalGet(3)); // i
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));

    func.instruction(&Instruction::I32Store8(memarg(0)));

    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(3));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // return dst
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_get(vec: i32, idx: i64) -> i64`.
pub fn emit_vec_get(func: &mut Function) {
    // local 0 = vec (i32), local 1 = idx (i64)
    // Load vec[8 + idx*8]
    func.instruction(&Instruction::LocalGet(0)); // vec ptr
    func.instruction(&Instruction::LocalGet(1)); // idx
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I64Load(memarg(8))); // offset 8 for length slot
    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_len(vec: i32) -> i64`.
pub fn emit_vec_len(func: &mut Function) {
    // local 0 = vec (i32)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Load(memarg(0))); // length at offset 0
    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_set(vec: i32, idx: i64, val: i64) -> Unit`.
pub fn emit_vec_set(func: &mut Function) {
    // local 0 = vec (i32), local 1 = idx (i64), local 2 = val (i64)
    func.instruction(&Instruction::LocalGet(0)); // vec ptr
    func.instruction(&Instruction::LocalGet(1)); // idx
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(2)); // val
    func.instruction(&Instruction::I64Store(memarg(8))); // offset 8
    func.instruction(&Instruction::End);
}

/// Emit `weir_str_eq(a: i32, b: i32) -> i64` (returns 0 or 1 as i64).
pub fn emit_str_eq(func: &mut Function) {
    // local 0 = a (i32), local 1 = b (i32)
    // local 2 = i (i32), local 3 = ca (i32), local 4 = cb (i32)

    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(2)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // ca = a[i]
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::LocalSet(3));

    // cb = b[i]
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::LocalSet(4));

    // if ca != cb: return 0
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Ne);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // if ca == 0: return 1 (both ended)
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // i++
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Unreachable, but needed for wasm validation
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::End);
}

/// Emit `weir_str_concat(a: i32, b: i32) -> i32`.
pub fn emit_str_concat(func: &mut Function) {
    // local 0 = a (i32), local 1 = b (i32)
    // local 2 = len_a (i32), local 3 = len_b (i32)
    // local 4 = dst (i32), local 5 = i (i32)

    // Compute len_a
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // Compute len_b
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3));
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(3));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // Allocate len_a + len_b + 1, aligned
    // dst = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(4)); // dst

    // heap_ptr += alloc_size
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Copy a to dst
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5)); // i = 0
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(2)); // len_a
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // Copy b to dst + len_a
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(3)); // len_b
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(2)); // len_a
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // Null terminator
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(0)));

    // return dst
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::End);
}

/// Emit `weir_string_length(s: i32) -> i64`.
pub fn emit_string_length(func: &mut Function) {
    // local 0 = s (i32), local 1 = len (i32)
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(1));

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64ExtendI32U);
    func.instruction(&Instruction::End);
}

/// Emit `weir_i64_to_str(n: i64) -> i32`.
///
/// Simple implementation: just allocates a string buffer and converts.
/// This is a placeholder — a real implementation would handle all digits.
pub fn emit_i64_to_str(func: &mut Function) {
    // For now, just return a pointer to a static "?" string.
    // A real implementation would do digit extraction.
    // This is sufficient to get the pipeline working end-to-end.

    // Allocate 24 bytes for the number string
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(24));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // TODO: implement actual digit extraction
    // For now store "0\0"
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(24));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Store8(memarg(0)));

    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(23));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(0)); // null terminator
    func.instruction(&Instruction::I32Store8(memarg(0)));

    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(24));
    func.instruction(&Instruction::I32Sub);

    func.instruction(&Instruction::End);
}

/// Emit no-op functions for stubs.
pub fn emit_noop(func: &mut Function) {
    func.instruction(&Instruction::End);
}

/// Emit `weir_shadow_push(ptr: i32)` — no-op for now.
pub fn emit_shadow_push(func: &mut Function) {
    func.instruction(&Instruction::End);
}

/// Emit `weir_shadow_pop()` — no-op for now.
pub fn emit_shadow_pop(func: &mut Function) {
    func.instruction(&Instruction::End);
}

/// Helper to create a MemArg with a given offset.
pub fn memarg(offset: u64) -> MemArg {
    MemArg {
        offset,
        align: 0,
        memory_index: 0,
    }
}
