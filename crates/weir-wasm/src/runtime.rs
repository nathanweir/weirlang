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
pub const GLOBAL_RNG_STATE: u32 = 3;

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
/// Converts an i64 integer to a null-terminated decimal string.
/// Handles negative numbers and zero. Allocates from the GC heap.
pub fn emit_i64_to_str(func: &mut Function) {
    // local 0 = n (i64)       — parameter
    // local 1 = buf (i32)     — pointer to 24-byte buffer
    // local 2 = pos (i32)     — write position (end of buffer, writes backwards)
    // local 3 = is_neg (i32)  — 1 if negative
    // local 4 = digit (i64)   — current digit
    // local 5 = dst (i32)     — final string pointer
    // local 6 = len (i32)     — string length
    // local 7 = i (i32)       — copy counter

    // Allocate 24-byte buffer from heap
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1)); // buf
    func.instruction(&Instruction::I32Const(24));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // pos = buf + 22 (leave room for null terminator at buf+23)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(22));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2)); // pos

    // Store null terminator at buf+23
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(23)));

    // Check if n == 0
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    // n == 0: store "0\0" and return
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // Check if negative
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // is_neg = 0
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64LtS);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::LocalSet(3)); // is_neg = 1
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::LocalSet(0)); // n = -n
    func.instruction(&Instruction::End);

    // Extract digits in reverse (loop while n > 0)
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if n == 0, break
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::BrIf(1));

    // digit = n % 10
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::LocalSet(4));

    // n = n / 10
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::LocalSet(0));

    // buf[pos] = '0' + digit
    func.instruction(&Instruction::LocalGet(2)); // pos
    func.instruction(&Instruction::LocalGet(4)); // digit
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(memarg(0)));

    // pos--
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));

    func.instruction(&Instruction::Br(0)); // continue loop
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // If negative, store '-'
    func.instruction(&Instruction::LocalGet(3)); // is_neg
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2)); // pos
    func.instruction(&Instruction::I32Const(45)); // '-'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    // String starts at pos+1
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_set_nth(vec: i32, idx: i64, val: i64) -> i32`.
///
/// Immutable vector update: allocates a new vector, copies all elements,
/// replaces the element at idx with val. Returns the new vector pointer.
pub fn emit_vec_set_nth(func: &mut Function) {
    // local 0 = vec (i32), local 1 = idx (i64), local 2 = val (i64)
    // local 3 = len (i64), local 4 = new_vec (i32), local 5 = i (i64)

    // len = vec[0]
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(3)); // len

    // Allocate new vector: (1 + len) * 8 bytes — inline bump alloc
    func.instruction(&Instruction::LocalGet(3)); // len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    // total_size is on stack
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(4)); // new_vec = heap_ptr
    // heap_ptr += total_size
    func.instruction(&Instruction::I32Add); // heap_ptr + total_size
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Store length
    func.instruction(&Instruction::LocalGet(4)); // new_vec
    func.instruction(&Instruction::LocalGet(3)); // len
    func.instruction(&Instruction::I64Store(memarg(0)));

    // Copy elements, replacing at idx
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::LocalSet(5)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if i >= len, break
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I64GeU);
    func.instruction(&Instruction::BrIf(1));

    // new_vec[8 + i*8] = (i == idx) ? val : vec[8 + i*8]
    func.instruction(&Instruction::LocalGet(4)); // new_vec
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    // ^ destination address = new_vec + 8 + i*8 (we add the base 8 via memarg)

    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::LocalGet(1)); // idx
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    func.instruction(&Instruction::LocalGet(2)); // val
    func.instruction(&Instruction::Else);
    // Load from old vec
    func.instruction(&Instruction::LocalGet(0)); // vec
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I64Load(memarg(8))); // load vec[8+i*8]
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::I64Store(memarg(8))); // store to new_vec[8+i*8]

    // i++
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Return new_vec
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_append(vec: i32, val: i64) -> i32`.
///
/// Creates a new vector with length+1, copies all elements, appends val.
pub fn emit_vec_append(func: &mut Function) {
    // local 0 = vec (i32), local 1 = val (i64)
    // local 2 = old_len (i64), local 3 = new_vec (i32), local 4 = i (i64)

    // old_len = vec[0]
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(2)); // old_len

    // Allocate new vector: (1 + old_len + 1) * 8 = (2 + old_len) * 8
    func.instruction(&Instruction::LocalGet(2)); // old_len
    func.instruction(&Instruction::I64Const(2)); // +1 for length slot, +1 for new element
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    // total_size on stack
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(3)); // new_vec = heap_ptr
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Store new length = old_len + 1
    func.instruction(&Instruction::LocalGet(3)); // new_vec
    func.instruction(&Instruction::LocalGet(2)); // old_len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Store(memarg(0)));

    // Copy old elements
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::LocalSet(4)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(2)); // old_len
    func.instruction(&Instruction::I64GeU);
    func.instruction(&Instruction::BrIf(1));

    // new_vec[8 + i*8] = vec[8 + i*8]
    func.instruction(&Instruction::LocalGet(3)); // new_vec
    func.instruction(&Instruction::LocalGet(4)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::LocalGet(0)); // vec
    func.instruction(&Instruction::LocalGet(4)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I64Load(memarg(8)));

    func.instruction(&Instruction::I64Store(memarg(8)));

    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::LocalSet(4));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Store val at new_vec[8 + old_len*8]
    func.instruction(&Instruction::LocalGet(3)); // new_vec
    func.instruction(&Instruction::LocalGet(2)); // old_len
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(1)); // val
    func.instruction(&Instruction::I64Store(memarg(8)));

    // Return new_vec
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::End);
}

/// Emit `weir_random_seed(seed: i64)`.
///
/// Sets the xorshift64 PRNG state.
pub fn emit_random_seed(func: &mut Function) {
    // local 0 = seed (i64)
    // Store seed in linear memory at SHADOW_STACK_START (RNG state location)
    func.instruction(&Instruction::I32Const(SHADOW_STACK_START as i32));
    // Ensure non-zero (xorshift requires non-zero state)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    func.instruction(&Instruction::I64Const(0x123456789ABCDEF0u64 as i64));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::End);
    // Stack: [i32 (addr), i64 (value)] → store
    func.instruction(&Instruction::I64Store(memarg(0)));
    func.instruction(&Instruction::End);
}

/// Emit `weir_random_int(max: i64) -> i64`.
///
/// Returns a random integer in [0, max) using xorshift64.
pub fn emit_random_int(func: &mut Function) {
    // local 0 = max (i64)
    // local 1 = state (i64)

    // Load RNG state from linear memory at SHADOW_STACK_START
    func.instruction(&Instruction::I32Const(SHADOW_STACK_START as i32));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalTee(1));

    // If state == 0, initialize
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(0x123456789ABCDEF0u64 as i64));
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::End);

    // xorshift64: state ^= state << 13
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64Const(13));
    func.instruction(&Instruction::I64Shl);
    func.instruction(&Instruction::I64Xor);
    func.instruction(&Instruction::LocalSet(1));

    // state ^= state >> 7
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64Const(7));
    func.instruction(&Instruction::I64ShrU);
    func.instruction(&Instruction::I64Xor);
    func.instruction(&Instruction::LocalSet(1));

    // state ^= state << 17
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64Const(17));
    func.instruction(&Instruction::I64Shl);
    func.instruction(&Instruction::I64Xor);
    func.instruction(&Instruction::LocalSet(1));

    // Store state back
    func.instruction(&Instruction::I32Const(SHADOW_STACK_START as i32));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64Store(memarg(0)));

    // Return (state % max) — ensure positive
    func.instruction(&Instruction::LocalGet(1));
    // Make positive: mask off sign bit
    func.instruction(&Instruction::I64Const(0x7FFFFFFFFFFFFFFFu64 as i64));
    func.instruction(&Instruction::I64And);
    func.instruction(&Instruction::LocalGet(0)); // max
    func.instruction(&Instruction::I64RemU);

    func.instruction(&Instruction::End);
}

/// Emit `weir_f64_to_str(n: f64) -> i32`.
///
/// Converts an f64 to a null-terminated decimal string.
/// Uses a simple digit-extraction approach. Handles negative, zero, integer-like floats.
/// Allocates from the GC heap.
pub fn emit_f64_to_str(func: &mut Function) {
    // local 0 = n (f64)        — parameter
    // local 1 = buf (i32)      — pointer to 32-byte buffer
    // local 2 = pos (i32)      — write position (end of buffer)
    // local 3 = is_neg (i32)   — 1 if negative
    // local 4 = int_part (i64) — integer part
    // local 5 = frac_part (i64) — fractional part (6 digits)
    // local 6 = digit (i64)    — current digit
    // local 7 = i (i32)        — counter
    // local 8 = temp_f64 (f64) — temp

    // Allocate 32-byte buffer from heap
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1)); // buf
    func.instruction(&Instruction::I32Const(32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // pos = buf + 30 (leave room for null terminator at buf+31)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(30));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));

    // Store null terminator at buf+31
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(31)));

    // Check if negative
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // is_neg = 0
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::F64Const(0.0));
    func.instruction(&Instruction::F64Lt);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::LocalSet(3)); // is_neg = 1
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::F64Neg);
    func.instruction(&Instruction::LocalSet(0)); // n = -n
    func.instruction(&Instruction::End);

    // int_part = trunc(n)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::F64Floor);
    func.instruction(&Instruction::I64TruncF64U);
    func.instruction(&Instruction::LocalSet(4)); // int_part

    // frac = n - int_part, scaled to 6 digits: round((n - int_part) * 1000000)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::F64ConvertI64U);
    func.instruction(&Instruction::F64Sub);
    func.instruction(&Instruction::F64Const(1000000.0));
    func.instruction(&Instruction::F64Mul);
    func.instruction(&Instruction::F64Nearest); // round
    func.instruction(&Instruction::I64TruncF64U);
    func.instruction(&Instruction::LocalSet(5)); // frac_part

    // Check if frac_part is 0 → integer-like float, skip decimal
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));

    // --- Integer-like path: just emit int_part digits ---
    // Handle zero
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    // If negative and zero, skip the minus sign (it's -0.0 → "0")
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // Extract digits from int_part
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(48));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::LocalSet(4));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // If negative, store '-'
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(45)); // '-'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::Return);

    func.instruction(&Instruction::Else);

    // --- Fractional path: emit int_part.frac_part ---
    // First emit fractional digits (reversed) - strip trailing zeros
    // Strip trailing zeros from frac_part
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::I32Eqz); // not zero → break
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Emit frac digits (reversed)
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(48));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Emit '.'
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(46)); // '.'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));

    // Emit int_part digits (same as above but reload int_part)
    // Re-compute int_part since we used it
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::F64Floor);
    func.instruction(&Instruction::I64TruncF64U);
    func.instruction(&Instruction::LocalSet(4));

    // Handle int_part == 0
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(48)); // '0'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64RemU);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(48));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I64Const(10));
    func.instruction(&Instruction::I64DivU);
    func.instruction(&Instruction::LocalSet(4));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // If negative, store '-'
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(45)); // '-'
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::Return);

    func.instruction(&Instruction::End); // end if/else (frac == 0)

    // Unreachable fallback
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::End);
}

/// Emit `weir_bool_to_str(b: i32) -> i32`.
///
/// Returns a pointer to "true" or "false" string.
/// Allocates from the GC heap.
pub fn emit_bool_to_str(func: &mut Function) {
    // local 0 = b (i32)       — parameter
    // local 1 = ptr (i32)     — result pointer

    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));

    // true: allocate 5 bytes "true\0"
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1));
    func.instruction(&Instruction::I32Const(8)); // aligned
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));
    // Store 't','r','u','e','\0'
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0x65757274)); // "true" as little-endian i32
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(4)));
    func.instruction(&Instruction::LocalGet(1));

    func.instruction(&Instruction::Else);

    // false: allocate 6 bytes "false\0"
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1));
    func.instruction(&Instruction::I32Const(8)); // aligned
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));
    // Store 'f','a','l','s','e','\0'
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0x736C6166)); // "fals" as little-endian i32
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0x65)); // 'e'
    func.instruction(&Instruction::I32Store8(memarg(4)));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(5)));
    func.instruction(&Instruction::LocalGet(1));

    func.instruction(&Instruction::End);

    func.instruction(&Instruction::End);
}

/// Emit `weir_str_cmp(a: i32, b: i32) -> i64`.
///
/// Returns -1, 0, or 1 (as i64) for lexicographic comparison.
pub fn emit_str_cmp(func: &mut Function) {
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

    // if ca < cb: return -1
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(-1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // if ca > cb: return 1
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // if ca == 0 (and ca == cb): both ended, return 0
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(0));
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

    // Unreachable fallback
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::End);
}

/// Emit `weir_substring(s: i32, start: i64, end: i64) -> i32`.
///
/// Returns a new string that is a slice of s from [start, end).
pub fn emit_substring(func: &mut Function) {
    // local 0 = s (i32), local 1 = start (i64), local 2 = end (i64)
    // local 3 = len (i32), local 4 = dst (i32), local 5 = i (i32)

    // len = end - start
    func.instruction(&Instruction::LocalGet(2)); // end
    func.instruction(&Instruction::LocalGet(1)); // start
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(3)); // len

    // Allocate len+1 bytes, aligned
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(4)); // dst

    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Copy bytes from s[start..end]
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(3)); // len
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // dst[i] = s[start + i]
    func.instruction(&Instruction::LocalGet(4)); // dst
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::LocalGet(0)); // s
    func.instruction(&Instruction::LocalGet(1)); // start
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));

    func.instruction(&Instruction::I32Store8(memarg(0)));

    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Null terminator
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(0)));

    // Return dst
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::End);
}

/// Emit `weir_string_ref(s: i32, idx: i64) -> i64`.
///
/// Returns the byte at index idx in the string.
pub fn emit_string_ref(func: &mut Function) {
    // local 0 = s (i32), local 1 = idx (i64)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I64ExtendI32U);
    func.instruction(&Instruction::End);
}

/// Emit `weir_string_contains(haystack: i32, needle: i32) -> i64`.
///
/// Returns 1 if needle is a substring of haystack, 0 otherwise.
/// Simple O(n*m) search.
pub fn emit_string_contains(func: &mut Function) {
    // local 0 = haystack (i32), local 1 = needle (i32)
    // local 2 = h_len (i32), local 3 = n_len (i32)
    // local 4 = i (i32), local 5 = j (i32)
    // local 6 = matched (i32)

    // Compute h_len
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

    // Compute n_len
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

    // If n_len == 0, return 1 (empty string is always contained)
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // For i = 0 to h_len - n_len
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(4)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if i > h_len - n_len, break (not found)
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(2)); // h_len
    func.instruction(&Instruction::LocalGet(3)); // n_len
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add); // h_len - n_len + 1
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // Check if haystack[i..i+n_len] == needle
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::LocalSet(6)); // matched = 1
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5)); // j = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(3)); // n_len
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // if haystack[i+j] != needle[j], matched = 0, break
    func.instruction(&Instruction::LocalGet(0)); // haystack
    func.instruction(&Instruction::LocalGet(4)); // i
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5)); // j
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));

    func.instruction(&Instruction::LocalGet(1)); // needle
    func.instruction(&Instruction::LocalGet(5)); // j
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load8U(memarg(0)));

    func.instruction(&Instruction::I32Ne);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(6)); // matched = 0
    func.instruction(&Instruction::Br(2)); // break inner loop
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // inner loop
    func.instruction(&Instruction::End); // inner block

    // If matched, return 1
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End);

    // i++
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(4));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // outer loop
    func.instruction(&Instruction::End); // outer block

    // Not found
    func.instruction(&Instruction::I64Const(0));
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
