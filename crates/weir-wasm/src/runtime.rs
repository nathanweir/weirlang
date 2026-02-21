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
pub const GLOBAL_FREE_LIST: u32 = 4;

/// Object header constants
pub const HEADER_SIZE: u32 = 8;
pub const OBJ_KIND_STRUCT: u32 = 0;
pub const OBJ_KIND_VECTOR: u32 = 1;
pub const OBJ_KIND_STRING: u32 = 2;
pub const OBJ_KIND_BUFFER: u32 = 3;
pub const MARK_WHITE: u32 = 0;
pub const MARK_BLACK: u32 = 2;
pub const MARK_FREE: u32 = 0xFF;

/// GC worklist region (at top of shadow stack region)
pub const GC_WORKLIST_START: u32 = 0x1C000;
pub const GC_WORKLIST_END: u32 = 0x20000; // 16KB = 4096 entries

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

/// Helper: emit instructions to write an 8-byte object header at the address on TOS.
///
/// Header layout (8 bytes, little-endian):
///   byte 0:    mark byte (0 = white)
///   byte 1:    object kind
///   bytes 2-3: shape_id (u16)
///   bytes 4-7: data size in bytes (u32)
///
/// Expects stack: [header_addr]
/// After: stack is empty (header_addr consumed)
///
/// `kind` and `shape_id_local`/`size_local` specify how to get those values.
fn emit_header_write(
    func: &mut Function,
    header_addr_local: u32,
    kind: u32,
    shape_id_local: Option<u32>, // local holding shape_id as i32; None => use 0
    data_size_local: u32,        // local holding data size as i32
) {
    // Write low 32 bits: mark(0) | (kind << 8) | (shape_id << 16)
    func.instruction(&Instruction::LocalGet(header_addr_local));
    let kind_byte = kind & 0xFF;
    if let Some(sid) = shape_id_local {
        func.instruction(&Instruction::LocalGet(sid));
        func.instruction(&Instruction::I32Const(16));
        func.instruction(&Instruction::I32Shl);
        func.instruction(&Instruction::I32Const((kind_byte << 8) as i32));
        func.instruction(&Instruction::I32Or);
    } else {
        func.instruction(&Instruction::I32Const((kind_byte << 8) as i32));
    }
    func.instruction(&Instruction::I32Store(memarg(0)));

    // Write high 32 bits: data size
    func.instruction(&Instruction::LocalGet(header_addr_local));
    func.instruction(&Instruction::LocalGet(data_size_local));
    func.instruction(&Instruction::I32Store(memarg(4)));
}

/// Emit `weir_gc_alloc(size: i32, shape: i64) -> i32`.
///
/// Allocates a struct with an 8-byte GC header. First checks the free list for
/// a chunk with sufficient size (first-fit), otherwise bump-allocates.
/// Returns pointer to data area (header_addr + 8). shape low 16 bits = shape_id.
pub fn emit_gc_alloc(func: &mut Function) {
    // local 0 = size (i32), local 1 = shape (i64)
    // local 2 = ptr (i32) — header address / free list cursor
    // local 3 = shape_id (i32) / prev_ptr for free list

    // Extract shape_id from low 16 bits of shape
    func.instruction(&Instruction::LocalGet(1)); // shape
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(0xFFFF));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(3)); // shape_id

    // Align data size to 8
    func.instruction(&Instruction::LocalGet(0)); // size
    func.instruction(&Instruction::I32Const(7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(0)); // aligned data size

    // ── Try free list first (first-fit) ──

    // Walk free list: cursor = free_list_head
    func.instruction(&Instruction::GlobalGet(GLOBAL_FREE_LIST));
    func.instruction(&Instruction::LocalSet(2)); // cursor (header addr)

    func.instruction(&Instruction::Block(BlockType::Empty)); // block for "not found" fallthrough
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // if cursor == 0, break (not found)
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::BrIf(1)); // break to "not found" block

    // Check if this free chunk has enough space: data_size >= aligned_size
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Load(memarg(4))); // data_size at header+4
    func.instruction(&Instruction::LocalGet(0)); // aligned_size
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty));

    // Found a suitable chunk! Unlink from free list.
    // The next pointer is stored at cursor + HEADER_SIZE (data area, first 4 bytes)
    // We need to update whoever pointed to cursor to point to cursor->next instead.
    // For simplicity, just set the free list head to cursor->next (unlinks from head).
    // This works because we always walk from head. Not optimal but correct.
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load(memarg(0))); // next ptr
    func.instruction(&Instruction::GlobalSet(GLOBAL_FREE_LIST));

    // Write new header (overwrite the free header)
    emit_header_write(func, 2, OBJ_KIND_STRUCT, Some(3), 0);

    // Return cursor + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::Return);
    func.instruction(&Instruction::End); // end if (size fits)

    // Move to next in free list: cursor = mem[cursor + HEADER_SIZE]
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::Br(0)); // continue loop
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end "not found" block

    // ── Free list exhausted: bump allocate ──

    // ptr = heap_ptr (header address)
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalSet(2));

    // heap_ptr += HEADER_SIZE + aligned_size
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Write header
    emit_header_write(func, 2, OBJ_KIND_STRUCT, Some(3), 0);

    // Return ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_vec_alloc(len: i64, shape: i64) -> i32`.
///
/// Allocates a vector with GC header: [header(8)] + [len(8)] + [len * 8 elements].
/// shape low 16 bits = 0 for scalar elements, 1 for pointer elements.
/// Returns pointer to data area (after header).
pub fn emit_gc_vec_alloc(func: &mut Function) {
    // local 0 = len (i64), local 1 = shape (i64)
    // local 2 = data_size (i32), local 3 = ptr (i32)
    // local 4 = shape_id (i32)

    // Extract shape_id from low 16 bits of shape
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(0xFFFF));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(4)); // shape_id

    // data_size = (1 + len) * 8 (length slot + elements)
    func.instruction(&Instruction::LocalGet(0)); // len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(2)); // data_size

    // ptr = heap_ptr (header address)
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(3));

    // heap_ptr += HEADER_SIZE + data_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Write header
    emit_header_write(func, 3, OBJ_KIND_VECTOR, Some(4), 2);

    // Store length at data area[0] (ptr + HEADER_SIZE)
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(0)); // len as i64
    func.instruction(&Instruction::I64Store(memarg(0)));

    // return ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_str_alloc(len: i64) -> i32`.
///
/// Allocates len+1 bytes for a null-terminated string with GC header.
/// Returns pointer to data area (after header).
pub fn emit_gc_str_alloc(func: &mut Function) {
    // local 0 = len (i64)           — parameter
    // local 1 = ptr (i32)           — header address
    // local 2 = aligned_size (i32)  — aligned data size

    // aligned_size = align8(len + 1)
    func.instruction(&Instruction::LocalGet(0)); // len
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(2)); // aligned_size

    // ptr = heap_ptr (header address)
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(1));

    // heap_ptr += HEADER_SIZE + aligned_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Write header: kind=STRING, shape_id=0, data_size=aligned_size
    emit_header_write(func, 1, OBJ_KIND_STRING, None, 2);

    // return ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_str_dup(src: i32) -> i32`.
///
/// Copies a null-terminated string to a new allocation with GC header.
/// Returns pointer to data area (after header).
pub fn emit_gc_str_dup(func: &mut Function) {
    // local 0 = src (i32)
    // local 1 = len (i32), local 2 = dst_header (i32), local 3 = i (i32)
    // local 4 = aligned_size (i32)

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
    func.instruction(&Instruction::BrIf(1));

    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // Compute aligned data size
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(4)); // aligned_size

    // dst_header = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(2));

    // heap_ptr += HEADER_SIZE + aligned_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Write header: kind=STRING, shape_id=0
    emit_header_write(func, 2, OBJ_KIND_STRING, None, 4);

    // Copy bytes: for i in 0..=len (to data area starting at dst_header + HEADER_SIZE)
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(3)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    // if i > len then break
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32GtU);
    func.instruction(&Instruction::BrIf(1));

    // (dst_header + HEADER_SIZE + i) = src[i]
    func.instruction(&Instruction::LocalGet(2)); // dst_header
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
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

    // return dst_header + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
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
///
/// Concatenates two strings with GC header. Returns pointer to data area.
pub fn emit_str_concat(func: &mut Function) {
    // local 0 = a (i32), local 1 = b (i32)
    // local 2 = len_a (i32), local 3 = len_b (i32)
    // local 4 = header_ptr (i32), local 5 = i (i32)
    // local 6 = aligned_size (i32), local 7 = dst (i32)

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

    // Compute aligned data size
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(6)); // aligned_size

    // header_ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(4));

    // heap_ptr += HEADER_SIZE + aligned_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Write header
    emit_header_write(func, 4, OBJ_KIND_STRING, None, 6);

    // dst = header_ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(7)); // dst

    // Copy a to dst
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(5)); // i = 0
    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::LocalGet(2)); // len_a
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));
    func.instruction(&Instruction::LocalGet(7)); // dst
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
    func.instruction(&Instruction::LocalGet(7)); // dst
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
    func.instruction(&Instruction::LocalGet(7)); // dst
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::I32Store8(memarg(0)));

    // return dst
    func.instruction(&Instruction::LocalGet(7));
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
/// Handles negative numbers and zero. Allocates from the GC heap with BUFFER header.
pub fn emit_i64_to_str(func: &mut Function) {
    // local 0 = n (i64)       — parameter
    // local 1 = buf (i32)     — pointer to 24-byte data area (after header)
    // local 2 = pos (i32)     — write position (end of buffer, writes backwards)
    // local 3 = is_neg (i32)  — 1 if negative
    // local 4 = digit (i64)   — current digit
    // local 5 = dst (i32)     — final string pointer
    // local 6 = len (i32)     — string length
    // local 7 = i (i32)       — copy counter

    // Allocate HEADER + 24-byte buffer from heap
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    // Write header at heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const((OBJ_KIND_BUFFER << 8) as i32));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(24));
    func.instruction(&Instruction::I32Store(memarg(4)));
    // buf = heap_ptr + HEADER_SIZE
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalTee(1)); // buf (data area)
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
/// Immutable vector update with GC header. Returns pointer to data area.
pub fn emit_vec_set_nth(func: &mut Function) {
    // local 0 = vec (i32), local 1 = idx (i64), local 2 = val (i64)
    // local 3 = len (i64), local 4 = header_ptr (i32), local 5 = i (i64)
    // local 6 = data_size (i32), local 7 = new_vec (i32)

    // len = vec[0]
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(3)); // len

    // data_size = (1 + len) * 8
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(6)); // data_size

    // Copy shape_id from source vector's header
    // Source vector header is at vec - HEADER_SIZE
    // We read bytes 2-3 (shape_id) — but for simplicity, we'll read the
    // old header's low i32 and extract shape bits.
    // Actually for vec_set_nth we don't have a shape_id param. We'll use 0
    // (same as source — the source could be pointer or scalar, but we don't
    // know at this point in a runtime function). The shape_id is set correctly
    // at the initial vec_alloc call and we should copy it from the source header.

    // header_ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(4));

    // heap_ptr += HEADER_SIZE + data_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Copy header from source vector (preserves shape_id)
    // Source header is at vec - HEADER_SIZE
    func.instruction(&Instruction::LocalGet(4)); // dest header
    func.instruction(&Instruction::LocalGet(0)); // vec (data ptr)
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load(memarg(0))); // low 32 bits of source header
    // Clear mark byte (set to white)
    func.instruction(&Instruction::I32Const(!0xFF_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Store(memarg(0)));
    // Write data size
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::I32Store(memarg(4)));

    // new_vec = header_ptr + HEADER_SIZE (data area)
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(7)); // new_vec

    // Store length
    func.instruction(&Instruction::LocalGet(7));
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
    func.instruction(&Instruction::LocalGet(7)); // new_vec
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::LocalGet(1)); // idx
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    func.instruction(&Instruction::LocalGet(2)); // val
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0)); // vec
    func.instruction(&Instruction::LocalGet(5)); // i
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I64Load(memarg(8)));
    func.instruction(&Instruction::End);

    func.instruction(&Instruction::I64Store(memarg(8)));

    // i++
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::LocalSet(5));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    // Return new_vec
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::End);
}

/// Emit `weir_vec_append(vec: i32, val: i64) -> i32`.
///
/// Creates a new vector with length+1 and GC header. Returns pointer to data area.
pub fn emit_vec_append(func: &mut Function) {
    // local 0 = vec (i32), local 1 = val (i64)
    // local 2 = old_len (i64), local 3 = header_ptr (i32), local 4 = i (i64)
    // local 5 = data_size (i32), local 6 = new_vec (i32)

    // old_len = vec[0]
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(2)); // old_len

    // data_size = (2 + old_len) * 8
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I64Const(2));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Const(8));
    func.instruction(&Instruction::I64Mul);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(5)); // data_size

    // header_ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalTee(3));

    // heap_ptr += HEADER_SIZE + data_size
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // Copy header from source vector (preserves shape_id)
    func.instruction(&Instruction::LocalGet(3)); // dest header
    func.instruction(&Instruction::LocalGet(0)); // vec (data ptr)
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load(memarg(0)));
    func.instruction(&Instruction::I32Const(!0xFF_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Store(memarg(4)));

    // new_vec = header_ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(6)); // new_vec

    // Store new length = old_len + 1
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::I64Store(memarg(0)));

    // Copy old elements
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::LocalSet(4)); // i = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));

    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I64GeU);
    func.instruction(&Instruction::BrIf(1));

    func.instruction(&Instruction::LocalGet(6)); // new_vec
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);

    func.instruction(&Instruction::LocalGet(0)); // vec
    func.instruction(&Instruction::LocalGet(4));
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
    func.instruction(&Instruction::LocalGet(6)); // new_vec
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(1)); // val
    func.instruction(&Instruction::I64Store(memarg(8)));

    // Return new_vec
    func.instruction(&Instruction::LocalGet(6));
    func.instruction(&Instruction::End);
}

/// Emit `weir_random_seed(seed: i64)`.
///
/// Sets the xorshift64 PRNG state in global 3.
pub fn emit_random_seed(func: &mut Function) {
    // local 0 = seed (i64)
    // Ensure non-zero (xorshift requires non-zero state)
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::If(BlockType::Result(ValType::I64)));
    func.instruction(&Instruction::I64Const(0x123456789ABCDEF0u64 as i64));
    func.instruction(&Instruction::Else);
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::GlobalSet(GLOBAL_RNG_STATE));
    func.instruction(&Instruction::End);
}

/// Emit `weir_random_int(max: i64) -> i64`.
///
/// Returns a random integer in [0, max) using xorshift64.
/// RNG state is stored in global 3.
pub fn emit_random_int(func: &mut Function) {
    // local 0 = max (i64)
    // local 1 = state (i64)

    // Load RNG state from global 3
    func.instruction(&Instruction::GlobalGet(GLOBAL_RNG_STATE));
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

    // Store state back to global 3
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::GlobalSet(GLOBAL_RNG_STATE));

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

    // Allocate HEADER + 32-byte buffer from heap
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    // Write header at heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const((OBJ_KIND_BUFFER << 8) as i32));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(32));
    func.instruction(&Instruction::I32Store(memarg(4)));
    // buf = heap_ptr + HEADER_SIZE
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalTee(1)); // buf (data area)
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
/// Allocates from the GC heap with BUFFER header.
pub fn emit_bool_to_str(func: &mut Function) {
    // local 0 = b (i32)       — parameter
    // local 1 = ptr (i32)     — data area pointer (after header)

    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::If(BlockType::Result(ValType::I32)));

    // true: allocate HEADER + 8 bytes "true\0"
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    // Write header
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const((OBJ_KIND_BUFFER << 8) as i32));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(8)); // data size
    func.instruction(&Instruction::I32Store(memarg(4)));
    // ptr = heap_ptr + HEADER_SIZE
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalTee(1));
    func.instruction(&Instruction::I32Const(8)); // data area size
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

    // false: allocate HEADER + 8 bytes "false\0"
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    // Write header
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const((OBJ_KIND_BUFFER << 8) as i32));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32Const(8)); // data size
    func.instruction(&Instruction::I32Store(memarg(4)));
    // ptr = heap_ptr + HEADER_SIZE
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalTee(1));
    func.instruction(&Instruction::I32Const(8)); // data area size
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
/// Allocates with GC header (STRING kind).
pub fn emit_substring(func: &mut Function) {
    // local 0 = s (i32), local 1 = start (i64), local 2 = end (i64)
    // local 3 = len (i32), local 4 = dst (i32), local 5 = i (i32)
    // (we reuse local 4 as header_ptr, then compute dst from it)

    // len = end - start
    func.instruction(&Instruction::LocalGet(2)); // end
    func.instruction(&Instruction::LocalGet(1)); // start
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(3)); // len

    // header_ptr = heap_ptr
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::LocalSet(4)); // header_ptr

    // Compute aligned data size
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(1 + 7));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(-8_i32));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::LocalSet(5)); // reuse local 5 temporarily for aligned_size

    // Write header
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const((OBJ_KIND_STRING << 8) as i32));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::LocalGet(5)); // aligned_size as data_size
    func.instruction(&Instruction::I32Store(memarg(4)));

    // heap_ptr += HEADER_SIZE + aligned_size
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_HEAP_PTR));

    // dst = header_ptr + HEADER_SIZE
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(4)); // now local 4 = dst

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

/// Emit `weir_shadow_push(ptr: i32)`.
///
/// Pushes a pointer onto the shadow stack for GC root tracing.
/// Shadow stack grows upward from SHADOW_STACK_START.
pub fn emit_shadow_push(func: &mut Function) {
    // local 0 = ptr (i32)
    // Store ptr at shadow_sp
    func.instruction(&Instruction::GlobalGet(GLOBAL_SHADOW_SP));
    func.instruction(&Instruction::LocalGet(0));
    func.instruction(&Instruction::I32Store(memarg(0)));
    // shadow_sp += 4
    func.instruction(&Instruction::GlobalGet(GLOBAL_SHADOW_SP));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalSet(GLOBAL_SHADOW_SP));
    func.instruction(&Instruction::End);
}

/// Emit `weir_shadow_pop()` — no-op (shadow stack is restored by SP reset).
pub fn emit_shadow_pop(func: &mut Function) {
    func.instruction(&Instruction::End);
}

/// Emit `weir_gc_collect(shape_table_addr: i32)`.
///
/// Mark-and-sweep garbage collector.
/// - Mark phase: walk shadow stack + wasm_state global, push roots onto worklist,
///   then iteratively trace children via object headers and shape bitmasks.
/// - Sweep phase: linear walk of heap, free unmarked objects into free list.
pub fn emit_gc_collect(func: &mut Function) {
    // local 0 = shape_table_addr (i32)  — parameter
    // local 1 = scan_ptr (i32)          — current pointer being processed
    // local 2 = worklist_ptr (i32)      — top of worklist (grows upward from GC_WORKLIST_START)
    // local 3 = header_word (i32)       — low 32 bits of object header
    // local 4 = data_size (i32)         — from header high 32 bits
    // local 5 = child_ptr (i32)         — a child pointer found during tracing
    // local 6 = bitmask (i64)           — shape bitmask for struct tracing
    // local 7 = field_idx (i32)         — field index during struct trace
    // local 8 = prev_ptr (i32)          — previous free list pointer during sweep

    // Initialize worklist pointer
    func.instruction(&Instruction::I32Const(GC_WORKLIST_START as i32));
    func.instruction(&Instruction::LocalSet(2)); // worklist_ptr = GC_WORKLIST_START

    // ── Phase 1: Push roots onto worklist ────────────────────

    // Walk shadow stack entries: from SHADOW_STACK_START to shadow_sp
    func.instruction(&Instruction::I32Const(SHADOW_STACK_START as i32));
    func.instruction(&Instruction::LocalSet(1)); // scan_ptr = SHADOW_STACK_START

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // if scan_ptr >= shadow_sp, break
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::GlobalGet(GLOBAL_SHADOW_SP));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // child_ptr = mem[scan_ptr] (i32 pointer from shadow stack)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(5));

    // If child_ptr is a valid heap pointer (>= GC_HEAP_START and < heap_ptr),
    // and not already marked, push onto worklist
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(GC_HEAP_START as i32));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    // Read mark byte at child_ptr - HEADER_SIZE
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz); // mark == 0 (white)?
    func.instruction(&Instruction::If(BlockType::Empty));
    // Mark as black
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(MARK_BLACK as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    // Push onto worklist (if space)
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(GC_WORKLIST_END as i32));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End); // end if (worklist space)
    func.instruction(&Instruction::End); // end if (white)
    func.instruction(&Instruction::End); // end if (< heap_ptr)
    func.instruction(&Instruction::End); // end if (>= GC_HEAP_START)

    // scan_ptr += 4
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(0)); // continue
    func.instruction(&Instruction::End); // end loop
    func.instruction(&Instruction::End); // end block

    // Push GLOBAL_WASM_STATE if non-zero
    func.instruction(&Instruction::GlobalGet(GLOBAL_WASM_STATE));
    func.instruction(&Instruction::LocalTee(5));
    func.instruction(&Instruction::I32Const(GC_HEAP_START as i32));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(MARK_BLACK as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(GC_WORKLIST_END as i32));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // ── Phase 2: Trace worklist (iterative mark) ─────────────

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // if worklist is empty (worklist_ptr == GC_WORKLIST_START), break
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(GC_WORKLIST_START as i32));
    func.instruction(&Instruction::I32LeU);
    func.instruction(&Instruction::BrIf(1));

    // Pop from worklist: worklist_ptr -= 4; scan_ptr = mem[worklist_ptr]
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::LocalTee(2));
    func.instruction(&Instruction::I32Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(1)); // scan_ptr = popped data pointer

    // Read header: header is at scan_ptr - HEADER_SIZE
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(3)); // header_word (mark|kind|shape_id)

    // Read data_size from header+4
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load(memarg(4)));
    func.instruction(&Instruction::LocalSet(4)); // data_size

    // Extract kind = (header_word >> 8) & 0xFF
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32ShrU);
    func.instruction(&Instruction::I32Const(0xFF));
    func.instruction(&Instruction::I32And);
    // kind is on stack

    // Dispatch by kind
    // kind == 0 (STRUCT): trace fields using shape bitmask
    func.instruction(&Instruction::I32Eqz); // kind == STRUCT?
    func.instruction(&Instruction::If(BlockType::Empty));

    // STRUCT tracing: extract shape_id = (header_word >> 16) & 0xFFFF
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(16));
    func.instruction(&Instruction::I32ShrU);
    func.instruction(&Instruction::I32Const(0xFFFF));
    func.instruction(&Instruction::I32And);
    // shape_id on stack → load bitmask from shape table
    // bitmask = i64.load(shape_table_addr + shape_id * 8)
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::LocalGet(0)); // shape_table_addr
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::LocalSet(6)); // bitmask

    // Iterate fields: field_idx = 0; while bitmask != 0
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(7)); // field_idx = 0

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // if field_idx * 8 >= data_size, break
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::LocalGet(4)); // data_size
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // if bitmask & (1 << field_idx) != 0
    func.instruction(&Instruction::LocalGet(6)); // bitmask
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::LocalGet(7)); // field_idx
    func.instruction(&Instruction::I64ExtendI32U);
    func.instruction(&Instruction::I64Shl);
    func.instruction(&Instruction::I64And);
    func.instruction(&Instruction::I64Eqz);
    func.instruction(&Instruction::I32Eqz); // NOT zero → bit is set
    func.instruction(&Instruction::If(BlockType::Empty));

    // child_ptr = low 32 bits of i64 at scan_ptr + field_idx * 8
    // (fields store i32 pointers widened to i64 via I64ExtendI32U)
    func.instruction(&Instruction::LocalGet(1)); // scan_ptr
    func.instruction(&Instruction::LocalGet(7)); // field_idx
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load(memarg(0))); // low 32 bits of i64
    func.instruction(&Instruction::LocalSet(5)); // child_ptr

    // Validate and push child
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(GC_HEAP_START as i32));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz); // white?
    func.instruction(&Instruction::If(BlockType::Empty));
    // Mark black
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(MARK_BLACK as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    // Push to worklist
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(GC_WORKLIST_END as i32));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End); // worklist space
    func.instruction(&Instruction::End); // white
    func.instruction(&Instruction::End); // < heap_ptr
    func.instruction(&Instruction::End); // >= GC_HEAP_START

    func.instruction(&Instruction::End); // end if (bit set)

    // field_idx++
    func.instruction(&Instruction::LocalGet(7));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(7));
    func.instruction(&Instruction::Br(0)); // continue
    func.instruction(&Instruction::End); // end loop (fields)
    func.instruction(&Instruction::End); // end block (fields)

    func.instruction(&Instruction::Else);

    // kind == 1 (VECTOR) with shape_id == 1 (pointer elements): trace each element
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32ShrU);
    func.instruction(&Instruction::I32Const(0xFF));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Const(OBJ_KIND_VECTOR as i32));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty));

    // Check shape_id == 1 (pointer elements)
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(16));
    func.instruction(&Instruction::I32ShrU);
    func.instruction(&Instruction::I32Const(0xFFFF));
    func.instruction(&Instruction::I32And);
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty));

    // Read vec length from scan_ptr[0] (i64, but we just need it as count)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I64Load(memarg(0)));
    func.instruction(&Instruction::I32WrapI64);
    func.instruction(&Instruction::LocalSet(7)); // field_idx = len (reuse as count)

    // Iterate elements: i from 0 to len-1
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::LocalSet(8)); // reuse prev_ptr as loop counter

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(8));
    func.instruction(&Instruction::LocalGet(7)); // len
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // child_ptr = low 32 bits of i64 at scan_ptr + 8 + i*8
    func.instruction(&Instruction::LocalGet(1)); // scan_ptr
    func.instruction(&Instruction::LocalGet(8)); // i
    func.instruction(&Instruction::I32Const(8));
    func.instruction(&Instruction::I32Mul);
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Const(8)); // skip length slot
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::I32Load(memarg(0))); // low 32 bits
    func.instruction(&Instruction::LocalSet(5));

    // Validate and push child (same pattern as struct fields)
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(GC_HEAP_START as i32));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Sub);
    func.instruction(&Instruction::I32Const(MARK_BLACK as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(GC_WORKLIST_END as i32));
    func.instruction(&Instruction::I32LtU);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::LocalGet(5));
    func.instruction(&Instruction::I32Store(memarg(0)));
    func.instruction(&Instruction::LocalGet(2));
    func.instruction(&Instruction::I32Const(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(2));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);

    // i++
    func.instruction(&Instruction::LocalGet(8));
    func.instruction(&Instruction::I32Const(1));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(8));
    func.instruction(&Instruction::Br(0));
    func.instruction(&Instruction::End); // loop
    func.instruction(&Instruction::End); // block

    func.instruction(&Instruction::End); // end if (shape_id == 1)
    func.instruction(&Instruction::End); // end if (kind == VECTOR)
    // STRING/BUFFER/VECTOR(scalar): no children to trace

    func.instruction(&Instruction::End); // end if/else (kind == STRUCT)

    func.instruction(&Instruction::Br(0)); // continue worklist loop
    func.instruction(&Instruction::End); // end loop (worklist)
    func.instruction(&Instruction::End); // end block (worklist)

    // ── Phase 3: Sweep (linear heap walk) ────────────────────

    // Clear free list
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::GlobalSet(GLOBAL_FREE_LIST));

    // scan_ptr = GC_HEAP_START
    func.instruction(&Instruction::I32Const(GC_HEAP_START as i32));
    func.instruction(&Instruction::LocalSet(1));

    func.instruction(&Instruction::Block(BlockType::Empty));
    func.instruction(&Instruction::Loop(BlockType::Empty));
    // if scan_ptr >= heap_ptr, break
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::GlobalGet(GLOBAL_HEAP_PTR));
    func.instruction(&Instruction::I32GeU);
    func.instruction(&Instruction::BrIf(1));

    // Read mark byte
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Load8U(memarg(0)));
    func.instruction(&Instruction::LocalSet(3)); // mark byte

    // Read data_size
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Load(memarg(4)));
    func.instruction(&Instruction::LocalSet(4)); // data_size

    // Check if data_size is 0 (corrupted header) — skip to avoid infinite loop
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Eqz);
    func.instruction(&Instruction::If(BlockType::Empty));
    // Skip this object with minimum step
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(2)); // continue outer loop
    func.instruction(&Instruction::End);

    // Check mark byte
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(MARK_BLACK as i32));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty));
    // Marked: clear mark byte back to white (0)
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(MARK_WHITE as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    func.instruction(&Instruction::Else);
    // Unmarked (or already free): set mark to FREE (0xFF), link into free list
    func.instruction(&Instruction::LocalGet(3));
    func.instruction(&Instruction::I32Const(MARK_FREE as i32));
    func.instruction(&Instruction::I32Ne); // only if not already free
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(MARK_FREE as i32));
    func.instruction(&Instruction::I32Store8(memarg(0)));
    // Link into free list: store current free list head at data area
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::GlobalGet(GLOBAL_FREE_LIST));
    func.instruction(&Instruction::I32Store(memarg(0)));
    // Set free list head to this header
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::GlobalSet(GLOBAL_FREE_LIST));
    func.instruction(&Instruction::End); // end if (not already free)
    func.instruction(&Instruction::End); // end if/else (marked)

    // Advance: scan_ptr += HEADER_SIZE + data_size
    func.instruction(&Instruction::LocalGet(1));
    func.instruction(&Instruction::I32Const(HEADER_SIZE as i32));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalGet(4));
    func.instruction(&Instruction::I32Add);
    func.instruction(&Instruction::LocalSet(1));
    func.instruction(&Instruction::Br(0)); // continue
    func.instruction(&Instruction::End); // end loop (sweep)
    func.instruction(&Instruction::End); // end block (sweep)

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
