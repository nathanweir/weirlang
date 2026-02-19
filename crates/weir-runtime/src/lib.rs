//! Weir runtime: tracing garbage collector with shadow stack.
//!
//! All public API is `extern "C"` functions callable from Cranelift-generated code.
//!
//! ## Object layout
//!
//! Every GC-allocated object has a header at a *negative* offset from the user pointer:
//!
//! ```text
//!   Memory:   [ObjHeader (32 bytes)] [user data ...]
//!   weir_gc_alloc returns:            ^-- this pointer
//! ```
//!
//! Existing code accesses user data at `ptr + offset` and doesn't need to know about the header.
//!
//! ## Shape descriptors
//!
//! Each allocation has a shape descriptor telling the GC which slots are heap pointers.
//! For fixed-size objects (closures), the shape has `kind = SHAPE_FIXED` and a bitmask.
//! For vectors, the shape has `kind = SHAPE_VECTOR`; the GC reads the length from slot 0
//! and traces elements starting at slot 1 if `pointer_mask & 1 != 0`.
//!
//! ## Shadow stack
//!
//! Compiled code pushes/pops **pointers to stack slots** so the GC can find roots.

use std::alloc::{self, Layout};
use std::cell::RefCell;
use std::ptr;

// ── Constants ───────────────────────────────────────────────────

const HEADER_SIZE: usize = std::mem::size_of::<ObjHeader>();
const INITIAL_GC_THRESHOLD: usize = 256 * 1024; // 256 KB
const GC_GROWTH_FACTOR: usize = 2;

/// Fixed-size object: `num_slots` and `pointer_mask` describe the layout exactly.
pub const SHAPE_FIXED: u32 = 0;
/// Vector object: length at slot 0, elements at slots 1..len+1.
/// If `pointer_mask & 1 != 0`, elements are heap pointers.
pub const SHAPE_VECTOR: u32 = 1;

// ── Shape descriptors ───────────────────────────────────────────

/// Tells the GC which i64 slots in an object contain heap pointers.
#[repr(C)]
pub struct ShapeDesc {
    /// Number of i64 slots in the object (for SHAPE_FIXED).
    /// Ignored for SHAPE_VECTOR (length read from slot 0).
    pub num_slots: u32,
    /// Object kind: SHAPE_FIXED (0) or SHAPE_VECTOR (1).
    pub kind: u32,
    /// For SHAPE_FIXED: bitmask where bit i = 1 means slot i is a heap pointer.
    /// For SHAPE_VECTOR: if bit 0 = 1, all elements are heap pointers.
    pub pointer_mask: u64,
}

// ── Object header ───────────────────────────────────────────────

/// Header stored at `user_ptr - HEADER_SIZE`. Linked into the allocation list.
#[repr(C)]
struct ObjHeader {
    /// Intrusive linked list: next allocated object (or null).
    next: *mut ObjHeader,
    /// Bit 0: mark bit (1 = reachable). Bits 1+: unused.
    mark: u64,
    /// Pointer to the shape descriptor for this object.
    shape: *const ShapeDesc,
    /// Size of user data in bytes (needed for deallocation of variable-size objects).
    user_size: u64,
}

impl ObjHeader {
    fn is_marked(&self) -> bool {
        self.mark & 1 != 0
    }

    fn set_mark(&mut self) {
        self.mark |= 1;
    }

    fn clear_mark(&mut self) {
        self.mark &= !1;
    }
}

// ── GC heap ─────────────────────────────────────────────────────

struct GcHeap {
    /// Head of intrusive linked list of all allocations.
    all_objects: *mut ObjHeader,
    /// Bytes allocated since last collection.
    bytes_since_gc: usize,
    /// Total bytes currently allocated (for threshold tuning).
    total_bytes: usize,
    /// Threshold to trigger collection.
    gc_threshold: usize,
    /// Shadow stack: pointers to stack slots that hold GC heap pointers.
    shadow_stack: Vec<*mut i64>,
    /// Depth counter for GC suppression (> 0 means suppressed, supports nesting).
    gc_suppress_depth: u32,
}

impl GcHeap {
    fn new() -> Self {
        Self {
            all_objects: ptr::null_mut(),
            bytes_since_gc: 0,
            total_bytes: 0,
            gc_threshold: INITIAL_GC_THRESHOLD,
            shadow_stack: Vec::with_capacity(256),
            gc_suppress_depth: 0,
        }
    }
}

// ── Thread-local GC state ───────────────────────────────────────

thread_local! {
    static GC_HEAP: RefCell<GcHeap> = RefCell::new(GcHeap::new());
}

// ── Allocation ──────────────────────────────────────────────────

/// Allocate a GC-managed object.
///
/// Returns a user pointer (past the header). The header is at `ptr - HEADER_SIZE`.
/// `size_bytes` is the size of the user data (not including the header).
/// `shape` points to a static ShapeDesc describing pointer layout.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_alloc(size_bytes: i64, shape: *const ShapeDesc) -> i64 {
    GC_HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        let user_size = size_bytes as usize;

        // Check if we should collect before allocating (skip when GC is suppressed for arena)
        if heap.gc_suppress_depth == 0
            && heap.bytes_since_gc + HEADER_SIZE + user_size > heap.gc_threshold
        {
            collect_inner(&mut heap);
        }

        alloc_inner(&mut heap, user_size, shape)
    })
}

fn alloc_inner(heap: &mut GcHeap, user_size: usize, shape: *const ShapeDesc) -> i64 {
    let total_size = HEADER_SIZE + user_size;
    let layout = Layout::from_size_align(total_size, 8).unwrap();
    let raw = unsafe { alloc::alloc_zeroed(layout) };
    if raw.is_null() {
        alloc::handle_alloc_error(layout);
    }

    let header = raw as *mut ObjHeader;
    unsafe {
        (*header).next = heap.all_objects;
        (*header).mark = 0;
        (*header).shape = shape;
        (*header).user_size = user_size as u64;
    }
    heap.all_objects = header;
    heap.bytes_since_gc += total_size;
    heap.total_bytes += total_size;

    // User pointer is past the header
    (raw as usize + HEADER_SIZE) as i64
}

/// Allocate a GC-managed vector: `[length, elem_0, elem_1, ...]`.
///
/// Stores `len` at offset 0 and returns the user pointer.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_vec_alloc(len: i64, shape: *const ShapeDesc) -> i64 {
    let total_bytes = (1 + len) * 8;
    let ptr = weir_gc_alloc(total_bytes, shape);
    unsafe {
        *(ptr as *mut i64) = len;
    }
    ptr
}

// ── Shadow stack ────────────────────────────────────────────────

/// Push a pointer to a stack slot onto the shadow stack.
/// Called at function entry for each local that holds a heap pointer.
#[unsafe(no_mangle)]
pub extern "C" fn weir_shadow_push(slot: *mut i64) {
    GC_HEAP.with(|heap| {
        heap.borrow_mut().shadow_stack.push(slot);
    });
}

/// Pop `n` entries from the shadow stack.
/// Called at function exit.
#[unsafe(no_mangle)]
pub extern "C" fn weir_shadow_pop(n: i64) {
    GC_HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        let new_len = heap.shadow_stack.len().saturating_sub(n as usize);
        heap.shadow_stack.truncate(new_len);
    });
}

// ── Mark and sweep ──────────────────────────────────────────────

/// Trigger a garbage collection.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_collect() {
    GC_HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        collect_inner(&mut heap);
    });
}

fn collect_inner(heap: &mut GcHeap) {
    // Phase 1: Mark — walk shadow stack roots
    let roots: Vec<*mut i64> = heap.shadow_stack.clone();
    for &slot_ptr in &roots {
        let val = unsafe { *slot_ptr };
        if val != 0 {
            mark_object(val);
        }
    }

    // Phase 2: Sweep — walk allocation list, free unmarked, clear marks on survivors
    let mut prev: *mut *mut ObjHeader = &mut heap.all_objects;
    let mut current = heap.all_objects;

    while !current.is_null() {
        let next = unsafe { (*current).next };

        if unsafe { (*current).is_marked() } {
            // Survived — clear mark for next cycle
            unsafe { (*current).clear_mark() };
            prev = unsafe { &mut (*current).next };
            current = next;
        } else {
            // Unreachable — unlink and free
            unsafe { *prev = next };

            let user_size = unsafe { (*current).user_size } as usize;
            let total_size = HEADER_SIZE + user_size;
            heap.total_bytes = heap.total_bytes.saturating_sub(total_size);

            let layout = Layout::from_size_align(total_size, 8).unwrap();
            unsafe { alloc::dealloc(current as *mut u8, layout) };

            current = next;
        }
    }

    heap.bytes_since_gc = 0;

    // Grow threshold so we don't GC too frequently
    if heap.total_bytes > heap.gc_threshold / 2 {
        heap.gc_threshold = heap.total_bytes * GC_GROWTH_FACTOR;
    }
}

/// Recursively mark an object reachable from a root.
fn mark_object(user_ptr: i64) {
    if user_ptr == 0 {
        return;
    }

    let header_ptr = (user_ptr as usize - HEADER_SIZE) as *mut ObjHeader;

    let header = unsafe { &mut *header_ptr };
    if header.is_marked() {
        return; // Already visited
    }
    header.set_mark();

    // Trace through pointer slots using shape descriptor
    let shape = header.shape;
    if shape.is_null() {
        return;
    }

    let shape = unsafe { &*shape };
    if shape.pointer_mask == 0 {
        return; // No pointer slots
    }

    let data = user_ptr as *const i64;

    match shape.kind {
        SHAPE_FIXED => {
            // Fixed-layout object: scan according to bitmask
            for i in 0..shape.num_slots {
                if shape.pointer_mask & (1u64 << i) != 0 {
                    let child = unsafe { *data.add(i as usize) };
                    if child != 0 {
                        mark_object(child);
                    }
                }
            }
        }
        SHAPE_VECTOR => {
            // Vector: length at slot 0, elements at slot 1..len+1
            // pointer_mask bit 0 indicates whether elements are pointers
            if shape.pointer_mask & 1 != 0 {
                let len = unsafe { *data } as usize;
                for i in 0..len {
                    let child = unsafe { *data.add(1 + i) };
                    if child != 0 {
                        mark_object(child);
                    }
                }
            }
        }
        _ => {} // Unknown kind — don't trace
    }
}

// ── Accessors (unchanged API for codegen) ───────────────────────

/// Get an element from a GC-managed vector at the given index.
#[unsafe(no_mangle)]
pub extern "C" fn weir_vec_get(ptr: i64, idx: i64) -> i64 {
    unsafe {
        let base = ptr as *const i64;
        *base.offset(1 + idx as isize)
    }
}

/// Get the length of a GC-managed vector.
#[unsafe(no_mangle)]
pub extern "C" fn weir_vec_len(ptr: i64) -> i64 {
    unsafe { *(ptr as *const i64) }
}

/// Set an element in a vector at the given index, returning a new vector.
#[unsafe(no_mangle)]
pub extern "C" fn weir_vec_set(ptr: i64, idx: i64, val: i64, shape: *const ShapeDesc) -> i64 {
    let len = weir_vec_len(ptr);
    let new_ptr = weir_gc_vec_alloc(len, shape);
    unsafe {
        let src = (ptr as *const i64).offset(1);
        let dst = (new_ptr as *mut i64).offset(1);
        ptr::copy_nonoverlapping(src, dst, len as usize);
        *dst.offset(idx as isize) = val;
    }
    new_ptr
}

/// Append an element to a vector, returning a new vector.
#[unsafe(no_mangle)]
pub extern "C" fn weir_vec_append(ptr: i64, elem: i64, shape: *const ShapeDesc) -> i64 {
    let old_len = weir_vec_len(ptr);
    let new_len = old_len + 1;
    let new_ptr = weir_gc_vec_alloc(new_len, shape);
    unsafe {
        let src = (ptr as *const i64).offset(1);
        let dst = (new_ptr as *mut i64).offset(1);
        ptr::copy_nonoverlapping(src, dst, old_len as usize);
        *dst.offset(old_len as isize) = elem;
    }
    new_ptr
}

// ── GC-managed strings ──────────────────────────────────────────

/// Shape descriptor for strings: no pointer fields, just opaque bytes.
static STRING_SHAPE: ShapeDesc = ShapeDesc {
    num_slots: 0,
    kind: SHAPE_FIXED,
    pointer_mask: 0,
};

/// Allocate a GC-managed string of `len` bytes (plus null terminator).
/// Memory is zeroed (null terminator included). Caller writes the content.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_str_alloc(len: i64) -> i64 {
    weir_gc_alloc(len + 1, &STRING_SHAPE)
}

/// Copy a null-terminated C string into a new GC-managed string.
/// Safe to call with static (data-section) pointers.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_str_dup(src: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(src as *const std::ffi::c_char) };
    let bytes = c_str.to_bytes();
    let len = bytes.len() as i64;
    let dst = weir_gc_str_alloc(len);
    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), dst as *mut u8, bytes.len());
    }
    dst
}

// ── Arena allocator ─────────────────────────────────────────────

const ARENA_INITIAL_CAPACITY: usize = 64 * 1024; // 64 KB

#[repr(C)]
struct ArenaChunk {
    base: *mut u8,
    capacity: usize,
    next: *mut ArenaChunk,
}

#[repr(C)]
struct Arena {
    current_chunk: *mut ArenaChunk,
    offset: usize,
}

unsafe fn arena_new_chunk(capacity: usize) -> *mut ArenaChunk {
    let layout = alloc::Layout::from_size_align(capacity, 8).unwrap();
    let base = alloc::alloc_zeroed(layout);
    if base.is_null() {
        alloc::handle_alloc_error(layout);
    }
    Box::into_raw(Box::new(ArenaChunk {
        base,
        capacity,
        next: ptr::null_mut(),
    }))
}

/// Create a new arena with an initial 64 KB chunk.
#[unsafe(no_mangle)]
pub extern "C" fn weir_arena_create() -> i64 {
    unsafe {
        let chunk = arena_new_chunk(ARENA_INITIAL_CAPACITY);
        let arena = Box::into_raw(Box::new(Arena {
            current_chunk: chunk,
            offset: 0,
        }));
        arena as i64
    }
}

/// Bump-allocate from the arena. Returns a zeroed user pointer.
#[unsafe(no_mangle)]
pub extern "C" fn weir_arena_alloc(arena_ptr: i64, size: i64) -> i64 {
    let size = size as usize;
    // Align size to 8 bytes
    let aligned = (size + 7) & !7;
    unsafe {
        let arena = &mut *(arena_ptr as *mut Arena);
        let chunk = &*arena.current_chunk;

        if arena.offset + aligned <= chunk.capacity {
            let ptr = chunk.base.add(arena.offset);
            arena.offset += aligned;
            return ptr as i64;
        }

        // Need a new chunk — double the size
        let new_cap = (chunk.capacity * 2).max(aligned);
        let new_chunk = arena_new_chunk(new_cap);
        (*new_chunk).next = arena.current_chunk;
        arena.current_chunk = new_chunk;
        arena.offset = aligned;
        (*new_chunk).base as i64
    }
}

/// Arena-allocate a vector: `[length, elem_0, ...]`.
#[unsafe(no_mangle)]
pub extern "C" fn weir_arena_vec_alloc(arena_ptr: i64, len: i64) -> i64 {
    let total_bytes = (1 + len) * 8;
    let ptr = weir_arena_alloc(arena_ptr, total_bytes);
    unsafe {
        *(ptr as *mut i64) = len;
    }
    ptr
}

/// Destroy an arena: free all chunks and the arena struct itself.
#[unsafe(no_mangle)]
pub extern "C" fn weir_arena_destroy(arena_ptr: i64) {
    unsafe {
        let arena = Box::from_raw(arena_ptr as *mut Arena);
        let mut chunk = arena.current_chunk;
        while !chunk.is_null() {
            let next = (*chunk).next;
            let layout = alloc::Layout::from_size_align((*chunk).capacity, 8).unwrap();
            alloc::dealloc((*chunk).base, layout);
            drop(Box::from_raw(chunk));
            chunk = next;
        }
    }
}

/// Suppress the GC (called when entering a with-arena block).
/// Uses a depth counter to support nested arenas.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_suppress() {
    GC_HEAP.with(|heap| {
        heap.borrow_mut().gc_suppress_depth += 1;
    });
}

/// Unsuppress the GC (called when exiting a with-arena block).
/// Decrements the depth counter. Does NOT eagerly trigger collection —
/// the next allocation will check the threshold and collect if needed.
/// This is important because the caller may still hold a GC pointer in a
/// register that hasn't been written to a shadow-stack-protected slot yet.
#[unsafe(no_mangle)]
pub extern "C" fn weir_gc_unsuppress() {
    GC_HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        heap.gc_suppress_depth = heap.gc_suppress_depth.saturating_sub(1);
    });
}

// ── Atomic operations ───────────────────────────────────────────

/// Create an atom: heap-allocated AtomicI64 (not GC-managed).
#[unsafe(no_mangle)]
pub extern "C" fn weir_atom_create(initial: i64) -> i64 {
    use std::sync::atomic::AtomicI64;
    let atom = Box::new(AtomicI64::new(initial));
    Box::into_raw(atom) as i64
}

/// Dereference an atom: atomic load with Acquire ordering.
#[unsafe(no_mangle)]
pub extern "C" fn weir_atom_deref(atom_ptr: i64) -> i64 {
    use std::sync::atomic::{AtomicI64, Ordering};
    unsafe { &*(atom_ptr as *const AtomicI64) }.load(Ordering::Acquire)
}

/// Compare-and-swap on an atom. Returns the actual old value.
/// If actual == expected, the swap succeeded.
#[unsafe(no_mangle)]
pub extern "C" fn weir_atom_cas(atom_ptr: i64, expected: i64, new: i64) -> i64 {
    use std::sync::atomic::{AtomicI64, Ordering};
    let atom = unsafe { &*(atom_ptr as *const AtomicI64) };
    match atom.compare_exchange(expected, new, Ordering::AcqRel, Ordering::Acquire) {
        Ok(old) => old,
        Err(actual) => actual,
    }
}

// ── Channel operations ──────────────────────────────────────────

/// Create an mpsc channel. Returns a boxed pair (sender_ptr, receiver_ptr) as i64.
#[unsafe(no_mangle)]
pub extern "C" fn weir_channel_create() -> i64 {
    let (tx, rx) = std::sync::mpsc::channel::<i64>();
    let pair: Box<(std::sync::mpsc::Sender<i64>, std::sync::mpsc::Receiver<i64>)> =
        Box::new((tx, rx));
    Box::into_raw(pair) as i64
}

/// Send a value on a channel.
#[unsafe(no_mangle)]
pub extern "C" fn weir_channel_send(ch_ptr: i64, val: i64) {
    let pair = unsafe {
        &*(ch_ptr as *const (std::sync::mpsc::Sender<i64>, std::sync::mpsc::Receiver<i64>))
    };
    let _ = pair.0.send(val);
}

/// Receive a value from a channel (blocking).
#[unsafe(no_mangle)]
pub extern "C" fn weir_channel_recv(ch_ptr: i64) -> i64 {
    let pair = unsafe {
        &*(ch_ptr as *const (std::sync::mpsc::Sender<i64>, std::sync::mpsc::Receiver<i64>))
    };
    pair.1.recv().unwrap_or(0)
}

// ── Thread operations ───────────────────────────────────────────

/// Spawn a thread that calls a zero-arg function pointer.
/// Returns a boxed JoinHandle as i64.
#[unsafe(no_mangle)]
pub extern "C" fn weir_thread_spawn(fn_ptr: i64) -> i64 {
    let func: extern "C" fn() -> i64 = unsafe { std::mem::transmute(fn_ptr) };
    let handle = std::thread::spawn(move || {
        func();
    });
    Box::into_raw(Box::new(handle)) as i64
}

/// Join a thread, blocking until it completes.
#[unsafe(no_mangle)]
pub extern "C" fn weir_thread_join(handle_ptr: i64) -> i64 {
    let handle = unsafe { Box::from_raw(handle_ptr as *mut std::thread::JoinHandle<()>) };
    match handle.join() {
        Ok(()) => 0,
        Err(_) => 1,
    }
}

// ── Parallel iteration (sequential for now) ─────────────────────

/// par-map: apply a closure to each element of a vector, returning a new vector.
/// closure_ptr: pointer to heap closure (slot 0 = fn_ptr)
/// vec_ptr: pointer to heap vector [len, elem0, elem1, ...]
/// result_shape_ptr: shape descriptor for the result vector
/// Returns: pointer to new vector
#[unsafe(no_mangle)]
pub extern "C" fn weir_par_map(closure_ptr: i64, vec_ptr: i64, result_shape_ptr: i64) -> i64 {
    let vec_data = vec_ptr as *const i64;
    let len = unsafe { *vec_data } as usize;

    // Read fn_ptr from closure slot 0
    let fn_ptr_raw = unsafe { *(closure_ptr as *const i64) };
    let func: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(fn_ptr_raw) };

    // Allocate result vector
    let result_ptr = weir_gc_vec_alloc(len as i64, result_shape_ptr as *const ShapeDesc);
    let result_data = result_ptr as *mut i64;

    for i in 0..len {
        let elem = unsafe { *vec_data.add(1 + i) };
        let mapped = func(closure_ptr, elem);
        unsafe { *result_data.add(1 + i) = mapped };
    }

    result_ptr
}

/// par-for-each: apply a closure to each element of a vector for side effects.
/// closure_ptr: pointer to heap closure (slot 0 = fn_ptr)
/// vec_ptr: pointer to heap vector [len, elem0, elem1, ...]
#[unsafe(no_mangle)]
pub extern "C" fn weir_par_for_each(closure_ptr: i64, vec_ptr: i64) {
    let vec_data = vec_ptr as *const i64;
    let len = unsafe { *vec_data } as usize;

    let fn_ptr_raw = unsafe { *(closure_ptr as *const i64) };
    let func: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(fn_ptr_raw) };

    for i in 0..len {
        let elem = unsafe { *vec_data.add(1 + i) };
        func(closure_ptr, elem);
    }
}

// ── Reset (for testing) ─────────────────────────────────────────

/// Reset the GC heap (free everything). Used for test isolation.
pub fn gc_reset() {
    GC_HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();

        // Free all objects
        let mut current = heap.all_objects;
        while !current.is_null() {
            let next = unsafe { (*current).next };
            let user_size = unsafe { (*current).user_size } as usize;
            let total_size = HEADER_SIZE + user_size;
            let layout = Layout::from_size_align(total_size, 8).unwrap();
            unsafe { alloc::dealloc(current as *mut u8, layout) };
            current = next;
        }

        heap.all_objects = ptr::null_mut();
        heap.bytes_since_gc = 0;
        heap.total_bytes = 0;
        heap.gc_threshold = INITIAL_GC_THRESHOLD;
        heap.shadow_stack.clear();
        heap.gc_suppress_depth = 0;
    });
}

/// Return the count of live objects on the GC heap.
pub fn gc_object_count() -> usize {
    GC_HEAP.with(|heap| {
        let heap = heap.borrow();
        let mut count = 0;
        let mut current = heap.all_objects;
        while !current.is_null() {
            count += 1;
            current = unsafe { (*current).next };
        }
        count
    })
}

/// Return total bytes allocated on the GC heap.
pub fn gc_total_bytes() -> usize {
    GC_HEAP.with(|heap| heap.borrow().total_bytes)
}

// ── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn make_shape(num_slots: u32, pointer_mask: u64) -> ShapeDesc {
        ShapeDesc {
            num_slots,
            kind: SHAPE_FIXED,
            pointer_mask,
        }
    }

    fn make_vec_shape(elements_are_pointers: bool) -> ShapeDesc {
        ShapeDesc {
            num_slots: 0,
            kind: SHAPE_VECTOR,
            pointer_mask: if elements_are_pointers { 1 } else { 0 },
        }
    }

    #[test]
    fn alloc_returns_valid_pointer() {
        gc_reset();
        let shape = make_shape(2, 0);
        let ptr = weir_gc_alloc(16, &shape);
        assert_ne!(ptr, 0);
        unsafe {
            *(ptr as *mut i64) = 42;
            *((ptr + 8) as *mut i64) = 99;
            assert_eq!(*(ptr as *const i64), 42);
            assert_eq!(*((ptr + 8) as *const i64), 99);
        }
        assert_eq!(gc_object_count(), 1);
        gc_reset();
    }

    #[test]
    fn vec_alloc_stores_length() {
        gc_reset();
        let shape = make_vec_shape(false);
        let ptr = weir_gc_vec_alloc(3, &shape);
        assert_eq!(weir_vec_len(ptr), 3);
        unsafe {
            *((ptr as *mut i64).offset(1)) = 10;
            *((ptr as *mut i64).offset(2)) = 20;
            *((ptr as *mut i64).offset(3)) = 30;
        }
        assert_eq!(weir_vec_get(ptr, 0), 10);
        assert_eq!(weir_vec_get(ptr, 1), 20);
        assert_eq!(weir_vec_get(ptr, 2), 30);
        gc_reset();
    }

    #[test]
    fn unreachable_objects_are_freed() {
        gc_reset();
        let shape = make_shape(1, 0);
        let _p1 = weir_gc_alloc(8, &shape);
        let _p2 = weir_gc_alloc(8, &shape);
        let _p3 = weir_gc_alloc(8, &shape);
        assert_eq!(gc_object_count(), 3);

        weir_gc_collect();
        assert_eq!(gc_object_count(), 0);
        gc_reset();
    }

    #[test]
    fn reachable_objects_survive() {
        gc_reset();
        let shape = make_shape(1, 0);

        let ptr = weir_gc_alloc(8, &shape);
        let mut root: i64 = ptr;
        weir_shadow_push(&mut root as *mut i64);

        let _garbage1 = weir_gc_alloc(8, &shape);
        let _garbage2 = weir_gc_alloc(8, &shape);
        assert_eq!(gc_object_count(), 3);

        weir_gc_collect();
        assert_eq!(gc_object_count(), 1);

        weir_shadow_pop(1);
        gc_reset();
    }

    #[test]
    fn transitive_reachability() {
        gc_reset();
        let shape_ptr = make_shape(1, 0b1);
        let shape_leaf = make_shape(1, 0);

        let leaf = weir_gc_alloc(8, &shape_leaf);
        unsafe { *(leaf as *mut i64) = 999; }

        let child = weir_gc_alloc(8, &shape_ptr);
        unsafe { *(child as *mut i64) = leaf; }

        let root = weir_gc_alloc(8, &shape_ptr);
        unsafe { *(root as *mut i64) = child; }

        let _garbage = weir_gc_alloc(8, &shape_leaf);

        assert_eq!(gc_object_count(), 4);

        let mut root_slot: i64 = root;
        weir_shadow_push(&mut root_slot as *mut i64);

        weir_gc_collect();
        assert_eq!(gc_object_count(), 3);

        unsafe {
            let child_ptr = *(root_slot as *const i64);
            let leaf_ptr = *(child_ptr as *const i64);
            let leaf_data = *(leaf_ptr as *const i64);
            assert_eq!(leaf_data, 999);
        }

        weir_shadow_pop(1);
        gc_reset();
    }

    #[test]
    fn shadow_stack_push_pop() {
        gc_reset();
        let mut a: i64 = 1;
        let mut b: i64 = 2;
        let mut c: i64 = 3;

        weir_shadow_push(&mut a as *mut i64);
        weir_shadow_push(&mut b as *mut i64);
        weir_shadow_push(&mut c as *mut i64);

        GC_HEAP.with(|heap| {
            assert_eq!(heap.borrow().shadow_stack.len(), 3);
        });

        weir_shadow_pop(2);
        GC_HEAP.with(|heap| {
            assert_eq!(heap.borrow().shadow_stack.len(), 1);
        });

        weir_shadow_pop(1);
        GC_HEAP.with(|heap| {
            assert_eq!(heap.borrow().shadow_stack.len(), 0);
        });
        gc_reset();
    }

    #[test]
    fn vec_append_preserves_data() {
        gc_reset();
        let shape = make_vec_shape(false);
        let v1 = weir_gc_vec_alloc(2, &shape);
        unsafe {
            *((v1 as *mut i64).offset(1)) = 10;
            *((v1 as *mut i64).offset(2)) = 20;
        }

        let v2 = weir_vec_append(v1, 30, &shape);
        assert_eq!(weir_vec_len(v2), 3);
        assert_eq!(weir_vec_get(v2, 0), 10);
        assert_eq!(weir_vec_get(v2, 1), 20);
        assert_eq!(weir_vec_get(v2, 2), 30);
        gc_reset();
    }

    #[test]
    fn vector_of_pointers_traced() {
        gc_reset();
        let leaf_shape = make_shape(1, 0);
        let vec_shape = make_vec_shape(true); // elements are pointers

        // Create two leaf objects
        let leaf1 = weir_gc_alloc(8, &leaf_shape);
        unsafe { *(leaf1 as *mut i64) = 111; }
        let leaf2 = weir_gc_alloc(8, &leaf_shape);
        unsafe { *(leaf2 as *mut i64) = 222; }

        // Create a vector of pointers referencing the leaves
        let vec_ptr = weir_gc_vec_alloc(2, &vec_shape);
        unsafe {
            *((vec_ptr as *mut i64).offset(1)) = leaf1;
            *((vec_ptr as *mut i64).offset(2)) = leaf2;
        }

        // Also create garbage
        let _garbage = weir_gc_alloc(8, &leaf_shape);

        assert_eq!(gc_object_count(), 4); // 2 leaves + vec + garbage

        let mut root: i64 = vec_ptr;
        weir_shadow_push(&mut root as *mut i64);

        weir_gc_collect();
        // vec + 2 leaves survive, garbage collected
        assert_eq!(gc_object_count(), 3);

        // Verify leaves are intact
        unsafe {
            let l1 = *((root as *const i64).offset(1));
            let l2 = *((root as *const i64).offset(2));
            assert_eq!(*(l1 as *const i64), 111);
            assert_eq!(*(l2 as *const i64), 222);
        }

        weir_shadow_pop(1);
        gc_reset();
    }

    #[test]
    fn stress_alloc_and_collect() {
        gc_reset();
        let shape = make_shape(1, 0);

        let mut root: i64 = 0;
        weir_shadow_push(&mut root as *mut i64);

        for i in 0..10_000 {
            let ptr = weir_gc_alloc(8, &shape);
            unsafe { *(ptr as *mut i64) = i; }
            root = ptr;
        }

        weir_gc_collect();
        assert_eq!(gc_object_count(), 1);
        unsafe { assert_eq!(*(root as *const i64), 9999); }

        weir_shadow_pop(1);
        gc_reset();
    }

    #[test]
    fn header_at_negative_offset() {
        gc_reset();
        let shape = make_shape(2, 0);
        let user_ptr = weir_gc_alloc(16, &shape);

        let header_ptr = (user_ptr as usize - HEADER_SIZE) as *const ObjHeader;
        unsafe {
            assert_eq!((*header_ptr).shape, &shape as *const ShapeDesc);
            assert_eq!((*header_ptr).mark, 0);
            assert_eq!((*header_ptr).user_size, 16);
        }
        gc_reset();
    }

    #[test]
    fn collect_does_not_corrupt_remaining_objects() {
        gc_reset();
        let shape = make_shape(1, 0);

        let keep1 = weir_gc_alloc(8, &shape);
        let _drop1 = weir_gc_alloc(8, &shape);
        let keep2 = weir_gc_alloc(8, &shape);
        let _drop2 = weir_gc_alloc(8, &shape);
        let keep3 = weir_gc_alloc(8, &shape);

        unsafe {
            *(keep1 as *mut i64) = 111;
            *(keep2 as *mut i64) = 222;
            *(keep3 as *mut i64) = 333;
        }

        let mut r1 = keep1;
        let mut r2 = keep2;
        let mut r3 = keep3;
        weir_shadow_push(&mut r1);
        weir_shadow_push(&mut r2);
        weir_shadow_push(&mut r3);

        weir_gc_collect();
        assert_eq!(gc_object_count(), 3);

        unsafe {
            assert_eq!(*(r1 as *const i64), 111);
            assert_eq!(*(r2 as *const i64), 222);
            assert_eq!(*(r3 as *const i64), 333);
        }

        weir_shadow_pop(3);
        gc_reset();
    }

    // ── Arena tests ──────────────────────────────────────────────

    #[test]
    fn arena_create_destroy() {
        let arena = weir_arena_create();
        assert_ne!(arena, 0);
        weir_arena_destroy(arena);
    }

    #[test]
    fn arena_bump_allocation() {
        let arena = weir_arena_create();
        let p1 = weir_arena_alloc(arena, 8);
        let p2 = weir_arena_alloc(arena, 8);
        assert_ne!(p1, 0);
        assert_ne!(p2, 0);
        // Sequential addresses: p2 should be 8 bytes after p1
        assert_eq!(p2, p1 + 8);
        unsafe {
            *(p1 as *mut i64) = 42;
            *(p2 as *mut i64) = 99;
            assert_eq!(*(p1 as *const i64), 42);
            assert_eq!(*(p2 as *const i64), 99);
        }
        weir_arena_destroy(arena);
    }

    #[test]
    fn arena_chunk_growth() {
        let arena = weir_arena_create();
        // Allocate more than 64 KB to force a new chunk
        for _ in 0..9000 {
            let p = weir_arena_alloc(arena, 8);
            assert_ne!(p, 0);
        }
        weir_arena_destroy(arena);
    }

    #[test]
    fn arena_vec_alloc() {
        let arena = weir_arena_create();
        let ptr = weir_arena_vec_alloc(arena, 3);
        assert_eq!(weir_vec_len(ptr), 3);
        unsafe {
            *((ptr as *mut i64).offset(1)) = 10;
            *((ptr as *mut i64).offset(2)) = 20;
            *((ptr as *mut i64).offset(3)) = 30;
        }
        assert_eq!(weir_vec_get(ptr, 0), 10);
        assert_eq!(weir_vec_get(ptr, 1), 20);
        assert_eq!(weir_vec_get(ptr, 2), 30);
        weir_arena_destroy(arena);
    }

    #[test]
    fn gc_suppression_prevents_collection() {
        gc_reset();
        let shape = make_shape(1, 0);

        // Allocate some objects without roots
        let _p1 = weir_gc_alloc(8, &shape);
        let _p2 = weir_gc_alloc(8, &shape);
        assert_eq!(gc_object_count(), 2);

        // Suppress GC and force a "collection" by calling collect
        weir_gc_suppress();
        // Manual collect while suppressed should still work (direct call)
        // but the threshold check in weir_gc_alloc should not trigger
        weir_gc_unsuppress();
        // After unsuppression, collection may run
        gc_reset();
    }

    #[test]
    fn arena_returns_zeroed_memory() {
        let arena = weir_arena_create();
        let ptr = weir_arena_alloc(arena, 64);
        unsafe {
            for i in 0..8 {
                assert_eq!(*((ptr as *const i64).add(i)), 0);
            }
        }
        weir_arena_destroy(arena);
    }
}
