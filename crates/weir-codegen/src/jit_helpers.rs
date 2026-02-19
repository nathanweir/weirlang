// ── Runtime print helpers (JIT in-process) ───────────────────────
//
// These are extern "C" functions that JIT-compiled code calls to print.
// They write to a thread-local String buffer for testability.

use cranelift_jit::JITBuilder;

use weir_runtime::{
    weir_arena_alloc, weir_arena_create, weir_arena_destroy, weir_arena_vec_alloc,
    weir_atom_cas, weir_atom_create, weir_atom_deref, weir_channel_create, weir_channel_recv,
    weir_channel_send, weir_gc_alloc, weir_gc_collect, weir_gc_suppress, weir_gc_unsuppress,
    weir_gc_vec_alloc, weir_par_for_each, weir_par_map, weir_shadow_pop, weir_shadow_push,
    weir_thread_join, weir_thread_spawn, weir_vec_append, weir_vec_get, weir_vec_len,
};

std::thread_local! {
    static OUTPUT_BUF: std::cell::RefCell<String> = const { std::cell::RefCell::new(String::new()) };
}

pub(crate) fn output_reset() {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().clear());
}

pub(crate) fn output_take() -> String {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().split_off(0))
}

extern "C" fn weir_print_i64(val: i64) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(buf.borrow_mut(), "{}", val).unwrap();
    });
}

extern "C" fn weir_print_f64(val: f64) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        if val.fract() == 0.0 {
            write!(buf.borrow_mut(), "{:.1}", val).unwrap();
        } else {
            write!(buf.borrow_mut(), "{}", val).unwrap();
        }
    });
}

extern "C" fn weir_print_bool(val: i8) {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(
            buf.borrow_mut(),
            "{}",
            if val != 0 { "true" } else { "false" }
        )
        .unwrap();
    });
}

extern "C" fn weir_print_unit() {
    OUTPUT_BUF.with(|buf| {
        use std::fmt::Write;
        write!(buf.borrow_mut(), "()").unwrap();
    });
}

extern "C" fn weir_print_newline() {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push('\n'));
}

extern "C" fn weir_sleep_ms(ms: i64) {
    std::thread::sleep(std::time::Duration::from_millis(ms as u64));
}

extern "C" fn weir_print_str(ptr: i64) {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let s = c_str.to_str().unwrap_or("<invalid utf8>");
    OUTPUT_BUF.with(|buf| buf.borrow_mut().push_str(s));
}

extern "C" fn weir_i64_to_str(val: i64) -> i64 {
    let s = format!("{}", val);
    let c_string = std::ffi::CString::new(s).unwrap();
    c_string.into_raw() as i64
}

extern "C" fn weir_f64_to_str(val: f64) -> i64 {
    let s = if val.fract() == 0.0 {
        format!("{:.1}", val)
    } else {
        format!("{}", val)
    };
    let c_string = std::ffi::CString::new(s).unwrap();
    c_string.into_raw() as i64
}

extern "C" fn weir_bool_to_str(val: i8) -> i64 {
    let s = if val != 0 { "true" } else { "false" };
    let c_string = std::ffi::CString::new(s).unwrap();
    c_string.into_raw() as i64
}

extern "C" fn weir_str_concat(a: i64, b: i64) -> i64 {
    let a_str = unsafe { std::ffi::CStr::from_ptr(a as *const _) }
        .to_str()
        .unwrap_or("");
    let b_str = unsafe { std::ffi::CStr::from_ptr(b as *const _) }
        .to_str()
        .unwrap_or("");
    let combined = format!("{}{}", a_str, b_str);
    std::ffi::CString::new(combined).unwrap().into_raw() as i64
}

extern "C" fn weir_str_eq(a: i64, b: i64) -> i8 {
    let a_str = unsafe { std::ffi::CStr::from_ptr(a as *const _) };
    let b_str = unsafe { std::ffi::CStr::from_ptr(b as *const _) };
    if a_str == b_str { 1 } else { 0 }
}

extern "C" fn weir_str_cmp(a: i64, b: i64) -> i64 {
    let a_str = unsafe { std::ffi::CStr::from_ptr(a as *const _) };
    let b_str = unsafe { std::ffi::CStr::from_ptr(b as *const _) };
    match a_str.cmp(b_str) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

/// Registers all JIT runtime symbols on a JITBuilder.
pub(crate) fn register_jit_symbols(builder: &mut JITBuilder) {
    builder.symbol("weir_print_i64", weir_print_i64 as *const u8);
    builder.symbol("weir_print_f64", weir_print_f64 as *const u8);
    builder.symbol("weir_print_bool", weir_print_bool as *const u8);
    builder.symbol("weir_print_unit", weir_print_unit as *const u8);
    builder.symbol("weir_print_newline", weir_print_newline as *const u8);
    builder.symbol("weir_sleep_ms", weir_sleep_ms as *const u8);
    builder.symbol("weir_print_str", weir_print_str as *const u8);
    builder.symbol("weir_i64_to_str", weir_i64_to_str as *const u8);
    builder.symbol("weir_f64_to_str", weir_f64_to_str as *const u8);
    builder.symbol("weir_bool_to_str", weir_bool_to_str as *const u8);
    builder.symbol("weir_str_concat", weir_str_concat as *const u8);
    builder.symbol("weir_str_eq", weir_str_eq as *const u8);
    builder.symbol("weir_str_cmp", weir_str_cmp as *const u8);
    builder.symbol("weir_gc_alloc", weir_gc_alloc as *const u8);
    builder.symbol("weir_gc_vec_alloc", weir_gc_vec_alloc as *const u8);
    builder.symbol("weir_gc_collect", weir_gc_collect as *const u8);
    builder.symbol("weir_vec_get", weir_vec_get as *const u8);
    builder.symbol("weir_vec_len", weir_vec_len as *const u8);
    builder.symbol("weir_vec_append", weir_vec_append as *const u8);
    builder.symbol("weir_shadow_push", weir_shadow_push as *const u8);
    builder.symbol("weir_shadow_pop", weir_shadow_pop as *const u8);
    builder.symbol("weir_arena_create", weir_arena_create as *const u8);
    builder.symbol("weir_arena_alloc", weir_arena_alloc as *const u8);
    builder.symbol("weir_arena_vec_alloc", weir_arena_vec_alloc as *const u8);
    builder.symbol("weir_arena_destroy", weir_arena_destroy as *const u8);
    builder.symbol("weir_gc_suppress", weir_gc_suppress as *const u8);
    builder.symbol("weir_gc_unsuppress", weir_gc_unsuppress as *const u8);
    builder.symbol("weir_atom_create", weir_atom_create as *const u8);
    builder.symbol("weir_atom_deref", weir_atom_deref as *const u8);
    builder.symbol("weir_atom_cas", weir_atom_cas as *const u8);
    builder.symbol("weir_channel_create", weir_channel_create as *const u8);
    builder.symbol("weir_channel_send", weir_channel_send as *const u8);
    builder.symbol("weir_channel_recv", weir_channel_recv as *const u8);
    builder.symbol("weir_thread_spawn", weir_thread_spawn as *const u8);
    builder.symbol("weir_thread_join", weir_thread_join as *const u8);
    builder.symbol("weir_par_map", weir_par_map as *const u8);
    builder.symbol("weir_par_for_each", weir_par_for_each as *const u8);
}
