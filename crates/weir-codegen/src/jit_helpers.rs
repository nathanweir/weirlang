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

// ── Math runtime (libm wrappers) ────────────────────────────────

extern "C" fn weir_sin(x: f64) -> f64 { x.sin() }
extern "C" fn weir_cos(x: f64) -> f64 { x.cos() }
extern "C" fn weir_tan(x: f64) -> f64 { x.tan() }
extern "C" fn weir_asin(x: f64) -> f64 { x.asin() }
extern "C" fn weir_acos(x: f64) -> f64 { x.acos() }
extern "C" fn weir_atan(x: f64) -> f64 { x.atan() }
extern "C" fn weir_atan2(y: f64, x: f64) -> f64 { y.atan2(x) }
extern "C" fn weir_exp(x: f64) -> f64 { x.exp() }
extern "C" fn weir_log(x: f64) -> f64 { x.ln() }
extern "C" fn weir_pow(x: f64, y: f64) -> f64 { x.powf(y) }
extern "C" fn weir_round(x: f64) -> f64 { x.round() }

// ── Random (xorshift64) ─────────────────────────────────────────

std::thread_local! {
    static RNG_STATE: std::cell::Cell<u64> = const { std::cell::Cell::new(0x12345678_9abcdef0) };
}

extern "C" fn weir_random() -> f64 {
    RNG_STATE.with(|state| {
        let mut s = state.get();
        s ^= s << 13;
        s ^= s >> 7;
        s ^= s << 17;
        state.set(s);
        (s >> 11) as f64 / ((1u64 << 53) as f64)
    })
}

extern "C" fn weir_random_int(n: i64) -> i64 {
    let r = weir_random();
    ((r * (n as f64)) as i64).min(n - 1)
}

extern "C" fn weir_random_seed(seed: i64) {
    let s = if seed == 0 { 1u64 } else { seed as u64 };
    RNG_STATE.with(|state| state.set(s));
}

// ── String operations ────────────────────────────────────────────

extern "C" fn weir_string_length(ptr: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    c_str.to_bytes().len() as i64
}

extern "C" fn weir_substring(ptr: i64, start: i64, end: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let s = c_str.to_str().unwrap_or("");
    let start = start as usize;
    let end = (end as usize).min(s.len());
    let sub = if start > end || start > s.len() {
        ""
    } else {
        &s[start..end]
    };
    std::ffi::CString::new(sub).unwrap().into_raw() as i64
}

extern "C" fn weir_string_ref(ptr: i64, idx: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let bytes = c_str.to_bytes();
    let idx = idx as usize;
    if idx < bytes.len() {
        bytes[idx] as i64
    } else {
        0 // out of bounds
    }
}

extern "C" fn weir_string_contains(haystack: i64, needle: i64) -> i8 {
    let h = unsafe { std::ffi::CStr::from_ptr(haystack as *const _) }
        .to_str().unwrap_or("");
    let n = unsafe { std::ffi::CStr::from_ptr(needle as *const _) }
        .to_str().unwrap_or("");
    if h.contains(n) { 1 } else { 0 }
}

extern "C" fn weir_string_upcase(ptr: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let s = c_str.to_str().unwrap_or("");
    std::ffi::CString::new(s.to_uppercase()).unwrap().into_raw() as i64
}

extern "C" fn weir_string_downcase(ptr: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let s = c_str.to_str().unwrap_or("");
    std::ffi::CString::new(s.to_lowercase()).unwrap().into_raw() as i64
}

extern "C" fn weir_string_trim(ptr: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(ptr as *const std::ffi::c_char) };
    let s = c_str.to_str().unwrap_or("");
    std::ffi::CString::new(s.trim()).unwrap().into_raw() as i64
}

extern "C" fn weir_char_to_string(code: i64) -> i64 {
    let c = char::from_u32(code as u32).unwrap_or('\u{FFFD}');
    std::ffi::CString::new(c.to_string()).unwrap().into_raw() as i64
}

// ── File I/O ─────────────────────────────────────────────────────

extern "C" fn weir_read_file(path_ptr: i64) -> i64 {
    let c_str = unsafe { std::ffi::CStr::from_ptr(path_ptr as *const std::ffi::c_char) };
    let path = c_str.to_str().unwrap_or("");
    match std::fs::read_to_string(path) {
        Ok(contents) => std::ffi::CString::new(contents).unwrap().into_raw() as i64,
        Err(e) => {
            eprintln!("read-file failed: {}", e);
            std::ffi::CString::new("").unwrap().into_raw() as i64
        }
    }
}

extern "C" fn weir_write_file(path_ptr: i64, contents_ptr: i64) {
    let path = unsafe { std::ffi::CStr::from_ptr(path_ptr as *const std::ffi::c_char) }
        .to_str().unwrap_or("");
    let contents = unsafe { std::ffi::CStr::from_ptr(contents_ptr as *const std::ffi::c_char) }
        .to_str().unwrap_or("");
    if let Err(e) = std::fs::write(path, contents) {
        eprintln!("write-file failed: {}", e);
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
    // Math (libm)
    builder.symbol("weir_sin", weir_sin as *const u8);
    builder.symbol("weir_cos", weir_cos as *const u8);
    builder.symbol("weir_tan", weir_tan as *const u8);
    builder.symbol("weir_asin", weir_asin as *const u8);
    builder.symbol("weir_acos", weir_acos as *const u8);
    builder.symbol("weir_atan", weir_atan as *const u8);
    builder.symbol("weir_atan2", weir_atan2 as *const u8);
    builder.symbol("weir_exp", weir_exp as *const u8);
    builder.symbol("weir_log", weir_log as *const u8);
    builder.symbol("weir_pow", weir_pow as *const u8);
    builder.symbol("weir_round", weir_round as *const u8);
    // Random
    builder.symbol("weir_random", weir_random as *const u8);
    builder.symbol("weir_random_int", weir_random_int as *const u8);
    builder.symbol("weir_random_seed", weir_random_seed as *const u8);
    // String operations
    builder.symbol("weir_string_length", weir_string_length as *const u8);
    builder.symbol("weir_substring", weir_substring as *const u8);
    builder.symbol("weir_string_ref", weir_string_ref as *const u8);
    builder.symbol("weir_string_contains", weir_string_contains as *const u8);
    builder.symbol("weir_string_upcase", weir_string_upcase as *const u8);
    builder.symbol("weir_string_downcase", weir_string_downcase as *const u8);
    builder.symbol("weir_string_trim", weir_string_trim as *const u8);
    builder.symbol("weir_char_to_string", weir_char_to_string as *const u8);
    // File I/O
    builder.symbol("weir_read_file", weir_read_file as *const u8);
    builder.symbol("weir_write_file", weir_write_file as *const u8);
}
