use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::ir::{AbiParam, BlockArg, Function, InstBuilder, Type, Value};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::Path;
use std::sync::atomic::{AtomicPtr, Ordering};
use weir_ast::*;
use weir_typeck::{Ty, TyVarId, TypeCheckResult};

// ── Error ────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
}

impl CodegenError {
    fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

// ── Runtime print helpers (JIT in-process) ───────────────────────
//
// These are extern "C" functions that JIT-compiled code calls to print.
// They write to a thread-local String buffer for testability.

std::thread_local! {
    static OUTPUT_BUF: std::cell::RefCell<String> = const { std::cell::RefCell::new(String::new()) };
}

fn output_reset() {
    OUTPUT_BUF.with(|buf| buf.borrow_mut().clear());
}

fn output_take() -> String {
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

// Allocation and vector operations are provided by weir-runtime (GC-managed).
// The following re-exports make them available for JIT symbol registration.
use weir_runtime::{
    weir_arena_alloc, weir_arena_create, weir_arena_destroy, weir_arena_vec_alloc,
    weir_gc_alloc, weir_gc_collect, weir_gc_suppress, weir_gc_unsuppress, weir_gc_vec_alloc,
    weir_shadow_pop, weir_shadow_push, weir_vec_append, weir_vec_get, weir_vec_len, SHAPE_FIXED,
    SHAPE_VECTOR,
};

// ── C runtime for AOT binaries ──────────────────────────────────

const AOT_RUNTIME_C: &str = r#"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern void weir_main(void);

void weir_print_i64(int64_t val) { printf("%lld", (long long)val); }
void weir_print_f64(double val) {
    if (val == (double)(long long)val) printf("%.1f", val);
    else printf("%g", val);
}
void weir_print_bool(int8_t val) { printf("%s", val ? "true" : "false"); }
void weir_print_unit(void) { printf("()"); }
void weir_print_newline(void) { printf("\n"); }
void weir_print_str(int64_t ptr) { printf("%s", (const char*)ptr); }

int64_t weir_i64_to_str(int64_t val) {
    char buf[32]; snprintf(buf, sizeof(buf), "%lld", (long long)val);
    char *s = strdup(buf); return (int64_t)s;
}
int64_t weir_f64_to_str(double val) {
    char buf[64];
    if (val == (double)(long long)val) snprintf(buf, sizeof(buf), "%.1f", val);
    else snprintf(buf, sizeof(buf), "%g", val);
    char *s = strdup(buf); return (int64_t)s;
}
int64_t weir_bool_to_str(int8_t val) {
    char *s = strdup(val ? "true" : "false"); return (int64_t)s;
}
int64_t weir_str_concat(int64_t a, int64_t b) {
    const char *sa = (const char*)a, *sb = (const char*)b;
    size_t la = strlen(sa), lb = strlen(sb);
    char *out = malloc(la + lb + 1);
    memcpy(out, sa, la); memcpy(out + la, sb, lb + 1);
    return (int64_t)out;
}
int64_t weir_str_eq(int64_t a, int64_t b) {
    return strcmp((const char*)a, (const char*)b) == 0 ? 1 : 0;
}
int64_t weir_str_cmp(int64_t a, int64_t b) {
    int r = strcmp((const char*)a, (const char*)b);
    return r < 0 ? -1 : (r > 0 ? 1 : 0);
}

/* ── GC runtime ── */

#define SHAPE_FIXED  0
#define SHAPE_VECTOR 1

typedef struct ShapeDesc {
    uint32_t num_slots;
    uint32_t kind;
    uint64_t pointer_mask;
} ShapeDesc;

typedef struct ObjHeader {
    struct ObjHeader *next;
    uint64_t mark;
    const ShapeDesc *shape;
    uint64_t user_size;
} ObjHeader;

#define HEADER_SIZE sizeof(ObjHeader)
#define INITIAL_GC_THRESHOLD (256 * 1024)
#define GC_GROWTH_FACTOR 2

static ObjHeader *gc_all_objects = NULL;
static size_t gc_bytes_since = 0;
static size_t gc_total_bytes = 0;
static size_t gc_threshold = INITIAL_GC_THRESHOLD;
static int gc_suppress_depth = 0;

/* Shadow stack */
static int64_t **shadow_stack = NULL;
static size_t shadow_stack_len = 0;
static size_t shadow_stack_cap = 0;

void weir_shadow_push(int64_t *slot) {
    if (shadow_stack_len >= shadow_stack_cap) {
        shadow_stack_cap = shadow_stack_cap ? shadow_stack_cap * 2 : 256;
        shadow_stack = realloc(shadow_stack, shadow_stack_cap * sizeof(int64_t*));
    }
    shadow_stack[shadow_stack_len++] = slot;
}

void weir_shadow_pop(int64_t n) {
    if ((size_t)n > shadow_stack_len) shadow_stack_len = 0;
    else shadow_stack_len -= (size_t)n;
}

/* Forward declarations */
static void mark_object(int64_t user_ptr);

static void gc_collect(void) {
    /* Mark phase: walk shadow stack roots */
    for (size_t i = 0; i < shadow_stack_len; i++) {
        int64_t val = *shadow_stack[i];
        if (val != 0) mark_object(val);
    }
    /* Sweep phase */
    ObjHeader **prev = &gc_all_objects;
    ObjHeader *cur = gc_all_objects;
    while (cur) {
        ObjHeader *next = cur->next;
        if (cur->mark & 1) {
            cur->mark &= ~(uint64_t)1;
            prev = &cur->next;
            cur = next;
        } else {
            *prev = next;
            size_t total = HEADER_SIZE + (size_t)cur->user_size;
            if (gc_total_bytes >= total) gc_total_bytes -= total;
            free(cur);
            cur = next;
        }
    }
    gc_bytes_since = 0;
    if (gc_total_bytes > gc_threshold / 2)
        gc_threshold = gc_total_bytes * GC_GROWTH_FACTOR;
}

static void mark_object(int64_t user_ptr) {
    if (user_ptr == 0) return;
    ObjHeader *h = (ObjHeader*)((char*)(intptr_t)user_ptr - HEADER_SIZE);
    if (h->mark & 1) return;
    h->mark |= 1;
    const ShapeDesc *s = h->shape;
    if (!s || s->pointer_mask == 0) return;
    int64_t *data = (int64_t*)(intptr_t)user_ptr;
    if (s->kind == SHAPE_FIXED) {
        for (uint32_t i = 0; i < s->num_slots; i++) {
            if (s->pointer_mask & (1ULL << i)) {
                if (data[i] != 0) mark_object(data[i]);
            }
        }
    } else if (s->kind == SHAPE_VECTOR) {
        if (s->pointer_mask & 1) {
            int64_t len = data[0];
            for (int64_t i = 0; i < len; i++) {
                if (data[1 + i] != 0) mark_object(data[1 + i]);
            }
        }
    }
}

void weir_gc_collect(void) { gc_collect(); }

int64_t weir_gc_alloc(int64_t size_bytes, int64_t shape_ptr) {
    size_t user_size = (size_t)size_bytes;
    if (gc_suppress_depth == 0 && gc_bytes_since + HEADER_SIZE + user_size > gc_threshold)
        gc_collect();
    size_t total = HEADER_SIZE + user_size;
    ObjHeader *h = (ObjHeader*)calloc(1, total);
    h->next = gc_all_objects;
    h->mark = 0;
    h->shape = (const ShapeDesc*)(intptr_t)shape_ptr;
    h->user_size = (uint64_t)user_size;
    gc_all_objects = h;
    gc_bytes_since += total;
    gc_total_bytes += total;
    return (int64_t)(intptr_t)((char*)h + HEADER_SIZE);
}

int64_t weir_gc_vec_alloc(int64_t len, int64_t shape_ptr) {
    int64_t total = (1 + len) * 8;
    int64_t ptr = weir_gc_alloc(total, shape_ptr);
    *(int64_t*)(intptr_t)ptr = len;
    return ptr;
}

int64_t weir_vec_get(int64_t ptr, int64_t idx) {
    return ((int64_t*)(intptr_t)ptr)[1 + idx];
}
int64_t weir_vec_len(int64_t ptr) {
    return *(int64_t*)(intptr_t)ptr;
}
int64_t weir_vec_append(int64_t ptr, int64_t elem, int64_t shape_ptr) {
    int64_t old_len = weir_vec_len(ptr);
    int64_t new_len = old_len + 1;
    int64_t new_ptr = weir_gc_vec_alloc(new_len, shape_ptr);
    memcpy((int64_t*)(intptr_t)new_ptr + 1, (int64_t*)(intptr_t)ptr + 1, old_len * 8);
    ((int64_t*)(intptr_t)new_ptr)[1 + old_len] = elem;
    return new_ptr;
}

/* ── Arena allocator ── */

#define ARENA_INITIAL_CAPACITY (64 * 1024)

typedef struct ArenaChunk {
    void *base;
    size_t capacity;
    struct ArenaChunk *next;
} ArenaChunk;

typedef struct Arena {
    ArenaChunk *current_chunk;
    size_t offset;
} Arena;

static ArenaChunk *arena_new_chunk(size_t capacity) {
    ArenaChunk *c = (ArenaChunk*)malloc(sizeof(ArenaChunk));
    c->base = calloc(1, capacity);
    c->capacity = capacity;
    c->next = NULL;
    return c;
}

int64_t weir_arena_create(void) {
    ArenaChunk *chunk = arena_new_chunk(ARENA_INITIAL_CAPACITY);
    Arena *a = (Arena*)malloc(sizeof(Arena));
    a->current_chunk = chunk;
    a->offset = 0;
    return (int64_t)(intptr_t)a;
}

int64_t weir_arena_alloc(int64_t arena_ptr, int64_t size) {
    size_t aligned = ((size_t)size + 7) & ~(size_t)7;
    Arena *a = (Arena*)(intptr_t)arena_ptr;
    ArenaChunk *c = a->current_chunk;
    if (a->offset + aligned <= c->capacity) {
        void *ptr = (char*)c->base + a->offset;
        a->offset += aligned;
        return (int64_t)(intptr_t)ptr;
    }
    size_t new_cap = c->capacity * 2;
    if (new_cap < aligned) new_cap = aligned;
    ArenaChunk *nc = arena_new_chunk(new_cap);
    nc->next = a->current_chunk;
    a->current_chunk = nc;
    a->offset = aligned;
    return (int64_t)(intptr_t)nc->base;
}

int64_t weir_arena_vec_alloc(int64_t arena_ptr, int64_t len) {
    int64_t total = (1 + len) * 8;
    int64_t ptr = weir_arena_alloc(arena_ptr, total);
    *(int64_t*)(intptr_t)ptr = len;
    return ptr;
}

void weir_arena_destroy(int64_t arena_ptr) {
    Arena *a = (Arena*)(intptr_t)arena_ptr;
    ArenaChunk *c = a->current_chunk;
    while (c) {
        ArenaChunk *next = c->next;
        free(c->base);
        free(c);
        c = next;
    }
    free(a);
}

void weir_gc_suppress(void) { gc_suppress_depth++; }
void weir_gc_unsuppress(void) {
    if (gc_suppress_depth > 0) gc_suppress_depth--;
    if (gc_suppress_depth == 0 && gc_bytes_since > gc_threshold) gc_collect();
}

int main(void) { weir_main(); return 0; }
"#;

// ── Type mapping ─────────────────────────────────────────────────

fn ty_to_cl_with(ty: &Ty, tagged_adts: Option<&HashMap<SmolStr, TaggedAdt>>) -> Option<Type> {
    match ty {
        Ty::I8 | Ty::U8 | Ty::Bool => Some(types::I8),
        Ty::I16 | Ty::U16 => Some(types::I16),
        Ty::I32 | Ty::U32 => Some(types::I32),
        Ty::I64 | Ty::U64 => Some(types::I64),
        Ty::F32 => Some(types::F32),
        Ty::F64 => Some(types::F64),
        Ty::Str => Some(types::I64), // pointer to null-terminated C string
        Ty::Vector(_) => Some(types::I64), // pointer to heap [len, elem_0, ...]
        Ty::Fn(_, _) => Some(types::I64), // pointer to heap closure object
        Ty::Unit => None, // void — no value
        Ty::Con(name, _) => {
            if let Some(adts) = tagged_adts {
                if adts.contains_key(name) {
                    return Some(types::I64); // packed tag+payload representation
                }
            }
            None
        }
        _ => None,
    }
}

fn is_float(ty: &Ty) -> bool {
    matches!(ty, Ty::F32 | Ty::F64)
}

fn is_signed(ty: &Ty) -> bool {
    matches!(ty, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64)
}

// ── ISA construction (shared between JIT and AOT) ───────────────

fn make_isa(
    pic: bool,
) -> Result<std::sync::Arc<dyn cranelift_codegen::isa::TargetIsa>, CodegenError> {
    let mut flag_builder = settings::builder();
    flag_builder
        .set("opt_level", "speed")
        .map_err(|e| CodegenError::new(format!("setting opt_level: {}", e)))?;
    if pic {
        flag_builder
            .set("is_pic", "true")
            .map_err(|e| CodegenError::new(format!("setting is_pic: {}", e)))?;
    }

    let isa_builder =
        cranelift_native::builder().map_err(|e| CodegenError::new(format!("native ISA: {}", e)))?;

    isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| CodegenError::new(format!("ISA finish: {}", e)))
}

// ── Compiler (generic over Module) ──────────────────────────────

pub fn compile_and_run(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> Result<String, CodegenError> {
    let isa = make_isa(false)?;

    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

    // Register runtime print helpers as in-process function pointers
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

    let jit_module = JITModule::new(builder);

    let mut compiler = Compiler::new(module, type_info, jit_module);
    compiler.declare_runtime_helpers()?;
    compiler.declare_user_functions(Linkage::Local)?;
    compiler.compile_user_functions()?;
    compiler
        .module
        .finalize_definitions()
        .map_err(|e| CodegenError::new(format!("finalize: {}", e)))?;
    compiler.run_main()
}

pub fn compile_to_object(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> Result<Vec<u8>, CodegenError> {
    let isa = make_isa(true)?;

    let obj_builder = ObjectBuilder::new(
        isa,
        "weir_output",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| CodegenError::new(format!("ObjectBuilder: {}", e)))?;

    let obj_module = ObjectModule::new(obj_builder);

    let mut compiler = Compiler::new(module, type_info, obj_module);
    compiler.declare_runtime_helpers()?;
    // Export main as weir_main so C runtime can call it; others are local
    compiler.declare_user_functions_aot()?;
    compiler.compile_user_functions()?;

    let product = compiler.module.finish();
    product
        .emit()
        .map_err(|e| CodegenError::new(format!("emit object: {}", e)))
}

pub fn build_executable(
    module: &weir_ast::Module,
    type_info: &TypeCheckResult,
    output_path: &Path,
) -> Result<(), CodegenError> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);

    let obj_bytes = compile_to_object(module, type_info)?;

    let unique = format!(
        "{}_{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::Relaxed)
    );
    let tmp_dir = std::env::temp_dir();
    let obj_path = tmp_dir.join(format!("weir_output_{}.o", unique));
    let runtime_path = tmp_dir.join(format!("weir_runtime_{}.c", unique));

    std::fs::write(&obj_path, &obj_bytes)
        .map_err(|e| CodegenError::new(format!("write object file: {}", e)))?;
    std::fs::write(&runtime_path, AOT_RUNTIME_C)
        .map_err(|e| CodegenError::new(format!("write runtime: {}", e)))?;

    let status = std::process::Command::new("cc")
        .arg("-o")
        .arg(output_path)
        .arg(&obj_path)
        .arg(&runtime_path)
        .status()
        .map_err(|e| CodegenError::new(format!("run cc: {}", e)))?;

    // Clean up temp files (best-effort)
    let _ = std::fs::remove_file(&obj_path);
    let _ = std::fs::remove_file(&runtime_path);

    if !status.success() {
        return Err(CodegenError::new(format!(
            "linker failed with exit code: {}",
            status.code().unwrap_or(-1)
        )));
    }

    Ok(())
}

/// Info about a constructor variant for codegen.
#[derive(Clone, Debug)]
struct ConstructorInfo {
    #[allow(dead_code)]
    type_name: SmolStr,
    tag: i64,
    /// Whether this constructor carries a payload (1 field).
    has_payload: bool,
    /// If has_payload, the index of the type parameter that the field references.
    /// E.g., for `(Ok 'ok)` in `(deftype (Result 'ok 'err) ...)`, this is 0.
    /// None if the field type is a concrete type (not a type parameter).
    field_type_param_index: Option<usize>,
}

/// A tagged ADT type: all variants have 0 or 1 fields, represented as packed i64.
#[derive(Clone, Debug)]
struct TaggedAdt {
    /// Ordered list of variant names; tag = index in this list.
    #[allow(dead_code)]
    variants: Vec<SmolStr>,
    /// For each variant, the number of fields (0 or 1).
    #[allow(dead_code)]
    field_counts: Vec<usize>,
}

/// Info about a lambda that needs to be compiled after the enclosing function.
struct LambdaInfo {
    func_id: FuncId,
    name: SmolStr,
    param_tys: Vec<Ty>,
    ret_ty: Ty,
    params: Vec<Param>,
    body: Vec<ExprId>,
    captures: Vec<(SmolStr, Ty)>,
    /// Type variable substitution inherited from enclosing specialization context.
    type_subst: HashMap<TyVarId, Ty>,
}

/// Walk two type trees (generic and concrete) in parallel, collecting Var → concrete mappings.
fn collect_var_mappings(generic: &Ty, concrete: &Ty, subst: &mut HashMap<TyVarId, Ty>) {
    match (generic, concrete) {
        (Ty::Var(id), _) if !matches!(concrete, Ty::Var(_)) => {
            subst.insert(*id, concrete.clone());
        }
        (Ty::Fn(gp, gr), Ty::Fn(cp, cr)) => {
            for (g, c) in gp.iter().zip(cp.iter()) {
                collect_var_mappings(g, c, subst);
            }
            collect_var_mappings(gr, cr, subst);
        }
        (Ty::Vector(g), Ty::Vector(c)) => {
            collect_var_mappings(g, c, subst);
        }
        (Ty::Con(gn, ga), Ty::Con(cn, ca)) if gn == cn => {
            for (g, c) in ga.iter().zip(ca.iter()) {
                collect_var_mappings(g, c, subst);
            }
        }
        (Ty::Map(gk, gv), Ty::Map(ck, cv)) => {
            collect_var_mappings(gk, ck, subst);
            collect_var_mappings(gv, cv, subst);
        }
        _ => {}
    }
}

struct Compiler<'a, M: Module> {
    ast_module: &'a weir_ast::Module,
    type_info: &'a TypeCheckResult,
    module: M,

    /// Maps user function name -> (FuncId, param types, return type)
    user_fns: HashMap<SmolStr, (FuncId, Vec<Ty>, Ty)>,
    /// Maps runtime helper name -> FuncId
    runtime_fns: HashMap<&'static str, FuncId>,
    /// Tagged ADT types (including simple enums): type_name -> TaggedAdt
    tagged_adts: HashMap<SmolStr, TaggedAdt>,
    /// Constructor name -> ConstructorInfo
    constructor_tags: HashMap<SmolStr, ConstructorInfo>,
    /// Counter for generating unique lambda names
    lambda_counter: usize,
}

impl<'a, M: Module> Compiler<'a, M> {
    fn new(ast_module: &'a weir_ast::Module, type_info: &'a TypeCheckResult, module: M) -> Self {
        let mut tagged_adts = HashMap::new();
        let mut constructor_tags = HashMap::new();

        // Collect tagged ADTs from deftype definitions (all variants with 0 or 1 fields)
        for (item, _span) in &ast_module.items {
            if let Item::Deftype(d) = item {
                let all_supported =
                    !d.variants.is_empty() && d.variants.iter().all(|v| v.fields.len() <= 1);
                if all_supported {
                    let variants: Vec<SmolStr> =
                        d.variants.iter().map(|v| v.name.clone()).collect();
                    let field_counts: Vec<usize> =
                        d.variants.iter().map(|v| v.fields.len()).collect();
                    for (i, v) in d.variants.iter().enumerate() {
                        let field_type_param_index = if v.fields.len() == 1 {
                            // Look up the field's type expression to find which type param it references
                            let field_texpr = &ast_module.type_exprs[v.fields[0]];
                            if let weir_ast::TypeExprKind::TypeVar(tvar_name) = &field_texpr.kind {
                                d.type_params.iter().position(|p| p == tvar_name)
                            } else {
                                None // concrete type, not a type parameter
                            }
                        } else {
                            None
                        };
                        constructor_tags.insert(
                            v.name.clone(),
                            ConstructorInfo {
                                type_name: d.name.clone(),
                                tag: i as i64,
                                has_payload: !v.fields.is_empty(),
                                field_type_param_index,
                            },
                        );
                    }
                    tagged_adts.insert(
                        d.name.clone(),
                        TaggedAdt {
                            variants,
                            field_counts,
                        },
                    );
                }
            }
        }

        Self {
            ast_module,
            type_info,
            module,
            user_fns: HashMap::new(),
            runtime_fns: HashMap::new(),
            tagged_adts,
            constructor_tags,
            lambda_counter: 0,
        }
    }

    // ── Runtime helper declarations ─────────────────────────────

    fn declare_runtime_helpers(&mut self) -> Result<(), CodegenError> {
        // weir_print_i64(i64) -> void
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_i64", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_i64: {}", e)))?;
        self.runtime_fns.insert("print_i64", id);

        // weir_print_f64(f64) -> void
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_f64", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_f64: {}", e)))?;
        self.runtime_fns.insert("print_f64", id);

        // weir_print_bool(i8) -> void
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I8));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_bool", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_bool: {}", e)))?;
        self.runtime_fns.insert("print_bool", id);

        // weir_print_unit() -> void
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_unit", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_unit: {}", e)))?;
        self.runtime_fns.insert("print_unit", id);

        // weir_print_newline() -> void
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_newline", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_newline: {}", e)))?;
        self.runtime_fns.insert("print_newline", id);

        // weir_sleep_ms(i64) -> void
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_sleep_ms", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_sleep_ms: {}", e)))?;
        self.runtime_fns.insert("sleep_ms", id);

        // weir_print_str(i64) -> void
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_print_str", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_print_str: {}", e)))?;
        self.runtime_fns.insert("print_str", id);

        // weir_i64_to_str(i64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_i64_to_str", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_i64_to_str: {}", e)))?;
        self.runtime_fns.insert("i64_to_str", id);

        // weir_f64_to_str(f64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_f64_to_str", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_f64_to_str: {}", e)))?;
        self.runtime_fns.insert("f64_to_str", id);

        // weir_bool_to_str(i8) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I8));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_bool_to_str", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_bool_to_str: {}", e)))?;
        self.runtime_fns.insert("bool_to_str", id);

        // weir_str_concat(i64, i64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_str_concat", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_str_concat: {}", e)))?;
        self.runtime_fns.insert("str_concat", id);

        // weir_str_eq(i64, i64) -> i8
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I8));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_str_eq", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_str_eq: {}", e)))?;
        self.runtime_fns.insert("str_eq", id);

        // weir_str_cmp(i64, i64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_str_cmp", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_str_cmp: {}", e)))?;
        self.runtime_fns.insert("str_cmp", id);

        // weir_gc_alloc(i64, i64) -> i64  [size_bytes, shape_ptr]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_gc_alloc", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_gc_alloc: {}", e)))?;
        self.runtime_fns.insert("gc_alloc", id);

        // weir_gc_vec_alloc(i64, i64) -> i64  [len, shape_ptr]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_gc_vec_alloc", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_gc_vec_alloc: {}", e)))?;
        self.runtime_fns.insert("gc_vec_alloc", id);

        // weir_gc_collect() -> void
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_gc_collect", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_gc_collect: {}", e)))?;
        self.runtime_fns.insert("gc_collect", id);

        // weir_vec_get(i64, i64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_vec_get", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_vec_get: {}", e)))?;
        self.runtime_fns.insert("vec_get", id);

        // weir_vec_len(i64) -> i64
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_vec_len", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_vec_len: {}", e)))?;
        self.runtime_fns.insert("vec_len", id);

        // weir_vec_append(i64, i64, i64) -> i64  [vec_ptr, elem, shape_ptr]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_vec_append", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_vec_append: {}", e)))?;
        self.runtime_fns.insert("vec_append", id);

        // weir_shadow_push(i64) -> void  [slot_ptr]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_shadow_push", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_shadow_push: {}", e)))?;
        self.runtime_fns.insert("shadow_push", id);

        // weir_shadow_pop(i64) -> void  [count]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_shadow_pop", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_shadow_pop: {}", e)))?;
        self.runtime_fns.insert("shadow_pop", id);

        // weir_arena_create() -> i64
        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_arena_create", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_arena_create: {}", e)))?;
        self.runtime_fns.insert("arena_create", id);

        // weir_arena_alloc(i64, i64) -> i64  [arena, size]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_arena_alloc", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_arena_alloc: {}", e)))?;
        self.runtime_fns.insert("arena_alloc", id);

        // weir_arena_vec_alloc(i64, i64) -> i64  [arena, len]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_arena_vec_alloc", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_arena_vec_alloc: {}", e)))?;
        self.runtime_fns.insert("arena_vec_alloc", id);

        // weir_arena_destroy(i64) -> void  [arena]
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_arena_destroy", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_arena_destroy: {}", e)))?;
        self.runtime_fns.insert("arena_destroy", id);

        // weir_gc_suppress() -> void
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_gc_suppress", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_gc_suppress: {}", e)))?;
        self.runtime_fns.insert("gc_suppress", id);

        // weir_gc_unsuppress() -> void
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        let id = self
            .module
            .declare_function("weir_gc_unsuppress", Linkage::Import, &sig)
            .map_err(|e| CodegenError::new(format!("declare weir_gc_unsuppress: {}", e)))?;
        self.runtime_fns.insert("gc_unsuppress", id);

        Ok(())
    }

    // ── Pass 1: declare all user function signatures ────────────

    fn ty_cl(&self, ty: &Ty) -> Option<Type> {
        ty_to_cl_with(ty, Some(&self.tagged_adts))
    }

    fn declare_user_functions(&mut self, linkage: Linkage) -> Result<(), CodegenError> {
        for (item, _span) in &self.ast_module.items {
            if let Item::Defn(defn) = item {
                // Skip generic functions — they can't be compiled directly,
                // only their monomorphized specializations are compiled.
                let (param_tys, ret_ty) = match self.resolve_fn_signature(defn) {
                    Ok(sig) => sig,
                    Err(_) => continue,
                };

                let mut sig = self.module.make_signature();
                sig.call_conv = CallConv::SystemV;
                for pty in &param_tys {
                    if let Some(cl_ty) = self.ty_cl(pty) {
                        sig.params.push(AbiParam::new(cl_ty));
                    }
                }
                if let Some(cl_ty) = self.ty_cl(&ret_ty) {
                    sig.returns.push(AbiParam::new(cl_ty));
                }

                let func_id = self
                    .module
                    .declare_function(&defn.name, linkage, &sig)
                    .map_err(|e| CodegenError::new(format!("declare fn '{}': {}", defn.name, e)))?;

                self.user_fns
                    .insert(defn.name.clone(), (func_id, param_tys, ret_ty));
            }
        }

        // Declare specializations (instance methods + generic function instantiations)
        self.declare_specializations(linkage)?;

        Ok(())
    }

    /// Declare instance method specializations from type_info.specializations.
    fn declare_specializations(&mut self, linkage: Linkage) -> Result<(), CodegenError> {
        for spec in &self.type_info.specializations {
            // Only handle specializations whose types are all primitives (codegen-able)
            let all_primitive = spec
                .param_types
                .iter()
                .all(|t| self.ty_cl(t).is_some() || *t == Ty::Unit)
                && (self.ty_cl(&spec.return_type).is_some() || spec.return_type == Ty::Unit);

            if !all_primitive {
                continue;
            }

            let mut sig = self.module.make_signature();
            sig.call_conv = CallConv::SystemV;
            for pty in &spec.param_types {
                if let Some(cl_ty) = self.ty_cl(pty) {
                    sig.params.push(AbiParam::new(cl_ty));
                }
            }
            if let Some(cl_ty) = self.ty_cl(&spec.return_type) {
                sig.returns.push(AbiParam::new(cl_ty));
            }

            let func_id = self
                .module
                .declare_function(&spec.mangled_name, linkage, &sig)
                .map_err(|e| {
                    CodegenError::new(format!(
                        "declare specialization '{}': {}",
                        spec.mangled_name, e
                    ))
                })?;

            self.user_fns.insert(
                spec.mangled_name.clone(),
                (func_id, spec.param_types.clone(), spec.return_type.clone()),
            );
        }
        Ok(())
    }

    /// AOT variant: export `main` as `weir_main`, keep everything else local.
    fn declare_user_functions_aot(&mut self) -> Result<(), CodegenError> {
        for (item, _span) in &self.ast_module.items {
            if let Item::Defn(defn) = item {
                // Skip generic functions — only specializations are compiled.
                let (param_tys, ret_ty) = match self.resolve_fn_signature(defn) {
                    Ok(sig) => sig,
                    Err(_) => continue,
                };

                let mut sig = self.module.make_signature();
                sig.call_conv = CallConv::SystemV;
                for pty in &param_tys {
                    if let Some(cl_ty) = self.ty_cl(pty) {
                        sig.params.push(AbiParam::new(cl_ty));
                    }
                }
                if let Some(cl_ty) = self.ty_cl(&ret_ty) {
                    sig.returns.push(AbiParam::new(cl_ty));
                }

                let (export_name, linkage) = if defn.name == "main" {
                    ("weir_main", Linkage::Export)
                } else {
                    (defn.name.as_str(), Linkage::Local)
                };

                let func_id = self
                    .module
                    .declare_function(export_name, linkage, &sig)
                    .map_err(|e| CodegenError::new(format!("declare fn '{}': {}", defn.name, e)))?;

                self.user_fns
                    .insert(defn.name.clone(), (func_id, param_tys, ret_ty));
            }
        }

        // Declare specializations for AOT too
        self.declare_specializations(Linkage::Local)?;

        Ok(())
    }

    /// Resolve function parameter types and return type from the type checker results.
    fn resolve_fn_signature(&self, defn: &Defn) -> Result<(Vec<Ty>, Ty), CodegenError> {
        let mut param_tys = Vec::new();
        for param in &defn.params {
            if let Some(type_ann_id) = param.type_ann {
                let ty = self.resolve_type_expr_to_ty(type_ann_id)?;
                param_tys.push(ty);
            } else {
                return Err(CodegenError::new(format!(
                    "function '{}' parameter '{}' requires a type annotation for codegen",
                    defn.name, param.name
                )));
            }
        }

        let ret_ty = if let Some(ret_id) = defn.return_type {
            self.resolve_type_expr_to_ty(ret_id)?
        } else if !defn.body.is_empty() {
            let last_expr_id = defn.body[defn.body.len() - 1];
            if let Some(ty) = self.type_info.expr_types.get(last_expr_id) {
                ty.clone()
            } else {
                Ty::Unit
            }
        } else {
            Ty::Unit
        };

        Ok((param_tys, ret_ty))
    }

    fn resolve_type_expr_to_ty(&self, id: TypeExprId) -> Result<Ty, CodegenError> {
        let texpr = &self.ast_module.type_exprs[id];
        match &texpr.kind {
            TypeExprKind::Named(name) => match name.as_str() {
                "i8" => Ok(Ty::I8),
                "i16" => Ok(Ty::I16),
                "i32" => Ok(Ty::I32),
                "i64" => Ok(Ty::I64),
                "u8" => Ok(Ty::U8),
                "u16" => Ok(Ty::U16),
                "u32" => Ok(Ty::U32),
                "u64" => Ok(Ty::U64),
                "f32" => Ok(Ty::F32),
                "f64" => Ok(Ty::F64),
                "Bool" | "bool" => Ok(Ty::Bool),
                "String" => Ok(Ty::Str),
                "Unit" => Ok(Ty::Unit),
                other => {
                    // Check if it's a tagged ADT type
                    let name_smol = SmolStr::new(other);
                    if self.tagged_adts.contains_key(&name_smol) {
                        Ok(Ty::Con(name_smol, vec![]))
                    } else {
                        Err(CodegenError::new(format!(
                            "unsupported type '{}' for codegen",
                            other
                        )))
                    }
                }
            },
            TypeExprKind::Applied { constructor, args } => {
                let con_expr = &self.ast_module.type_exprs[*constructor];
                match &con_expr.kind {
                    TypeExprKind::Named(name) => {
                        if name == "Vector" && args.len() == 1 {
                            let elem_ty = self.resolve_type_expr_to_ty(args[0])?;
                            return Ok(Ty::Vector(Box::new(elem_ty)));
                        }
                        let name_smol = SmolStr::new(name.as_str());
                        if self.tagged_adts.contains_key(&name_smol) {
                            // Resolve args (for type checking), but the runtime rep is just i64
                            let mut arg_tys = Vec::new();
                            for &arg_id in args {
                                arg_tys.push(self.resolve_type_expr_to_ty(arg_id)?);
                            }
                            Ok(Ty::Con(name_smol, arg_tys))
                        } else {
                            Err(CodegenError::new(format!(
                                "unsupported parameterized type '{}' for codegen",
                                name
                            )))
                        }
                    }
                    _ => Err(CodegenError::new(
                        "complex type expressions not yet supported in codegen",
                    )),
                }
            }
            TypeExprKind::Fn {
                params,
                return_type,
            } => {
                let mut param_tys = Vec::new();
                for &p in params {
                    param_tys.push(self.resolve_type_expr_to_ty(p)?);
                }
                let ret_ty = self.resolve_type_expr_to_ty(*return_type)?;
                Ok(Ty::Fn(param_tys, Box::new(ret_ty)))
            }
            _ => Err(CodegenError::new(
                "complex type expressions not yet supported in codegen",
            )),
        }
    }

    // ── Pass 2: compile each function body ──────────────────────

    fn compile_user_functions(&mut self) -> Result<(), CodegenError> {
        self.compile_user_functions_with(None, None)
    }

    fn compile_user_functions_with(
        &mut self,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
    ) -> Result<(), CodegenError> {
        let defns: Vec<Defn> = self
            .ast_module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Defn(d) = item {
                    Some(d.clone())
                } else {
                    None
                }
            })
            .collect();

        for defn in &defns {
            // Skip generic functions not in user_fns (they were skipped during declaration)
            if !self.user_fns.contains_key(&defn.name) {
                continue;
            }
            self.compile_function_with(defn, fn_table_slots, table_data_id)?;
        }

        // Compile instance method specializations
        self.compile_specializations(fn_table_slots, table_data_id)?;

        Ok(())
    }

    /// Like `compile_user_functions_with`, but when `dirty_set` is `Some`, only compile
    /// functions whose name is in the dirty set. All functions must still be declared.
    fn compile_user_functions_selective(
        &mut self,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
        dirty_set: Option<&HashSet<SmolStr>>,
    ) -> Result<(), CodegenError> {
        let defns: Vec<Defn> = self
            .ast_module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Defn(d) = item {
                    Some(d.clone())
                } else {
                    None
                }
            })
            .collect();

        for defn in &defns {
            if !self.user_fns.contains_key(&defn.name) {
                continue;
            }
            // Skip functions not in the dirty set (if provided)
            if let Some(ds) = dirty_set {
                if !ds.contains(&defn.name) {
                    continue;
                }
            }
            self.compile_function_with(defn, fn_table_slots, table_data_id)?;
        }

        // Compile specializations, also filtered by dirty set
        self.compile_specializations_selective(fn_table_slots, table_data_id, dirty_set)?;

        Ok(())
    }

    /// Compile specialization bodies, filtered by dirty set.
    fn compile_specializations_selective(
        &mut self,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
        dirty_set: Option<&HashSet<SmolStr>>,
    ) -> Result<(), CodegenError> {
        let items = &self.ast_module.items;
        let mut to_compile: Vec<(SmolStr, Defn)> = Vec::new();

        for spec in &self.type_info.specializations {
            if !self.user_fns.contains_key(&spec.mangled_name) {
                continue;
            }
            // Skip specializations not in the dirty set
            if let Some(ds) = dirty_set {
                if !ds.contains(&spec.mangled_name) {
                    continue;
                }
            }

            if let Some(item_idx) = spec.instance_item_index {
                if let Some((Item::Instance(inst), _)) = items.get(item_idx) {
                    for method_defn in &inst.methods {
                        if method_defn.name == spec.original_name {
                            to_compile.push((spec.mangled_name.clone(), method_defn.clone()));
                        }
                    }
                }
            } else {
                for (item, _) in items {
                    if let Item::Defn(defn) = item {
                        if defn.name == spec.original_name {
                            to_compile.push((spec.mangled_name.clone(), defn.clone()));
                        }
                    }
                }
            }
        }

        for (mangled_name, defn) in &to_compile {
            self.compile_function_named(mangled_name, defn, fn_table_slots, table_data_id)?;
        }

        Ok(())
    }

    /// Compile specialization bodies (instance methods + generic function instantiations).
    fn compile_specializations(
        &mut self,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
    ) -> Result<(), CodegenError> {
        let items = &self.ast_module.items;
        let mut to_compile: Vec<(SmolStr, Defn)> = Vec::new();

        for spec in &self.type_info.specializations {
            if !self.user_fns.contains_key(&spec.mangled_name) {
                continue;
            }

            if let Some(item_idx) = spec.instance_item_index {
                // Instance method: use the exact AST instance identified by the type checker
                if let Some((Item::Instance(inst), _)) = items.get(item_idx) {
                    for method_defn in &inst.methods {
                        if method_defn.name == spec.original_name {
                            to_compile.push((spec.mangled_name.clone(), method_defn.clone()));
                        }
                    }
                }
            } else {
                // Generic user function: find the top-level defn
                for (item, _) in items {
                    if let Item::Defn(defn) = item {
                        if defn.name == spec.original_name {
                            to_compile.push((spec.mangled_name.clone(), defn.clone()));
                        }
                    }
                }
            }
        }

        for (mangled_name, defn) in &to_compile {
            self.compile_function_named(mangled_name, defn, fn_table_slots, table_data_id)?;
        }

        Ok(())
    }

    fn compile_function_with(
        &mut self,
        defn: &Defn,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
    ) -> Result<(), CodegenError> {
        self.compile_function_named(&defn.name.clone(), defn, fn_table_slots, table_data_id)
    }

    /// Compile a function body, looking up the declaration under `lookup_name`.
    fn compile_function_named(
        &mut self,
        lookup_name: &SmolStr,
        defn: &Defn,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
    ) -> Result<(), CodegenError> {
        let (func_id, param_tys, ret_ty) = self
            .user_fns
            .get(lookup_name)
            .ok_or_else(|| CodegenError::new(format!("unknown function '{}'", lookup_name)))?
            .clone();

        // Build type substitution for specializations: map generic type vars to concrete types.
        let type_subst = if lookup_name != &defn.name {
            // This is a specialization — build mapping from generic fn_type to concrete types
            if let Some(fn_type) = self.type_info.fn_types.get(&defn.name) {
                let mut subst = HashMap::new();
                for (generic, concrete) in fn_type.param_types.iter().zip(param_tys.iter()) {
                    collect_var_mappings(generic, concrete, &mut subst);
                }
                collect_var_mappings(&fn_type.return_type, &ret_ty, &mut subst);
                subst
            } else {
                HashMap::new()
            }
        } else {
            HashMap::new()
        };

        let adts = &self.tagged_adts;
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        for pty in &param_tys {
            if let Some(cl_ty) = ty_to_cl_with(pty, Some(adts)) {
                sig.params.push(AbiParam::new(cl_ty));
            }
        }
        if let Some(cl_ty) = ty_to_cl_with(&ret_ty, Some(adts)) {
            sig.returns.push(AbiParam::new(cl_ty));
        }

        let mut func =
            Function::with_name_signature(cranelift_codegen::ir::UserFuncName::default(), sig);

        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let pending_lambdas = {
            let mut fn_ctx = FnCompileCtx {
                builder: &mut builder,
                ast_module: self.ast_module,
                type_info: self.type_info,
                user_fns: &self.user_fns,
                runtime_fns: &self.runtime_fns,
                module: &mut self.module,
                vars: HashMap::new(),
                fn_table_slots,
                table_data_id,
                tagged_adts: &self.tagged_adts,
                constructor_tags: &self.constructor_tags,
                pending_lambdas: Vec::new(),
                lambda_counter: &mut self.lambda_counter,
                type_subst: type_subst.clone(),
                shadow_depth: 0,
                arena_stack: Vec::new(),
            };

            // Bind parameters to variables
            let block_params: Vec<Value> = fn_ctx.builder.block_params(entry_block).to_vec();
            let mut param_idx = 0;
            for (i, param) in defn.params.iter().enumerate() {
                if let Some(cl_ty) = fn_ctx.ty_cl(&param_tys[i]) {
                    let var = fn_ctx.declare_variable(cl_ty);
                    fn_ctx.builder.def_var(var, block_params[param_idx]);
                    fn_ctx
                        .vars
                        .insert(param.name.clone(), (var, param_tys[i].clone()));
                    // Push heap-pointer-typed parameters onto the GC shadow stack
                    if fn_ctx.is_heap_pointer(&param_tys[i]) {
                        fn_ctx.push_gc_root(block_params[param_idx]);
                    }
                    param_idx += 1;
                }
            }

            // Compile body
            let body_val = fn_ctx.compile_body(&defn.body, &ret_ty)?;

            // Pop shadow stack entries before returning
            let depth = fn_ctx.shadow_depth;
            fn_ctx.emit_shadow_pop(depth);

            // Return
            if fn_ctx.ty_cl(&ret_ty).is_some() {
                if let Some(val) = body_val {
                    fn_ctx.builder.ins().return_(&[val]);
                } else {
                    let cl_ty = fn_ctx.ty_cl(&ret_ty).unwrap();
                    let zero = if cl_ty.is_float() {
                        fn_ctx.builder.ins().f64const(0.0)
                    } else {
                        fn_ctx.builder.ins().iconst(cl_ty, 0)
                    };
                    fn_ctx.builder.ins().return_(&[zero]);
                }
            } else {
                fn_ctx.builder.ins().return_(&[]);
            }

            // Extract pending lambdas before fn_ctx is dropped
            std::mem::take(&mut fn_ctx.pending_lambdas)
        };

        builder.finalize();

        let mut ctx = Context::for_function(func);
        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| CodegenError::new(format!("define fn '{}': {}", defn.name, e)))?;

        // Compile any pending lambdas (and their nested lambdas)
        let mut lambdas = pending_lambdas;
        while !lambdas.is_empty() {
            let batch = std::mem::take(&mut lambdas);
            for info in batch {
                let nested = self.compile_lambda(&info, fn_table_slots, table_data_id)?;
                lambdas.extend(nested);
            }
        }

        Ok(())
    }

    /// Compile a lambda body (like compile_function_named but for closures).
    fn compile_lambda(
        &mut self,
        info: &LambdaInfo,
        fn_table_slots: Option<&HashMap<SmolStr, usize>>,
        table_data_id: Option<DataId>,
    ) -> Result<Vec<LambdaInfo>, CodegenError> {
        let adts = &self.tagged_adts;

        // Build signature: (env_ptr: i64, param_0, param_1, ...) -> ret
        let mut sig = self.module.make_signature();
        sig.call_conv = CallConv::SystemV;
        sig.params.push(AbiParam::new(types::I64)); // env_ptr
        for pty in &info.param_tys {
            if let Some(cl_ty) = ty_to_cl_with(pty, Some(adts)) {
                sig.params.push(AbiParam::new(cl_ty));
            }
        }
        if let Some(cl_ty) = ty_to_cl_with(&info.ret_ty, Some(adts)) {
            sig.returns.push(AbiParam::new(cl_ty));
        }

        let mut func =
            Function::with_name_signature(cranelift_codegen::ir::UserFuncName::default(), sig);

        let mut fb_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut func, &mut fb_ctx);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let pending_lambdas = {
            let mut fn_ctx = FnCompileCtx {
                builder: &mut builder,
                ast_module: self.ast_module,
                type_info: self.type_info,
                user_fns: &self.user_fns,
                runtime_fns: &self.runtime_fns,
                module: &mut self.module,
                vars: HashMap::new(),
                fn_table_slots,
                table_data_id,
                tagged_adts: &self.tagged_adts,
                constructor_tags: &self.constructor_tags,
                pending_lambdas: Vec::new(),
                lambda_counter: &mut self.lambda_counter,
                type_subst: info.type_subst.clone(),
                shadow_depth: 0,
                arena_stack: Vec::new(),
            };

            let block_params: Vec<Value> = fn_ctx.builder.block_params(entry_block).to_vec();

            // block_params[0] is env_ptr — it's a heap pointer, push as GC root
            let env_ptr = block_params[0];
            fn_ctx.push_gc_root(env_ptr);

            // Load captures from env_ptr at known offsets
            for (i, (cap_name, cap_ty)) in info.captures.iter().enumerate() {
                let offset = ((2 + i) * 8) as i32;
                let raw = fn_ctx.builder.ins().load(types::I64, MemFlags::trusted(), env_ptr, offset);
                let narrowed = fn_ctx.narrow_from_i64(raw, cap_ty);
                let cl_ty = fn_ctx.ty_cl(cap_ty).unwrap_or(types::I64);
                let var = fn_ctx.declare_variable(cl_ty);
                fn_ctx.builder.def_var(var, narrowed);
                fn_ctx.vars.insert(cap_name.clone(), (var, cap_ty.clone()));
            }

            // Bind user parameters (offset by 1 for env_ptr)
            let mut param_idx = 1;
            for (i, param) in info.params.iter().enumerate() {
                if let Some(cl_ty) = fn_ctx.ty_cl(&info.param_tys[i]) {
                    let var = fn_ctx.declare_variable(cl_ty);
                    fn_ctx.builder.def_var(var, block_params[param_idx]);
                    fn_ctx.vars.insert(param.name.clone(), (var, info.param_tys[i].clone()));
                    // Push heap-pointer-typed lambda parameters onto the GC shadow stack
                    if fn_ctx.is_heap_pointer(&info.param_tys[i]) {
                        fn_ctx.push_gc_root(block_params[param_idx]);
                    }
                    param_idx += 1;
                }
            }

            // Compile body
            let body_val = fn_ctx.compile_body(&info.body, &info.ret_ty)?;

            // Pop shadow stack entries before returning
            let depth = fn_ctx.shadow_depth;
            fn_ctx.emit_shadow_pop(depth);

            // Return
            if fn_ctx.ty_cl(&info.ret_ty).is_some() {
                if let Some(val) = body_val {
                    fn_ctx.builder.ins().return_(&[val]);
                } else {
                    let cl_ty = fn_ctx.ty_cl(&info.ret_ty).unwrap();
                    let zero = if cl_ty.is_float() {
                        fn_ctx.builder.ins().f64const(0.0)
                    } else {
                        fn_ctx.builder.ins().iconst(cl_ty, 0)
                    };
                    fn_ctx.builder.ins().return_(&[zero]);
                }
            } else {
                fn_ctx.builder.ins().return_(&[]);
            }

            std::mem::take(&mut fn_ctx.pending_lambdas)
        };

        builder.finalize();

        let mut ctx = Context::for_function(func);
        self.module
            .define_function(info.func_id, &mut ctx)
            .map_err(|e| CodegenError::new(format!("define lambda '{}': {}", info.name, e)))?;

        Ok(pending_lambdas)
    }
}

// ── JIT-specific methods ────────────────────────────────────────

impl Compiler<'_, JITModule> {
    fn run_main(&self) -> Result<String, CodegenError> {
        let (func_id, _, _) = self
            .user_fns
            .get("main")
            .ok_or_else(|| CodegenError::new("no 'main' function defined"))?;

        let code_ptr = self.module.get_finalized_function(*func_id);

        output_reset();

        unsafe {
            let main_fn: fn() = std::mem::transmute(code_ptr);
            main_fn();
        }

        Ok(output_take())
    }
}

// ── Capture analysis ─────────────────────────────────────────────

/// Walk a lambda body's AST and find all variable references that need to be captured.
/// Returns deduplicated list of (name, Variable, Ty) for captured vars.
fn find_captures(
    ast_module: &weir_ast::Module,
    lambda_params: &[Param],
    body: &[ExprId],
    enclosing_vars: &HashMap<SmolStr, (Variable, Ty)>,
) -> Vec<(SmolStr, Variable, Ty)> {
    let param_names: HashSet<&str> = lambda_params.iter().map(|p| p.name.as_str()).collect();
    let mut captured: Vec<(SmolStr, Variable, Ty)> = Vec::new();
    let mut seen: HashSet<SmolStr> = HashSet::new();

    fn walk_expr(
        expr_id: ExprId,
        ast_module: &weir_ast::Module,
        param_names: &HashSet<&str>,
        local_names: &HashSet<SmolStr>,
        enclosing_vars: &HashMap<SmolStr, (Variable, Ty)>,
        captured: &mut Vec<(SmolStr, Variable, Ty)>,
        seen: &mut HashSet<SmolStr>,
    ) {
        let expr = &ast_module.exprs[expr_id];
        match &expr.kind {
            ExprKind::Var(name) => {
                if !param_names.contains(name.as_str())
                    && !local_names.contains(name)
                    && !seen.contains(name)
                {
                    if let Some((var, ty)) = enclosing_vars.get(name) {
                        seen.insert(name.clone());
                        captured.push((name.clone(), *var, ty.clone()));
                    }
                }
            }
            ExprKind::Call { func, args } => {
                walk_expr(*func, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                for arg in args {
                    walk_expr(arg.value, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::Let { bindings, body } => {
                let mut inner_locals = local_names.clone();
                for b in bindings {
                    walk_expr(b.value, ast_module, param_names, &inner_locals, enclosing_vars, captured, seen);
                    inner_locals.insert(b.name.clone());
                }
                for &e in body {
                    walk_expr(e, ast_module, param_names, &inner_locals, enclosing_vars, captured, seen);
                }
            }
            ExprKind::If { condition, then_branch, else_branch } => {
                walk_expr(*condition, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                walk_expr(*then_branch, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                if let Some(e) = else_branch {
                    walk_expr(*e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::Do { body } => {
                for &e in body {
                    walk_expr(e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::Lambda { params: inner_params, body, .. } => {
                // Nested lambda: its params shadow, but it can also capture from enclosing
                let mut inner_param_names = param_names.clone();
                for p in inner_params {
                    inner_param_names.insert(p.name.as_str());
                }
                for &e in body {
                    walk_expr(e, ast_module, &inner_param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::Lit(_) => {}
            ExprKind::Cond { clauses, else_clause } => {
                for (test, body) in clauses {
                    walk_expr(*test, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                    walk_expr(*body, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
                if let Some(e) = else_clause {
                    walk_expr(*e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::When { condition, body } | ExprKind::Unless { condition, body } => {
                walk_expr(*condition, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                for &e in body {
                    walk_expr(e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::SetBang { place, value } => {
                walk_expr(*place, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                walk_expr(*value, ast_module, param_names, local_names, enclosing_vars, captured, seen);
            }
            ExprKind::Match { scrutinee, arms } => {
                walk_expr(*scrutinee, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                for arm in arms {
                    for &e in &arm.body {
                        walk_expr(e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                    }
                }
            }
            ExprKind::Ann { expr, .. } => {
                walk_expr(*expr, ast_module, param_names, local_names, enclosing_vars, captured, seen);
            }
            ExprKind::Try(inner) => {
                walk_expr(*inner, ast_module, param_names, local_names, enclosing_vars, captured, seen);
            }
            ExprKind::VectorLit(elems) => {
                for &e in elems {
                    walk_expr(e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            ExprKind::WithArena { body, .. } => {
                for &e in body {
                    walk_expr(e, ast_module, param_names, local_names, enclosing_vars, captured, seen);
                }
            }
            _ => {}
        }
    }

    let local_names = HashSet::new();
    for &expr_id in body {
        walk_expr(expr_id, ast_module, &param_names, &local_names, enclosing_vars, &mut captured, &mut seen);
    }
    captured
}

// ── Per-function compilation context ─────────────────────────────

struct FnCompileCtx<'a, 'b, M: Module> {
    builder: &'a mut FunctionBuilder<'b>,
    ast_module: &'a weir_ast::Module,
    type_info: &'a TypeCheckResult,
    user_fns: &'a HashMap<SmolStr, (FuncId, Vec<Ty>, Ty)>,
    runtime_fns: &'a HashMap<&'static str, FuncId>,
    module: &'a mut M,
    vars: HashMap<SmolStr, (Variable, Ty)>,
    /// If Some, use indirect dispatch via function table. Maps fn name → slot index.
    fn_table_slots: Option<&'a HashMap<SmolStr, usize>>,
    /// Data ID for the function table base pointer (used with indirect dispatch).
    table_data_id: Option<DataId>,
    /// Tagged ADT types (from Compiler)
    tagged_adts: &'a HashMap<SmolStr, TaggedAdt>,
    /// Constructor name -> ConstructorInfo
    constructor_tags: &'a HashMap<SmolStr, ConstructorInfo>,
    /// Lambdas collected during expression compilation, compiled after the enclosing function.
    pending_lambdas: Vec<LambdaInfo>,
    /// Counter for generating unique lambda names (shared with Compiler).
    lambda_counter: &'a mut usize,
    /// Type variable substitution for specialization contexts.
    /// Maps generic type var IDs to concrete types.
    type_subst: HashMap<TyVarId, Ty>,
    /// Number of entries currently pushed onto the GC shadow stack by this function.
    shadow_depth: usize,
    /// Stack of active arena variables (outermost to innermost).
    /// Used for cleanup on early return (? operator) and allocation redirection.
    arena_stack: Vec<Variable>,
}

impl<M: Module> FnCompileCtx<'_, '_, M> {
    fn ty_cl(&self, ty: &Ty) -> Option<Type> {
        ty_to_cl_with(ty, Some(self.tagged_adts))
    }

    /// Returns true if a value of this type is a GC heap pointer.
    fn is_heap_pointer(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Str | Ty::Vector(_) | Ty::Fn(_, _) => true,
            Ty::Con(name, _) => {
                // ADTs with any variant that has a payload field are heap-relevant
                // (the payload might be a heap pointer when packed)
                if let Some(adt) = self.tagged_adts.get(name) {
                    adt.field_counts.iter().any(|&c| c > 0)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Emit a static ShapeDesc in the Cranelift data section and return a Value
    /// holding the pointer to it.
    fn emit_shape_desc(
        &mut self,
        num_slots: u32,
        kind: u32,
        pointer_mask: u64,
    ) -> Value {
        // ShapeDesc layout: [num_slots: u32, kind: u32, pointer_mask: u64] = 16 bytes
        let mut bytes = Vec::with_capacity(16);
        bytes.extend_from_slice(&num_slots.to_le_bytes());
        bytes.extend_from_slice(&kind.to_le_bytes());
        bytes.extend_from_slice(&pointer_mask.to_le_bytes());

        let mut data_desc = DataDescription::new();
        data_desc.define(bytes.into_boxed_slice());
        let data_id = self
            .module
            .declare_anonymous_data(false, false)
            .expect("declare shape data");
        self.module
            .define_data(data_id, &data_desc)
            .expect("define shape data");
        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        self.builder.ins().global_value(types::I64, gv)
    }

    /// Create a stack slot for a heap-pointer value, store it, and push
    /// its address onto the GC shadow stack so the GC can find it as a root.
    fn push_gc_root(&mut self, val: Value) {
        use cranelift_codegen::ir::{StackSlotData, StackSlotKind};

        let ss = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            8,
            0,
        ));
        self.builder.ins().stack_store(val, ss, 0);
        let addr = self.builder.ins().stack_addr(types::I64, ss, 0);

        let push_id = self.runtime_fns["shadow_push"];
        let push_ref = self.module.declare_func_in_func(push_id, self.builder.func);
        self.builder.ins().call(push_ref, &[addr]);
        self.shadow_depth += 1;
    }

    /// Emit a call to `weir_shadow_pop(count)` to pop entries from the shadow stack.
    fn emit_shadow_pop(&mut self, count: usize) {
        if count == 0 {
            return;
        }
        let count_val = self.builder.ins().iconst(types::I64, count as i64);
        let pop_id = self.runtime_fns["shadow_pop"];
        let pop_ref = self.module.declare_func_in_func(pop_id, self.builder.func);
        self.builder.ins().call(pop_ref, &[count_val]);
        self.shadow_depth -= count;
    }

    fn declare_variable(&mut self, cl_ty: Type) -> Variable {
        self.builder.declare_var(cl_ty)
    }

    fn expr_ty(&self, expr_id: ExprId) -> Ty {
        let ty = self.type_info
            .expr_types
            .get(expr_id)
            .cloned()
            .unwrap_or(Ty::Unit);
        if self.type_subst.is_empty() {
            ty
        } else {
            self.apply_type_subst(&ty)
        }
    }

    /// Apply the specialization type substitution to resolve generic type vars.
    fn apply_type_subst(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(id) => self.type_subst.get(id).cloned().unwrap_or_else(|| ty.clone()),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.apply_type_subst(p)).collect(),
                Box::new(self.apply_type_subst(ret)),
            ),
            Ty::Vector(e) => Ty::Vector(Box::new(self.apply_type_subst(e))),
            Ty::Con(name, args) => Ty::Con(
                name.clone(),
                args.iter().map(|a| self.apply_type_subst(a)).collect(),
            ),
            Ty::Map(k, v) => Ty::Map(
                Box::new(self.apply_type_subst(k)),
                Box::new(self.apply_type_subst(v)),
            ),
            _ => ty.clone(),
        }
    }

    /// Compile a body (sequence of expressions), returning the value of the last one.
    fn compile_body(
        &mut self,
        body: &[ExprId],
        expected_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        if body.is_empty() {
            return Ok(None);
        }
        let mut val = None;
        for (i, &expr_id) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            let ty = if is_last {
                expected_ty.clone()
            } else {
                self.expr_ty(expr_id)
            };
            val = self.compile_expr(expr_id, &ty)?;
        }
        Ok(val)
    }

    /// Compile an expression, returning its Cranelift Value (or None for Unit).
    fn compile_expr(
        &mut self,
        expr_id: ExprId,
        _hint_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let expr = &self.ast_module.exprs[expr_id];
        let resolved_ty = self.expr_ty(expr_id);

        match &expr.kind {
            ExprKind::Lit(lit) => self.compile_lit(lit, &resolved_ty),

            ExprKind::Var(name) => {
                if let Some((var, _ty)) = self.vars.get(name) {
                    Ok(Some(self.builder.use_var(*var)))
                } else if let Some(info) = self.constructor_tags.get(name) {
                    if info.has_payload {
                        // Data-carrying constructor referenced without args — error
                        Err(CodegenError::new(format!(
                            "constructor '{}' requires an argument",
                            name
                        )))
                    } else {
                        // Nullary constructor — emit its tag as an i64 constant
                        Ok(Some(self.builder.ins().iconst(types::I64, info.tag)))
                    }
                } else {
                    Err(CodegenError::new(format!(
                        "undefined variable '{}' in codegen",
                        name
                    )))
                }
            }

            ExprKind::Call { func, args } => self.compile_call(*func, args, &resolved_ty),

            ExprKind::Let { bindings, body } => {
                let mut let_push_count = 0usize;
                for b in bindings {
                    let val_ty = self.expr_ty(b.value);
                    let val = self.compile_expr(b.value, &val_ty)?;
                    if let Some(cl_ty) = self.ty_cl(&val_ty) {
                        let var = self.declare_variable(cl_ty);
                        if let Some(v) = val {
                            self.builder.def_var(var, v);
                            // Push heap-pointer-typed let bindings onto the GC shadow stack
                            if self.is_heap_pointer(&val_ty) {
                                self.push_gc_root(v);
                                let_push_count += 1;
                            }
                        }
                        self.vars.insert(b.name.clone(), (var, val_ty));
                    }
                }
                let result = self.compile_body(body, &resolved_ty);
                // Pop let-scoped shadow stack entries
                self.emit_shadow_pop(let_push_count);
                result
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if(*condition, *then_branch, *else_branch, &resolved_ty),

            ExprKind::Cond {
                clauses,
                else_clause,
            } => self.compile_cond(clauses, *else_clause, &resolved_ty),

            ExprKind::When { condition, body } => {
                self.compile_when_unless(*condition, body, false)?;
                Ok(None)
            }

            ExprKind::Unless { condition, body } => {
                self.compile_when_unless(*condition, body, true)?;
                Ok(None)
            }

            ExprKind::Do { body } => self.compile_body(body, &resolved_ty),

            ExprKind::WithArena { name: _, body } => {
                // 1. Create arena
                let create_id = self.runtime_fns["arena_create"];
                let create_ref = self.module.declare_func_in_func(create_id, self.builder.func);
                let call = self.builder.ins().call(create_ref, &[]);
                let arena_ptr = self.builder.inst_results(call)[0];

                // Store arena pointer in a Cranelift variable
                let arena_var = self.declare_variable(types::I64);
                self.builder.def_var(arena_var, arena_ptr);

                // 2. Suppress GC
                let suppress_id = self.runtime_fns["gc_suppress"];
                let suppress_ref = self.module.declare_func_in_func(suppress_id, self.builder.func);
                self.builder.ins().call(suppress_ref, &[]);

                // 3. Push arena onto stack so allocations redirect
                self.arena_stack.push(arena_var);

                // 4. Compile body
                let result = self.compile_body(body, &resolved_ty);

                // 5. Pop arena from stack
                self.arena_stack.pop();

                // 6. Unsuppress GC
                let unsuppress_id = self.runtime_fns["gc_unsuppress"];
                let unsuppress_ref = self.module.declare_func_in_func(unsuppress_id, self.builder.func);
                self.builder.ins().call(unsuppress_ref, &[]);

                // 7. Destroy arena
                let arena_val = self.builder.use_var(arena_var);
                let destroy_id = self.runtime_fns["arena_destroy"];
                let destroy_ref = self.module.declare_func_in_func(destroy_id, self.builder.func);
                self.builder.ins().call(destroy_ref, &[arena_val]);

                result
            }

            ExprKind::SetBang { place, value } => {
                let place_expr = &self.ast_module.exprs[*place];
                if let ExprKind::Var(name) = &place_expr.kind {
                    let name = name.clone();
                    let val_ty = self.expr_ty(*value);
                    let val = self.compile_expr(*value, &val_ty)?;
                    if let Some(v) = val {
                        if let Some((var, _)) = self.vars.get(&name) {
                            let var = *var;
                            self.builder.def_var(var, v);
                        } else {
                            return Err(CodegenError::new(format!(
                                "set! on undefined variable '{}'",
                                name
                            )));
                        }
                    }
                    Ok(None) // set! returns Unit
                } else {
                    Err(CodegenError::new("set! target must be a variable"))
                }
            }

            ExprKind::Match { scrutinee, arms } => {
                self.compile_match(*scrutinee, arms, &resolved_ty)
            }

            ExprKind::Ann { expr, .. } => self.compile_expr(*expr, &resolved_ty),

            ExprKind::Try(inner) => self.compile_try(*inner, &resolved_ty),

            ExprKind::Lambda {
                params: lambda_params,
                body,
                ..
            } => {
                // Extract type info
                let (param_tys, ret_ty) = match &resolved_ty {
                    Ty::Fn(p, r) => (p.clone(), r.as_ref().clone()),
                    _ => {
                        return Err(CodegenError::new(format!(
                            "lambda has non-function type: {}",
                            resolved_ty
                        )));
                    }
                };

                // Capture analysis
                let captures = find_captures(
                    self.ast_module,
                    lambda_params,
                    body,
                    &self.vars,
                );

                let num_captures = captures.len();

                // Generate unique lambda name
                let lambda_idx = *self.lambda_counter;
                *self.lambda_counter += 1;
                let lambda_name = SmolStr::new(format!("__lambda_{}", lambda_idx));

                // Build lambda signature: (env_ptr: i64, param_0, ...) -> ret
                let adts = self.tagged_adts;
                let mut sig = self.module.make_signature();
                sig.call_conv = CallConv::SystemV;
                sig.params.push(AbiParam::new(types::I64)); // env_ptr
                for pty in &param_tys {
                    if let Some(cl_ty) = ty_to_cl_with(pty, Some(adts)) {
                        sig.params.push(AbiParam::new(cl_ty));
                    }
                }
                if let Some(cl_ty) = ty_to_cl_with(&ret_ty, Some(adts)) {
                    sig.returns.push(AbiParam::new(cl_ty));
                }

                // Declare the lambda function in the module
                let func_id = self
                    .module
                    .declare_function(&lambda_name, Linkage::Local, &sig)
                    .map_err(|e| CodegenError::new(format!("declare lambda '{}': {}", lambda_name, e)))?;

                // Build capture info for deferred compilation
                let capture_info: Vec<(SmolStr, Ty)> = captures
                    .iter()
                    .map(|(name, _, ty)| (name.clone(), ty.clone()))
                    .collect();

                self.pending_lambdas.push(LambdaInfo {
                    func_id,
                    name: lambda_name,
                    param_tys: param_tys.clone(),
                    ret_ty,
                    params: lambda_params.clone(),
                    body: body.clone(),
                    captures: capture_info,
                    type_subst: self.type_subst.clone(),
                });

                // Allocate closure: [fn_ptr, num_captures, cap_0, cap_1, ...]
                // Build shape: slots 0,1 are fn_ptr and num_captures (not heap ptrs).
                // Slots 2..2+n are captures; bit is set if capture type is a heap pointer.
                let total_slots = (2 + num_captures) as u32;
                let mut pointer_mask: u64 = 0;
                for (i, (_, _, cap_ty)) in captures.iter().enumerate() {
                    if self.is_heap_pointer(cap_ty) {
                        pointer_mask |= 1u64 << (2 + i);
                    }
                }
                let shape_val = self.emit_shape_desc(total_slots, SHAPE_FIXED, pointer_mask);

                let alloc_size = ((2 + num_captures) * 8) as i64;
                let alloc_size_val = self.builder.ins().iconst(types::I64, alloc_size);
                let closure_ptr = if let Some(&arena_v) = self.arena_stack.last() {
                    let arena_val = self.builder.use_var(arena_v);
                    let alloc_id = self.runtime_fns["arena_alloc"];
                    let alloc_ref = self.module.declare_func_in_func(alloc_id, self.builder.func);
                    let call = self.builder.ins().call(alloc_ref, &[arena_val, alloc_size_val]);
                    self.builder.inst_results(call)[0]
                } else {
                    let alloc_id = self.runtime_fns["gc_alloc"];
                    let alloc_ref = self.module.declare_func_in_func(alloc_id, self.builder.func);
                    let call = self.builder.ins().call(alloc_ref, &[alloc_size_val, shape_val]);
                    self.builder.inst_results(call)[0]
                };

                // Store fn_ptr at offset 0
                let func_ref_for_addr = self.module.declare_func_in_func(func_id, self.builder.func);
                let fn_addr = self.builder.ins().func_addr(types::I64, func_ref_for_addr);
                self.builder.ins().store(MemFlags::trusted(), fn_addr, closure_ptr, 0);

                // Store num_captures at offset 8
                let num_cap_val = self.builder.ins().iconst(types::I64, num_captures as i64);
                self.builder.ins().store(MemFlags::trusted(), num_cap_val, closure_ptr, 8);

                // Store each capture
                for (i, (_, var, ty)) in captures.iter().enumerate() {
                    let val = self.builder.use_var(*var);
                    let widened = self.widen_to_i64(val, ty);
                    let offset = ((2 + i) * 8) as i32;
                    self.builder.ins().store(MemFlags::trusted(), widened, closure_ptr, offset);
                }

                Ok(Some(closure_ptr))
            }

            ExprKind::VectorLit(elems) => {
                let elem_ty = if let Ty::Vector(et) = &resolved_ty {
                    et.as_ref().clone()
                } else {
                    Ty::I64
                };
                // Emit vector shape: SHAPE_VECTOR, pointer_mask bit 0 = elements are heap ptrs
                let elems_are_ptrs = self.is_heap_pointer(&elem_ty);
                let shape_val = self.emit_shape_desc(
                    0,
                    SHAPE_VECTOR,
                    if elems_are_ptrs { 1 } else { 0 },
                );

                let len = elems.len() as i64;
                let len_val = self.builder.ins().iconst(types::I64, len);
                let ptr = if let Some(&arena_v) = self.arena_stack.last() {
                    let arena_val = self.builder.use_var(arena_v);
                    let alloc_id = self.runtime_fns["arena_vec_alloc"];
                    let alloc_ref = self.module.declare_func_in_func(alloc_id, self.builder.func);
                    let call = self.builder.ins().call(alloc_ref, &[arena_val, len_val]);
                    self.builder.inst_results(call)[0]
                } else {
                    let alloc_id = self.runtime_fns["gc_vec_alloc"];
                    let alloc_ref = self.module.declare_func_in_func(alloc_id, self.builder.func);
                    let call = self.builder.ins().call(alloc_ref, &[len_val, shape_val]);
                    self.builder.inst_results(call)[0]
                };
                for (i, &elem_id) in elems.iter().enumerate() {
                    let val = self.compile_expr(elem_id, &elem_ty)?.unwrap();
                    let widened = self.widen_to_i64(val, &elem_ty);
                    let offset = ((1 + i) * 8) as i32;
                    self.builder.ins().store(MemFlags::trusted(), widened, ptr, offset);
                }
                Ok(Some(ptr))
            }

            _ => Err(CodegenError::new(format!(
                "unsupported expression kind in codegen: {:?}",
                std::mem::discriminant(&expr.kind)
            ))),
        }
    }

    // ── Literals ────────────────────────────────────────────────

    fn compile_lit(&mut self, lit: &Literal, ty: &Ty) -> Result<Option<Value>, CodegenError> {
        match lit {
            Literal::Int(n) => {
                let cl_ty = self.ty_cl(ty).unwrap_or(types::I64);
                let val = self.builder.ins().iconst(cl_ty, *n);
                Ok(Some(val))
            }
            Literal::Float(n) => match ty {
                Ty::F32 => Ok(Some(self.builder.ins().f32const(*n as f32))),
                _ => Ok(Some(self.builder.ins().f64const(*n))),
            },
            Literal::Bool(b) => {
                let val = self.builder.ins().iconst(types::I8, if *b { 1 } else { 0 });
                Ok(Some(val))
            }
            Literal::String(s) => {
                // Embed string as null-terminated data in a Cranelift data section
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0); // null terminator
                let mut data_desc = DataDescription::new();
                data_desc.define(bytes.into_boxed_slice());
                let data_id = self
                    .module
                    .declare_anonymous_data(false, false)
                    .map_err(|e| CodegenError::new(format!("declare string data: {}", e)))?;
                self.module
                    .define_data(data_id, &data_desc)
                    .map_err(|e| CodegenError::new(format!("define string data: {}", e)))?;
                let gv = self.module.declare_data_in_func(data_id, self.builder.func);
                let ptr = self.builder.ins().global_value(types::I64, gv);
                Ok(Some(ptr))
            }
        }
    }

    // ── Function calls / builtins ───────────────────────────────

    fn compile_call(
        &mut self,
        func_id: ExprId,
        args: &[Arg],
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let func_expr = &self.ast_module.exprs[func_id];

        if let ExprKind::Var(name) = &func_expr.kind {
            // Check if this is a typeclass method call with a resolved instance
            if let Some(instance_key) = self.type_info.method_resolutions.get(&func_id) {
                let mangled_name = SmolStr::new(format!("{}#{}", instance_key, name));
                if let Some((callee_id, param_tys, ret_ty)) =
                    self.user_fns.get(&mangled_name).cloned()
                {
                    let mut arg_vals = Vec::new();
                    for (i, arg) in args.iter().enumerate() {
                        let pty = if i < param_tys.len() {
                            &param_tys[i]
                        } else {
                            return Err(CodegenError::new(format!(
                                "too many arguments for method '{}' (instance {})",
                                name, instance_key
                            )));
                        };
                        if let Some(val) = self.compile_expr(arg.value, pty)? {
                            arg_vals.push(val);
                        }
                    }

                    let func_ref = self
                        .module
                        .declare_func_in_func(callee_id, self.builder.func);
                    let call_inst = self.builder.ins().call(func_ref, &arg_vals);
                    if self.ty_cl(&ret_ty).is_some() {
                        let results = self.builder.inst_results(call_inst);
                        return Ok(Some(results[0]));
                    } else {
                        return Ok(None);
                    }
                }
            }

            // Check if this is a constructor call
            if let Some(info) = self.constructor_tags.get(name) {
                if info.has_payload {
                    if args.len() != 1 {
                        return Err(CodegenError::new(format!(
                            "constructor '{}' expects 1 argument, got {}",
                            name,
                            args.len()
                        )));
                    }
                    let arg_ty = self.expr_ty(args[0].value);
                    let payload = self.compile_expr(args[0].value, &arg_ty)?.unwrap();
                    // Promote payload to i64
                    let payload_i64 = match self.ty_cl(&arg_ty) {
                        Some(t) if t == types::I8 || t == types::I16 || t == types::I32 => {
                            self.builder.ins().sextend(types::I64, payload)
                        }
                        Some(t) if t == types::I64 => payload,
                        _ => {
                            // Payload is itself an ADT (packed i64) — use directly
                            payload
                        }
                    };
                    let tag = info.tag;
                    let shifted = {
                        let eight = self.builder.ins().iconst(types::I64, 8);
                        self.builder.ins().ishl(payload_i64, eight)
                    };
                    let tag_val = self.builder.ins().iconst(types::I64, tag);
                    let packed = self.builder.ins().bor(shifted, tag_val);
                    return Ok(Some(packed));
                } else {
                    // Nullary constructor called as (None) — emit tag
                    return Ok(Some(self.builder.ins().iconst(types::I64, info.tag)));
                }
            }

            match name.as_str() {
                "+" | "-" | "*" | "/" | "mod" => {
                    return self.compile_arith(name, args, result_ty);
                }
                "<" | ">" | "<=" | ">=" | "=" | "!=" => {
                    return self.compile_comparison(name, args);
                }
                "not" => {
                    return self.compile_not(args);
                }
                "and" => {
                    return self.compile_and(args);
                }
                "or" => {
                    return self.compile_or(args);
                }
                "println" => {
                    return self.compile_println(args);
                }
                "print" => {
                    return self.compile_print(args);
                }
                "sleep" => {
                    return self.compile_sleep(args);
                }
                "str" => {
                    return self.compile_str_builtin(args);
                }
                "len" => {
                    if args.len() != 1 {
                        return Err(CodegenError::new("len expects exactly 1 argument"));
                    }
                    let arg_ty = self.expr_ty(args[0].value);
                    if matches!(arg_ty, Ty::Vector(_)) {
                        let vec_ptr = self.compile_expr(args[0].value, &arg_ty)?.unwrap();
                        let len = self.builder.ins().load(types::I64, MemFlags::trusted(), vec_ptr, 0);
                        return Ok(Some(len));
                    }
                }
                "nth" => {
                    if args.len() != 2 {
                        return Err(CodegenError::new("nth expects exactly 2 arguments"));
                    }
                    let vec_ty = self.expr_ty(args[0].value);
                    if let Ty::Vector(elem_ty) = &vec_ty {
                        let elem_ty = elem_ty.as_ref().clone();
                        let vec_ptr = self.compile_expr(args[0].value, &vec_ty)?.unwrap();
                        let idx_ty = self.expr_ty(args[1].value);
                        let idx = self.compile_expr(args[1].value, &idx_ty)?.unwrap();
                        // Extend index to i64 if needed
                        let idx_i64 = match &idx_ty {
                            Ty::I8 | Ty::I16 | Ty::I32 => self.builder.ins().sextend(types::I64, idx),
                            Ty::U8 | Ty::U16 | Ty::U32 => self.builder.ins().uextend(types::I64, idx),
                            _ => idx,
                        };
                        // ptr + (1 + idx) * 8
                        let one = self.builder.ins().iconst(types::I64, 1);
                        let offset_slots = self.builder.ins().iadd(idx_i64, one);
                        let eight = self.builder.ins().iconst(types::I64, 8);
                        let byte_offset = self.builder.ins().imul(offset_slots, eight);
                        let addr = self.builder.ins().iadd(vec_ptr, byte_offset);
                        let raw = self.builder.ins().load(types::I64, MemFlags::trusted(), addr, 0);
                        let val = self.narrow_from_i64(raw, &elem_ty);
                        return Ok(Some(val));
                    }
                }
                "append" => {
                    if args.len() != 2 {
                        return Err(CodegenError::new("append expects exactly 2 arguments"));
                    }
                    let vec_ty = self.expr_ty(args[0].value);
                    if let Ty::Vector(elem_ty) = &vec_ty {
                        let elem_ty = elem_ty.as_ref().clone();
                        let vec_ptr = self.compile_expr(args[0].value, &vec_ty)?.unwrap();
                        let elem_val = self.compile_expr(args[1].value, &elem_ty)?.unwrap();
                        let widened = self.widen_to_i64(elem_val, &elem_ty);
                        // Build vector shape for the appended result
                        let elems_are_ptrs = self.is_heap_pointer(&elem_ty);
                        let shape_val = self.emit_shape_desc(
                            0,
                            SHAPE_VECTOR,
                            if elems_are_ptrs { 1 } else { 0 },
                        );
                        if let Some(&arena_v) = self.arena_stack.last() {
                            // Arena path: arena_vec_alloc + manual copy
                            let old_len_id = self.runtime_fns["vec_len"];
                            let old_len_ref = self.module.declare_func_in_func(old_len_id, self.builder.func);
                            let call = self.builder.ins().call(old_len_ref, &[vec_ptr]);
                            let old_len = self.builder.inst_results(call)[0];
                            let one = self.builder.ins().iconst(types::I64, 1);
                            let new_len = self.builder.ins().iadd(old_len, one);
                            let arena_val = self.builder.use_var(arena_v);
                            let alloc_id = self.runtime_fns["arena_vec_alloc"];
                            let alloc_ref = self.module.declare_func_in_func(alloc_id, self.builder.func);
                            let call = self.builder.ins().call(alloc_ref, &[arena_val, new_len]);
                            let new_ptr = self.builder.inst_results(call)[0];
                            // Copy old elements: memcpy(new+8, old+8, old_len*8)
                            let eight = self.builder.ins().iconst(types::I64, 8);
                            let src = self.builder.ins().iadd(vec_ptr, eight);
                            let dst = self.builder.ins().iadd(new_ptr, eight);
                            let byte_count = self.builder.ins().imul(old_len, eight);
                            self.builder.call_memcpy(self.module.target_config(), dst, src, byte_count);
                            // Store new element at new_ptr + (1+old_len)*8
                            let offset_slots = self.builder.ins().iadd(old_len, one);
                            let byte_off = self.builder.ins().imul(offset_slots, eight);
                            let elem_addr = self.builder.ins().iadd(new_ptr, byte_off);
                            self.builder.ins().store(MemFlags::trusted(), widened, elem_addr, 0);
                            return Ok(Some(new_ptr));
                        } else {
                            let func_id = self.runtime_fns["vec_append"];
                            let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                            let call = self.builder.ins().call(func_ref, &[vec_ptr, widened, shape_val]);
                            let new_ptr = self.builder.inst_results(call)[0];
                            return Ok(Some(new_ptr));
                        }
                    }
                }
                _ => {}
            }

            // Check if this is a call to a generic function — redirect to specialization.
            // Generic functions are not in user_fns; find the mangled specialization name
            // from the concrete type at this call site.
            if !self.user_fns.contains_key(name) {
                if let Some(Ty::Fn(param_tys, ret_ty)) = self.type_info.expr_types.get(func_id) {
                    let type_key: String = param_tys
                        .iter()
                        .chain(std::iter::once(ret_ty.as_ref()))
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",");
                    let mangled = SmolStr::new(format!("{}#{}", name, type_key));
                    if let Some((callee_id, spec_param_tys, spec_ret_ty)) =
                        self.user_fns.get(&mangled).cloned()
                    {
                        let mut arg_vals = Vec::new();
                        for (i, arg) in args.iter().enumerate() {
                            let pty = if i < spec_param_tys.len() {
                                &spec_param_tys[i]
                            } else {
                                return Err(CodegenError::new(format!(
                                    "too many arguments for specialization '{}'",
                                    mangled
                                )));
                            };
                            if let Some(val) = self.compile_expr(arg.value, pty)? {
                                arg_vals.push(val);
                            }
                        }

                        let func_ref = self
                            .module
                            .declare_func_in_func(callee_id, self.builder.func);
                        let call_inst = self.builder.ins().call(func_ref, &arg_vals);
                        if self.ty_cl(&spec_ret_ty).is_some() {
                            let results = self.builder.inst_results(call_inst);
                            return Ok(Some(results[0]));
                        } else {
                            return Ok(None);
                        }
                    }
                }
            }

            // User function call
            if let Some((callee_id, param_tys, ret_ty)) = self.user_fns.get(name).cloned() {
                let mut arg_vals = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let pty = if i < param_tys.len() {
                        &param_tys[i]
                    } else {
                        return Err(CodegenError::new(format!(
                            "too many arguments for function '{}'",
                            name
                        )));
                    };
                    if let Some(val) = self.compile_expr(arg.value, pty)? {
                        arg_vals.push(val);
                    }
                }

                // Check if we should use indirect dispatch (dev mode)
                if let (Some(slots), Some(data_id)) = (self.fn_table_slots, self.table_data_id) {
                    if let Some(&slot_index) = slots.get(name) {
                        // Build the callee signature
                        let mut sig = cranelift_codegen::ir::Signature::new(CallConv::SystemV);
                        for pty in &param_tys {
                            if let Some(cl_ty) = self.ty_cl(pty) {
                                sig.params.push(AbiParam::new(cl_ty));
                            }
                        }
                        if let Some(cl_ty) = self.ty_cl(&ret_ty) {
                            sig.returns.push(AbiParam::new(cl_ty));
                        }

                        // Load function pointer from table
                        let table_gv = self.module.declare_data_in_func(data_id, self.builder.func);
                        let table_base = self.builder.ins().global_value(types::I64, table_gv);
                        let offset = self
                            .builder
                            .ins()
                            .iconst(types::I64, (slot_index * 8) as i64);
                        let slot_addr = self.builder.ins().iadd(table_base, offset);
                        let fn_ptr =
                            self.builder
                                .ins()
                                .load(types::I64, MemFlags::trusted(), slot_addr, 0);

                        let sig_ref = self.builder.import_signature(sig);
                        let call_inst =
                            self.builder.ins().call_indirect(sig_ref, fn_ptr, &arg_vals);

                        if self.ty_cl(&ret_ty).is_some() {
                            let results = self.builder.inst_results(call_inst);
                            return Ok(Some(results[0]));
                        } else {
                            return Ok(None);
                        }
                    }
                }

                // Direct call (normal JIT/AOT path)
                let func_ref = self
                    .module
                    .declare_func_in_func(callee_id, self.builder.func);
                let call_inst = self.builder.ins().call(func_ref, &arg_vals);
                if self.ty_cl(&ret_ty).is_some() {
                    let results = self.builder.inst_results(call_inst);
                    Ok(Some(results[0]))
                } else {
                    Ok(None)
                }
            } else if let Some((_var, var_ty)) = self.vars.get(name).cloned() {
                // Variable holds a closure — do indirect call
                if let Ty::Fn(param_tys, ret_ty) = &var_ty {
                    let name = name.clone();
                    let param_tys = param_tys.clone();
                    let ret_ty = ret_ty.as_ref().clone();
                    let closure_ptr = self.builder.use_var(self.vars[&name].0);
                    return self.compile_closure_call(closure_ptr, &param_tys, &ret_ty, args);
                }
                Err(CodegenError::new(format!(
                    "variable '{}' is not callable (type: {})",
                    name, var_ty
                )))
            } else {
                Err(CodegenError::new(format!(
                    "unknown function '{}' in codegen",
                    name
                )))
            }
        } else {
            // Non-variable function expression (e.g., inline lambda call)
            let func_ty = self.expr_ty(func_id);
            if let Ty::Fn(param_tys, ret_ty) = &func_ty {
                let param_tys = param_tys.clone();
                let ret_ty = ret_ty.as_ref().clone();
                let closure_ptr = self.compile_expr(func_id, &func_ty)?.unwrap();
                self.compile_closure_call(closure_ptr, &param_tys, &ret_ty, args)
            } else {
                Err(CodegenError::new(
                    "non-variable function calls not supported in codegen",
                ))
            }
        }
    }

    // ── Closure calls ─────────────────────────────────────────

    fn compile_closure_call(
        &mut self,
        closure_ptr: Value,
        param_tys: &[Ty],
        ret_ty: &Ty,
        args: &[Arg],
    ) -> Result<Option<Value>, CodegenError> {
        // Load fn_ptr from closure offset 0
        let fn_ptr = self.builder.ins().load(types::I64, MemFlags::trusted(), closure_ptr, 0);

        // Build callee signature: (env_ptr: i64, param_0, param_1, ...) -> ret
        let mut sig = cranelift_codegen::ir::Signature::new(CallConv::SystemV);
        sig.params.push(AbiParam::new(types::I64)); // env_ptr
        for pty in param_tys {
            if let Some(cl_ty) = self.ty_cl(pty) {
                sig.params.push(AbiParam::new(cl_ty));
            }
        }
        if let Some(cl_ty) = self.ty_cl(ret_ty) {
            sig.returns.push(AbiParam::new(cl_ty));
        }

        // Compile arguments
        let mut call_args = vec![closure_ptr]; // first arg is env_ptr (the closure itself)
        for (i, arg) in args.iter().enumerate() {
            let pty = if i < param_tys.len() {
                &param_tys[i]
            } else {
                return Err(CodegenError::new("too many arguments for closure call"));
            };
            if let Some(val) = self.compile_expr(arg.value, pty)? {
                call_args.push(val);
            }
        }

        let sig_ref = self.builder.import_signature(sig);
        let call_inst = self.builder.ins().call_indirect(sig_ref, fn_ptr, &call_args);

        if self.ty_cl(ret_ty).is_some() {
            let results = self.builder.inst_results(call_inst);
            Ok(Some(results[0]))
        } else {
            Ok(None)
        }
    }

    // ── Arithmetic ──────────────────────────────────────────────

    fn compile_arith(
        &mut self,
        op: &str,
        args: &[Arg],
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return match op {
                "+" => {
                    let cl_ty = self.ty_cl(result_ty).unwrap_or(types::I64);
                    Ok(Some(self.builder.ins().iconst(cl_ty, 0)))
                }
                "*" => {
                    let cl_ty = self.ty_cl(result_ty).unwrap_or(types::I64);
                    Ok(Some(self.builder.ins().iconst(cl_ty, 1)))
                }
                _ => Err(CodegenError::new(format!("{} requires arguments", op))),
            };
        }

        // Unary minus
        if op == "-" && args.len() == 1 {
            let arg_ty = self.expr_ty(args[0].value);
            let val = self.compile_expr(args[0].value, &arg_ty)?.unwrap();
            if is_float(&arg_ty) {
                return Ok(Some(self.builder.ins().fneg(val)));
            } else {
                // ineg = 0 - val
                let cl_ty = self.ty_cl(&arg_ty).unwrap();
                let zero = self.builder.ins().iconst(cl_ty, 0);
                return Ok(Some(self.builder.ins().isub(zero, val)));
            }
        }

        let float = is_float(result_ty);

        // Compile first argument
        let first_ty = self.expr_ty(args[0].value);
        let mut acc = self.compile_expr(args[0].value, &first_ty)?.unwrap();

        // Fold remaining arguments
        for arg in &args[1..] {
            let arg_ty = self.expr_ty(arg.value);
            let val = self.compile_expr(arg.value, &arg_ty)?.unwrap();

            acc = match op {
                "+" if float => self.builder.ins().fadd(acc, val),
                "+" => self.builder.ins().iadd(acc, val),
                "-" if float => self.builder.ins().fsub(acc, val),
                "-" => self.builder.ins().isub(acc, val),
                "*" if float => self.builder.ins().fmul(acc, val),
                "*" => self.builder.ins().imul(acc, val),
                "/" if float => self.builder.ins().fdiv(acc, val),
                "/" if is_signed(result_ty) => self.builder.ins().sdiv(acc, val),
                "/" => self.builder.ins().udiv(acc, val),
                "mod" if float => {
                    let div = self.builder.ins().fdiv(acc, val);
                    let trunc = self.builder.ins().trunc(div);
                    let prod = self.builder.ins().fmul(trunc, val);
                    self.builder.ins().fsub(acc, prod)
                }
                "mod" if is_signed(result_ty) => self.builder.ins().srem(acc, val),
                "mod" => self.builder.ins().urem(acc, val),
                _ => unreachable!(),
            };
        }

        Ok(Some(acc))
    }

    // ── Comparisons ─────────────────────────────────────────────

    fn compile_comparison(
        &mut self,
        op: &str,
        args: &[Arg],
    ) -> Result<Option<Value>, CodegenError> {
        if args.len() != 2 {
            return Err(CodegenError::new(format!(
                "{} requires exactly 2 arguments",
                op
            )));
        }

        let lhs_ty = self.expr_ty(args[0].value);
        let lhs = self.compile_expr(args[0].value, &lhs_ty)?.unwrap();
        let rhs_ty = self.expr_ty(args[1].value);
        let rhs = self.compile_expr(args[1].value, &rhs_ty)?.unwrap();

        // String comparison via runtime helpers
        if lhs_ty == Ty::Str && rhs_ty == Ty::Str {
            if op == "=" || op == "!=" {
                let func_id = self.runtime_fns["str_eq"];
                let func_ref = self
                    .module
                    .declare_func_in_func(func_id, self.builder.func);
                let call = self.builder.ins().call(func_ref, &[lhs, rhs]);
                let eq_val = self.builder.inst_results(call)[0];
                if op == "!=" {
                    let one = self.builder.ins().iconst(types::I8, 1);
                    return Ok(Some(self.builder.ins().bxor(eq_val, one)));
                }
                return Ok(Some(eq_val));
            }
            // Ordering comparisons via weir_str_cmp -> {-1, 0, 1}
            let func_id = self.runtime_fns["str_cmp"];
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
            let call = self.builder.ins().call(func_ref, &[lhs, rhs]);
            let cmp_val = self.builder.inst_results(call)[0]; // i64: -1, 0, 1
            let zero = self.builder.ins().iconst(types::I64, 0);
            let cc = match op {
                "<" => IntCC::SignedLessThan,
                ">" => IntCC::SignedGreaterThan,
                "<=" => IntCC::SignedLessThanOrEqual,
                ">=" => IntCC::SignedGreaterThanOrEqual,
                _ => unreachable!(),
            };
            return Ok(Some(self.builder.ins().icmp(cc, cmp_val, zero)));
        }

        // Check if this is a primitive numeric or bool type
        let is_primitive = matches!(
            &lhs_ty,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
            | Ty::F32 | Ty::F64 | Ty::Bool
        );

        if is_primitive {
            let float = is_float(&lhs_ty);

            let result = if float {
                let cc = match op {
                    "<" => FloatCC::LessThan,
                    ">" => FloatCC::GreaterThan,
                    "<=" => FloatCC::LessThanOrEqual,
                    ">=" => FloatCC::GreaterThanOrEqual,
                    "=" => FloatCC::Equal,
                    "!=" => FloatCC::NotEqual,
                    _ => unreachable!(),
                };
                self.builder.ins().fcmp(cc, lhs, rhs)
            } else {
                let signed = is_signed(&lhs_ty);
                let cc = match (op, signed) {
                    ("<", true) => IntCC::SignedLessThan,
                    ("<", false) => IntCC::UnsignedLessThan,
                    (">", true) => IntCC::SignedGreaterThan,
                    (">", false) => IntCC::UnsignedGreaterThan,
                    ("<=", true) => IntCC::SignedLessThanOrEqual,
                    ("<=", false) => IntCC::UnsignedLessThanOrEqual,
                    (">=", true) => IntCC::SignedGreaterThanOrEqual,
                    (">=", false) => IntCC::UnsignedGreaterThanOrEqual,
                    ("=", _) => IntCC::Equal,
                    ("!=", _) => IntCC::NotEqual,
                    _ => unreachable!(),
                };
                self.builder.ins().icmp(cc, lhs, rhs)
            };

            return Ok(Some(result));
        }

        // Non-primitive: dispatch through Ord typeclass
        if matches!(op, "<" | ">" | "<=" | ">=") {
            return self.compile_ord_comparison(op, lhs, rhs, &lhs_ty);
        }

        // = / != on non-primitive types: compare packed i64 values directly
        let cc = match op {
            "=" => IntCC::Equal,
            "!=" => IntCC::NotEqual,
            _ => unreachable!(),
        };
        Ok(Some(self.builder.ins().icmp(cc, lhs, rhs)))
    }

    fn compile_ord_comparison(
        &mut self,
        op: &str,
        lhs: Value,
        rhs: Value,
        arg_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        // Construct the mangled name for the Ord compare method
        let type_name = match arg_ty {
            Ty::Con(name, _) => name.as_str(),
            _ => {
                return Err(CodegenError::new(format!(
                    "Ord dispatch not supported for type {} in codegen",
                    arg_ty
                )));
            }
        };

        let mangled = format!("Ord#{}#compare", type_name);
        let func_info = self.user_fns.get(mangled.as_str()).cloned();
        let (func_id, _, _) = func_info.ok_or_else(|| {
            CodegenError::new(format!(
                "no Ord instance found for type {} (expected function {})",
                arg_ty, mangled
            ))
        })?;

        let func_ref = self
            .module
            .declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, &[lhs, rhs]);
        let ordering_val = self.builder.inst_results(call)[0]; // packed ADT tag

        // Extract the tag (low 8 bits)
        let tag = {
            let mask = self.builder.ins().iconst(types::I64, 0xFF);
            self.builder.ins().band(ordering_val, mask)
        };

        // Look up LT/EQ/GT tag values from constructor_tags
        let lt_tag = self.constructor_tags.get("LT")
            .map(|c| c.tag)
            .unwrap_or(0);
        let gt_tag = self.constructor_tags.get("GT")
            .map(|c| c.tag)
            .unwrap_or(2);

        let result = match op {
            "<" => {
                let lt_val = self.builder.ins().iconst(types::I64, lt_tag);
                self.builder.ins().icmp(IntCC::Equal, tag, lt_val)
            }
            ">" => {
                let gt_val = self.builder.ins().iconst(types::I64, gt_tag);
                self.builder.ins().icmp(IntCC::Equal, tag, gt_val)
            }
            "<=" => {
                let gt_val = self.builder.ins().iconst(types::I64, gt_tag);
                self.builder.ins().icmp(IntCC::NotEqual, tag, gt_val)
            }
            ">=" => {
                let lt_val = self.builder.ins().iconst(types::I64, lt_tag);
                self.builder.ins().icmp(IntCC::NotEqual, tag, lt_val)
            }
            _ => unreachable!(),
        };

        Ok(Some(result))
    }

    // ── Boolean ops ─────────────────────────────────────────────

    fn compile_not(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::new("not requires exactly 1 argument"));
        }
        let val = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        let one = self.builder.ins().iconst(types::I8, 1);
        let result = self.builder.ins().bxor(val, one);
        Ok(Some(result))
    }

    fn compile_and(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return Ok(Some(self.builder.ins().iconst(types::I8, 1)));
        }
        let mut acc = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        for arg in &args[1..] {
            let val = self.compile_expr(arg.value, &Ty::Bool)?.unwrap();
            acc = self.builder.ins().band(acc, val);
        }
        Ok(Some(acc))
    }

    fn compile_or(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return Ok(Some(self.builder.ins().iconst(types::I8, 0)));
        }
        let mut acc = self.compile_expr(args[0].value, &Ty::Bool)?.unwrap();
        for arg in &args[1..] {
            let val = self.compile_expr(arg.value, &Ty::Bool)?.unwrap();
            acc = self.builder.ins().bor(acc, val);
        }
        Ok(Some(acc))
    }

    // ── Print ───────────────────────────────────────────────────

    fn compile_println(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() > 1 {
            return Err(CodegenError::new("println expects 0 or 1 arguments"));
        }

        if args.len() == 1 {
            self.compile_print(args)?;
        }

        // Print newline
        let newline_id = self.runtime_fns["print_newline"];
        let newline_ref = self
            .module
            .declare_func_in_func(newline_id, self.builder.func);
        self.builder.ins().call(newline_ref, &[]);

        Ok(None) // println returns Unit
    }

    fn compile_print(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::new("print expects exactly 1 argument"));
        }

        let arg_ty = self.expr_ty(args[0].value);
        let val = self.compile_expr(args[0].value, &arg_ty)?;

        let (helper_name, need_extend) = match &arg_ty {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 => {
                ("print_i64", true)
            }
            Ty::F32 => ("print_f64", true),
            Ty::F64 => ("print_f64", false),
            Ty::Bool => ("print_bool", false),
            Ty::Unit => ("print_unit", false),
            Ty::Str => ("print_str", false),
            _ => {
                return Err(CodegenError::new(format!(
                    "cannot print type {} in codegen",
                    arg_ty
                )))
            }
        };

        let func_id = self.runtime_fns[helper_name];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

        match &arg_ty {
            Ty::Unit => {
                self.builder.ins().call(func_ref, &[]);
            }
            _ => {
                let val = val.unwrap();
                let arg_val = if need_extend {
                    match &arg_ty {
                        Ty::I8 | Ty::I16 | Ty::I32 => self.builder.ins().sextend(types::I64, val),
                        Ty::U8 | Ty::U16 | Ty::U32 => self.builder.ins().uextend(types::I64, val),
                        Ty::F32 => self.builder.ins().fpromote(types::F64, val),
                        _ => val, // I64, U64 already right size
                    }
                } else {
                    val
                };
                self.builder.ins().call(func_ref, &[arg_val]);
            }
        }

        Ok(None)
    }

    // ── Sleep ────────────────────────────────────────────────────

    fn compile_sleep(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.len() != 1 {
            return Err(CodegenError::new(
                "sleep expects exactly 1 argument (milliseconds)",
            ));
        }
        let arg_ty = self.expr_ty(args[0].value);
        let val = self.compile_expr(args[0].value, &arg_ty)?.unwrap();
        // Extend to i64 if needed
        let arg_val = match &arg_ty {
            Ty::I8 | Ty::I16 | Ty::I32 => self.builder.ins().sextend(types::I64, val),
            Ty::U8 | Ty::U16 | Ty::U32 => self.builder.ins().uextend(types::I64, val),
            _ => val,
        };
        let func_id = self.runtime_fns["sleep_ms"];
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, &[arg_val]);
        Ok(None)
    }

    // ── str builtin ─────────────────────────────────────────────

    /// Convert a single value to a string pointer (i64).
    fn compile_to_str(&mut self, arg: &Arg) -> Result<Value, CodegenError> {
        let arg_ty = self.expr_ty(arg.value);
        let val = self.compile_expr(arg.value, &arg_ty)?;
        match &arg_ty {
            Ty::Str => Ok(val.unwrap()),
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 => {
                let val = val.unwrap();
                let extended = match &arg_ty {
                    Ty::I8 | Ty::I16 | Ty::I32 => self.builder.ins().sextend(types::I64, val),
                    Ty::U8 | Ty::U16 | Ty::U32 => self.builder.ins().uextend(types::I64, val),
                    _ => val,
                };
                let func_id = self.runtime_fns["i64_to_str"];
                let func_ref = self
                    .module
                    .declare_func_in_func(func_id, self.builder.func);
                let call = self.builder.ins().call(func_ref, &[extended]);
                Ok(self.builder.inst_results(call)[0])
            }
            Ty::F32 | Ty::F64 => {
                let val = val.unwrap();
                let promoted = if arg_ty == Ty::F32 {
                    self.builder.ins().fpromote(types::F64, val)
                } else {
                    val
                };
                let func_id = self.runtime_fns["f64_to_str"];
                let func_ref = self
                    .module
                    .declare_func_in_func(func_id, self.builder.func);
                let call = self.builder.ins().call(func_ref, &[promoted]);
                Ok(self.builder.inst_results(call)[0])
            }
            Ty::Bool => {
                let val = val.unwrap();
                let func_id = self.runtime_fns["bool_to_str"];
                let func_ref = self
                    .module
                    .declare_func_in_func(func_id, self.builder.func);
                let call = self.builder.ins().call(func_ref, &[val]);
                Ok(self.builder.inst_results(call)[0])
            }
            _ => Err(CodegenError::new(format!(
                "str: cannot convert type {} to string",
                arg_ty
            ))),
        }
    }

    fn compile_str_builtin(&mut self, args: &[Arg]) -> Result<Option<Value>, CodegenError> {
        if args.is_empty() {
            return Err(CodegenError::new("str expects at least 1 argument"));
        }
        // Convert first arg to string
        let mut acc = self.compile_to_str(&args[0])?;
        // Convert and concatenate remaining args
        for arg in &args[1..] {
            let s = self.compile_to_str(arg)?;
            let func_id = self.runtime_fns["str_concat"];
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
            let call = self.builder.ins().call(func_ref, &[acc, s]);
            acc = self.builder.inst_results(call)[0];
        }
        Ok(Some(acc))
    }

    // ── If/else ─────────────────────────────────────────────────

    fn compile_if(
        &mut self,
        condition: ExprId,
        then_branch: ExprId,
        else_branch: Option<ExprId>,
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let cond_val = self.compile_expr(condition, &Ty::Bool)?.unwrap();

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        // Add block param for the result value if the if-expr produces a value
        let has_value = self.ty_cl(result_ty).is_some() && else_branch.is_some();
        if has_value {
            let cl_ty = self.ty_cl(result_ty).unwrap();
            self.builder.append_block_param(merge_block, cl_ty);
        }

        self.builder
            .ins()
            .brif(cond_val, then_block, &[], else_block, &[]);

        // Then branch
        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_val = self.compile_expr(then_branch, result_ty)?;
        if has_value {
            let val = then_val.unwrap_or_else(|| self.zero_value(result_ty));
            self.builder
                .ins()
                .jump(merge_block, &[BlockArg::Value(val)]);
        } else {
            self.builder.ins().jump(merge_block, &[]);
        }

        // Else branch
        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        if let Some(else_id) = else_branch {
            let else_val = self.compile_expr(else_id, result_ty)?;
            if has_value {
                let val = else_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        } else {
            // No else branch — Unit result
            self.builder.ins().jump(merge_block, &[]);
        }

        // Merge
        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        if has_value {
            let result = self.builder.block_params(merge_block)[0];
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    // ── Cond ────────────────────────────────────────────────────

    fn compile_cond(
        &mut self,
        clauses: &[(ExprId, ExprId)],
        else_clause: Option<ExprId>,
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let has_value = self.ty_cl(result_ty).is_some();
        let merge_block = self.builder.create_block();
        if has_value {
            let cl_ty = self.ty_cl(result_ty).unwrap();
            self.builder.append_block_param(merge_block, cl_ty);
        }

        for (test_id, body_id) in clauses {
            let then_block = self.builder.create_block();
            let next_block = self.builder.create_block();

            let cond_val = self.compile_expr(*test_id, &Ty::Bool)?.unwrap();
            self.builder
                .ins()
                .brif(cond_val, then_block, &[], next_block, &[]);

            self.builder.switch_to_block(then_block);
            self.builder.seal_block(then_block);
            let body_val = self.compile_expr(*body_id, result_ty)?;
            if has_value {
                let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }

            self.builder.switch_to_block(next_block);
            self.builder.seal_block(next_block);
        }

        // Else clause (or default)
        if let Some(else_id) = else_clause {
            let else_val = self.compile_expr(else_id, result_ty)?;
            if has_value {
                let val = else_val.unwrap_or_else(|| self.zero_value(result_ty));
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(val)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        } else {
            // No else — jump to merge with default
            if has_value {
                let zero = self.zero_value(result_ty);
                self.builder
                    .ins()
                    .jump(merge_block, &[BlockArg::Value(zero)]);
            } else {
                self.builder.ins().jump(merge_block, &[]);
            }
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        if has_value {
            let result = self.builder.block_params(merge_block)[0];
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    // ── When / Unless ───────────────────────────────────────────

    fn compile_when_unless(
        &mut self,
        condition: ExprId,
        body: &[ExprId],
        invert: bool,
    ) -> Result<(), CodegenError> {
        let cond_val = self.compile_expr(condition, &Ty::Bool)?.unwrap();

        let body_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        if invert {
            // unless: run body when condition is false
            self.builder
                .ins()
                .brif(cond_val, merge_block, &[], body_block, &[]);
        } else {
            // when: run body when condition is true
            self.builder
                .ins()
                .brif(cond_val, body_block, &[], merge_block, &[]);
        }

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);
        self.compile_body(body, &Ty::Unit)?;
        self.builder.ins().jump(merge_block, &[]);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        Ok(())
    }

    // ── Match (tagged ADTs) ────────────────────────────────────

    fn compile_match(
        &mut self,
        scrutinee: ExprId,
        arms: &[MatchArm],
        result_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let scrut_ty = self.expr_ty(scrutinee);
        let scrut_val = self
            .compile_expr(scrutinee, &scrut_ty)?
            .ok_or_else(|| CodegenError::new("match scrutinee produced no value"))?;

        let has_value = self.ty_cl(result_ty).is_some();
        let merge_block = self.builder.create_block();
        if has_value {
            let cl_ty = self.ty_cl(result_ty).unwrap();
            self.builder.append_block_param(merge_block, cl_ty);
        }

        for (i, arm) in arms.iter().enumerate() {
            let pat = &self.ast_module.patterns[arm.pattern];
            let is_last = i == arms.len() - 1;

            match &pat.kind {
                PatternKind::Constructor { name, args } => {
                    let info = self
                        .constructor_tags
                        .get(name)
                        .ok_or_else(|| {
                            CodegenError::new(format!("unknown constructor '{}' in match", name))
                        })?
                        .clone();

                    // Extract tag from scrutinee: tag = scrutinee & 0xFF
                    let mask = self.builder.ins().iconst(types::I64, 0xFF);
                    let extracted_tag = self.builder.ins().band(scrut_val, mask);

                    // Compute payload type for data-carrying constructors
                    let payload_ty = Self::constructor_payload_ty(&info, &scrut_ty);

                    if is_last {
                        // Last arm — compile body directly, binding payload if needed
                        if info.has_payload && args.len() == 1 {
                            let sub_pat = &self.ast_module.patterns[args[0]];
                            let eight = self.builder.ins().iconst(types::I64, 8);
                            let payload = self.builder.ins().sshr(scrut_val, eight);
                            self.bind_pattern_var(&sub_pat.kind, payload, &payload_ty)?;
                        }
                        let body_val = self.compile_body(&arm.body, result_ty)?;
                        if has_value {
                            let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                            self.builder
                                .ins()
                                .jump(merge_block, &[BlockArg::Value(val)]);
                        } else {
                            self.builder.ins().jump(merge_block, &[]);
                        }
                    } else {
                        let arm_block = self.builder.create_block();
                        let next_block = self.builder.create_block();
                        let tag_val = self.builder.ins().iconst(types::I64, info.tag);
                        let cmp = self
                            .builder
                            .ins()
                            .icmp(IntCC::Equal, extracted_tag, tag_val);
                        self.builder
                            .ins()
                            .brif(cmp, arm_block, &[], next_block, &[]);

                        // Compile arm body
                        self.builder.switch_to_block(arm_block);
                        self.builder.seal_block(arm_block);

                        // Extract payload and bind if data-carrying
                        if info.has_payload && args.len() == 1 {
                            let sub_pat = &self.ast_module.patterns[args[0]];
                            let eight = self.builder.ins().iconst(types::I64, 8);
                            let payload = self.builder.ins().sshr(scrut_val, eight);
                            self.bind_pattern_var(&sub_pat.kind, payload, &payload_ty)?;
                        }

                        let body_val = self.compile_body(&arm.body, result_ty)?;
                        if has_value {
                            let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                            self.builder
                                .ins()
                                .jump(merge_block, &[BlockArg::Value(val)]);
                        } else {
                            self.builder.ins().jump(merge_block, &[]);
                        }

                        // Continue to next arm
                        self.builder.switch_to_block(next_block);
                        self.builder.seal_block(next_block);
                    }
                }

                PatternKind::Wildcard => {
                    // Catch-all: compile body directly in current block
                    let body_val = self.compile_body(&arm.body, result_ty)?;
                    if has_value {
                        let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                        self.builder
                            .ins()
                            .jump(merge_block, &[BlockArg::Value(val)]);
                    } else {
                        self.builder.ins().jump(merge_block, &[]);
                    }
                }

                PatternKind::Var(name) => {
                    // Bind scrutinee to variable, then compile body
                    let cl_ty = self
                        .ty_cl(&scrut_ty)
                        .ok_or_else(|| CodegenError::new("match var pattern on non-value type"))?;
                    let var = self.declare_variable(cl_ty);
                    self.builder.def_var(var, scrut_val);
                    self.vars.insert(name.clone(), (var, scrut_ty.clone()));

                    let body_val = self.compile_body(&arm.body, result_ty)?;
                    if has_value {
                        let val = body_val.unwrap_or_else(|| self.zero_value(result_ty));
                        self.builder
                            .ins()
                            .jump(merge_block, &[BlockArg::Value(val)]);
                    } else {
                        self.builder.ins().jump(merge_block, &[]);
                    }
                }

                _ => {
                    return Err(CodegenError::new(format!(
                        "unsupported pattern kind in codegen match: {:?}",
                        std::mem::discriminant(&pat.kind)
                    )));
                }
            }
        }

        // Seal and switch to merge block
        self.builder.seal_block(merge_block);
        self.builder.switch_to_block(merge_block);

        if has_value {
            let result = self.builder.block_params(merge_block)[0];
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    // ── Try operator (?) ──────────────────────────────────────

    fn compile_try(
        &mut self,
        inner: ExprId,
        result_ok_ty: &Ty,
    ) -> Result<Option<Value>, CodegenError> {
        let inner_ty = self.expr_ty(inner);
        let result_val = self.compile_expr(inner, &inner_ty)?.unwrap();

        // Extract tag: tag = result & 0xFF
        let mask = self.builder.ins().iconst(types::I64, 0xFF);
        let tag = self.builder.ins().band(result_val, mask);

        // Ok tag is 0 (first variant in (deftype (Result 'ok 'err) (Ok 'ok) (Err 'err)))
        let zero = self.builder.ins().iconst(types::I64, 0);
        let is_ok = self.builder.ins().icmp(IntCC::Equal, tag, zero);

        let ok_block = self.builder.create_block();
        let err_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(is_ok, ok_block, &[], err_block, &[]);

        // Err block: clean up arenas + shadow stack, then early return
        self.builder.switch_to_block(err_block);
        self.builder.seal_block(err_block);

        // Clean up any active arenas (innermost first)
        for &arena_v in self.arena_stack.iter().rev() {
            // Unsuppress GC (decrements depth counter)
            let unsuppress_id = self.runtime_fns["gc_unsuppress"];
            let unsuppress_ref = self.module.declare_func_in_func(unsuppress_id, self.builder.func);
            self.builder.ins().call(unsuppress_ref, &[]);
            // Destroy arena
            let arena_ptr = self.builder.use_var(arena_v);
            let destroy_id = self.runtime_fns["arena_destroy"];
            let destroy_ref = self.module.declare_func_in_func(destroy_id, self.builder.func);
            self.builder.ins().call(destroy_ref, &[arena_ptr]);
        }

        // Pop ALL shadow stack entries for this function before early return
        if self.shadow_depth > 0 {
            let count_val = self.builder.ins().iconst(types::I64, self.shadow_depth as i64);
            let pop_id = self.runtime_fns["shadow_pop"];
            let pop_ref = self.module.declare_func_in_func(pop_id, self.builder.func);
            self.builder.ins().call(pop_ref, &[count_val]);
        }
        self.builder.ins().return_(&[result_val]);

        // Ok block: extract payload
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);

        let eight = self.builder.ins().iconst(types::I64, 8);
        let payload = self.builder.ins().sshr(result_val, eight);

        // The payload type is the ok_ty; if it's smaller than i64, truncate
        let final_val = match self.ty_cl(result_ok_ty) {
            Some(t) if t == types::I8 || t == types::I16 || t == types::I32 => {
                self.builder.ins().ireduce(t, payload)
            }
            _ => payload, // i64 or another packed ADT — use directly
        };

        Ok(Some(final_val))
    }

    // ── Pattern binding ────────────────────────────────────────

    /// Determine the payload type for a constructor from the scrutinee type.
    fn constructor_payload_ty(info: &ConstructorInfo, scrut_ty: &Ty) -> Ty {
        if let Some(idx) = info.field_type_param_index {
            if let Ty::Con(_, args) = scrut_ty {
                if idx < args.len() {
                    return args[idx].clone();
                }
            }
        }
        // Fallback: payload is i64 (e.g., concrete field type or unknown)
        Ty::I64
    }

    /// Bind a pattern variable to a payload value extracted from a constructor match.
    /// `payload` is the raw i64 extracted from the packed representation.
    /// `payload_ty` is the logical type of the payload (e.g., i32 for Ok's value in Result i32 err).
    fn bind_pattern_var(
        &mut self,
        pat_kind: &PatternKind,
        payload: Value,
        payload_ty: &Ty,
    ) -> Result<(), CodegenError> {
        match pat_kind {
            PatternKind::Var(name) => {
                let cl_ty = self.ty_cl(payload_ty).unwrap_or(types::I64);
                // Truncate from i64 to the target type if needed
                let final_val = if cl_ty != types::I64 && cl_ty != types::F32 && cl_ty != types::F64
                {
                    self.builder.ins().ireduce(cl_ty, payload)
                } else {
                    payload
                };
                let var = self.declare_variable(cl_ty);
                self.builder.def_var(var, final_val);
                self.vars.insert(name.clone(), (var, payload_ty.clone()));
                Ok(())
            }
            PatternKind::Wildcard => Ok(()),
            _ => Err(CodegenError::new(
                "only variable or wildcard patterns supported in constructor payload",
            )),
        }
    }

    // ── Type widening/narrowing for heap storage ───────────────

    /// Widen a value to i64 for heap storage (vectors, closure captures).
    fn widen_to_i64(&mut self, val: Value, ty: &Ty) -> Value {
        match ty {
            Ty::I8 | Ty::I16 | Ty::I32 => self.builder.ins().sextend(types::I64, val),
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::Bool => self.builder.ins().uextend(types::I64, val),
            Ty::F64 => self.builder.ins().bitcast(types::I64, MemFlags::new(), val),
            Ty::F32 => {
                let promoted = self.builder.ins().fpromote(types::F64, val);
                self.builder.ins().bitcast(types::I64, MemFlags::new(), promoted)
            }
            // I64, U64, Str, Vector, Fn, Con — already i64
            _ => val,
        }
    }

    /// Narrow a value from i64 back to its original type.
    fn narrow_from_i64(&mut self, val: Value, ty: &Ty) -> Value {
        match ty {
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::U8 | Ty::U16 | Ty::U32 => {
                let cl_ty = self.ty_cl(ty).unwrap();
                self.builder.ins().ireduce(cl_ty, val)
            }
            Ty::Bool => self.builder.ins().ireduce(types::I8, val),
            Ty::F64 => self.builder.ins().bitcast(types::F64, MemFlags::new(), val),
            Ty::F32 => {
                let f64_val = self.builder.ins().bitcast(types::F64, MemFlags::new(), val);
                self.builder.ins().fdemote(types::F32, f64_val)
            }
            // I64, U64, Str, Vector, Fn, Con — already i64
            _ => val,
        }
    }

    // ── Helpers ─────────────────────────────────────────────────

    fn zero_value(&mut self, ty: &Ty) -> Value {
        match ty {
            Ty::F32 => self.builder.ins().f32const(0.0),
            Ty::F64 => self.builder.ins().f64const(0.0),
            _ => {
                let cl_ty = self.ty_cl(ty).unwrap_or(types::I64);
                self.builder.ins().iconst(cl_ty, 0)
            }
        }
    }
}

// ── Dev Session (live reload) ────────────────────────────────────

/// Registers all JIT runtime symbols on a JITBuilder.
fn register_jit_symbols(builder: &mut JITBuilder) {
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
}

/// What changed between two versions of the source.
struct ChangeSet {
    body_changed: HashSet<SmolStr>,
    sig_changed: HashSet<SmolStr>,
    added: HashSet<SmolStr>,
    removed: HashSet<SmolStr>,
    types_changed: HashSet<SmolStr>,
}

/// Result of a selective reload.
pub struct ReloadResult {
    pub recompiled: Vec<SmolStr>,
    pub skipped: usize,
    pub type_warnings: Vec<SmolStr>,
}

/// Hash the body text of each function from source using its Defn span.
fn compute_body_hashes(ast_module: &weir_ast::Module, source: &str) -> HashMap<SmolStr, u64> {
    use std::hash::{Hash, Hasher};
    let mut hashes = HashMap::new();
    for (item, _) in &ast_module.items {
        if let Item::Defn(defn) = item {
            let start = defn.span.start as usize;
            let end = defn.span.end as usize;
            if end <= source.len() {
                let body_text = &source[start..end];
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                body_text.hash(&mut hasher);
                hashes.insert(defn.name.clone(), hasher.finish());
            }
        }
    }
    hashes
}

/// Extract type→constructor signatures for type change detection.
fn compute_type_sigs(
    ast_module: &weir_ast::Module,
    type_info: &TypeCheckResult,
) -> HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>> {
    let mut sigs: HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>> = HashMap::new();
    for (item, _) in &ast_module.items {
        match item {
            Item::Deftype(dt) => {
                let mut con_sigs = Vec::new();
                for variant in &dt.variants {
                    if let Some(ft) = type_info.fn_types.get(&variant.name) {
                        con_sigs.push((variant.name.clone(), ft.param_types.clone()));
                    } else {
                        con_sigs.push((variant.name.clone(), vec![]));
                    }
                }
                sigs.insert(dt.name.clone(), con_sigs);
            }
            Item::Defstruct(ds) => {
                if let Some(ft) = type_info.fn_types.get(&ds.name) {
                    sigs.insert(
                        ds.name.clone(),
                        vec![(ds.name.clone(), ft.param_types.clone())],
                    );
                }
            }
            _ => {}
        }
    }
    sigs
}

pub struct DevSession {
    ast_module: weir_ast::Module,
    type_info: TypeCheckResult,
    /// Stable function pointer table — indirect calls load from here.
    /// Boxed slice with stable address; AtomicPtr for thread-safe swaps.
    fn_table: Box<[AtomicPtr<u8>]>,
    /// Maps function name → slot index in fn_table.
    fn_slots: HashMap<SmolStr, usize>,
    /// Maps function name → (param types, return type) for signature reconstruction.
    fn_sigs: HashMap<SmolStr, (Vec<Ty>, Ty)>,
    /// Current JIT module (holds live code pages).
    current_module: JITModule,
    /// Old modules kept alive to prevent code page deallocation.
    _old_modules: Vec<JITModule>,
    /// Body hashes from the previous source for change detection.
    body_hashes: HashMap<SmolStr, u64>,
    /// Type constructor signatures from previous source for type change detection.
    type_sigs: HashMap<SmolStr, Vec<(SmolStr, Vec<Ty>)>>,
    /// Previous expanded source for short-circuit.
    prev_source: String,
}

type UserFnMap = HashMap<SmolStr, (FuncId, Vec<Ty>, Ty)>;
type FnSigMap = HashMap<SmolStr, (Vec<Ty>, Ty)>;

/// Helper: compile functions with indirect dispatch into a JITModule.
/// When `dirty_set` is `Some`, only compile functions in the set (others are declared but not defined).
/// Returns (jit_module, user_fns_map, fn_sigs_map).
fn compile_dev_module(
    ast_module: &weir_ast::Module,
    type_info: &TypeCheckResult,
    fn_table_ptr: *const AtomicPtr<u8>,
    fn_slots: &HashMap<SmolStr, usize>,
    dirty_set: Option<&HashSet<SmolStr>>,
) -> Result<(JITModule, UserFnMap, FnSigMap), CodegenError> {
    let isa = make_isa(false)?;
    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    register_jit_symbols(&mut builder);
    builder.symbol("weir_fn_table", fn_table_ptr as *const u8);

    let jit_module = JITModule::new(builder);
    let mut compiler = Compiler::new(ast_module, type_info, jit_module);
    compiler.declare_runtime_helpers()?;
    compiler.declare_user_functions(Linkage::Local)?;

    let table_data_id = compiler
        .module
        .declare_data("weir_fn_table", Linkage::Import, false, false)
        .map_err(|e| CodegenError::new(format!("declare weir_fn_table data: {}", e)))?;

    let mut fn_sigs = HashMap::new();
    for (name, (_fid, param_tys, ret_ty)) in &compiler.user_fns {
        fn_sigs.insert(name.clone(), (param_tys.clone(), ret_ty.clone()));
    }

    compiler.compile_user_functions_selective(
        Some(fn_slots),
        Some(table_data_id),
        dirty_set,
    )?;

    compiler
        .module
        .finalize_definitions()
        .map_err(|e| CodegenError::new(format!("finalize: {}", e)))?;

    let user_fns = compiler.user_fns.clone();
    Ok((compiler.module, user_fns, fn_sigs))
}

impl DevSession {
    /// Create a new dev session: parse, typecheck, compile with indirect dispatch.
    pub fn new(source: &str) -> Result<Self, CodegenError> {
        let (ast_module, parse_errors) = weir_parser::parse(source);
        if !parse_errors.is_empty() {
            return Err(CodegenError::new(format!(
                "parse error: {}",
                parse_errors[0].message
            )));
        }

        let type_info = weir_typeck::check(&ast_module);
        if !type_info.errors.is_empty() {
            return Err(CodegenError::new(format!(
                "type error: {}",
                type_info.errors[0].message
            )));
        }

        // Assign slots to all user functions
        let mut fn_slots = HashMap::new();
        let mut slot = 0usize;
        for (item, _) in &ast_module.items {
            if let Item::Defn(defn) = item {
                fn_slots.insert(defn.name.clone(), slot);
                slot += 1;
            }
        }

        // Create function table (all null initially)
        let fn_table: Box<[AtomicPtr<u8>]> = (0..slot)
            .map(|_| AtomicPtr::new(std::ptr::null_mut()))
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let (jit_module, user_fns, fn_sigs) =
            compile_dev_module(&ast_module, &type_info, fn_table.as_ptr(), &fn_slots, None)?;

        // Populate fn_table with finalized function pointers
        for (name, &slot_idx) in &fn_slots {
            let (func_id, _, _) = &user_fns[name];
            let ptr = jit_module.get_finalized_function(*func_id);
            fn_table[slot_idx].store(ptr as *mut u8, Ordering::Release);
        }

        let body_hashes = compute_body_hashes(&ast_module, source);
        let type_sigs = compute_type_sigs(&ast_module, &type_info);

        Ok(DevSession {
            ast_module,
            type_info,
            fn_table,
            fn_slots,
            fn_sigs,
            current_module: jit_module,
            _old_modules: Vec::new(),
            body_hashes,
            type_sigs,
            prev_source: source.to_string(),
        })
    }

    /// Selectively recompile only dirty functions from new source and swap pointers.
    /// Returns a `ReloadResult` with recompilation stats and type warnings.
    /// On parse/type error, returns Err but keeps old code running.
    pub fn reload(&mut self, new_source: &str) -> Result<ReloadResult, CodegenError> {
        // Short-circuit: if expanded source is identical, skip everything
        if new_source == self.prev_source {
            return Ok(ReloadResult {
                recompiled: vec![],
                skipped: self.fn_slots.len(),
                type_warnings: vec![],
            });
        }

        let (ast_module, parse_errors) = weir_parser::parse(new_source);
        if !parse_errors.is_empty() {
            return Err(CodegenError::new(format!(
                "parse error: {}",
                parse_errors[0].message
            )));
        }

        let type_info = weir_typeck::check(&ast_module);
        if !type_info.errors.is_empty() {
            return Err(CodegenError::new(format!(
                "type error: {}",
                type_info.errors[0].message
            )));
        }

        // Compute change set
        let new_body_hashes = compute_body_hashes(&ast_module, new_source);
        let new_type_sigs = compute_type_sigs(&ast_module, &type_info);

        let old_fn_names: HashSet<SmolStr> = self.fn_slots.keys().cloned().collect();
        let new_fn_names: HashSet<SmolStr> = ast_module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Defn(defn) = item {
                    Some(defn.name.clone())
                } else {
                    None
                }
            })
            .collect();

        let mut change_set = ChangeSet {
            body_changed: HashSet::new(),
            sig_changed: HashSet::new(),
            added: HashSet::new(),
            removed: HashSet::new(),
            types_changed: HashSet::new(),
        };

        // Detect added/removed functions
        for name in &new_fn_names {
            if !old_fn_names.contains(name) {
                change_set.added.insert(name.clone());
            }
        }
        for name in &old_fn_names {
            if !new_fn_names.contains(name) {
                change_set.removed.insert(name.clone());
            }
        }

        // Detect body and signature changes for existing functions
        for name in new_fn_names.intersection(&old_fn_names) {
            // Signature change?
            let old_sig = self.fn_sigs.get(name);
            let new_sig = type_info.fn_types.get(name).map(|ft| {
                (ft.param_types.clone(), ft.return_type.clone())
            });
            let sig_changed = match (old_sig, &new_sig) {
                (Some(old), Some(new)) => old != new,
                _ => true,
            };
            if sig_changed {
                change_set.sig_changed.insert(name.clone());
                continue; // sig change subsumes body change
            }

            // Body change?
            let old_hash = self.body_hashes.get(name);
            let new_hash = new_body_hashes.get(name);
            if old_hash != new_hash {
                change_set.body_changed.insert(name.clone());
            }
        }

        // Detect type changes
        for (type_name, new_cons) in &new_type_sigs {
            match self.type_sigs.get(type_name) {
                Some(old_cons) if old_cons == new_cons => {}
                _ => {
                    change_set.types_changed.insert(type_name.clone());
                }
            }
        }
        // Check for removed types
        for type_name in self.type_sigs.keys() {
            if !new_type_sigs.contains_key(type_name) {
                change_set.types_changed.insert(type_name.clone());
            }
        }

        // Compute dirty set using dependency graph + change set
        let deps = &type_info.deps;
        let mut dirty: HashSet<SmolStr> = HashSet::new();

        // Body-only changes: just the function itself
        for name in &change_set.body_changed {
            dirty.insert(name.clone());
        }

        // Signature changes: function + transitive callers
        for name in &change_set.sig_changed {
            dirty.insert(name.clone());
            self.add_transitive_callers(name, deps, &mut dirty);
        }

        // Type changes: all functions using the type + their transitive callers
        for type_name in &change_set.types_changed {
            if let Some(users) = deps.type_users.get(type_name) {
                let users_snapshot: Vec<SmolStr> = users.iter().cloned().collect();
                for user in &users_snapshot {
                    dirty.insert(user.clone());
                    self.add_transitive_callers(user, deps, &mut dirty);
                }
            }
        }

        // Added functions
        for name in &change_set.added {
            dirty.insert(name.clone());
        }

        // Expand dirty set to include specializations of dirty functions
        let mut spec_dirty: Vec<SmolStr> = Vec::new();
        for spec in &type_info.specializations {
            if dirty.contains(&spec.original_name) {
                spec_dirty.push(spec.mangled_name.clone());
            }
        }
        dirty.extend(spec_dirty);

        // Type warnings
        let type_warnings: Vec<SmolStr> = change_set.types_changed.iter().cloned().collect();

        // If nothing is dirty, skip compilation
        if dirty.is_empty() {
            self.prev_source = new_source.to_string();
            return Ok(ReloadResult {
                recompiled: vec![],
                skipped: self.fn_slots.len(),
                type_warnings,
            });
        }

        // Reassign slots for the new source
        let mut new_fn_slots = HashMap::new();
        let mut slot = 0usize;
        for (item, _) in &ast_module.items {
            if let Item::Defn(defn) = item {
                new_fn_slots.insert(defn.name.clone(), slot);
                slot += 1;
            }
        }

        // If function count changed, grow the table
        if slot > self.fn_table.len() {
            let mut new_table: Vec<AtomicPtr<u8>> = Vec::with_capacity(slot);
            for i in 0..slot {
                if i < self.fn_table.len() {
                    let old_ptr = self.fn_table[i].load(Ordering::Acquire);
                    new_table.push(AtomicPtr::new(old_ptr));
                } else {
                    new_table.push(AtomicPtr::new(std::ptr::null_mut()));
                }
            }
            self.fn_table = new_table.into_boxed_slice();
        }

        let (jit_module, user_fns, new_fn_sigs) = compile_dev_module(
            &ast_module,
            &type_info,
            self.fn_table.as_ptr(),
            &new_fn_slots,
            Some(&dirty),
        )?;

        // Swap only dirty function pointers in the table
        let mut recompiled = Vec::new();
        for (name, &slot_idx) in &new_fn_slots {
            if !dirty.contains(name) {
                continue;
            }
            if let Some((func_id, _, _)) = user_fns.get(name) {
                let ptr = jit_module.get_finalized_function(*func_id);
                self.fn_table[slot_idx].store(ptr as *mut u8, Ordering::Release);
                recompiled.push(name.clone());
            }
        }

        let skipped = new_fn_slots.len() - recompiled.len();

        // Keep old module alive
        let old_module = std::mem::replace(&mut self.current_module, jit_module);
        self._old_modules.push(old_module);

        // Update session state
        self.ast_module = ast_module;
        self.type_info = type_info;
        self.fn_slots = new_fn_slots;
        self.fn_sigs = new_fn_sigs;
        self.body_hashes = new_body_hashes;
        self.type_sigs = new_type_sigs;
        self.prev_source = new_source.to_string();

        Ok(ReloadResult {
            recompiled,
            skipped,
            type_warnings,
        })
    }

    /// BFS to find all transitive callers of a function.
    fn add_transitive_callers(
        &self,
        name: &SmolStr,
        deps: &weir_typeck::DependencyGraph,
        dirty: &mut HashSet<SmolStr>,
    ) {
        let mut queue = std::collections::VecDeque::new();
        queue.push_back(name.clone());
        while let Some(current) = queue.pop_front() {
            if let Some(callers) = deps.callers.get(&current) {
                for caller in callers {
                    if dirty.insert(caller.clone()) {
                        queue.push_back(caller.clone());
                    }
                }
            }
        }
    }

    /// Get a callable function pointer for main() (reads through the fn_table).
    fn get_main_fn_ptr(&self) -> Result<*const u8, CodegenError> {
        let slot = self
            .fn_slots
            .get("main")
            .ok_or_else(|| CodegenError::new("no 'main' function defined"))?;
        let ptr = self.fn_table[*slot].load(Ordering::Acquire);
        if ptr.is_null() {
            return Err(CodegenError::new("main function pointer is null"));
        }
        Ok(ptr as *const u8)
    }

    /// Run main() and capture output (for testing).
    pub fn run_main(&self) -> Result<String, CodegenError> {
        let ptr = self.get_main_fn_ptr()?;
        output_reset();
        unsafe {
            let main_fn: fn() = std::mem::transmute(ptr);
            main_fn();
        }
        Ok(output_take())
    }

    /// Run the dev loop: spawn main() on a background thread, watch for file changes.
    /// `transform_source` is called on the raw file contents before reload (e.g. prepend prelude + expand macros).
    pub fn run_dev_loop<F>(
        mut self,
        source_path: &Path,
        transform_source: F,
    ) -> Result<(), CodegenError>
    where
        F: Fn(&str) -> Result<String, String>,
    {
        use notify::{RecursiveMode, Watcher};
        use std::sync::mpsc;
        use std::time::Duration;

        let main_ptr = self.get_main_fn_ptr()?;

        // Cast to usize (which is Send) to transfer across thread boundary.
        // Safety: the fn_table and _old_modules keep the code pages alive.
        let main_ptr_int = main_ptr as usize;

        // Spawn main() on a background thread.
        // main_fn calls through the fn_table, so pointer swaps take effect automatically.
        let main_handle = std::thread::spawn(move || unsafe {
            let main_fn: fn() = std::mem::transmute(main_ptr_int);
            main_fn();
        });

        eprintln!(
            "[weir dev] running — watching {} for changes",
            source_path.display()
        );

        // Set up file watcher
        let (tx, rx) = mpsc::channel();
        let mut watcher = notify::recommended_watcher(move |res: notify::Result<notify::Event>| {
            if let Ok(event) = res {
                if event.kind.is_modify() {
                    let _ = tx.send(());
                }
            }
        })
        .map_err(|e| CodegenError::new(format!("watcher setup: {}", e)))?;

        let watch_path = source_path.parent().unwrap_or(Path::new("."));
        watcher
            .watch(watch_path, RecursiveMode::NonRecursive)
            .map_err(|e| CodegenError::new(format!("watch: {}", e)))?;

        let source_path = source_path.to_path_buf();

        loop {
            // Check if main thread has exited
            if main_handle.is_finished() {
                eprintln!("[weir dev] main() exited");
                break;
            }

            // Wait for a file change event (with timeout to check main thread)
            match rx.recv_timeout(Duration::from_millis(200)) {
                Ok(()) => {
                    // Debounce: drain any queued events
                    std::thread::sleep(Duration::from_millis(50));
                    while rx.try_recv().is_ok() {}

                    // Read new source and apply transform (prelude + macro expansion)
                    let raw_source = match std::fs::read_to_string(&source_path) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("[weir dev] error reading file: {}", e);
                            continue;
                        }
                    };

                    let new_source = match transform_source(&raw_source) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!("[weir dev] transform error: {}", e);
                            continue;
                        }
                    };

                    match self.reload(&new_source) {
                        Ok(result) => {
                            for tw in &result.type_warnings {
                                eprintln!(
                                    "[weir dev] type '{}' was redefined — live instances have stale layout",
                                    tw
                                );
                            }
                            if result.recompiled.is_empty() {
                                eprintln!("[weir dev] no changes detected");
                            } else {
                                let names: Vec<&str> =
                                    result.recompiled.iter().map(|s| s.as_str()).collect();
                                eprintln!(
                                    "[weir dev] reloaded {} fn(s): {} (skipped {})",
                                    result.recompiled.len(),
                                    names.join(", "),
                                    result.skipped
                                );
                            }
                        }
                        Err(e) => {
                            eprintln!("[weir dev] reload error (old code still running): {}", e);
                        }
                    }
                }
                Err(mpsc::RecvTimeoutError::Timeout) => continue,
                Err(mpsc::RecvTimeoutError::Disconnected) => break,
            }
        }

        Ok(())
    }
}

// ── Tests ────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn expand(source: &str) -> String {
        let result = weir_macros::expand(source);
        assert!(
            result.errors.is_empty(),
            "macro errors: {:?}",
            result.errors
        );
        result.source
    }

    fn compile_run(source: &str) -> String {
        let expanded = expand(source);
        let (module, parse_errors) = weir_parser::parse(&expanded);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let type_info = weir_typeck::check(&module);
        assert!(
            type_info.errors.is_empty(),
            "type errors: {:?}",
            type_info.errors
        );
        compile_and_run(&module, &type_info).expect("codegen error")
    }

    /// Parse + typecheck + compile, expecting codegen to return an Err.
    /// Returns the error message string.
    fn compile_err(source: &str) -> String {
        let expanded = expand(source);
        let (module, parse_errors) = weir_parser::parse(&expanded);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let type_info = weir_typeck::check(&module);
        assert!(
            type_info.errors.is_empty(),
            "type errors: {:?}",
            type_info.errors
        );
        match compile_and_run(&module, &type_info) {
            Ok(output) => panic!("expected codegen error, got output: {}", output),
            Err(e) => e.message,
        }
    }

    fn interp_run(source: &str) -> String {
        let expanded = expand(source);
        let (module, parse_errors) = weir_parser::parse(&expanded);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        weir_interp::interpret(&module).expect("interpreter error")
    }

    /// Run through both interpreter and compiler, assert same output.
    fn oracle_test(source: &str) {
        let compiled = compile_run(source);
        let interpreted = interp_run(source);
        assert_eq!(
            compiled, interpreted,
            "\n--- COMPILED ---\n{}\n--- INTERPRETED ---\n{}\n--- SOURCE ---\n{}",
            compiled, interpreted, source
        );
    }

    // ── Basic arithmetic ────────────────────────────────────────

    #[test]
    fn test_integer_add() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1 2)))",
        );
        assert_eq!(out, "3\n");
    }

    #[test]
    fn test_integer_sub() {
        let out = compile_run(
            "(defn main () : Unit
               (println (- 10 3)))",
        );
        assert_eq!(out, "7\n");
    }

    #[test]
    fn test_integer_mul() {
        let out = compile_run(
            "(defn main () : Unit
               (println (* 4 5)))",
        );
        assert_eq!(out, "20\n");
    }

    #[test]
    fn test_integer_div() {
        let out = compile_run(
            "(defn main () : Unit
               (println (/ 15 4)))",
        );
        assert_eq!(out, "3\n");
    }

    #[test]
    fn test_integer_mod() {
        let out = compile_run(
            "(defn main () : Unit
               (println (mod 17 5)))",
        );
        assert_eq!(out, "2\n");
    }

    #[test]
    fn test_variadic_add() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1 2 3 4 5)))",
        );
        assert_eq!(out, "15\n");
    }

    #[test]
    fn test_nested_arithmetic() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ (* 2 3) (- 10 4))))",
        );
        assert_eq!(out, "12\n");
    }

    #[test]
    fn test_negation() {
        let out = compile_run(
            "(defn main () : Unit
               (println (- 5)))",
        );
        assert_eq!(out, "-5\n");
    }

    #[test]
    fn test_float_arithmetic() {
        let out = compile_run(
            "(defn main () : Unit
               (println (+ 1.5 2.5)))",
        );
        assert_eq!(out, "4.0\n");
    }

    // ── Comparisons ─────────────────────────────────────────────

    #[test]
    fn test_comparisons() {
        let out = compile_run(
            "(defn main () : Unit
               (println (< 1 2))
               (println (> 1 2))
               (println (= 3 3))
               (println (!= 3 4))
               (println (<= 5 5))
               (println (>= 5 6)))",
        );
        assert_eq!(out, "true\nfalse\ntrue\ntrue\ntrue\nfalse\n");
    }

    // ── Boolean ops ─────────────────────────────────────────────

    #[test]
    fn test_boolean_ops() {
        let out = compile_run(
            "(defn main () : Unit
               (println (not true))
               (println (not false))
               (println (and true true))
               (println (and true false))
               (println (or false false))
               (println (or false true)))",
        );
        assert_eq!(out, "false\ntrue\ntrue\nfalse\nfalse\ntrue\n");
    }

    // ── If/else ─────────────────────────────────────────────────

    #[test]
    fn test_if_else() {
        let out = compile_run(
            "(defn main () : Unit
               (println (if (> 5 3) 1 0))
               (println (if (< 5 3) 1 0)))",
        );
        assert_eq!(out, "1\n0\n");
    }

    #[test]
    fn test_when_unless() {
        let out = compile_run(
            "(defn main () : Unit
               (when true (println 42))
               (when false (println 99))
               (unless false (println 7))
               (unless true (println 88)))",
        );
        assert_eq!(out, "42\n7\n");
    }

    #[test]
    fn test_cond() {
        let out = compile_run(
            "(defn classify ((n : i32)) : i32
               (cond
                 ((< n 0) (- 1))
                 ((= n 0) 0)
                 (else 1)))
             (defn main () : Unit
               (println (classify (- 5)))
               (println (classify 0))
               (println (classify 10)))",
        );
        assert_eq!(out, "-1\n0\n1\n");
    }

    // ── Let and mutation ────────────────────────────────────────

    #[test]
    fn test_let_binding() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((x : i32) 10)
                     ((y : i32) 20))
                 (println (+ x y))))",
        );
        assert_eq!(out, "30\n");
    }

    #[test]
    fn test_mutable_binding() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((mut x : i32) 0))
                 (set! x 42)
                 (println x)))",
        );
        assert_eq!(out, "42\n");
    }

    #[test]
    fn test_do_block() {
        let out = compile_run(
            "(defn main () : Unit
               (let (((result : i32) (do (println 1) 42)))
                 (println result)))",
        );
        assert_eq!(out, "1\n42\n");
    }

    // ── Function calls ──────────────────────────────────────────

    #[test]
    fn test_function_call() {
        let out = compile_run(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () : Unit (println (add 3 4)))",
        );
        assert_eq!(out, "7\n");
    }

    #[test]
    fn test_recursion() {
        let out = compile_run(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 10)))",
        );
        assert_eq!(out, "3628800\n");
    }

    #[test]
    fn test_fibonacci() {
        let out = compile_run(
            "(defn fib ((n : i32)) : i32
               (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
             (defn main () : Unit
               (println (fib 0))
               (println (fib 1))
               (println (fib 10)))",
        );
        assert_eq!(out, "0\n1\n55\n");
    }

    #[test]
    fn test_mutual_recursion() {
        let out = compile_run(
            "(defn is-even ((n : i32)) : Bool
               (if (= n 0) true (is-odd (- n 1))))
             (defn is-odd ((n : i32)) : Bool
               (if (= n 0) false (is-even (- n 1))))
             (defn main () : Unit
               (println (is-even 10))
               (println (is-odd 10)))",
        );
        assert_eq!(out, "true\nfalse\n");
    }

    // ── Oracle tests (compiler vs interpreter agree) ────────────

    #[test]
    fn oracle_arithmetic() {
        oracle_test(
            "(defn main () : Unit
               (println (+ 1 2))
               (println (- 10 3))
               (println (* 4 5))
               (println (/ 15 4))
               (println (mod 17 5))
               (println (+ 1 2 3 4 5)))",
        );
    }

    #[test]
    fn oracle_comparisons() {
        oracle_test(
            "(defn main () : Unit
               (println (< 1 2))
               (println (> 1 2))
               (println (= 3 3))
               (println (!= 3 4))
               (println (<= 5 5))
               (println (>= 5 6)))",
        );
    }

    #[test]
    fn oracle_booleans() {
        oracle_test(
            "(defn main () : Unit
               (println (not true))
               (println (and true true))
               (println (and true false))
               (println (or false true)))",
        );
    }

    #[test]
    fn oracle_if_else() {
        oracle_test(
            "(defn main () : Unit
               (println (if (> 5 3) 1 0))
               (println (if (< 5 3) 1 0)))",
        );
    }

    #[test]
    fn oracle_function_calls() {
        oracle_test(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () : Unit (println (add 3 4)))",
        );
    }

    #[test]
    fn oracle_factorial() {
        oracle_test(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 1))
               (println (factorial 5))
               (println (factorial 10)))",
        );
    }

    #[test]
    fn oracle_fibonacci() {
        oracle_test(
            "(defn fib ((n : i32)) : i32
               (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
             (defn main () : Unit
               (println (fib 0))
               (println (fib 1))
               (println (fib 10)))",
        );
    }

    // ── Fixture-based tests ─────────────────────────────────────

    const PRELUDE_SOURCE: &str = include_str!("../../../std/prelude.weir");

    fn fixture_path(name: &str) -> String {
        format!(
            "{}/tests/fixtures/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-codegen", ""),
            name
        )
    }

    fn run_fixture_compiled(name: &str) -> String {
        let path = fixture_path(name);
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        let source = format!("{}\n{}", PRELUDE_SOURCE, source);
        compile_run(&source)
    }

    fn run_fixture_interpreted(name: &str) -> String {
        let path = fixture_path(name);
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        let source = format!("{}\n{}", PRELUDE_SOURCE, source);
        interp_run(&source)
    }

    #[test]
    fn fixture_codegen_arithmetic() {
        let compiled = run_fixture_compiled("codegen-arithmetic");
        let interpreted = run_fixture_interpreted("codegen-arithmetic");
        assert_eq!(
            compiled, interpreted,
            "codegen-arithmetic: compiler and interpreter disagree"
        );
    }

    #[test]
    fn fixture_codegen_control_flow() {
        let compiled = run_fixture_compiled("codegen-control-flow");
        let interpreted = run_fixture_interpreted("codegen-control-flow");
        assert_eq!(
            compiled, interpreted,
            "codegen-control-flow: compiler and interpreter disagree"
        );
    }

    #[test]
    fn fixture_codegen_functions() {
        let compiled = run_fixture_compiled("codegen-functions");
        let interpreted = run_fixture_interpreted("codegen-functions");
        assert_eq!(
            compiled, interpreted,
            "codegen-functions: compiler and interpreter disagree"
        );
    }

    #[test]
    fn fixture_codegen_let_mutation() {
        let compiled = run_fixture_compiled("codegen-let-mutation");
        let interpreted = run_fixture_interpreted("codegen-let-mutation");
        assert_eq!(
            compiled, interpreted,
            "codegen-let-mutation: compiler and interpreter disagree"
        );
    }

    #[test]
    fn fixture_macros() {
        let compiled = run_fixture_compiled("macros");
        let interpreted = run_fixture_interpreted("macros");
        assert_eq!(
            compiled, interpreted,
            "macros: compiler and interpreter disagree"
        );
        assert_eq!(compiled, "12\n30\n33\n100\n200\n300\n");
    }

    // ── AOT compilation tests ───────────────────────────────────

    #[test]
    fn test_aot_compile_to_object() {
        let source = "(defn main () : Unit (println 42))";
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty());
        let obj_bytes = compile_to_object(&module, &type_info).expect("compile_to_object failed");
        assert!(!obj_bytes.is_empty(), "object bytes should be non-empty");
    }

    #[test]
    fn test_aot_build_and_run() {
        let source = "(defn main () : Unit
           (println (+ 1 2))
           (println (* 3 4)))";
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty());

        let tmp_dir = std::env::temp_dir();
        let binary_path = tmp_dir.join("weir_test_aot");
        build_executable(&module, &type_info, &binary_path).expect("build_executable failed");

        let output = std::process::Command::new(&binary_path)
            .output()
            .expect("failed to run AOT binary");
        let _ = std::fs::remove_file(&binary_path);

        assert!(output.status.success(), "AOT binary exited with error");
        let stdout = String::from_utf8(output.stdout).unwrap();
        assert_eq!(stdout, "3\n12\n");
    }

    #[test]
    fn test_aot_oracle_factorial() {
        let source = "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 1))
               (println (factorial 5))
               (println (factorial 10)))";
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty());

        let jit_output = compile_and_run(&module, &type_info).expect("JIT failed");
        let interp_output = weir_interp::interpret(&module).expect("interp failed");

        let tmp_dir = std::env::temp_dir();
        let binary_path = tmp_dir.join("weir_test_aot_factorial");
        build_executable(&module, &type_info, &binary_path).expect("build_executable failed");
        let output = std::process::Command::new(&binary_path)
            .output()
            .expect("failed to run AOT binary");
        let _ = std::fs::remove_file(&binary_path);

        let aot_output = String::from_utf8(output.stdout).unwrap();
        assert_eq!(jit_output, interp_output, "JIT and interpreter disagree");
        assert_eq!(jit_output, aot_output, "JIT and AOT disagree");
    }

    /// Oracle test across all three backends for each codegen fixture.
    fn fixture_oracle_all(name: &str) {
        let path = fixture_path(name);
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));

        let (module, parse_errors) = weir_parser::parse(&source);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty());

        let jit_output = compile_and_run(&module, &type_info).expect("JIT failed");
        let interp_output = weir_interp::interpret(&module).expect("interp failed");

        let tmp_dir = std::env::temp_dir();
        let binary_path = tmp_dir.join(format!("weir_test_aot_{}", name));
        build_executable(&module, &type_info, &binary_path).expect("build_executable failed");
        let output = std::process::Command::new(&binary_path)
            .output()
            .expect("failed to run AOT binary");
        let _ = std::fs::remove_file(&binary_path);

        let aot_output = String::from_utf8(output.stdout).unwrap();
        assert_eq!(
            jit_output, interp_output,
            "{}: JIT and interpreter disagree",
            name
        );
        assert_eq!(
            jit_output, aot_output,
            "{}: JIT and AOT disagree\nJIT: {:?}\nAOT: {:?}",
            name, jit_output, aot_output
        );
    }

    #[test]
    fn fixture_aot_arithmetic() {
        fixture_oracle_all("codegen-arithmetic");
    }

    #[test]
    fn fixture_aot_control_flow() {
        fixture_oracle_all("codegen-control-flow");
    }

    #[test]
    fn fixture_aot_functions() {
        fixture_oracle_all("codegen-functions");
    }

    #[test]
    fn fixture_aot_let_mutation() {
        fixture_oracle_all("codegen-let-mutation");
    }

    // ── Dev mode (indirect dispatch) tests ───────────────────────

    #[test]
    fn test_dev_session_basic() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () : Unit (println (add 3 4)))";
        let session = DevSession::new(source).expect("DevSession::new failed");
        let output = session.run_main().expect("run_main failed");
        assert_eq!(output, "7\n");
    }

    #[test]
    fn test_dev_session_matches_jit() {
        // Verify indirect dispatch produces same output as direct-call JIT
        let source = "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))
             (defn main () : Unit
               (println (factorial 1))
               (println (factorial 5))
               (println (factorial 10)))";
        let jit_output = compile_run(source);
        let session = DevSession::new(source).expect("DevSession::new failed");
        let dev_output = session.run_main().expect("run_main failed");
        assert_eq!(jit_output, dev_output, "JIT and dev-mode disagree");
    }

    #[test]
    fn test_dev_session_reload() {
        let source_v1 = "(defn greet () : Unit (println 1))
             (defn main () : Unit (greet))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        let out1 = session.run_main().expect("run_main v1 failed");
        assert_eq!(out1, "1\n");

        let source_v2 = "(defn greet () : Unit (println 2))
             (defn main () : Unit (greet))";
        let result = session.reload(source_v2).expect("reload failed");
        assert!(!result.recompiled.is_empty(), "should have reloaded functions");

        let out2 = session.run_main().expect("run_main v2 failed");
        assert_eq!(out2, "2\n");
    }

    #[test]
    fn test_dev_session_error_resilience() {
        let source = "(defn main () : Unit (println 42))";
        let mut session = DevSession::new(source).expect("DevSession::new failed");
        let out1 = session.run_main().expect("run_main failed");
        assert_eq!(out1, "42\n");

        // Reload with broken source — should fail
        let broken = "(defn main () : Unit (println";
        let result = session.reload(broken);
        assert!(result.is_err(), "reload with broken source should fail");

        // Old code should still work
        let out2 = session
            .run_main()
            .expect("run_main after failed reload should work");
        assert_eq!(out2, "42\n");
    }

    #[test]
    fn test_dev_session_all_fixtures() {
        // Verify all codegen fixtures produce same output in dev mode as JIT
        for name in &[
            "codegen-arithmetic",
            "codegen-control-flow",
            "codegen-functions",
            "codegen-let-mutation",
        ] {
            let path = fixture_path(name);
            let source = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
            let jit_output = compile_run(&source);
            let session = DevSession::new(&source)
                .unwrap_or_else(|e| panic!("DevSession::new failed for {}: {}", name, e));
            let dev_output = session
                .run_main()
                .unwrap_or_else(|e| panic!("run_main failed for {}: {}", name, e));
            assert_eq!(
                jit_output, dev_output,
                "{}: JIT and dev-mode disagree",
                name
            );
        }
    }

    // ── Selective reload tests ────────────────────────────────────

    #[test]
    fn test_dev_no_change() {
        let source = "(defn helper () : i32 42)
             (defn main () : Unit (println (helper)))";
        let mut session = DevSession::new(source).expect("DevSession::new failed");
        let out1 = session.run_main().expect("run_main failed");
        assert_eq!(out1, "42\n");

        // Reload with identical source → 0 recompilations
        let result = session.reload(source).expect("reload failed");
        assert!(result.recompiled.is_empty(), "no changes should mean no recompilations");
        assert_eq!(result.skipped, 2); // helper + main

        let out2 = session.run_main().expect("run_main after no-change reload");
        assert_eq!(out2, "42\n");
    }

    #[test]
    fn test_dev_body_change() {
        let source_v1 = "(defn helper () : i32 42)
             (defn main () : Unit (println (helper)))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        assert_eq!(session.run_main().unwrap(), "42\n");

        // Change only helper's body (same signature)
        let source_v2 = "(defn helper () : i32 99)
             (defn main () : Unit (println (helper)))";
        let result = session.reload(source_v2).expect("reload failed");

        // Only helper should be recompiled (body change), not main
        assert!(
            result.recompiled.iter().any(|n| n == "helper"),
            "helper should be recompiled: {:?}",
            result.recompiled
        );
        // main should NOT be in the recompiled list (body didn't change,
        // and body-only changes don't cascade to callers)
        assert!(
            !result.recompiled.iter().any(|n| n == "main"),
            "main should NOT be recompiled for body-only change: {:?}",
            result.recompiled
        );

        assert_eq!(session.run_main().unwrap(), "99\n");
    }

    #[test]
    fn test_dev_sig_change() {
        let source_v1 = "(defn helper () : i32 42)
             (defn main () : Unit (println (helper)))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        assert_eq!(session.run_main().unwrap(), "42\n");

        // Change helper's return type (i32 → i64) — signature change
        let source_v2 = "(defn helper () : i64 99)
             (defn main () : Unit (println (helper)))";
        let result = session.reload(source_v2).expect("reload failed");

        // Both helper and main should be recompiled (sig change cascades)
        assert!(
            result.recompiled.iter().any(|n| n == "helper"),
            "helper should be recompiled"
        );
        assert!(
            result.recompiled.iter().any(|n| n == "main"),
            "main should be recompiled (calls helper whose sig changed)"
        );

        assert_eq!(session.run_main().unwrap(), "99\n");
    }

    #[test]
    fn test_dev_type_change() {
        let source_v1 = "(deftype Color (Red) (Blue))
             (defn pick () : Color Red)
             (defn main () : Unit (println 1))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        assert_eq!(session.run_main().unwrap(), "1\n");

        // Add a variant to Color — type change
        let source_v2 = "(deftype Color (Red) (Blue) (Green))
             (defn pick () : Color Red)
             (defn main () : Unit (println 1))";
        let result = session.reload(source_v2).expect("reload failed");

        // pick uses Color, so it should be recompiled
        assert!(
            result.recompiled.iter().any(|n| n == "pick"),
            "pick should be recompiled (uses changed type): {:?}",
            result.recompiled
        );
        // Type warning should mention Color
        assert!(
            result.type_warnings.iter().any(|t| t == "Color"),
            "should warn about Color type change: {:?}",
            result.type_warnings
        );
    }

    #[test]
    fn test_dev_added_fn() {
        let source_v1 = "(defn main () : Unit (println 1))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        assert_eq!(session.run_main().unwrap(), "1\n");

        // Add a new function
        let source_v2 = "(defn helper () : i32 99)
             (defn main () : Unit (println (helper)))";
        let result = session.reload(source_v2).expect("reload failed");

        assert!(
            result.recompiled.iter().any(|n| n == "helper"),
            "new function should be compiled"
        );

        assert_eq!(session.run_main().unwrap(), "99\n");
    }

    #[test]
    fn test_dev_macro_reload() {
        // Verify that macro expansion works correctly through the transform
        // (The integration is in CLI, but we can test that reload with different
        // expanded source works)
        let source_v1 = "(defn main () : Unit (println 1))";
        let mut session = DevSession::new(source_v1).expect("DevSession::new failed");
        assert_eq!(session.run_main().unwrap(), "1\n");

        // Simulate what would happen after macro re-expansion changes the source
        let source_v2 = "(defn main () : Unit (println 2))";
        let result = session.reload(source_v2).expect("reload failed");
        assert!(!result.recompiled.is_empty());
        assert_eq!(session.run_main().unwrap(), "2\n");
    }

    // ── Typeclass codegen tests ──────────────────────────────────

    #[test]
    fn test_typeclass_eq_i32() {
        let out = compile_run(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () : Unit
               (println (== 1 1))
               (println (== 1 2)))",
        );
        assert_eq!(out, "true\nfalse\n");
    }

    #[test]
    fn oracle_typeclass_eq() {
        oracle_test(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () : Unit
               (println (== 1 1))
               (println (== 1 2)))",
        );
    }

    #[test]
    fn test_generic_identity() {
        let out = compile_run(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : i32 (id 42))",
        );
        assert_eq!(out, "");
    }

    #[test]
    fn test_generic_identity_println() {
        let out = compile_run(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : Unit (println (id 42)))",
        );
        assert_eq!(out, "42\n");
    }

    #[test]
    fn oracle_generic_identity() {
        oracle_test(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : Unit (println (id 42)))",
        );
    }

    #[test]
    fn test_generic_const() {
        let out = compile_run(
            "(defn const ((x : 'a) (y : 'b)) : 'a x)
             (defn main () : Unit (println (const 99 true)))",
        );
        assert_eq!(out, "99\n");
    }

    #[test]
    fn oracle_generic_const() {
        oracle_test(
            "(defn const ((x : 'a) (y : 'b)) : 'a x)
             (defn main () : Unit (println (const 99 true)))",
        );
    }

    #[test]
    fn test_generic_multiple_calls() {
        let out = compile_run(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : Unit
               (println (id 1))
               (println (id true)))",
        );
        assert_eq!(out, "1\ntrue\n");
    }

    #[test]
    fn oracle_generic_multiple_calls() {
        oracle_test(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : Unit
               (println (id 1))
               (println (id true)))",
        );
    }

    // ── Ord typeclass + match + simple enums ─────────────────────

    #[test]
    fn test_ord_codegen() {
        let out = compile_run(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (instance (Ord i32)
               (defn compare ((x : i32) (y : i32)) : Ordering
                 (if (< x y) LT (if (> x y) GT EQ))))
             (defn min-of ((x : i32) (y : i32)) : i32
               (match (compare x y)
                 (LT x) (EQ x) (GT y)))
             (defn main () : Unit
               (println (min-of 10 3))
               (println (min-of 2 7))
               (println (min-of 5 5)))",
        );
        assert_eq!(out, "3\n2\n5\n");
    }

    #[test]
    fn oracle_ord() {
        oracle_test(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (instance (Ord i32)
               (defn compare ((x : i32) (y : i32)) : Ordering
                 (if (< x y) LT (if (> x y) GT EQ))))
             (defn min-of ((x : i32) (y : i32)) : i32
               (match (compare x y)
                 (LT x) (EQ x) (GT y)))
             (defn max-of ((x : i32) (y : i32)) : i32
               (match (compare x y)
                 (GT x) (_ y)))
             (defn main () : Unit
               (println (min-of 10 3))
               (println (max-of 10 3))
               (println (min-of 2 7))
               (println (max-of 2 7))
               (println (min-of 5 5))
               (println (max-of 5 5)))",
        );
    }

    #[test]
    fn fixture_typeclasses_ord() {
        let compiled = run_fixture_compiled("typeclasses-ord");
        let interpreted = run_fixture_interpreted("typeclasses-ord");
        assert_eq!(
            compiled, interpreted,
            "typeclasses-ord: compiler and interpreter disagree"
        );
    }

    #[test]
    fn test_from_codegen() {
        let out = compile_run(
            "(deftype Color Red Green Blue)
             (deftype Primary Yes No)
             (defclass (From 'a 'b)
               (from : (Fn ['a] 'b)))
             (instance (From Color Primary)
               (defn from ((c : Color)) : Primary
                 (match c
                   (Red Yes)
                   (Blue Yes)
                   (_ No))))
             (defn primary-to-i32 ((p : Primary)) : i32
               (match p (Yes 1) (No 0)))
             (defn main () : Unit
               (let (((r : Primary) (from Red)))
                 (println (primary-to-i32 r)))
               (let (((g : Primary) (from Green)))
                 (println (primary-to-i32 g)))
               (let (((b : Primary) (from Blue)))
                 (println (primary-to-i32 b))))",
        );
        assert_eq!(out, "1\n0\n1\n");
    }

    #[test]
    fn oracle_from() {
        oracle_test(
            "(deftype Color Red Green Blue)
             (deftype Primary Yes No)
             (defclass (From 'a 'b)
               (from : (Fn ['a] 'b)))
             (instance (From Color Primary)
               (defn from ((c : Color)) : Primary
                 (match c
                   (Red Yes)
                   (Blue Yes)
                   (_ No))))
             (defn primary-to-i32 ((p : Primary)) : i32
               (match p (Yes 1) (No 0)))
             (defn main () : Unit
               (let (((r : Primary) (from Red)))
                 (println (primary-to-i32 r)))
               (let (((g : Primary) (from Green)))
                 (println (primary-to-i32 g)))
               (let (((b : Primary) (from Blue)))
                 (println (primary-to-i32 b))))",
        );
    }

    #[test]
    fn fixture_typeclasses_from() {
        let compiled = run_fixture_compiled("typeclasses-from");
        let interpreted = run_fixture_interpreted("typeclasses-from");
        assert_eq!(
            compiled, interpreted,
            "typeclasses-from: compiler and interpreter disagree"
        );
    }

    // ── Data-carrying constructors + Result + ? operator tests ──

    #[test]
    fn test_option_some_none_codegen() {
        let output = compile_run(
            "(deftype (Option 'a) (Some 'a) (None))
             (defn unwrap-or ((o : (Option i32)) (default : i32)) : i32
               (match o
                 ((Some val) val)
                 ((None) default)))
             (defn main ()
               (println (unwrap-or (Some 42) 0))
               (println (unwrap-or (None) 99)))",
        );
        assert_eq!(output, "42\n99\n");
    }

    #[test]
    fn test_result_codegen() {
        let output = compile_run(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn unwrap-or ((r : (Result i32 i32)) (default : i32)) : i32
               (match r
                 ((Ok val) val)
                 ((Err _) default)))
             (defn main ()
               (println (unwrap-or (Ok 42) 0))
               (println (unwrap-or (Err 99) 0)))",
        );
        assert_eq!(output, "42\n0\n");
    }

    #[test]
    fn test_try_operator_codegen() {
        let output = compile_run(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn safe-div ((x : i32) (y : i32)) : (Result i32 i32)
               (if (= y 0) (Err y) (Ok (/ x y))))
             (defn try-add ((x : i32) (y : i32)) : (Result i32 i32)
               (let ((result (safe-div x y)?))
                 (Ok (+ result 1))))
             (defn unwrap-or ((r : (Result i32 i32)) (default : i32)) : i32
               (match r
                 ((Ok val) val)
                 ((Err _) default)))
             (defn main ()
               (println (unwrap-or (try-add 10 2) 0))
               (println (unwrap-or (try-add 10 0) 99)))",
        );
        assert_eq!(output, "6\n99\n");
    }

    #[test]
    fn oracle_result() {
        oracle_test(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn safe-div ((x : i32) (y : i32)) : (Result i32 i32)
               (if (= y 0) (Err y) (Ok (/ x y))))
             (defn unwrap-or ((r : (Result i32 i32)) (default : i32)) : i32
               (match r
                 ((Ok val) val)
                 ((Err _) default)))
             (defn try-add ((x : i32) (y : i32)) : (Result i32 i32)
               (let ((result (safe-div x y)?))
                 (Ok (+ result 1))))
             (defn main ()
               (println (unwrap-or (safe-div 10 2) 0))
               (println (unwrap-or (safe-div 10 0) 99))
               (println (unwrap-or (try-add 10 2) 0))
               (println (unwrap-or (try-add 10 0) 99)))",
        );
    }

    #[test]
    fn fixture_result() {
        let compiled = run_fixture_compiled("result");
        let interpreted = run_fixture_interpreted("result");
        assert_eq!(
            compiled, interpreted,
            "result: compiler and interpreter disagree"
        );
    }

    // ── String tests ──────────────────────────────────────────

    #[test]
    fn test_string_literal_print() {
        let out = compile_run(
            "(defn main () (println \"hello\"))",
        );
        assert_eq!(out, "hello\n");
    }

    #[test]
    fn test_string_literal_in_function() {
        let out = compile_run(
            "(defn greet ((name : String)) : String name)
             (defn main () (println (greet \"world\")))",
        );
        assert_eq!(out, "world\n");
    }

    #[test]
    fn test_str_builtin_i64() {
        let out = compile_run(
            "(defn main () (println (str 42)))",
        );
        assert_eq!(out, "42\n");
    }

    #[test]
    fn test_str_builtin_f64() {
        let out = compile_run(
            "(defn main () (println (str 3.14)))",
        );
        assert_eq!(out, "3.14\n");
    }

    #[test]
    fn test_str_builtin_bool() {
        let out = compile_run(
            "(defn main () (println (str true)))",
        );
        assert_eq!(out, "true\n");
    }

    #[test]
    fn test_str_concat() {
        let out = compile_run(
            "(defn main () (println (str \"hello \" \"world\")))",
        );
        assert_eq!(out, "hello world\n");
    }

    #[test]
    fn test_str_mixed_concat() {
        let out = compile_run(
            "(defn main () (println (str \"x=\" 42)))",
        );
        assert_eq!(out, "x=42\n");
    }

    #[test]
    fn test_string_equality() {
        let out = compile_run(
            "(defn main ()
               (println (= \"foo\" \"foo\"))
               (println (= \"foo\" \"bar\"))
               (println (!= \"foo\" \"bar\")))",
        );
        assert_eq!(out, "true\nfalse\ntrue\n");
    }

    #[test]
    fn oracle_strings() {
        oracle_test(
            "(defn main ()
               (println \"hello\")
               (println (str 42))
               (println (str 3.14))
               (println (str true))
               (println (str \"hello \" \"world\"))
               (println (str \"x=\" 42))
               (println (= \"foo\" \"foo\"))
               (println (= \"foo\" \"bar\")))",
        );
    }

    #[test]
    fn fixture_strings() {
        let compiled = run_fixture_compiled("strings");
        let interpreted = run_fixture_interpreted("strings");
        assert_eq!(
            compiled, interpreted,
            "strings: compiler and interpreter disagree"
        );
    }

    // ── Codegen error tests ────────────────────────────────────

    #[test]
    fn test_error_cannot_print_adt() {
        let msg = compile_err(
            "(deftype Color Red Green Blue)
             (defn main () (println Red))",
        );
        assert!(
            msg.contains("cannot print type"),
            "expected 'cannot print type' error, got: {}",
            msg
        );
    }

    #[test]
    fn test_error_dev_session_parse_error() {
        match DevSession::new("(defn main (") {
            Err(e) => assert!(
                e.message.contains("parse error"),
                "expected parse error, got: {}",
                e.message
            ),
            Ok(_) => panic!("expected DevSession::new to fail with parse error"),
        }
    }

    #[test]
    fn test_error_dev_session_type_error() {
        match DevSession::new("(defn main () (+ 1 true))") {
            Err(e) => assert!(
                e.message.contains("type error"),
                "expected type error, got: {}",
                e.message
            ),
            Ok(_) => panic!("expected DevSession::new to fail with type error"),
        }
    }

    #[test]
    fn test_error_dev_session_reload_parse_error() {
        let valid = "(defn main () (println 1))";
        let mut session = DevSession::new(valid).expect("initial session should succeed");
        match session.reload("(defn main (") {
            Err(e) => assert!(
                e.message.contains("parse error"),
                "expected parse error on reload, got: {}",
                e.message
            ),
            Ok(_) => panic!("expected reload to fail with parse error"),
        }
        // Session should still be functional with old code
        let output = session.run_main().expect("old code should still run");
        assert_eq!(output.trim(), "1");
    }

    #[test]
    fn test_error_dev_session_reload_type_error() {
        let valid = "(defn main () (println 1))";
        let mut session = DevSession::new(valid).expect("initial session should succeed");
        match session.reload("(defn main () (+ 1 true))") {
            Err(e) => assert!(
                e.message.contains("type error"),
                "expected type error on reload, got: {}",
                e.message
            ),
            Ok(_) => panic!("expected reload to fail with type error"),
        }
        // Session should still be functional with old code
        let output = session.run_main().expect("old code should still run");
        assert_eq!(output.trim(), "1");
    }

    // ── Vector tests ─────────────────────────────────────────────

    #[test]
    fn test_vector_literal_len() {
        let out = compile_run(
            "(defn main () (println (len [1 2 3])))",
        );
        assert_eq!(out, "3\n");
    }

    #[test]
    fn test_vector_nth() {
        let out = compile_run(
            "(defn main ()
               (let ((v [10 20 30]))
                 (println (nth v 0))
                 (println (nth v 1))
                 (println (nth v 2))))",
        );
        assert_eq!(out, "10\n20\n30\n");
    }

    #[test]
    fn test_vector_append() {
        let out = compile_run(
            "(defn main ()
               (let ((v [1 2 3]))
                 (let ((v2 (append v 4)))
                   (println (len v2))
                   (println (nth v2 3)))))",
        );
        assert_eq!(out, "4\n4\n");
    }

    #[test]
    fn test_vector_in_function() {
        let out = compile_run(
            "(defn sum-vec ((v : (Vector i64))) : i64
               (+ (nth v 0) (nth v 1) (nth v 2)))
             (defn main ()
               (println (sum-vec [10 20 30])))",
        );
        assert_eq!(out, "60\n");
    }

    #[test]
    fn oracle_vectors() {
        oracle_test(
            "(defn main ()
               (let ((v [10 20 30]))
                 (println (len v))
                 (println (nth v 0))
                 (println (nth v 1))
                 (println (nth v 2))
                 (let ((v2 (append v 40)))
                   (println (len v2))
                   (println (nth v2 3)))))",
        );
    }

    #[test]
    fn fixture_vectors() {
        let compiled = run_fixture_compiled("vectors");
        let interpreted = run_fixture_interpreted("vectors");
        assert_eq!(
            compiled, interpreted,
            "vectors: compiler and interpreter disagree"
        );
    }

    // ── Closure tests ────────────────────────────────────────────

    #[test]
    fn test_lambda_simple() {
        let out = compile_run(
            "(defn main ()
               (let ((f (fn ((x : i32)) : i32 (* x 2))))
                 (println (f 21))))",
        );
        assert_eq!(out, "42\n");
    }

    #[test]
    fn test_closure_capture() {
        let out = compile_run(
            "(defn make-adder ((n : i64)) : (Fn [i64] i64)
               (fn ((x : i64)) : i64 (+ x n)))
             (defn main ()
               (let ((add5 (make-adder 5))
                     (add10 (make-adder 10)))
                 (println (add5 1))
                 (println (add10 1))))",
        );
        assert_eq!(out, "6\n11\n");
    }

    #[test]
    fn test_higher_order() {
        let out = compile_run(
            "(defn apply-twice ((f : (Fn [i64] i64)) (x : i64)) : i64
               (f (f x)))
             (defn main ()
               (let ((double (fn ((x : i64)) : i64 (* x 2))))
                 (println (apply-twice double 3))))",
        );
        assert_eq!(out, "12\n");
    }

    #[test]
    fn test_inline_lambda_arg() {
        let out = compile_run(
            "(defn apply-twice ((f : (Fn [i64] i64)) (x : i64)) : i64
               (f (f x)))
             (defn main ()
               (println (apply-twice (fn ((x : i64)) : i64 (+ x 10)) 0)))",
        );
        assert_eq!(out, "20\n");
    }

    #[test]
    fn oracle_closures() {
        oracle_test(
            "(defn make-adder ((n : i64)) : (Fn [i64] i64)
               (fn ((x : i64)) : i64 (+ x n)))
             (defn apply-twice ((f : (Fn [i64] i64)) (x : i64)) : i64
               (f (f x)))
             (defn main ()
               (let ((add5 (make-adder 5)))
                 (println (add5 1))
                 (println (apply-twice add5 0))
                 (println (apply-twice (fn ((x : i64)) : i64 (* x 2)) 3))))",
        );
    }

    #[test]
    fn fixture_closures() {
        let path = fixture_path("closures");
        let source = std::fs::read_to_string(&path).unwrap();
        let source = format!("{}\n{}", PRELUDE_SOURCE, source);
        let expanded = expand(&source);
        let (module, parse_errors) = weir_parser::parse(&expanded);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty(), "type errors: {:?}", type_info.errors);
        let compiled = compile_and_run(&module, &type_info).expect("codegen error");
        let interpreted = weir_interp::interpret(&module).expect("interp error");
        assert_eq!(compiled, interpreted, "closures: compiler and interpreter disagree");
    }

    #[test]
    fn fixture_typeclasses_ord_custom() {
        let compiled = run_fixture_compiled("typeclasses-ord-custom");
        let interpreted = run_fixture_interpreted("typeclasses-ord-custom");
        assert_eq!(
            compiled, interpreted,
            "typeclasses-ord-custom: compiler and interpreter disagree"
        );
        assert_eq!(compiled, "true\ntrue\ntrue\nfalse\ntrue\n");
    }

    #[test]
    fn test_ord_custom_type_codegen() {
        let source = format!(
            "{}\n{}",
            PRELUDE_SOURCE,
            "(deftype Rank Low High)
             (instance (Ord Rank)
               (defn compare ((a : Rank) (b : Rank)) : Ordering
                 (let ((ra (match a (Low 0) (High 1)))
                       (rb (match b (Low 0) (High 1))))
                   (compare ra rb))))
             (defn main ()
               (println (< Low High))
               (println (> Low High))
               (println (<= High High))
               (println (>= High Low)))"
        );
        let compiled = compile_run(&source);
        assert_eq!(compiled, "true\nfalse\ntrue\ntrue\n");
    }

    #[test]
    fn oracle_ord_custom_type() {
        let source = format!(
            "{}\n{}",
            PRELUDE_SOURCE,
            "(deftype Rank Low High)
             (instance (Ord Rank)
               (defn compare ((a : Rank) (b : Rank)) : Ordering
                 (let ((ra (match a (Low 0) (High 1)))
                       (rb (match b (Low 0) (High 1))))
                   (compare ra rb))))
             (defn main ()
               (println (< Low High))
               (println (> Low High)))"
        );
        oracle_test(&source);
    }

    #[test]
    fn test_string_ordering_codegen() {
        let source = format!(
            "{}\n{}",
            PRELUDE_SOURCE,
            "(defn main ()
               (println (< \"apple\" \"banana\"))
               (println (> \"banana\" \"apple\"))
               (println (<= \"cat\" \"cat\"))
               (println (>= \"apple\" \"banana\")))"
        );
        let compiled = compile_run(&source);
        assert_eq!(compiled, "true\ntrue\ntrue\nfalse\n");
    }

    #[test]
    fn oracle_string_ordering() {
        let source = format!(
            "{}\n{}",
            PRELUDE_SOURCE,
            "(defn main ()
               (println (< \"apple\" \"banana\"))
               (println (> \"banana\" \"apple\"))
               (println (<= \"cat\" \"cat\")))"
        );
        oracle_test(&source);
    }

    // ── Property-based tests ────────────────────────────────────

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        fn arith_expr() -> impl Strategy<Value = String> {
            let leaf = prop_oneof![
                (1i64..=100).prop_map(|n| n.to_string()),
            ];
            leaf.prop_recursive(3, 32, 4, |inner| {
                let op = prop_oneof![
                    Just("+"),
                    Just("-"),
                    Just("*"),
                ];
                (op, inner.clone(), inner).prop_map(|(op, l, r)| {
                    format!("({} {} {})", op, l, r)
                })
            })
        }

        proptest! {
            #![proptest_config(ProptestConfig::with_cases(50))]

            #[test]
            fn interpreter_codegen_agree_on_arithmetic(expr in arith_expr()) {
                let source = format!("(defn main () (println {}))", expr);
                let expanded = expand(&source);
                let (module, parse_errors) = weir_parser::parse(&expanded);
                prop_assert!(parse_errors.is_empty());
                let type_info = weir_typeck::check(&module);
                prop_assert!(type_info.errors.is_empty());
                let compiled = compile_and_run(&module, &type_info);
                let interpreted = weir_interp::interpret(&module);
                match (compiled, interpreted) {
                    (Ok(c), Ok(i)) => prop_assert_eq!(c, i),
                    _ => {} // both can error (e.g. division by zero) — just don't panic
                }
            }

            #[test]
            fn interpreter_codegen_agree_on_simple_programs(
                a in 1i64..=50,
                b in 1i64..=50,
                use_let in any::<bool>(),
            ) {
                let source = if use_let {
                    format!(
                        "(defn main () (let ((x {}) (y {})) (println (+ x y))))",
                        a, b
                    )
                } else {
                    format!(
                        "(defn main () (println (if (< {} {}) {} {})))",
                        a, b, a, b
                    )
                };
                let expanded = expand(&source);
                let (module, parse_errors) = weir_parser::parse(&expanded);
                prop_assert!(parse_errors.is_empty());
                let type_info = weir_typeck::check(&module);
                prop_assert!(type_info.errors.is_empty());
                let compiled = compile_and_run(&module, &type_info).unwrap();
                let interpreted = weir_interp::interpret(&module).unwrap();
                prop_assert_eq!(compiled, interpreted);
            }
        }
    }

    // ── GC stress tests ──────────────────────────────────────────

    #[test]
    fn gc_stress_vector_allocation_loop() {
        // Allocate many vectors in a loop — GC should collect unreachable ones
        let source = r#"
(defn make-vec ((n : i64)) : (Vector i64)
  [n (+ n 1) (+ n 2)])

(defn main () : Unit
  (let ((result (make-vec 0)))
    (let ((result (make-vec 1)))
      (let ((result (make-vec 2)))
        (let ((result (make-vec 997)))
          (println (nth result 0))
          (println (nth result 1))
          (println (nth result 2)))))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "997\n998\n999\n");
    }

    #[test]
    fn gc_stress_closure_allocation() {
        // Allocate many closures — GC should handle them
        let source = r#"
(defn make-adder (x)
  (fn (y) (+ x y)))

(defn main ()
  (let ((add10 (make-adder 10))
        (add20 (make-adder 20)))
    (println (add10 5))
    (println (add20 5))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "15\n25\n");
    }

    #[test]
    fn gc_stress_vector_append_chain() {
        // Build a vector through repeated appends
        let source = r#"
(defn main () : Unit
  (let ((v [1])
        (v (append v 2))
        (v (append v 3))
        (v (append v 4))
        (v (append v 5)))
    (println (len v))
    (println (nth v 0))
    (println (nth v 4))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "5\n1\n5\n");
    }

    #[test]
    fn gc_stress_mixed_heap_types() {
        // Mix closures, vectors, and strings
        let source = r#"
(defn main () : Unit
  (let ((v [10 20 30])
        (s (str (nth v 1)))
        (f (fn ((x : i64)) : i64 (+ x (nth v 0)))))
    (println (f 5))
    (println s)
    (println (len v))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "15\n20\n3\n");
    }

    #[test]
    fn gc_oracle_vector_operations() {
        // Oracle test: GC shouldn't affect program semantics
        let source = r#"
(defn sum-vec ((v : (Vector i64)) (i : i64) (acc : i64)) : i64
  (if (= i (len v))
    acc
    (sum-vec v (+ i 1) (+ acc (nth v i)))))

(defn main () : Unit
  (let ((v [1 2 3 4 5]))
    (println (sum-vec v 0 0))))
"#;
        oracle_test(source);
    }

    // ── Arena allocation tests ──────────────────────────────────────

    #[test]
    fn arena_vec_len() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v [1 2 3]))
      (println (len v)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "3\n");
    }

    #[test]
    fn arena_vec_get() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v [10 20 30]))
      (println (nth v 1)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "20\n");
    }

    #[test]
    fn arena_closure() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((f (fn ((x : i64)) : i64 (+ x 1))))
      (println (f 41)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "42\n");
    }

    #[test]
    fn arena_closure_captures() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((y 10)
          (f (fn ((x : i64)) : i64 (+ x y))))
      (println (f 32)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "42\n");
    }

    #[test]
    fn arena_append() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v (append [1 2 3] 4)))
      (println (len v))
      (println (nth v 3)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "4\n4\n");
    }

    #[test]
    fn arena_returns_non_heap() {
        // The with-arena block returns an i64 (non-heap), which is fine
        let source = r#"
(defn main () : Unit
  (let ((result (with-arena a
                  (let ((v [10 20 30]))
                    (nth v 2)))))
    (println result)))
"#;
        let output = compile_run(source);
        assert_eq!(output, "30\n");
    }

    #[test]
    fn arena_oracle_vec_operations() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v [1 2 3 4 5]))
      (println (len v))
      (println (nth v 0))
      (println (nth v 4)))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_oracle_closure() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((f (fn ((x : i64)) : i64 (* x x))))
      (println (f 7)))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_oracle_mixed() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v [10 20 30])
          (f (fn ((x : i64)) : i64 (+ x (nth v 0)))))
      (println (f 5))
      (println (len v)))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_nested() {
        // Nested arenas: inner arena block returns a non-heap value
        let source = r#"
(defn main () : Unit
  (with-arena outer
    (let ((v [10 20 30]))
      (let ((sum (with-arena inner
                   (let ((w [1 2 3]))
                     (+ (nth w 0) (nth w 1))))))
        (println sum)
        (println (nth v 2))))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "3\n30\n");
    }

    #[test]
    fn arena_oracle_nested() {
        let source = r#"
(defn main () : Unit
  (with-arena outer
    (let ((v [10 20 30]))
      (let ((sum (with-arena inner
                   (let ((w [1 2 3]))
                     (+ (nth w 0) (nth w 1))))))
        (println sum)
        (println (nth v 2))))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_gc_object_referenced_from_arena_block() {
        // GC-allocated vector referenced from inside arena block.
        // The GC is suppressed during the arena, so v must survive.
        let source = r#"
(defn main () : Unit
  (let ((v [100 200 300]))
    (with-arena a
      (let ((w [1 2 3]))
        (println (nth v 1))
        (println (nth w 2))))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "200\n3\n");
    }

    #[test]
    fn arena_oracle_gc_object_in_arena() {
        let source = r#"
(defn main () : Unit
  (let ((v [100 200 300]))
    (with-arena a
      (let ((w [1 2 3]))
        (println (nth v 1))
        (println (nth w 2))))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_string_operations() {
        // Strings inside arena blocks use system allocation (not arena),
        // but should still work correctly.
        let source = r#"
(defn main () : Unit
  (with-arena a
    (println (str 42))
    (println (str true))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "42\ntrue\n");
    }

    #[test]
    fn arena_oracle_strings() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (println (str 42))
    (println (str true))))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_stress_many_vectors() {
        // Allocate enough vectors to force chunk growth (64 KB initial)
        let source = r#"
(defn make-vecs ((n : i64) (acc : i64)) : i64
  (if (<= n 0)
    acc
    (with-arena a
      (let ((v [1 2 3 4 5 6 7 8]))
        (make-vecs (- n 1) (+ acc (nth v 7)))))))

(defn main () : Unit
  (println (make-vecs 100 0)))
"#;
        let output = compile_run(source);
        assert_eq!(output, "800\n");
    }

    #[test]
    fn arena_closure_calling_allocating_fn() {
        // Closure in arena calls a function that allocates
        let source = r#"
(defn make-pair ((x : i64)) : (Vector i64) [x (+ x 1)])

(defn main () : Unit
  (with-arena a
    (let ((f (fn ((x : i64)) : i64
               (let ((p (make-pair x)))
                 (nth p 1)))))
      (println (f 10)))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "11\n");
    }

    #[test]
    fn arena_try_early_return_cleanup() {
        // ? inside arena triggers early return. Arena must still be cleaned up
        // (unsuppress GC + destroy arena). Verify by doing GC-allocating work after.
        let source = r#"
(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))

(defn risky ((fail : Bool)) : (Result i32 i32)
  (if fail (Err 0) (Ok 42)))

(defn use-arena ((fail : Bool)) : (Result i32 i32)
  (let ((result (with-arena a
                  (let ((v [1 2 3 4 5])
                        (x (risky fail) ?))
                    (+ x (nth v 0))))))
    (Ok result)))

(defn unwrap-or ((r : (Result i32 i32)) (default : i32)) : i32
  (match r ((Ok val) val) ((Err _) default)))

(defn main () : Unit
  (println (unwrap-or (use-arena true) 99))
  (println (unwrap-or (use-arena false) 99))
  (let ((v [10 20 30]))
    (println (nth v 1))))
"#;
        let output = compile_run(source);
        assert_eq!(output, "99\n43\n20\n");
    }

    #[test]
    fn arena_try_oracle() {
        let source = r#"
(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))

(defn risky ((fail : Bool)) : (Result i32 i32)
  (if fail (Err 0) (Ok 100)))

(defn use-arena ((fail : Bool)) : (Result i32 i32)
  (let ((result (with-arena a
                  (let ((v [1 2 3])
                        (x (risky fail) ?))
                    (+ x (nth v 0))))))
    (Ok result)))

(defn unwrap-or ((r : (Result i32 i32)) (default : i32)) : i32
  (match r ((Ok val) val) ((Err _) default)))

(defn main () : Unit
  (println (unwrap-or (use-arena true) 99))
  (println (unwrap-or (use-arena false) 99)))
"#;
        oracle_test(source);
    }

    #[test]
    fn arena_aot_build_and_run() {
        let source = r#"
(defn main () : Unit
  (with-arena a
    (let ((v [10 20 30])
          (f (fn ((x : i64)) : i64 (* x 2))))
      (println (f (nth v 1)))
      (println (len v)))))
"#;
        let expanded = expand(source);
        let (module, parse_errors) = weir_parser::parse(&expanded);
        assert!(parse_errors.is_empty());
        let type_info = weir_typeck::check(&module);
        assert!(type_info.errors.is_empty());

        let jit_output = compile_and_run(&module, &type_info).expect("JIT failed");
        let interp_output = weir_interp::interpret(&module).expect("interp failed");

        let tmp_dir = std::env::temp_dir();
        let binary_path = tmp_dir.join("weir_test_aot_arena");
        build_executable(&module, &type_info, &binary_path).expect("build_executable failed");
        let output = std::process::Command::new(&binary_path)
            .output()
            .expect("failed to run AOT binary");
        let _ = std::fs::remove_file(&binary_path);

        assert!(output.status.success(), "AOT binary exited with error");
        let aot_output = String::from_utf8(output.stdout).unwrap();
        assert_eq!(jit_output, "40\n3\n");
        assert_eq!(jit_output, interp_output, "JIT and interpreter disagree");
        assert_eq!(jit_output, aot_output, "JIT and AOT disagree");
    }
}
