// ── AOT compilation (object files and executables) ───────────────

use std::path::Path;

use cranelift_object::{ObjectBuilder, ObjectModule};

use super::{make_isa, CodegenError, Compiler};

// ── C runtime for AOT binaries ──────────────────────────────────

const AOT_RUNTIME_C: &str = r#"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

extern void weir_main(void);

/* Forward declarations for GC string allocation (defined after GC runtime) */
static int64_t gc_string_from_buf(const char *buf, size_t len);
int64_t weir_gc_str_alloc(int64_t len);
int64_t weir_gc_str_dup(int64_t src);

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
    char buf[32]; int n = snprintf(buf, sizeof(buf), "%lld", (long long)val);
    return gc_string_from_buf(buf, (size_t)n);
}
int64_t weir_f64_to_str(double val) {
    char buf[64]; int n;
    if (val == (double)(long long)val) n = snprintf(buf, sizeof(buf), "%.1f", val);
    else n = snprintf(buf, sizeof(buf), "%g", val);
    return gc_string_from_buf(buf, (size_t)n);
}
int64_t weir_bool_to_str(int8_t val) {
    const char *s = val ? "true" : "false";
    return gc_string_from_buf(s, strlen(s));
}
int64_t weir_str_concat(int64_t a, int64_t b) {
    /* Read inputs into stack buffer before GC allocation (GC safety) */
    const char *sa = (const char*)(intptr_t)a, *sb = (const char*)(intptr_t)b;
    size_t la = strlen(sa), lb = strlen(sb);
    size_t total = la + lb;
    char *tmp = (char*)malloc(total);
    memcpy(tmp, sa, la); memcpy(tmp + la, sb, lb);
    int64_t result = gc_string_from_buf(tmp, total);
    free(tmp);
    return result;
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
int64_t weir_vec_set(int64_t ptr, int64_t idx, int64_t val, int64_t shape_ptr) {
    int64_t len = weir_vec_len(ptr);
    int64_t new_ptr = weir_gc_vec_alloc(len, shape_ptr);
    memcpy((int64_t*)(intptr_t)new_ptr + 1, (int64_t*)(intptr_t)ptr + 1, len * 8);
    ((int64_t*)(intptr_t)new_ptr)[1 + idx] = val;
    return new_ptr;
}
int64_t weir_vec_append(int64_t ptr, int64_t elem, int64_t shape_ptr) {
    int64_t old_len = weir_vec_len(ptr);
    int64_t new_len = old_len + 1;
    int64_t new_ptr = weir_gc_vec_alloc(new_len, shape_ptr);
    memcpy((int64_t*)(intptr_t)new_ptr + 1, (int64_t*)(intptr_t)ptr + 1, old_len * 8);
    ((int64_t*)(intptr_t)new_ptr)[1 + old_len] = elem;
    return new_ptr;
}

/* ── GC-managed strings ── */

static const ShapeDesc gc_string_shape = { 0, SHAPE_FIXED, 0 };

int64_t weir_gc_str_alloc(int64_t len) {
    return weir_gc_alloc(len + 1, (int64_t)(intptr_t)&gc_string_shape);
}

int64_t weir_gc_str_dup(int64_t src) {
    const char *s = (const char*)(intptr_t)src;
    size_t len = strlen(s);
    int64_t ptr = weir_gc_str_alloc((int64_t)len);
    memcpy((char*)(intptr_t)ptr, s, len);
    return ptr;
}

static int64_t gc_string_from_buf(const char *buf, size_t len) {
    int64_t ptr = weir_gc_str_alloc((int64_t)len);
    memcpy((char*)(intptr_t)ptr, buf, len);
    return ptr;
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
    /* Do NOT eagerly collect here — the caller may still hold a GC pointer
       in a register that hasn't been stored to a shadow-stack slot yet.
       The next allocation will check the threshold and collect if needed. */
}

/* ── Atoms (lock-free AtomicI64) ── */
#include <stdatomic.h>

int64_t weir_atom_create(int64_t initial) {
    _Atomic int64_t *atom = (_Atomic int64_t *)malloc(sizeof(_Atomic int64_t));
    atomic_store_explicit(atom, initial, memory_order_release);
    return (int64_t)(intptr_t)atom;
}

int64_t weir_atom_deref(int64_t atom_ptr) {
    _Atomic int64_t *atom = (_Atomic int64_t *)(intptr_t)atom_ptr;
    return atomic_load_explicit(atom, memory_order_acquire);
}

int64_t weir_atom_cas(int64_t atom_ptr, int64_t expected, int64_t new_val) {
    _Atomic int64_t *atom = (_Atomic int64_t *)(intptr_t)atom_ptr;
    int64_t exp = expected;
    atomic_compare_exchange_strong_explicit(atom, &exp, new_val,
        memory_order_acq_rel, memory_order_acquire);
    return exp;
}

/* ── Math (libm wrappers) ── */
#include <math.h>

double weir_sin(double x) { return sin(x); }
double weir_cos(double x) { return cos(x); }
double weir_tan(double x) { return tan(x); }
double weir_asin(double x) { return asin(x); }
double weir_acos(double x) { return acos(x); }
double weir_atan(double x) { return atan(x); }
double weir_atan2(double y, double x) { return atan2(y, x); }
double weir_exp(double x) { return exp(x); }
double weir_log(double x) { return log(x); }
double weir_pow(double x, double y) { return pow(x, y); }
double weir_round(double x) { return round(x); }

/* ── Random (xorshift64) ── */
static uint64_t rng_state = 0x123456789abcdef0ULL;

double weir_random(void) {
    uint64_t s = rng_state;
    s ^= s << 13;
    s ^= s >> 7;
    s ^= s << 17;
    rng_state = s;
    return (double)(s >> 11) / (double)(1ULL << 53);
}

int64_t weir_random_int(int64_t n) {
    double r = weir_random();
    int64_t v = (int64_t)(r * (double)n);
    return v < n ? v : n - 1;
}

void weir_random_seed(int64_t seed) {
    rng_state = seed == 0 ? 1 : (uint64_t)seed;
}

/* ── String operations ── */
int64_t weir_string_length(int64_t ptr) {
    return (int64_t)strlen((const char*)(intptr_t)ptr);
}

int64_t weir_substring(int64_t ptr, int64_t start, int64_t end) {
    const char *s = (const char*)(intptr_t)ptr;
    size_t len = strlen(s);
    size_t st = (size_t)start;
    size_t en = (size_t)end;
    if (en > len) en = len;
    if (st > en) st = en;
    size_t sub_len = en - st;
    /* Copy to temp buffer before GC alloc (input might be GC-managed) */
    char *tmp = (char*)malloc(sub_len);
    memcpy(tmp, s + st, sub_len);
    int64_t result = gc_string_from_buf(tmp, sub_len);
    free(tmp);
    return result;
}

int64_t weir_string_ref(int64_t ptr, int64_t idx) {
    const char *s = (const char*)(intptr_t)ptr;
    return (int64_t)(unsigned char)s[idx];
}

int64_t weir_string_contains(int64_t haystack, int64_t needle) {
    return strstr((const char*)(intptr_t)haystack, (const char*)(intptr_t)needle) != NULL ? 1 : 0;
}

int64_t weir_string_upcase(int64_t ptr) {
    const char *s = (const char*)(intptr_t)ptr;
    size_t len = strlen(s);
    char *tmp = (char*)malloc(len);
    for (size_t i = 0; i < len; i++)
        tmp[i] = (s[i] >= 'a' && s[i] <= 'z') ? s[i] - 32 : s[i];
    int64_t result = gc_string_from_buf(tmp, len);
    free(tmp);
    return result;
}

int64_t weir_string_downcase(int64_t ptr) {
    const char *s = (const char*)(intptr_t)ptr;
    size_t len = strlen(s);
    char *tmp = (char*)malloc(len);
    for (size_t i = 0; i < len; i++)
        tmp[i] = (s[i] >= 'A' && s[i] <= 'Z') ? s[i] + 32 : s[i];
    int64_t result = gc_string_from_buf(tmp, len);
    free(tmp);
    return result;
}

int64_t weir_string_trim(int64_t ptr) {
    const char *s = (const char*)(intptr_t)ptr;
    size_t len = strlen(s);
    size_t start = 0, end = len;
    while (start < len && (s[start] == ' ' || s[start] == '\t' || s[start] == '\n' || s[start] == '\r')) start++;
    while (end > start && (s[end-1] == ' ' || s[end-1] == '\t' || s[end-1] == '\n' || s[end-1] == '\r')) end--;
    size_t sub_len = end - start;
    char *tmp = (char*)malloc(sub_len);
    memcpy(tmp, s + start, sub_len);
    int64_t result = gc_string_from_buf(tmp, sub_len);
    free(tmp);
    return result;
}

int64_t weir_char_to_string(int64_t code) {
    char buf[5] = {0};
    if (code < 0x80) { buf[0] = (char)code; }
    else if (code < 0x800) { buf[0] = 0xC0 | (code >> 6); buf[1] = 0x80 | (code & 0x3F); }
    else if (code < 0x10000) { buf[0] = 0xE0 | (code >> 12); buf[1] = 0x80 | ((code >> 6) & 0x3F); buf[2] = 0x80 | (code & 0x3F); }
    else { buf[0] = 0xF0 | (code >> 18); buf[1] = 0x80 | ((code >> 12) & 0x3F); buf[2] = 0x80 | ((code >> 6) & 0x3F); buf[3] = 0x80 | (code & 0x3F); }
    return gc_string_from_buf(buf, strlen(buf));
}

/* ── File I/O ── */
int64_t weir_read_file(int64_t path_ptr) {
    const char *path = (const char*)(intptr_t)path_ptr;
    FILE *f = fopen(path, "rb");
    if (!f) { fprintf(stderr, "read-file failed: cannot open %s\n", path); return gc_string_from_buf("", 0); }
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char*)malloc(len);
    fread(buf, 1, len, f);
    fclose(f);
    int64_t result = gc_string_from_buf(buf, (size_t)len);
    free(buf);
    return result;
}

void weir_write_file(int64_t path_ptr, int64_t contents_ptr) {
    const char *path = (const char*)(intptr_t)path_ptr;
    const char *contents = (const char*)(intptr_t)contents_ptr;
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "write-file failed: cannot open %s\n", path); return; }
    fwrite(contents, 1, strlen(contents), f);
    fclose(f);
}

/* ── Sleep ── */
#include <time.h>
void weir_sleep_ms(int64_t ms) {
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000;
    nanosleep(&ts, NULL);
}

/* ── Time ── */
int64_t weir_time_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + (int64_t)ts.tv_nsec / 1000000;
}

/* ── Terminal I/O ── */
#include <termios.h>
#include <fcntl.h>
#include <unistd.h>

static struct termios orig_termios;
static int term_saved = 0;

void weir_term_init(void) {
    tcgetattr(0, &orig_termios);
    term_saved = 1;
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ICANON | ECHO);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;
    tcsetattr(0, TCSANOW, &raw);
    int flags = fcntl(0, F_GETFL);
    fcntl(0, F_SETFL, flags | O_NONBLOCK);
}

void weir_term_restore(void) {
    if (term_saved) {
        tcsetattr(0, TCSANOW, &orig_termios);
        int flags = fcntl(0, F_GETFL);
        fcntl(0, F_SETFL, flags & ~O_NONBLOCK);
    }
}

int64_t weir_read_key(void) {
    unsigned char c;
    ssize_t n = read(0, &c, 1);
    return n <= 0 ? -1 : (int64_t)c;
}

/* ── MutVec (mutable vector) ── */
/* Layout: [capacity: i64, length: i64, elem_0, elem_1, ...] */

#define MUTVEC_INITIAL_CAPACITY 8

int64_t weir_mutvec_create(void) {
    int64_t cap = MUTVEC_INITIAL_CAPACITY;
    size_t total = (size_t)(2 + cap) * 8;
    int64_t *data = (int64_t*)calloc(1, total);
    data[0] = cap;   /* capacity */
    data[1] = 0;     /* length */
    return (int64_t)(intptr_t)data;
}

int64_t weir_mutvec_push(int64_t ptr, int64_t val) {
    int64_t *data = (int64_t*)(intptr_t)ptr;
    int64_t cap = data[0];
    int64_t len = data[1];
    if (len >= cap) {
        int64_t new_cap = cap * 2;
        size_t old_total = (size_t)(2 + cap) * 8;
        size_t new_total = (size_t)(2 + new_cap) * 8;
        data = (int64_t*)realloc(data, new_total);
        memset((char*)data + old_total, 0, new_total - old_total);
        data[0] = new_cap;
        data[1] = len + 1;
        data[2 + len] = val;
        return (int64_t)(intptr_t)data;
    }
    data[1] = len + 1;
    data[2 + len] = val;
    return ptr;
}

int64_t weir_mutvec_pop(int64_t ptr) {
    int64_t *data = (int64_t*)(intptr_t)ptr;
    int64_t len = data[1];
    if (len <= 0) return 0;
    int64_t val = data[2 + len - 1];
    data[1] = len - 1;
    return val;
}

int64_t weir_mutvec_get(int64_t ptr, int64_t idx) {
    int64_t *data = (int64_t*)(intptr_t)ptr;
    return data[2 + idx];
}

void weir_mutvec_set(int64_t ptr, int64_t idx, int64_t val) {
    int64_t *data = (int64_t*)(intptr_t)ptr;
    data[2 + idx] = val;
}

int64_t weir_mutvec_len(int64_t ptr) {
    int64_t *data = (int64_t*)(intptr_t)ptr;
    return data[1];
}

extern void __weir_init_globals(void) __attribute__((weak));
int main(void) {
    if (__weir_init_globals) __weir_init_globals();
    weir_main();
    return 0;
}
"#;

pub fn compile_to_object(
    module: &weir_ast::Module,
    type_info: &weir_typeck::TypeCheckResult,
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
    compiler.declare_globals()?;
    // Export main as weir_main so C runtime can call it; others are local
    compiler.declare_user_functions_aot()?;
    compiler.compile_user_functions()?;
    compiler.compile_global_inits()?;

    let product = compiler.module.finish();
    product
        .emit()
        .map_err(|e| CodegenError::new(format!("emit object: {}", e)))
}

pub fn build_executable(
    module: &weir_ast::Module,
    type_info: &weir_typeck::TypeCheckResult,
    output_path: &Path,
    cc_args: &[String],
    link_flags: &[String],
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

    let mut cc_cmd = std::process::Command::new("cc");
    cc_cmd
        .arg("-o")
        .arg(output_path)
        .arg(&obj_path)
        .arg(&runtime_path);
    for arg in cc_args {
        cc_cmd.arg(arg);
    }
    cc_cmd.arg("-lm");
    for flag in link_flags {
        cc_cmd.arg(flag);
    }
    let status = cc_cmd
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
