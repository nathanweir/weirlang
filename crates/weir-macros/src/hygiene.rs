use std::sync::atomic::{AtomicU64, Ordering};

static GENSYM_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Generate a unique symbol name that won't collide with user-defined names.
pub fn gensym(base: &str) -> String {
    let id = GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("__{}_{}", base, id)
}
