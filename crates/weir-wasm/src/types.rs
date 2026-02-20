use std::collections::{HashMap, HashSet};

use smol_str::SmolStr;
use wasm_encoder::ValType;
use weir_typeck::Ty;

/// Tagged ADT info for code generation.
#[derive(Debug, Clone)]
pub struct TaggedAdt {
    pub type_name: SmolStr,
    pub variants: Vec<(SmolStr, Vec<Ty>)>,
}

/// Map a Weir type to a WASM value type.
///
/// Returns `None` for `Unit` (void — no WASM value).
/// All pointer-like types (String, Vector, Fn, structs, ADTs) become `I32`
/// since WASM uses 32-bit linear memory addresses.
pub fn ty_to_wasm(
    ty: &Ty,
    tagged_adts: Option<&HashMap<SmolStr, TaggedAdt>>,
    struct_names: Option<&HashSet<SmolStr>>,
) -> Option<ValType> {
    match ty {
        // Integer types — WASM only has I32 and I64
        Ty::I8 | Ty::U8 | Ty::I16 | Ty::U16 | Ty::I32 | Ty::U32 => Some(ValType::I32),
        Ty::Bool => Some(ValType::I64),
        Ty::I64 | Ty::U64 => Some(ValType::I64),

        // Float types
        Ty::F32 => Some(ValType::F32),
        Ty::F64 => Some(ValType::F64),

        // Unit — no return value
        Ty::Unit => None,

        // Pointer types — 32-bit linear memory offsets
        Ty::Str | Ty::Ptr => Some(ValType::I32),

        // Heap-allocated types — pointer to linear memory
        Ty::Vector(_) | Ty::Map(_, _) | Ty::Atom(_) | Ty::Channel(_) => Some(ValType::I32),

        // Function type (closure pointer)
        Ty::Fn(_, _) => Some(ValType::I32),

        // Constructed types — ADTs, structs
        Ty::Con(name, _) => {
            if let Some(adts) = tagged_adts {
                if adts.contains_key(name) {
                    return Some(ValType::I64); // tag + payload packed
                }
            }
            if let Some(structs) = struct_names {
                if structs.contains(name) {
                    return Some(ValType::I32); // pointer to struct
                }
            }
            // Default: pointer
            Some(ValType::I32)
        }

        // Type variable — default to I64 (will be specialized)
        Ty::Var(_) => Some(ValType::I64),

        // Application — pointer
        Ty::App(_, _) => Some(ValType::I32),

        // Error sentinel
        Ty::Error => Some(ValType::I32),
    }
}

/// Check if a type is stored as a heap pointer in WASM (I32 offset).
pub fn is_heap_pointer(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Str
            | Ty::Vector(_)
            | Ty::Map(_, _)
            | Ty::Fn(_, _)
            | Ty::Atom(_)
            | Ty::Channel(_)
            | Ty::App(_, _)
    )
}

/// Return the size in bytes of a WASM value type.
pub fn wasm_val_size(vt: ValType) -> u32 {
    match vt {
        ValType::I32 | ValType::F32 => 4,
        ValType::I64 | ValType::F64 => 8,
        _ => 4,
    }
}
