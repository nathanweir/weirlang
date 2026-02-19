use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use weir_ast::ExprId;

use crate::error::TypeError;
use crate::types::Ty;

/// Resolved function type: param types + return type after substitution.
#[derive(Clone, Debug)]
pub struct FnType {
    pub param_types: Vec<Ty>,
    pub return_type: Ty,
}

/// Struct definition with resolved field types, for codegen.
#[derive(Clone, Debug)]
pub struct StructInfo {
    pub fields: Vec<(SmolStr, Ty)>,
}

/// A monomorphized specialization of a generic function for codegen.
#[derive(Clone, Debug)]
pub struct Specialization {
    /// Original function name (e.g., "id" or "==")
    pub original_name: SmolStr,
    /// Mangled name for the specialization (e.g., "id#i32" or "Eq#i32#==")
    pub mangled_name: SmolStr,
    /// Concrete parameter types
    pub param_types: Vec<Ty>,
    /// Concrete return type
    pub return_type: Ty,
    /// For instance methods: index of the source `Item::Instance` in the AST items list.
    /// `None` for generic user function specializations.
    pub instance_item_index: Option<usize>,
}

/// Tracks inter-function and function→type dependencies for selective recompilation.
#[derive(Debug, Clone, Default)]
pub struct DependencyGraph {
    /// Function A calls function B: A → {B, C, ...}
    pub call_deps: HashMap<SmolStr, HashSet<SmolStr>>,
    /// Function A uses type T (constructor, pattern match, field access)
    pub type_deps: HashMap<SmolStr, HashSet<SmolStr>>,
    /// Reverse: B is called by {A, C, ...}
    pub callers: HashMap<SmolStr, HashSet<SmolStr>>,
    /// Reverse: type T is used by {A, B, ...}
    pub type_users: HashMap<SmolStr, HashSet<SmolStr>>,
}

pub struct TypeCheckResult {
    pub errors: Vec<TypeError>,
    /// Resolved type for every expression, after substitution + numeric defaulting.
    pub expr_types: ArenaMap<ExprId, Ty>,
    /// Resolved function types for each named function, after substitution.
    pub fn_types: HashMap<SmolStr, FnType>,
    /// For each call site that invokes a class method, the resolved concrete function name.
    pub method_resolutions: HashMap<ExprId, SmolStr>,
    /// Monomorphized specializations needed for codegen.
    pub specializations: Vec<Specialization>,
    /// Dependency graph for selective recompilation.
    pub deps: DependencyGraph,
    /// Struct definitions with resolved field types, for codegen.
    pub struct_defs: HashMap<SmolStr, StructInfo>,
}

/// Check if a type contains any unresolved type variables.
pub(crate) fn has_type_var(ty: &Ty) -> bool {
    match ty {
        Ty::Var(_) => true,
        Ty::Con(_, args) => args.iter().any(has_type_var),
        Ty::Fn(params, ret) => params.iter().any(has_type_var) || has_type_var(ret),
        Ty::Vector(e) => has_type_var(e),
        Ty::Map(k, v) => has_type_var(k) || has_type_var(v),
        Ty::App(f, args) => has_type_var(f) || args.iter().any(has_type_var),
        _ => false,
    }
}
