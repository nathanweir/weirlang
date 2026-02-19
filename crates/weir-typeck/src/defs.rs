use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt;
use weir_ast::TypeExprId;

use crate::types::{Ty, TyVarId};

// ── Stored definitions ───────────────────────────────────────────

#[derive(Clone)]
pub(crate) struct TypeDef {
    #[allow(dead_code)]
    pub(crate) name: SmolStr,
    #[allow(dead_code)]
    pub(crate) params: Vec<SmolStr>,
    pub(crate) variants: Vec<VariantDef>,
}

#[derive(Clone)]
pub(crate) struct VariantDef {
    pub(crate) name: SmolStr,
    #[allow(dead_code)]
    pub(crate) field_type_exprs: Vec<TypeExprId>,
}

#[derive(Clone)]
pub(crate) struct StructDef {
    pub(crate) name: SmolStr,
    pub(crate) params: Vec<SmolStr>,
    pub(crate) fields: Vec<(SmolStr, TypeExprId)>,
}

/// A polymorphic function type: quantified type vars + constraints + param/return types.
#[derive(Clone)]
pub(crate) struct FnScheme {
    /// Type variable IDs that should be freshened at each call site.
    pub(crate) quantified: Vec<TyVarId>,
    /// Typeclass constraints (e.g., `(Eq 'a)`)
    pub(crate) constraints: Vec<ClassConstraint>,
    pub(crate) param_types: Vec<Ty>,
    pub(crate) return_type: Ty,
}

/// A resolved typeclass constraint: (Eq 'a), (Show i32), etc.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ClassConstraint {
    pub(crate) class_name: SmolStr,
    pub(crate) type_args: Vec<Ty>,
}

/// Typeclass definition stored during collection.
#[derive(Clone)]
pub(crate) struct ClassDef {
    #[allow(dead_code)]
    pub(crate) name: SmolStr,
    pub(crate) type_params: Vec<SmolStr>,
    /// Expected kind for each type param (e.g., `* -> *` for Functor's 'f)
    pub(crate) type_param_kinds: Vec<TyKind>,
    /// (method_name, method_type_expr_id)
    pub(crate) methods: Vec<(SmolStr, TypeExprId)>,
}

/// A registered typeclass instance.
#[derive(Clone)]
pub(crate) struct InstanceEntry {
    pub(crate) class_name: SmolStr,
    /// Concrete or partially-concrete type args, e.g., [Ty::I32] or [Ty::Con("Option", [Ty::Var(x)])]
    pub(crate) type_args: Vec<Ty>,
    /// Prerequisites: e.g., (Eq 'a) for instance (Eq (Option 'a))
    #[allow(dead_code)]
    pub(crate) constraints: Vec<ClassConstraint>,
    /// method_name -> mangled impl function name (e.g., "Eq#i32#==")
    #[allow(dead_code)]
    pub(crate) methods: HashMap<SmolStr, SmolStr>,
}

/// Kind of a type: `*` for concrete types, `* -> *` for type constructors like Option, etc.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TyKind {
    /// Concrete type kind: `*`
    Star,
    /// Higher-kinded: `k1 -> k2`
    Arrow(Box<TyKind>, Box<TyKind>),
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Star => write!(f, "*"),
            TyKind::Arrow(a, b) => {
                if matches!(**a, TyKind::Arrow(_, _)) {
                    write!(f, "({}) -> {}", a, b)
                } else {
                    write!(f, "{} -> {}", a, b)
                }
            }
        }
    }
}

impl TyKind {
    /// Create a kind from a number of type parameters.
    /// 0 params → `*`, 1 param → `* -> *`, 2 params → `* -> * -> *`, etc.
    pub(crate) fn from_param_count(n: usize) -> TyKind {
        if n == 0 {
            TyKind::Star
        } else {
            TyKind::Arrow(
                Box::new(TyKind::Star),
                Box::new(TyKind::from_param_count(n - 1)),
            )
        }
    }
}

/// Kind of a type variable — used for numeric literal defaulting.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum VarKind {
    General,
    IntLit,
    FloatLit,
}
