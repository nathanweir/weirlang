use smol_str::SmolStr;
use std::fmt;

// ── Types ────────────────────────────────────────────────────────

pub type TyVarId = u32;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Str,
    Unit,
    /// Type variable (for inference / unification)
    Var(TyVarId),
    /// Named type constructor with type arguments: (Option i32), (Result String i32)
    Con(SmolStr, Vec<Ty>),
    /// Function type: (params...) -> return
    Fn(Vec<Ty>, Box<Ty>),
    /// Homogeneous vector type: (Vec i32)
    Vector(Box<Ty>),
    /// Map type: (Map String i32)
    Map(Box<Ty>, Box<Ty>),
    /// Atom type: (Atom i64)
    Atom(Box<Ty>),
    /// Channel type: (Channel i64)
    Channel(Box<Ty>),
    /// Higher-kinded type application: F applied to args
    App(Box<Ty>, Vec<Ty>),
    /// Mutable vector: (MutVec i32)
    MutVec(Box<Ty>),
    /// Mutable map: (MutMap String i32)
    MutMap(Box<Ty>, Box<Ty>),
    /// Raw pointer (for FFI). Contents are opaque to Weir.
    Ptr,
    /// Error sentinel — prevents cascading errors
    Error,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::I8 => write!(f, "i8"),
            Ty::I16 => write!(f, "i16"),
            Ty::I32 => write!(f, "i32"),
            Ty::I64 => write!(f, "i64"),
            Ty::U8 => write!(f, "u8"),
            Ty::U16 => write!(f, "u16"),
            Ty::U32 => write!(f, "u32"),
            Ty::U64 => write!(f, "u64"),
            Ty::F32 => write!(f, "f32"),
            Ty::F64 => write!(f, "f64"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Str => write!(f, "String"),
            Ty::Unit => write!(f, "Unit"),
            Ty::Con(name, args) if args.is_empty() => write!(f, "{}", name),
            Ty::Con(name, args) => {
                write!(f, "({}", name)?;
                for a in args {
                    write!(f, " {}", a)?;
                }
                write!(f, ")")
            }
            Ty::Fn(params, ret) => {
                write!(f, "(Fn [")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, "] {})", ret)
            }
            Ty::Vector(elem) => write!(f, "(Vector {})", elem),
            Ty::Map(k, v) => write!(f, "(Map {} {})", k, v),
            Ty::Atom(inner) => write!(f, "(Atom {})", inner),
            Ty::Channel(inner) => write!(f, "(Channel {})", inner),
            Ty::App(func, args) => {
                write!(f, "({}", func)?;
                for a in args {
                    write!(f, " {}", a)?;
                }
                write!(f, ")")
            }
            Ty::MutVec(elem) => write!(f, "(MutVec {})", elem),
            Ty::MutMap(k, v) => write!(f, "(MutMap {} {})", k, v),
            Ty::Var(id) => write!(f, "?{}", id),
            Ty::Ptr => write!(f, "Ptr"),
            Ty::Error => write!(f, "<error>"),
        }
    }
}

impl Ty {
    #[allow(dead_code)]
    pub(crate) fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub(crate) fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
        )
    }

    pub(crate) fn is_float(&self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }
}
