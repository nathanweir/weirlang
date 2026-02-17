use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::HashMap;
use std::fmt;
use weir_ast::*;

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
    /// Named type with applied type args: `Con("Option", [I32])` = `(Option i32)`
    Con(SmolStr, Vec<Ty>),
    /// Function type: `Fn([param_types], return_type)`
    Fn(Vec<Ty>, Box<Ty>),
    /// Vector type
    Vector(Box<Ty>),
    /// Map type
    Map(Box<Ty>, Box<Ty>),
    /// Unification variable
    Var(TyVarId),
    /// Error sentinel — unifies with anything, propagates silently
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
            Ty::Var(id) => write!(f, "?{}", id),
            Ty::Error => write!(f, "<error>"),
        }
    }
}

impl Ty {
    #[allow(dead_code)]
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::F32
                | Ty::F64
        )
    }

    fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
        )
    }

    fn is_float(&self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }
}

// ── Errors ───────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TypeError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}:{}] {}",
            self.span.start, self.span.end, self.message
        )
    }
}

pub struct TypeCheckResult {
    pub errors: Vec<TypeError>,
    /// Resolved type for every expression, after substitution + numeric defaulting.
    pub expr_types: ArenaMap<ExprId, Ty>,
}

// ── Public API ───────────────────────────────────────────────────

pub fn check(module: &Module) -> TypeCheckResult {
    let mut checker = TypeChecker::new(module);
    checker.collect_definitions();
    checker.check_items();

    // Apply final substitution to all recorded expression types
    let mut expr_types = ArenaMap::default();
    for (id, ty) in checker.expr_types.iter() {
        expr_types.insert(id, checker.apply(ty));
    }

    TypeCheckResult {
        errors: checker.errors,
        expr_types,
    }
}

// ── Stored definitions ───────────────────────────────────────────

#[derive(Clone)]
struct TypeDef {
    #[allow(dead_code)]
    name: SmolStr,
    #[allow(dead_code)]
    params: Vec<SmolStr>,
    variants: Vec<VariantDef>,
}

#[derive(Clone)]
struct VariantDef {
    name: SmolStr,
    #[allow(dead_code)]
    field_type_exprs: Vec<TypeExprId>,
}

#[derive(Clone)]
struct StructDef {
    name: SmolStr,
    params: Vec<SmolStr>,
    fields: Vec<(SmolStr, TypeExprId)>,
}

/// A polymorphic function type: quantified type vars + param/return types.
#[derive(Clone)]
struct FnScheme {
    /// Type variable IDs that should be freshened at each call site.
    quantified: Vec<TyVarId>,
    param_types: Vec<Ty>,
    return_type: Ty,
}

/// Kind of a type variable — used for numeric literal defaulting.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VarKind {
    General,
    IntLit,
    FloatLit,
}

// ── Type checker ─────────────────────────────────────────────────

struct TypeChecker<'a> {
    module: &'a Module,

    // Substitution: index = TyVarId, value = resolved type (or None)
    subst: Vec<Option<Ty>>,
    var_kinds: Vec<VarKind>,

    // Definitions
    type_defs: HashMap<SmolStr, TypeDef>,
    struct_defs: HashMap<SmolStr, StructDef>,
    fn_schemes: HashMap<SmolStr, FnScheme>,
    /// Constructor name → (type_name, type_params, field_type_exprs)
    constructors: HashMap<SmolStr, (SmolStr, Vec<SmolStr>, Vec<TypeExprId>)>,

    // Scope stack for variable types
    scopes: Vec<HashMap<SmolStr, Ty>>,

    // Type parameter scope: maps 'a → Ty::Var(id) during resolution
    type_param_scope: HashMap<SmolStr, Ty>,

    errors: Vec<TypeError>,

    /// Records the type of every expression (pre-substitution).
    expr_types: ArenaMap<ExprId, Ty>,
}

impl<'a> TypeChecker<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            module,
            subst: Vec::new(),
            var_kinds: Vec::new(),
            type_defs: HashMap::new(),
            struct_defs: HashMap::new(),
            fn_schemes: HashMap::new(),
            constructors: HashMap::new(),
            scopes: vec![HashMap::new()],
            type_param_scope: HashMap::new(),
            errors: Vec::new(),
            expr_types: ArenaMap::default(),
        }
    }

    // ── Fresh type variables ─────────────────────────────────────

    fn fresh_var(&mut self) -> Ty {
        self.fresh_var_kind(VarKind::General)
    }

    fn fresh_int_var(&mut self) -> Ty {
        self.fresh_var_kind(VarKind::IntLit)
    }

    fn fresh_float_var(&mut self) -> Ty {
        self.fresh_var_kind(VarKind::FloatLit)
    }

    fn fresh_var_kind(&mut self, kind: VarKind) -> Ty {
        let id = self.subst.len() as TyVarId;
        self.subst.push(None);
        self.var_kinds.push(kind);
        Ty::Var(id)
    }

    // ── Substitution ─────────────────────────────────────────────

    fn apply(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(id) => {
                if let Some(Some(resolved)) = self.subst.get(*id as usize) {
                    self.apply(resolved)
                } else {
                    ty.clone()
                }
            }
            Ty::Con(name, args) => {
                Ty::Con(name.clone(), args.iter().map(|a| self.apply(a)).collect())
            }
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            Ty::Vector(elem) => Ty::Vector(Box::new(self.apply(elem))),
            Ty::Map(k, v) => Ty::Map(Box::new(self.apply(k)), Box::new(self.apply(v))),
            _ => ty.clone(),
        }
    }

    // ── Unification ──────────────────────────────────────────────

    fn unify(&mut self, a: &Ty, b: &Ty, span: Span) {
        let a = self.apply(a);
        let b = self.apply(b);

        if a == b {
            return;
        }

        match (&a, &b) {
            (Ty::Error, _) | (_, Ty::Error) => {}

            (Ty::Var(id), _) => {
                if self.occurs_in(*id, &b) {
                    self.error(format!("infinite type: ?{} = {}", id, b), span);
                } else if self.check_var_kind_compat(*id, &b, span) {
                    self.subst[*id as usize] = Some(b);
                }
            }

            (_, Ty::Var(id)) => {
                if self.occurs_in(*id, &a) {
                    self.error(format!("infinite type: ?{} = {}", id, a), span);
                } else if self.check_var_kind_compat(*id, &a, span) {
                    self.subst[*id as usize] = Some(a);
                }
            }

            (Ty::Con(n1, a1), Ty::Con(n2, a2)) if n1 == n2 && a1.len() == a2.len() => {
                for (x, y) in a1.iter().zip(a2.iter()) {
                    self.unify(x, y, span);
                }
            }

            (Ty::Fn(p1, r1), Ty::Fn(p2, r2)) if p1.len() == p2.len() => {
                for (x, y) in p1.iter().zip(p2.iter()) {
                    self.unify(x, y, span);
                }
                self.unify(r1, r2, span);
            }

            (Ty::Vector(e1), Ty::Vector(e2)) => self.unify(e1, e2, span),
            (Ty::Map(k1, v1), Ty::Map(k2, v2)) => {
                self.unify(k1, k2, span);
                self.unify(v1, v2, span);
            }

            _ => {
                self.error(format!("type mismatch: expected {}, got {}", a, b), span);
            }
        }
    }

    fn occurs_in(&self, var: TyVarId, ty: &Ty) -> bool {
        let ty = self.apply(ty);
        match &ty {
            Ty::Var(id) => *id == var,
            Ty::Con(_, args) => args.iter().any(|a| self.occurs_in(var, a)),
            Ty::Fn(params, ret) => {
                params.iter().any(|p| self.occurs_in(var, p)) || self.occurs_in(var, ret)
            }
            Ty::Vector(e) => self.occurs_in(var, e),
            Ty::Map(k, v) => self.occurs_in(var, k) || self.occurs_in(var, v),
            _ => false,
        }
    }

    /// Check that a type variable's kind is compatible with the given type.
    /// Returns true if compatible, false if an error was emitted.
    fn check_var_kind_compat(&mut self, var_id: TyVarId, ty: &Ty, span: Span) -> bool {
        // If the target is also a Var, merge kinds
        if let Ty::Var(other_id) = ty {
            let kind_a = self.var_kinds[var_id as usize];
            let kind_b = self.var_kinds[*other_id as usize];
            // Propagate the more specific kind
            if kind_a != VarKind::General && kind_b == VarKind::General {
                self.var_kinds[*other_id as usize] = kind_a;
            } else if kind_b != VarKind::General && kind_a == VarKind::General {
                self.var_kinds[var_id as usize] = kind_b;
            } else if kind_a != kind_b && kind_a != VarKind::General && kind_b != VarKind::General {
                self.error("cannot unify integer literal with float literal", span);
                return false;
            }
            return true;
        }

        let kind = self.var_kinds[var_id as usize];
        match kind {
            VarKind::General => true,
            VarKind::IntLit => {
                if ty.is_integer() {
                    true
                } else {
                    self.error(
                        format!("type mismatch: expected integer type, got {}", ty),
                        span,
                    );
                    false
                }
            }
            VarKind::FloatLit => {
                if ty.is_float() {
                    true
                } else {
                    self.error(
                        format!("type mismatch: expected float type, got {}", ty),
                        span,
                    );
                    false
                }
            }
        }
    }

    /// Instantiate a function scheme with fresh type variables.
    fn instantiate(&mut self, scheme: &FnScheme) -> (Vec<Ty>, Ty) {
        let mapping: HashMap<TyVarId, Ty> = scheme
            .quantified
            .iter()
            .map(|&v| (v, self.fresh_var()))
            .collect();

        let params = scheme
            .param_types
            .iter()
            .map(|t| self.subst_vars(t, &mapping))
            .collect();
        let ret = self.subst_vars(&scheme.return_type, &mapping);
        (params, ret)
    }

    fn subst_vars(&self, ty: &Ty, mapping: &HashMap<TyVarId, Ty>) -> Ty {
        match ty {
            Ty::Var(id) => mapping.get(id).cloned().unwrap_or_else(|| ty.clone()),
            Ty::Con(name, args) => Ty::Con(
                name.clone(),
                args.iter().map(|a| self.subst_vars(a, mapping)).collect(),
            ),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.subst_vars(p, mapping)).collect(),
                Box::new(self.subst_vars(ret, mapping)),
            ),
            Ty::Vector(e) => Ty::Vector(Box::new(self.subst_vars(e, mapping))),
            Ty::Map(k, v) => Ty::Map(
                Box::new(self.subst_vars(k, mapping)),
                Box::new(self.subst_vars(v, mapping)),
            ),
            _ => ty.clone(),
        }
    }

    // ── Scope management ─────────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_var(&mut self, name: SmolStr, ty: Ty) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    fn lookup_var(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn error(&mut self, message: impl Into<String>, span: Span) {
        self.errors.push(TypeError {
            message: message.into(),
            span,
        });
    }

    // ── Type resolution (AST TypeExpr → Ty) ──────────────────────

    fn resolve_type_expr(&mut self, id: TypeExprId) -> Ty {
        let texpr = &self.module.type_exprs[id];
        let span = texpr.span;
        match &texpr.kind {
            TypeExprKind::Named(name) => self.resolve_named_type(name, span),

            TypeExprKind::Fn {
                params,
                return_type,
            } => {
                let p: Vec<Ty> = params
                    .iter()
                    .map(|&id| self.resolve_type_expr(id))
                    .collect();
                let r = self.resolve_type_expr(*return_type);
                Ty::Fn(p, Box::new(r))
            }

            TypeExprKind::Applied { constructor, args } => {
                let con_expr = &self.module.type_exprs[*constructor];
                if let TypeExprKind::Named(name) = &con_expr.kind {
                    let name = name.clone();
                    let arg_tys: Vec<Ty> =
                        args.iter().map(|&id| self.resolve_type_expr(id)).collect();

                    // Special cases
                    if name == "Vector" && arg_tys.len() == 1 {
                        return Ty::Vector(Box::new(arg_tys[0].clone()));
                    }
                    if name == "Map" && arg_tys.len() == 2 {
                        return Ty::Map(Box::new(arg_tys[0].clone()), Box::new(arg_tys[1].clone()));
                    }

                    Ty::Con(name, arg_tys)
                } else {
                    self.error("expected type name in type application", span);
                    Ty::Error
                }
            }

            TypeExprKind::TypeVar(name) => {
                if let Some(ty) = self.type_param_scope.get(name.as_str()) {
                    ty.clone()
                } else {
                    // Unknown type variable — create a fresh one and register it
                    let ty = self.fresh_var();
                    self.type_param_scope.insert(name.clone(), ty.clone());
                    ty
                }
            }

            TypeExprKind::Constrained { inner, .. } => {
                // Skip constraints for now (Phase 8)
                self.resolve_type_expr(*inner)
            }
        }
    }

    fn resolve_named_type(&mut self, name: &str, span: Span) -> Ty {
        match name {
            "i8" => Ty::I8,
            "i16" => Ty::I16,
            "i32" => Ty::I32,
            "i64" => Ty::I64,
            "u8" => Ty::U8,
            "u16" => Ty::U16,
            "u32" => Ty::U32,
            "u64" => Ty::U64,
            "f32" => Ty::F32,
            "f64" => Ty::F64,
            "Bool" | "bool" => Ty::Bool,
            "String" => Ty::Str,
            "Unit" => Ty::Unit,
            _ => {
                if self.type_defs.contains_key(name) || self.struct_defs.contains_key(name) {
                    Ty::Con(SmolStr::new(name), vec![])
                } else {
                    self.error(format!("undefined type '{}'", name), span);
                    Ty::Error
                }
            }
        }
    }

    // ── First pass: collect definitions ──────────────────────────

    fn collect_definitions(&mut self) {
        // Register builtins
        self.register_builtin_schemes();

        let items: Vec<_> = self
            .module
            .items
            .iter()
            .map(|(item, _)| item.clone())
            .collect();

        for item in &items {
            match item {
                Item::Deftype(d) => self.collect_deftype(d),
                Item::Defstruct(d) => self.collect_defstruct(d),
                _ => {}
            }
        }

        for item in &items {
            if let Item::Defn(d) = item {
                self.collect_defn_sig(d);
            }
        }
    }

    fn collect_deftype(&mut self, d: &Deftype) {
        let mut variants = Vec::new();
        for v in &d.variants {
            self.constructors.insert(
                v.name.clone(),
                (d.name.clone(), d.type_params.clone(), v.fields.clone()),
            );
            variants.push(VariantDef {
                name: v.name.clone(),
                field_type_exprs: v.fields.clone(),
            });
        }
        self.type_defs.insert(
            d.name.clone(),
            TypeDef {
                name: d.name.clone(),
                params: d.type_params.clone(),
                variants,
            },
        );
    }

    fn collect_defstruct(&mut self, d: &Defstruct) {
        let fields: Vec<(SmolStr, TypeExprId)> = d
            .fields
            .iter()
            .map(|f| (f.name.clone(), f.type_ann))
            .collect();
        self.struct_defs.insert(
            d.name.clone(),
            StructDef {
                name: d.name.clone(),
                params: d.type_params.clone(),
                fields,
            },
        );
    }

    fn collect_defn_sig(&mut self, d: &Defn) {
        // Save and clear type param scope
        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        let mut quantified = Vec::new();

        let param_types: Vec<Ty> = d
            .params
            .iter()
            .map(|p| {
                if let Some(type_id) = p.type_ann {
                    self.resolve_type_expr(type_id)
                } else {
                    self.fresh_var()
                }
            })
            .collect();

        let return_type = if let Some(ret_id) = d.return_type {
            self.resolve_type_expr(ret_id)
        } else {
            self.fresh_var()
        };

        // Any type variables created during resolution are quantified
        for ty in self.type_param_scope.values() {
            if let Ty::Var(id) = ty {
                quantified.push(*id);
            }
        }

        self.fn_schemes.insert(
            d.name.clone(),
            FnScheme {
                quantified,
                param_types,
                return_type,
            },
        );

        self.type_param_scope = saved;
    }

    fn register_builtin_schemes(&mut self) {
        // Builtins use fresh vars at each call site, handled in check_call
        // We just need to register names so they don't show as "undefined"
        let builtins = [
            "+", "-", "*", "/", "mod", "<", ">", "<=", ">=", "=", "!=", "not", "and", "or",
            "println", "print", "str", "len", "nth", "append", "type-of",
        ];
        for name in builtins {
            self.scopes[0].insert(
                SmolStr::new(name),
                Ty::Fn(vec![], Box::new(Ty::Unit)), // placeholder
            );
        }
    }

    // ── Second pass: check items ─────────────────────────────────

    fn check_items(&mut self) {
        // Register all function names in scope
        let fn_names: Vec<SmolStr> = self.fn_schemes.keys().cloned().collect();
        for name in &fn_names {
            let scheme = self.fn_schemes[name].clone();
            let fn_ty = Ty::Fn(
                scheme.param_types.clone(),
                Box::new(scheme.return_type.clone()),
            );
            self.define_var(name.clone(), fn_ty);
        }

        // Register constructors in scope
        let cons: Vec<_> = self.constructors.clone().into_iter().collect();
        for (name, _) in &cons {
            // Constructor types are computed on-demand during instantiation
            self.define_var(name.clone(), Ty::Fn(vec![], Box::new(Ty::Unit))); // placeholder
        }

        // Check each function
        let items: Vec<_> = self
            .module
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

        for defn in &items {
            self.check_defn(defn);
        }
    }

    fn check_defn(&mut self, d: &Defn) {
        let scheme = match self.fn_schemes.get(&d.name) {
            Some(s) => s.clone(),
            None => return,
        };

        self.push_scope();

        // Bind parameters
        for (param, ty) in d.params.iter().zip(scheme.param_types.iter()) {
            self.define_var(param.name.clone(), ty.clone());
        }

        // Check body
        let body_ty = self.check_body(&d.body);

        // Unify body type with declared return type
        if !d.body.is_empty() {
            self.unify(&body_ty, &scheme.return_type, d.span);
        }

        self.pop_scope();

        // Default unresolved numeric literals
        self.default_numeric_vars();
    }

    fn check_body(&mut self, body: &[ExprId]) -> Ty {
        let mut ty = Ty::Unit;
        for &expr_id in body {
            ty = self.check_expr(expr_id);
        }
        ty
    }

    // ── Expression checking ──────────────────────────────────────

    fn check_expr(&mut self, expr_id: ExprId) -> Ty {
        let ty = self.check_expr_inner(expr_id);
        self.expr_types.insert(expr_id, ty.clone());
        ty
    }

    fn check_expr_inner(&mut self, expr_id: ExprId) -> Ty {
        let expr = &self.module.exprs[expr_id];
        let span = expr.span;

        match &expr.kind {
            ExprKind::Lit(lit) => match lit {
                Literal::Int(_) => self.fresh_int_var(),
                Literal::Float(_) => self.fresh_float_var(),
                Literal::String(_) => Ty::Str,
                Literal::Bool(_) => Ty::Bool,
            },

            ExprKind::Var(name) => {
                // Check if it's a constructor
                if let Some(info) = self.constructors.get(name).cloned() {
                    return self.instantiate_constructor(&info.0, &info.1, &info.2);
                }
                // Check if it's a known function (instantiate its scheme)
                if let Some(scheme) = self.fn_schemes.get(name).cloned() {
                    let (params, ret) = self.instantiate(&scheme);
                    return Ty::Fn(params, Box::new(ret));
                }
                if let Some(ty) = self.lookup_var(name) {
                    ty
                } else {
                    self.error(format!("undefined variable '{}'", name), span);
                    Ty::Error
                }
            }

            ExprKind::Call { func, args } => self.check_call(*func, args, span),

            ExprKind::Let { bindings, body } => {
                self.push_scope();
                for b in bindings {
                    let val_ty = self.check_expr(b.value);
                    if let Some(ann_id) = b.type_ann {
                        let ann_ty = self.resolve_type_expr(ann_id);
                        self.unify(&val_ty, &ann_ty, b.span);
                        self.define_var(b.name.clone(), ann_ty);
                    } else {
                        self.define_var(b.name.clone(), val_ty);
                    }
                }
                let result = self.check_body(body);
                self.pop_scope();
                result
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(*condition);
                self.unify(&cond_ty, &Ty::Bool, span);
                let then_ty = self.check_expr(*then_branch);
                if let Some(else_id) = else_branch {
                    let else_ty = self.check_expr(*else_id);
                    self.unify(&then_ty, &else_ty, span);
                    then_ty
                } else {
                    // if without else returns Unit
                    self.unify(&then_ty, &Ty::Unit, span);
                    Ty::Unit
                }
            }

            ExprKind::Cond {
                clauses,
                else_clause,
            } => {
                let result_ty = self.fresh_var();
                for &(test_id, body_id) in clauses {
                    let test_ty = self.check_expr(test_id);
                    self.unify(&test_ty, &Ty::Bool, span);
                    let body_ty = self.check_expr(body_id);
                    self.unify(&result_ty, &body_ty, span);
                }
                if let Some(else_id) = else_clause {
                    let else_ty = self.check_expr(*else_id);
                    self.unify(&result_ty, &else_ty, span);
                }
                result_ty
            }

            ExprKind::When { condition, body } | ExprKind::Unless { condition, body } => {
                let cond_ty = self.check_expr(*condition);
                self.unify(&cond_ty, &Ty::Bool, span);
                if !body.is_empty() {
                    self.check_body(body);
                }
                Ty::Unit
            }

            ExprKind::Match { scrutinee, arms } => {
                let scrut_ty = self.check_expr(*scrutinee);
                let result_ty = self.fresh_var();

                let mut seen_variants: Vec<SmolStr> = Vec::new();

                for arm in arms {
                    self.push_scope();
                    let bindings = self.check_pattern(arm.pattern, &scrut_ty);
                    for (name, ty) in bindings {
                        self.define_var(name, ty);
                    }
                    // Track variants for exhaustiveness
                    let pat = &self.module.patterns[arm.pattern];
                    if let PatternKind::Constructor { name, .. } = &pat.kind {
                        seen_variants.push(name.clone());
                    }
                    let arm_ty = self.check_body(&arm.body);
                    self.unify(&result_ty, &arm_ty, arm.span);
                    self.pop_scope();
                }

                // Basic exhaustiveness check for ADTs
                self.check_exhaustiveness(&scrut_ty, &seen_variants, arms, span);

                result_ty
            }

            ExprKind::Lambda {
                params,
                return_type,
                body,
            } => {
                self.push_scope();
                let saved = self.type_param_scope.clone();
                self.type_param_scope.clear();

                let param_types: Vec<Ty> = params
                    .iter()
                    .map(|p| {
                        let ty = if let Some(ann_id) = p.type_ann {
                            self.resolve_type_expr(ann_id)
                        } else {
                            self.fresh_var()
                        };
                        self.define_var(p.name.clone(), ty.clone());
                        ty
                    })
                    .collect();

                let body_ty = self.check_body(body);

                if let Some(ret_id) = return_type {
                    let ret_ty = self.resolve_type_expr(*ret_id);
                    self.unify(&body_ty, &ret_ty, span);
                }

                self.type_param_scope = saved;
                self.pop_scope();

                let ret = if let Some(ret_id) = return_type {
                    self.resolve_type_expr(*ret_id)
                } else {
                    body_ty
                };
                Ty::Fn(param_types, Box::new(ret))
            }

            ExprKind::Do { body } => self.check_body(body),

            ExprKind::SetBang { place, value } => {
                let place_ty = self.check_expr(*place);
                let val_ty = self.check_expr(*value);
                self.unify(&place_ty, &val_ty, span);
                Ty::Unit
            }

            ExprKind::Ann { type_ann, expr } => {
                let ann_ty = self.resolve_type_expr(*type_ann);
                let expr_ty = self.check_expr(*expr);
                self.unify(&expr_ty, &ann_ty, span);
                ann_ty
            }

            ExprKind::FieldAccess(field) => {
                // Standalone field access: becomes a function from struct → field type
                // Can't determine the full type without knowing the struct, return a fn type
                let arg_ty = self.fresh_var();
                let ret_ty = self.fresh_var();
                // We'd need to constrain arg_ty to have field `field`, skip for now
                let _ = field;
                Ty::Fn(vec![arg_ty], Box::new(ret_ty))
            }

            ExprKind::VectorLit(elems) => {
                let elem_ty = self.fresh_var();
                for &e in elems {
                    let ty = self.check_expr(e);
                    self.unify(&elem_ty, &ty, span);
                }
                Ty::Vector(Box::new(elem_ty))
            }

            ExprKind::MapLit(pairs) => {
                let key_ty = self.fresh_var();
                let val_ty = self.fresh_var();
                for &(k, v) in pairs {
                    let kt = self.check_expr(k);
                    let vt = self.check_expr(v);
                    self.unify(&key_ty, &kt, span);
                    self.unify(&val_ty, &vt, span);
                }
                Ty::Map(Box::new(key_ty), Box::new(val_ty))
            }

            ExprKind::Unsafe { body } => self.check_body(body),

            ExprKind::Try(inner) => {
                // For now, just check the inner expression
                self.check_expr(*inner)
            }
        }
    }

    // ── Call checking ────────────────────────────────────────────

    fn check_call(&mut self, func_id: ExprId, args: &[Arg], span: Span) -> Ty {
        let func_expr = &self.module.exprs[func_id];

        // Field access: (.field obj)
        if let ExprKind::FieldAccess(field) = &func_expr.kind {
            if args.is_empty() {
                self.error(
                    format!("field access .{} requires an argument", field),
                    span,
                );
                return Ty::Error;
            }
            let field = field.clone();
            let obj_ty = self.check_expr(args[0].value);
            return self.check_field_access(&obj_ty, &field, span);
        }

        // Check for builtin call
        if let ExprKind::Var(name) = &func_expr.kind {
            if self.is_builtin(name) {
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();
                return self.check_builtin_call(name, &arg_tys, span);
            }

            // Check for ADT constructor call
            if let Some(info) = self.constructors.get(name).cloned() {
                let con_ty = self.instantiate_constructor(&info.0, &info.1, &info.2);
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();

                let ret = self.fresh_var();
                let expected = Ty::Fn(arg_tys.clone(), Box::new(ret.clone()));
                // For zero-arg constructors used as calls (shouldn't happen but handle it)
                if let Ty::Fn(params, result) = &con_ty {
                    if params.len() != arg_tys.len() {
                        self.error(
                            format!(
                                "constructor {} expects {} arguments, got {}",
                                name,
                                params.len(),
                                arg_tys.len()
                            ),
                            span,
                        );
                        return Ty::Error;
                    }
                    for (p, a) in params.iter().zip(arg_tys.iter()) {
                        self.unify(p, a, span);
                    }
                    return *result.clone();
                } else {
                    // Zero-arg constructor being "called" — just return it
                    if arg_tys.is_empty() {
                        return con_ty;
                    }
                    self.unify(&con_ty, &expected, span);
                    return ret;
                }
            }

            // Check for struct constructor call
            if let Some(sdef) = self.struct_defs.get(name).cloned() {
                return self.check_struct_construction(&sdef, args, span);
            }

            // Regular function call — instantiate the scheme
            if let Some(scheme) = self.fn_schemes.get(name).cloned() {
                let (param_tys, ret_ty) = self.instantiate(&scheme);
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();

                if param_tys.len() != arg_tys.len() {
                    self.error(
                        format!(
                            "function '{}' expects {} arguments, got {}",
                            name,
                            param_tys.len(),
                            arg_tys.len()
                        ),
                        span,
                    );
                    return Ty::Error;
                }

                for (p, a) in param_tys.iter().zip(arg_tys.iter()) {
                    self.unify(p, a, span);
                }
                return ret_ty;
            }
        }

        // General case: evaluate function, check it's callable
        let func_ty = self.check_expr(func_id);
        let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();
        let ret = self.fresh_var();
        let expected = Ty::Fn(arg_tys, Box::new(ret.clone()));
        self.unify(&func_ty, &expected, span);
        ret
    }

    fn check_field_access(&mut self, obj_ty: &Ty, field: &str, span: Span) -> Ty {
        let obj_ty = self.apply(obj_ty);

        if let Ty::Con(name, type_args) = &obj_ty {
            if let Some(sdef) = self.struct_defs.get(name).cloned() {
                // Instantiate struct type params
                let mapping: HashMap<SmolStr, Ty> = sdef
                    .params
                    .iter()
                    .zip(type_args.iter())
                    .map(|(p, t)| (p.clone(), t.clone()))
                    .collect();

                for (fname, ftype_id) in &sdef.fields {
                    if fname == field {
                        let saved = self.type_param_scope.clone();
                        self.type_param_scope = mapping
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect();
                        let field_ty = self.resolve_type_expr(*ftype_id);
                        self.type_param_scope = saved;
                        return field_ty;
                    }
                }

                self.error(format!("struct {} has no field '{}'", name, field), span);
                return Ty::Error;
            }
        }

        if matches!(obj_ty, Ty::Var(_) | Ty::Error) {
            // Can't determine struct type yet — return fresh var
            return self.fresh_var();
        }

        self.error(
            format!("cannot access field .{} on type {}", field, obj_ty),
            span,
        );
        Ty::Error
    }

    fn check_struct_construction(&mut self, sdef: &StructDef, args: &[Arg], span: Span) -> Ty {
        // Create fresh type vars for type params
        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        let type_args: Vec<Ty> = sdef
            .params
            .iter()
            .map(|p| {
                let v = self.fresh_var();
                self.type_param_scope.insert(p.clone(), v.clone());
                v
            })
            .collect();

        let has_named = args.iter().any(|a| a.name.is_some());

        if has_named {
            // Named args — match by field name
            for arg in args {
                if let Some(arg_name) = &arg.name {
                    let field = sdef.fields.iter().find(|(n, _)| n == arg_name);
                    if let Some((_, ftype_id)) = field {
                        let expected = self.resolve_type_expr(*ftype_id);
                        let actual = self.check_expr(arg.value);
                        self.unify(&expected, &actual, arg.span);
                    } else {
                        self.error(
                            format!("struct {} has no field '{}'", sdef.name, arg_name),
                            span,
                        );
                    }
                } else {
                    self.error(
                        "cannot mix positional and named args in struct construction",
                        span,
                    );
                }
            }
        } else {
            // Positional args
            if args.len() != sdef.fields.len() {
                self.error(
                    format!(
                        "struct {} has {} fields, got {} arguments",
                        sdef.name,
                        sdef.fields.len(),
                        args.len()
                    ),
                    span,
                );
            } else {
                for (arg, (_, ftype_id)) in args.iter().zip(sdef.fields.iter()) {
                    let expected = self.resolve_type_expr(*ftype_id);
                    let actual = self.check_expr(arg.value);
                    self.unify(&expected, &actual, arg.span);
                }
            }
        }

        self.type_param_scope = saved;
        Ty::Con(sdef.name.clone(), type_args)
    }

    // ── Constructor instantiation ────────────────────────────────

    fn instantiate_constructor(
        &mut self,
        type_name: &SmolStr,
        type_params: &[SmolStr],
        field_type_exprs: &[TypeExprId],
    ) -> Ty {
        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        let fresh_args: Vec<Ty> = type_params
            .iter()
            .map(|p| {
                let v = self.fresh_var();
                self.type_param_scope.insert(p.clone(), v.clone());
                v
            })
            .collect();

        let field_types: Vec<Ty> = field_type_exprs
            .iter()
            .map(|&id| self.resolve_type_expr(id))
            .collect();

        self.type_param_scope = saved;

        let result_ty = Ty::Con(type_name.clone(), fresh_args);

        if field_types.is_empty() {
            // Nullary constructor, e.g. None
            result_ty
        } else {
            // Constructor function
            Ty::Fn(field_types, Box::new(result_ty))
        }
    }

    // ── Pattern checking ─────────────────────────────────────────

    fn check_pattern(&mut self, pat_id: PatternId, expected: &Ty) -> Vec<(SmolStr, Ty)> {
        let pat = &self.module.patterns[pat_id];
        let span = pat.span;

        match &pat.kind {
            PatternKind::Wildcard => vec![],

            PatternKind::Var(name) => {
                vec![(name.clone(), expected.clone())]
            }

            PatternKind::Literal(lit) => {
                let lit_ty = match lit {
                    Literal::Int(_) => self.fresh_int_var(),
                    Literal::Float(_) => self.fresh_float_var(),
                    Literal::String(_) => Ty::Str,
                    Literal::Bool(_) => Ty::Bool,
                };
                self.unify(expected, &lit_ty, span);
                vec![]
            }

            PatternKind::Constructor { name, args } => {
                let name = name.clone();
                let args = args.clone();

                if let Some(info) = self.constructors.get(&name).cloned() {
                    let con_ty = self.instantiate_constructor(&info.0, &info.1, &info.2);

                    match con_ty {
                        Ty::Fn(param_tys, result_ty) => {
                            self.unify(expected, &result_ty, span);
                            if param_tys.len() != args.len() {
                                self.error(
                                    format!(
                                        "constructor {} expects {} fields, got {} in pattern",
                                        name,
                                        param_tys.len(),
                                        args.len()
                                    ),
                                    span,
                                );
                                return vec![];
                            }
                            let mut bindings = Vec::new();
                            for (sub_pat, param_ty) in args.iter().zip(param_tys.iter()) {
                                bindings.extend(self.check_pattern(*sub_pat, param_ty));
                            }
                            bindings
                        }
                        _ => {
                            // Nullary constructor
                            self.unify(expected, &con_ty, span);
                            if !args.is_empty() {
                                self.error(
                                    format!(
                                        "constructor {} takes no arguments, got {} in pattern",
                                        name,
                                        args.len()
                                    ),
                                    span,
                                );
                            }
                            vec![]
                        }
                    }
                } else {
                    self.error(format!("unknown constructor '{}'", name), span);
                    vec![]
                }
            }

            PatternKind::StructDestructure { fields } => {
                let expected = self.apply(expected);
                let fields = fields.clone();

                if let Ty::Con(name, type_args) = &expected {
                    if let Some(sdef) = self.struct_defs.get(name).cloned() {
                        let mapping: HashMap<SmolStr, Ty> = sdef
                            .params
                            .iter()
                            .zip(type_args.iter())
                            .map(|(p, t)| (p.clone(), t.clone()))
                            .collect();

                        let mut bindings = Vec::new();
                        for field_pat in &fields {
                            let field_def =
                                sdef.fields.iter().find(|(n, _)| *n == field_pat.field_name);
                            if let Some((_, ftype_id)) = field_def {
                                let saved = self.type_param_scope.clone();
                                self.type_param_scope = mapping
                                    .iter()
                                    .map(|(k, v)| (k.clone(), v.clone()))
                                    .collect();
                                let field_ty = self.resolve_type_expr(*ftype_id);
                                self.type_param_scope = saved;

                                let bind_name = field_pat
                                    .binding
                                    .clone()
                                    .unwrap_or_else(|| field_pat.field_name.clone());
                                bindings.push((bind_name, field_ty));
                            } else {
                                self.error(
                                    format!(
                                        "struct {} has no field '{}'",
                                        name, field_pat.field_name
                                    ),
                                    span,
                                );
                            }
                        }
                        return bindings;
                    }
                }

                self.error(
                    format!("struct destructuring on non-struct type {}", expected),
                    span,
                );
                vec![]
            }
        }
    }

    // ── Exhaustiveness checking ──────────────────────────────────

    fn check_exhaustiveness(
        &mut self,
        scrut_ty: &Ty,
        seen_variants: &[SmolStr],
        arms: &[MatchArm],
        span: Span,
    ) {
        let scrut_ty = self.apply(scrut_ty);

        // Check if there's a wildcard or variable pattern (catches everything)
        let has_catchall = arms.iter().any(|arm| {
            let pat = &self.module.patterns[arm.pattern];
            matches!(pat.kind, PatternKind::Wildcard | PatternKind::Var(_))
        });

        if has_catchall {
            return;
        }

        // For ADTs, check all variants are covered
        if let Ty::Con(name, _) = &scrut_ty {
            if let Some(tdef) = self.type_defs.get(name).cloned() {
                let missing: Vec<&SmolStr> = tdef
                    .variants
                    .iter()
                    .filter(|v| !seen_variants.contains(&v.name))
                    .map(|v| &v.name)
                    .collect();

                if !missing.is_empty() {
                    let names: Vec<&str> = missing.iter().map(|n| n.as_str()).collect();
                    self.error(
                        format!(
                            "non-exhaustive match: missing variants: {}",
                            names.join(", ")
                        ),
                        span,
                    );
                }
            }
        }
    }

    // ── Builtin call checking ────────────────────────────────────

    fn is_builtin(&self, name: &str) -> bool {
        matches!(
            name,
            "+" | "-"
                | "*"
                | "/"
                | "mod"
                | "<"
                | ">"
                | "<="
                | ">="
                | "="
                | "!="
                | "not"
                | "and"
                | "or"
                | "println"
                | "print"
                | "str"
                | "len"
                | "nth"
                | "append"
                | "type-of"
        )
    }

    fn check_builtin_call(&mut self, name: &str, arg_tys: &[Ty], span: Span) -> Ty {
        match name {
            "+" | "*" => {
                if arg_tys.is_empty() {
                    return self.fresh_int_var();
                }
                let t = arg_tys[0].clone();
                for a in &arg_tys[1..] {
                    self.unify(&t, a, span);
                }
                t
            }
            "-" => {
                if arg_tys.is_empty() {
                    self.error("- requires at least 1 argument", span);
                    return Ty::Error;
                }
                if arg_tys.len() == 1 {
                    return arg_tys[0].clone();
                }
                let t = arg_tys[0].clone();
                for a in &arg_tys[1..] {
                    self.unify(&t, a, span);
                }
                t
            }
            "/" | "mod" => {
                if arg_tys.len() < 2 {
                    self.error(format!("{} requires at least 2 arguments", name), span);
                    return Ty::Error;
                }
                let t = arg_tys[0].clone();
                for a in &arg_tys[1..] {
                    self.unify(&t, a, span);
                }
                t
            }
            "<" | ">" | "<=" | ">=" => {
                if arg_tys.len() != 2 {
                    self.error("comparison requires exactly 2 arguments", span);
                    return Ty::Error;
                }
                self.unify(&arg_tys[0], &arg_tys[1], span);
                Ty::Bool
            }
            "=" | "!=" => {
                if arg_tys.len() != 2 {
                    self.error(format!("{} requires exactly 2 arguments", name), span);
                    return Ty::Error;
                }
                self.unify(&arg_tys[0], &arg_tys[1], span);
                Ty::Bool
            }
            "not" => {
                if arg_tys.len() != 1 {
                    self.error("not requires exactly 1 argument", span);
                    return Ty::Error;
                }
                self.unify(&arg_tys[0], &Ty::Bool, span);
                Ty::Bool
            }
            "and" | "or" => {
                for a in arg_tys {
                    self.unify(a, &Ty::Bool, span);
                }
                Ty::Bool
            }
            "println" => {
                // Accept any single argument (or none for bare newline)
                if arg_tys.len() > 1 {
                    self.error("println expects 0 or 1 arguments", span);
                }
                Ty::Unit
            }
            "print" => {
                if arg_tys.len() != 1 {
                    self.error("print expects exactly 1 argument", span);
                }
                Ty::Unit
            }
            "str" => Ty::Str,
            "len" => {
                // Accept Vector, Map, or String
                if arg_tys.len() != 1 {
                    self.error("len requires 1 argument", span);
                }
                Ty::I64
            }
            "nth" => {
                if arg_tys.len() != 2 {
                    self.error("nth requires 2 arguments", span);
                    return Ty::Error;
                }
                if let Ty::Vector(elem) = &self.apply(&arg_tys[0]) {
                    *elem.clone()
                } else {
                    self.fresh_var()
                }
            }
            "append" => {
                if arg_tys.len() != 2 {
                    self.error("append requires 2 arguments", span);
                    return Ty::Error;
                }
                let elem = self.fresh_var();
                let vec_ty = Ty::Vector(Box::new(elem.clone()));
                self.unify(&arg_tys[0], &vec_ty, span);
                self.unify(&arg_tys[1], &elem, span);
                vec_ty
            }
            "type-of" => {
                if arg_tys.len() != 1 {
                    self.error("type-of requires 1 argument", span);
                }
                Ty::Str
            }
            _ => {
                self.error(format!("unknown builtin '{}'", name), span);
                Ty::Error
            }
        }
    }

    // ── Numeric defaulting ───────────────────────────────────────

    fn default_numeric_vars(&mut self) {
        for i in 0..self.subst.len() {
            if self.subst[i].is_none() {
                match self.var_kinds[i] {
                    VarKind::IntLit => self.subst[i] = Some(Ty::I32),
                    VarKind::FloatLit => self.subst[i] = Some(Ty::F64),
                    VarKind::General => {}
                }
            }
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn check_src(source: &str) -> Vec<TypeError> {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let result = check(&module);
        result.errors
    }

    fn check_ok(source: &str) {
        let errors = check_src(source);
        assert!(errors.is_empty(), "unexpected type errors: {:?}", errors);
    }

    fn check_err(source: &str) -> String {
        let errors = check_src(source);
        assert!(!errors.is_empty(), "expected type errors, got none");
        errors
            .iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join("\n")
    }

    // ── Passing programs ─────────────────────────────────────────

    #[test]
    fn basic_arithmetic() {
        check_ok("(defn main () (+ 1 2))");
    }

    #[test]
    fn annotated_function() {
        check_ok("(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
    }

    #[test]
    fn annotated_return_unit() {
        check_ok("(defn greet () : Unit (println \"hello\"))");
    }

    #[test]
    fn let_binding_inferred() {
        check_ok(
            "(defn main ()
               (let ((x 5) (y 10))
                 (+ x y)))",
        );
    }

    #[test]
    fn let_binding_annotated() {
        check_ok(
            "(defn main ()
               (let (((x : i32) 5))
                 (+ x 1)))",
        );
    }

    #[test]
    fn if_expression_types_match() {
        check_ok(
            "(defn main ()
               (if (> 1 0) 42 0))",
        );
    }

    #[test]
    fn cond_expression() {
        check_ok(
            "(defn main ()
               (cond
                 ((> 1 0) 42)
                 (else 0)))",
        );
    }

    #[test]
    fn function_call_types() {
        check_ok(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1 2))",
        );
    }

    #[test]
    fn recursion() {
        check_ok(
            "(defn factorial ((n : i32)) : i32
               (if (<= n 1) 1 (* n (factorial (- n 1)))))",
        );
    }

    #[test]
    fn lambda() {
        check_ok(
            "(defn main ()
               (let ((f (fn (x) (+ x 1))))
                 (f 10)))",
        );
    }

    #[test]
    fn closures() {
        check_ok(
            "(defn make-adder ((n : i32)) : (Fn [i32] i32)
               (fn (x) (+ x n)))",
        );
    }

    #[test]
    fn deftype_and_match() {
        check_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn unwrap ((x : (Option i32))) : i32
               (match x
                 ((Some v) v)
                 (None 0)))",
        );
    }

    #[test]
    fn defstruct_and_field_access() {
        check_ok(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main ()
               (let ((p (Point 1.0 2.0)))
                 (.x p)))",
        );
    }

    #[test]
    fn vector_literal() {
        check_ok("(defn main () [1 2 3])");
    }

    #[test]
    fn numeric_defaults_i32() {
        // 42 defaults to i32; function expects i32 — should work
        check_ok(
            "(defn id ((x : i32)) : i32 x)
             (defn main () (id 42))",
        );
    }

    #[test]
    fn numeric_defaults_f64() {
        check_ok(
            "(defn id ((x : f64)) : f64 x)
             (defn main () (id 3.5))",
        );
    }

    #[test]
    fn numeric_promotion_i64() {
        // Integer literal should unify with i64 when used in that context
        check_ok(
            "(defn id ((x : i64)) : i64 x)
             (defn main () (id 42))",
        );
    }

    #[test]
    fn match_with_wildcard() {
        check_ok(
            "(defn classify ((n : i32)) : String
               (match n
                 (0 \"zero\")
                 (_ \"other\")))",
        );
    }

    #[test]
    fn when_unless() {
        check_ok(
            "(defn main ()
               (when (> 1 0) (println \"yes\"))
               (unless (> 1 0) (println \"no\")))",
        );
    }

    #[test]
    fn do_block() {
        check_ok(
            "(defn main () : i32
               (do (println \"hello\") 42))",
        );
    }

    #[test]
    fn set_bang() {
        check_ok(
            "(defn main ()
               (let ((mut x 0))
                 (set! x 5)))",
        );
    }

    #[test]
    fn mutual_recursion() {
        check_ok(
            "(defn is-even ((n : i32)) : Bool
               (if (= n 0) true (is-odd (- n 1))))
             (defn is-odd ((n : i32)) : Bool
               (if (= n 0) false (is-even (- n 1))))",
        );
    }

    #[test]
    fn polymorphic_function_multiple_calls() {
        // A polymorphic function can be called with different types
        check_ok(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main ()
               (let ((a (Some 42))
                     (b (Some \"hello\")))
                 (println a)
                 (println b)))",
        );
    }

    // ── Failing programs (type errors) ───────────────────────────

    #[test]
    fn error_return_type_mismatch() {
        let msg = check_err("(defn bad () : i32 (str \"hello\"))");
        insta::assert_snapshot!(msg, @"type mismatch: expected String, got i32");
    }

    #[test]
    fn error_arg_type_mismatch() {
        let msg = check_err(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1 \"two\"))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected i32, got String");
    }

    #[test]
    fn error_wrong_arg_count() {
        let msg = check_err(
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))
             (defn main () (add 1))",
        );
        insta::assert_snapshot!(msg, @"function 'add' expects 2 arguments, got 1");
    }

    #[test]
    fn error_undefined_variable() {
        let msg = check_err("(defn main () (+ x 1))");
        insta::assert_snapshot!(msg, @"undefined variable 'x'");
    }

    #[test]
    fn error_if_branch_mismatch() {
        let msg = check_err(
            "(defn main ()
               (if true 42 \"hello\"))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got String");
    }

    #[test]
    fn error_non_exhaustive_match() {
        let msg = check_err(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main ()
               (match (Some 1)
                 ((Some x) x)))",
        );
        insta::assert_snapshot!(msg, @"non-exhaustive match: missing variants: None");
    }

    #[test]
    fn error_wrong_constructor_arity() {
        let msg = check_err(
            "(deftype (Option 'a)
               (Some 'a)
               None)
             (defn main () (Some 1 2))",
        );
        insta::assert_snapshot!(msg, @"constructor Some expects 1 arguments, got 2");
    }

    #[test]
    fn error_struct_wrong_field_count() {
        let msg = check_err(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main () (Point 1.0))",
        );
        insta::assert_snapshot!(msg, @"struct Point has 2 fields, got 1 arguments");
    }

    #[test]
    fn error_struct_unknown_field() {
        let msg = check_err(
            "(defstruct Point
               (x : f64)
               (y : f64))
             (defn main ()
               (let ((p (Point 1.0 2.0)))
                 (.z p)))",
        );
        insta::assert_snapshot!(msg, @"struct Point has no field 'z'");
    }

    #[test]
    fn error_not_requires_bool() {
        let msg = check_err("(defn main () (not 42))");
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got Bool");
    }

    #[test]
    fn error_if_condition_not_bool() {
        let msg = check_err("(defn main () (if 42 1 2))");
        insta::assert_snapshot!(msg, @"type mismatch: expected integer type, got Bool");
    }

    #[test]
    fn error_float_literal_as_integer() {
        let msg = check_err(
            "(defn id ((x : i32)) : i32 x)
             (defn main () (id 3.5))",
        );
        insta::assert_snapshot!(msg, @"type mismatch: expected float type, got i32");
    }
}
