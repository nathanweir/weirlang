use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
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
    /// Higher-kinded type application: `App(f, [a])` represents `('f 'a)`.
    /// When `f` is resolved (e.g. to Option), `App(Con("Option", []), [Var(a)])` simplifies
    /// to `Con("Option", [Var(a)])` during unification/apply.
    App(Box<Ty>, Vec<Ty>),
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
            Ty::App(func, args) => {
                write!(f, "({}", func)?;
                for a in args {
                    write!(f, " {}", a)?;
                }
                write!(f, ")")
            }
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

/// Resolved function type: param types + return type after substitution.
#[derive(Clone, Debug)]
pub struct FnType {
    pub param_types: Vec<Ty>,
    pub return_type: Ty,
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
}

// ── Public API ───────────────────────────────────────────────────

pub fn check(module: &Module) -> TypeCheckResult {
    check_with_externals(module, &HashSet::new())
}

/// Type-check a module, treating `externals` as known symbols from other files.
/// Variables and constructors in `externals` won't produce "undefined" errors;
/// they'll be assigned fresh type variables (opaque types).
pub fn check_with_externals(module: &Module, externals: &HashSet<SmolStr>) -> TypeCheckResult {
    let mut checker = TypeChecker::new(module, externals);
    checker.collect_definitions();
    checker.check_items();
    checker.compute_reverse_deps();

    // Apply final substitution to all recorded expression types
    let mut expr_types = ArenaMap::default();
    for (id, ty) in checker.expr_types.iter() {
        expr_types.insert(id, checker.apply(ty));
    }

    // Apply final substitution to all function schemes
    let mut fn_types = HashMap::new();
    for (name, scheme) in &checker.fn_schemes {
        let param_types = scheme
            .param_types
            .iter()
            .map(|t| checker.apply(t))
            .collect();
        let return_type = checker.apply(&scheme.return_type);
        fn_types.insert(
            name.clone(),
            FnType {
                param_types,
                return_type,
            },
        );
    }

    // Collect specializations from instance methods.
    // Map type checker instances to AST item indices (they're collected in the same order).
    let instance_item_indices: Vec<usize> = module
        .items
        .iter()
        .enumerate()
        .filter_map(|(i, (item, _))| {
            if matches!(item, Item::Instance(_)) {
                Some(i)
            } else {
                None
            }
        })
        .collect();

    let mut specializations = Vec::new();
    for (inst_idx, inst) in checker.instances.iter().enumerate() {
        let item_index = instance_item_indices.get(inst_idx).copied();
        for (method_name, mangled_name) in &inst.methods {
            if let Some(scheme) = checker.fn_schemes.get(mangled_name) {
                let param_types = scheme
                    .param_types
                    .iter()
                    .map(|t| checker.apply(t))
                    .collect();
                let return_type = checker.apply(&scheme.return_type);
                specializations.push(Specialization {
                    original_name: method_name.clone(),
                    mangled_name: mangled_name.clone(),
                    param_types,
                    return_type,
                    instance_item_index: item_index,
                });
            }
        }
    }

    // Collect specializations for generic user function calls.
    // Walk all expressions looking for calls where the function type has concrete types
    // but the function's scheme has quantified type variables.
    let mut seen_specs: HashMap<SmolStr, Vec<SmolStr>> = HashMap::new();
    for (id, _ty) in expr_types.iter() {
        let expr = &module.exprs[id];
        if let ExprKind::Call { func, .. } = &expr.kind {
            let func_expr = &module.exprs[*func];
            if let ExprKind::Var(name) = &func_expr.kind {
                if let Some(scheme) = checker.fn_schemes.get(name) {
                    if scheme.quantified.is_empty() || !scheme.constraints.is_empty() {
                        continue; // not a generic user function (or is a class method)
                    }
                    // Get the instantiated type at this call site
                    if let Some(Ty::Fn(param_tys, ret_ty)) = expr_types.get(*func) {
                        // Check all types are concrete (no remaining Var)
                        let all_concrete =
                            param_tys.iter().all(|t| !has_type_var(t)) && !has_type_var(ret_ty);
                        if !all_concrete {
                            continue;
                        }
                        // Build mangled name from concrete types
                        let type_key: String = param_tys
                            .iter()
                            .chain(std::iter::once(ret_ty.as_ref()))
                            .map(|t| format!("{}", t))
                            .collect::<Vec<_>>()
                            .join(",");
                        let mangled = SmolStr::new(format!("{}#{}", name, type_key));

                        // Avoid duplicate specializations
                        let entry = seen_specs.entry(name.clone()).or_default();
                        if !entry.contains(&mangled) {
                            entry.push(mangled.clone());
                            specializations.push(Specialization {
                                original_name: name.clone(),
                                mangled_name: mangled,
                                param_types: param_tys.clone(),
                                return_type: *ret_ty.clone(),
                                instance_item_index: None,
                            });
                        }
                    }
                }
            }
        }
    }

    TypeCheckResult {
        errors: checker.errors,
        expr_types,
        fn_types,
        method_resolutions: checker.method_resolutions,
        specializations,
        deps: checker.dep_graph,
    }
}

/// Check if a type contains any unresolved type variables.
fn has_type_var(ty: &Ty) -> bool {
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

/// A polymorphic function type: quantified type vars + constraints + param/return types.
#[derive(Clone)]
struct FnScheme {
    /// Type variable IDs that should be freshened at each call site.
    quantified: Vec<TyVarId>,
    /// Typeclass constraints (e.g., `(Eq 'a)`)
    constraints: Vec<ClassConstraint>,
    param_types: Vec<Ty>,
    return_type: Ty,
}

/// A resolved typeclass constraint: (Eq 'a), (Show i32), etc.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ClassConstraint {
    class_name: SmolStr,
    type_args: Vec<Ty>,
}

/// Typeclass definition stored during collection.
#[derive(Clone)]
struct ClassDef {
    #[allow(dead_code)]
    name: SmolStr,
    type_params: Vec<SmolStr>,
    /// Expected kind for each type param (e.g., `* -> *` for Functor's 'f)
    type_param_kinds: Vec<TyKind>,
    /// (method_name, method_type_expr_id)
    methods: Vec<(SmolStr, TypeExprId)>,
}

/// A registered typeclass instance.
#[derive(Clone)]
struct InstanceEntry {
    class_name: SmolStr,
    /// Concrete or partially-concrete type args, e.g., [Ty::I32] or [Ty::Con("Option", [Ty::Var(x)])]
    type_args: Vec<Ty>,
    /// Prerequisites: e.g., (Eq 'a) for instance (Eq (Option 'a))
    #[allow(dead_code)]
    constraints: Vec<ClassConstraint>,
    /// method_name -> mangled impl function name (e.g., "Eq#i32#==")
    #[allow(dead_code)]
    methods: HashMap<SmolStr, SmolStr>,
}

/// Kind of a type: `*` for concrete types, `* -> *` for type constructors like Option, etc.
#[derive(Clone, Debug, PartialEq, Eq)]
enum TyKind {
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
    fn from_param_count(n: usize) -> TyKind {
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

    // Typeclass definitions and instances
    class_defs: HashMap<SmolStr, ClassDef>,
    instances: Vec<InstanceEntry>,
    /// Deferred constraints to check after function body is fully typed
    deferred_constraints: Vec<(ClassConstraint, ExprId, Span)>,

    // Kind system
    /// Maps type constructor names to their kinds (e.g., "Option" → `* -> *`)
    type_kinds: HashMap<SmolStr, TyKind>,

    // Scope stack for variable types
    scopes: Vec<HashMap<SmolStr, Ty>>,

    // Type parameter scope: maps 'a → Ty::Var(id) during resolution
    type_param_scope: HashMap<SmolStr, Ty>,

    errors: Vec<TypeError>,

    /// Records the type of every expression (pre-substitution).
    expr_types: ArenaMap<ExprId, Ty>,

    /// For each call site that invokes a class method, the resolved concrete function name.
    method_resolutions: HashMap<ExprId, SmolStr>,

    /// Known symbol names from other workspace files.
    /// Variables/constructors in this set produce fresh type variables instead of errors.
    external_names: HashSet<SmolStr>,

    /// Return type of the function currently being checked (for `?` operator).
    current_fn_return_type: Option<Ty>,

    /// Name of the function currently being checked (for dependency tracking).
    current_fn_name: Option<SmolStr>,
    /// Dependency graph built during typechecking.
    dep_graph: DependencyGraph,

    /// Current arena nesting depth (0 = not inside with-arena)
    arena_depth: u32,
    /// Per-variable arena provenance depth (0 = GC/stack, >0 = arena), scope-aware
    var_provenance: Vec<HashMap<SmolStr, u32>>,
    /// Per-expression arena provenance depth
    expr_provenance: ArenaMap<ExprId, u32>,
}

impl<'a> TypeChecker<'a> {
    fn new(module: &'a Module, externals: &HashSet<SmolStr>) -> Self {
        Self {
            module,
            subst: Vec::new(),
            var_kinds: Vec::new(),
            type_defs: HashMap::new(),
            struct_defs: HashMap::new(),
            fn_schemes: HashMap::new(),
            constructors: HashMap::new(),
            class_defs: HashMap::new(),
            instances: Vec::new(),
            deferred_constraints: Vec::new(),
            type_kinds: HashMap::new(),
            scopes: vec![HashMap::new()],
            type_param_scope: HashMap::new(),
            errors: Vec::new(),
            expr_types: ArenaMap::default(),
            method_resolutions: HashMap::new(),
            external_names: externals.clone(),
            current_fn_return_type: None,
            current_fn_name: None,
            dep_graph: DependencyGraph::default(),
            arena_depth: 0,
            var_provenance: vec![HashMap::new()],
            expr_provenance: ArenaMap::default(),
        }
    }

    // ── Dependency tracking ─────────────────────────────────────

    fn record_call_dep(&mut self, callee: &SmolStr) {
        if let Some(caller) = &self.current_fn_name {
            self.dep_graph
                .call_deps
                .entry(caller.clone())
                .or_default()
                .insert(callee.clone());
        }
    }

    fn record_type_dep(&mut self, type_name: &SmolStr) {
        if let Some(fn_name) = &self.current_fn_name {
            self.dep_graph
                .type_deps
                .entry(fn_name.clone())
                .or_default()
                .insert(type_name.clone());
        }
    }

    fn compute_reverse_deps(&mut self) {
        self.dep_graph.callers.clear();
        for (caller, callees) in &self.dep_graph.call_deps {
            for callee in callees {
                self.dep_graph
                    .callers
                    .entry(callee.clone())
                    .or_default()
                    .insert(caller.clone());
            }
        }
        self.dep_graph.type_users.clear();
        for (fn_name, types) in &self.dep_graph.type_deps {
            for type_name in types {
                self.dep_graph
                    .type_users
                    .entry(type_name.clone())
                    .or_default()
                    .insert(fn_name.clone());
            }
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
            Ty::App(func, args) => {
                let applied_func = self.apply(func);
                let applied_args: Vec<Ty> = args.iter().map(|a| self.apply(a)).collect();
                // If the function resolved to a Con, flatten into Con(name, args)
                match applied_func {
                    Ty::Con(name, existing_args) => {
                        let mut all_args = existing_args;
                        all_args.extend(applied_args);
                        Ty::Con(name, all_args)
                    }
                    _ => Ty::App(Box::new(applied_func), applied_args),
                }
            }
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

            // HKT application: App(f1, a1) vs App(f2, a2)
            (Ty::App(f1, a1), Ty::App(f2, a2)) if a1.len() == a2.len() => {
                self.unify(f1, f2, span);
                for (x, y) in a1.iter().zip(a2.iter()) {
                    self.unify(x, y, span);
                }
            }

            // App(f, args) vs Con(name, args2): bind f to the constructor, unify args
            (Ty::App(f, a1), Ty::Con(name, a2)) => {
                // f should unify with the bare constructor (zero-arg Con)
                self.unify(f, &Ty::Con(name.clone(), vec![]), span);
                if a1.len() == a2.len() {
                    for (x, y) in a1.iter().zip(a2.iter()) {
                        self.unify(x, y, span);
                    }
                } else {
                    self.error(
                        format!(
                            "type constructor arity mismatch: {} applied to {} args, expected {}",
                            name,
                            a1.len(),
                            a2.len()
                        ),
                        span,
                    );
                }
            }

            (Ty::Con(name, a1), Ty::App(f, a2)) => {
                self.unify(f, &Ty::Con(name.clone(), vec![]), span);
                if a1.len() == a2.len() {
                    for (x, y) in a1.iter().zip(a2.iter()) {
                        self.unify(x, y, span);
                    }
                } else {
                    self.error(
                        format!(
                            "type constructor arity mismatch: {} applied to {} args, expected {}",
                            name,
                            a2.len(),
                            a1.len()
                        ),
                        span,
                    );
                }
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
            Ty::App(func, args) => {
                self.occurs_in(var, func) || args.iter().any(|a| self.occurs_in(var, a))
            }
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
    /// Returns (param_types, return_type, instantiated_constraints).
    fn instantiate(&mut self, scheme: &FnScheme) -> (Vec<Ty>, Ty, Vec<ClassConstraint>) {
        // Resolve scheme types through the current substitution first.
        // This is necessary for functions with unannotated params/returns, where
        // the scheme stores raw Var(N) that gets resolved during check_defn.
        // Without this, the fresh vars created below would lose the structural
        // relationships discovered during body type-checking.
        let resolved_params: Vec<Ty> = scheme.param_types.iter().map(|t| self.apply(t)).collect();
        let resolved_ret = self.apply(&scheme.return_type);

        let mapping: HashMap<TyVarId, Ty> = scheme
            .quantified
            .iter()
            .map(|&v| (v, self.fresh_var()))
            .collect();

        let params = resolved_params
            .iter()
            .map(|t| self.subst_vars(t, &mapping))
            .collect();
        let ret = self.subst_vars(&resolved_ret, &mapping);
        let constraints = scheme
            .constraints
            .iter()
            .map(|c| ClassConstraint {
                class_name: c.class_name.clone(),
                type_args: c
                    .type_args
                    .iter()
                    .map(|t| {
                        let resolved = self.apply(t);
                        self.subst_vars(&resolved, &mapping)
                    })
                    .collect(),
            })
            .collect();
        (params, ret, constraints)
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
            Ty::App(func, args) => Ty::App(
                Box::new(self.subst_vars(func, mapping)),
                args.iter().map(|a| self.subst_vars(a, mapping)).collect(),
            ),
            _ => ty.clone(),
        }
    }

    // ── Scope management ─────────────────────────────────────────

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.var_provenance.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.var_provenance.pop();
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

    fn lookup_provenance(&self, name: &str) -> u32 {
        for scope in self.var_provenance.iter().rev() {
            if let Some(&prov) = scope.get(name) {
                return prov;
            }
        }
        0
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
                match &con_expr.kind {
                    TypeExprKind::Named(name) => {
                        let name = name.clone();
                        let arg_tys: Vec<Ty> =
                            args.iter().map(|&id| self.resolve_type_expr(id)).collect();

                        // Special cases
                        if name == "Vector" && arg_tys.len() == 1 {
                            return Ty::Vector(Box::new(arg_tys[0].clone()));
                        }
                        if name == "Map" && arg_tys.len() == 2 {
                            return Ty::Map(
                                Box::new(arg_tys[0].clone()),
                                Box::new(arg_tys[1].clone()),
                            );
                        }

                        Ty::Con(name, arg_tys)
                    }
                    TypeExprKind::TypeVar(name) => {
                        // HKT application: ('f 'a) where 'f is a type variable
                        let func_ty = if let Some(ty) = self.type_param_scope.get(name.as_str()) {
                            ty.clone()
                        } else {
                            let ty = self.fresh_var();
                            self.type_param_scope.insert(name.clone(), ty.clone());
                            ty
                        };
                        let arg_tys: Vec<Ty> =
                            args.iter().map(|&id| self.resolve_type_expr(id)).collect();
                        Ty::App(Box::new(func_ty), arg_tys)
                    }
                    _ => {
                        self.error("expected type name in type application", span);
                        Ty::Error
                    }
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
                if self.type_defs.contains_key(name)
                    || self.struct_defs.contains_key(name)
                    || self.external_names.contains(name)
                {
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

        // Pass 1: type and struct definitions
        for item in &items {
            match item {
                Item::Deftype(d) => self.collect_deftype(d),
                Item::Defstruct(d) => self.collect_defstruct(d),
                _ => {}
            }
        }

        // Pass 2: typeclass definitions
        for item in &items {
            if let Item::Defclass(d) = item {
                self.collect_defclass(d);
            }
        }

        // Pass 3: declare forms (constrained function signatures)
        for item in &items {
            if let Item::Declare(d) = item {
                self.collect_declare(d);
            }
        }

        // Pass 4: function signatures (skips those already declared)
        for item in &items {
            if let Item::Defn(d) = item {
                if !self.fn_schemes.contains_key(&d.name) {
                    self.collect_defn_sig(d);
                }
            }
        }

        // Pass 5: instance definitions
        for item in &items {
            if let Item::Instance(d) = item {
                self.collect_instance(d);
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

        // Register kind: number of type params determines the kind
        self.type_kinds.insert(
            d.name.clone(),
            TyKind::from_param_count(d.type_params.len()),
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

        // Register kind
        self.type_kinds.insert(
            d.name.clone(),
            TyKind::from_param_count(d.type_params.len()),
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
                    let ty = self.fresh_var();
                    // Unannotated params are implicitly generic — quantify them
                    if let Ty::Var(id) = &ty {
                        quantified.push(*id);
                    }
                    ty
                }
            })
            .collect();

        let return_type = if let Some(ret_id) = d.return_type {
            self.resolve_type_expr(ret_id)
        } else {
            let ty = self.fresh_var();
            if let Ty::Var(id) = &ty {
                quantified.push(*id);
            }
            ty
        };

        // Any type variables created during resolution of explicit annotations are quantified
        for ty in self.type_param_scope.values() {
            if let Ty::Var(id) = ty {
                quantified.push(*id);
            }
        }

        self.fn_schemes.insert(
            d.name.clone(),
            FnScheme {
                quantified,
                constraints: Vec::new(),
                param_types,
                return_type,
            },
        );

        self.type_param_scope = saved;
    }

    fn collect_defclass(&mut self, d: &Defclass) {
        if self.class_defs.contains_key(&d.name) {
            self.error(
                format!("duplicate typeclass definition '{}'", d.name),
                d.span,
            );
            return;
        }

        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        // Create fresh type vars for class type params
        for param in &d.type_params {
            let v = self.fresh_var();
            self.type_param_scope.insert(param.clone(), v);
        }

        // Collect quantified vars from the class type params
        let quantified: Vec<TyVarId> = self
            .type_param_scope
            .values()
            .filter_map(|ty| {
                if let Ty::Var(id) = ty {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        // Build the class constraint for this class
        let class_constraint = ClassConstraint {
            class_name: d.name.clone(),
            type_args: d
                .type_params
                .iter()
                .map(|p| self.type_param_scope[p].clone())
                .collect(),
        };

        let mut methods = Vec::new();

        for m in &d.methods {
            // Resolve the method type in the scope of the class type params
            let method_ty = self.resolve_type_expr(m.type_ann);

            // Create a function scheme for the method with the class constraint
            if let Ty::Fn(params, ret) = method_ty {
                self.fn_schemes.insert(
                    m.name.clone(),
                    FnScheme {
                        quantified: quantified.clone(),
                        constraints: vec![class_constraint.clone()],
                        param_types: params,
                        return_type: *ret,
                    },
                );
            } else {
                self.error(
                    format!("method '{}' must have function type", m.name),
                    m.span,
                );
            }

            methods.push((m.name.clone(), m.type_ann));
        }

        // Infer kind for each class type param from usage in method types.
        // A type param used as `('f 'a)` (Ty::App) has kind `* -> *`, etc.
        let type_param_kinds: Vec<TyKind> = d
            .type_params
            .iter()
            .map(|param| {
                if let Some(Ty::Var(var_id)) = self.type_param_scope.get(param.as_str()) {
                    let var_id = *var_id;
                    // Scan all method types for App nodes using this var
                    let mut max_arity = 0usize;
                    for m in &d.methods {
                        let method_ty = self.resolve_type_expr(m.type_ann);
                        max_arity = max_arity.max(self.infer_var_arity(var_id, &method_ty));
                    }
                    TyKind::from_param_count(max_arity)
                } else {
                    TyKind::Star
                }
            })
            .collect();

        self.class_defs.insert(
            d.name.clone(),
            ClassDef {
                name: d.name.clone(),
                type_params: d.type_params.clone(),
                type_param_kinds,
                methods,
            },
        );

        self.type_param_scope = saved;
    }

    fn collect_declare(&mut self, d: &Declare) {
        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        // Resolve the type expression — this handles Constrained types
        let ty = self.resolve_type_expr_with_constraints(d.type_ann);

        match ty {
            (Ty::Fn(params, ret), constraints) => {
                let quantified: Vec<TyVarId> = self
                    .type_param_scope
                    .values()
                    .filter_map(|ty| {
                        if let Ty::Var(id) = ty {
                            Some(*id)
                        } else {
                            None
                        }
                    })
                    .collect();

                self.fn_schemes.insert(
                    d.name.clone(),
                    FnScheme {
                        quantified,
                        constraints,
                        param_types: params,
                        return_type: *ret,
                    },
                );
            }
            (ty, constraints) => {
                // If there are no params, treat as a zero-arg function
                let quantified: Vec<TyVarId> = self
                    .type_param_scope
                    .values()
                    .filter_map(|t| if let Ty::Var(id) = t { Some(*id) } else { None })
                    .collect();

                self.fn_schemes.insert(
                    d.name.clone(),
                    FnScheme {
                        quantified,
                        constraints,
                        param_types: vec![],
                        return_type: ty,
                    },
                );
            }
        }

        self.type_param_scope = saved;
    }

    /// Resolve a type expression, extracting any Constrained wrapper into separate constraints.
    fn resolve_type_expr_with_constraints(&mut self, id: TypeExprId) -> (Ty, Vec<ClassConstraint>) {
        let texpr = &self.module.type_exprs[id];
        if let TypeExprKind::Constrained { constraints, inner } = &texpr.kind {
            let constraints = constraints.clone();
            let inner = *inner;
            let class_constraints: Vec<ClassConstraint> = constraints
                .iter()
                .map(|c| ClassConstraint {
                    class_name: c.class_name.clone(),
                    type_args: c
                        .type_args
                        .iter()
                        .map(|&t| self.resolve_type_expr(t))
                        .collect(),
                })
                .collect();
            let ty = self.resolve_type_expr(inner);
            (ty, class_constraints)
        } else {
            let ty = self.resolve_type_expr(id);
            (ty, Vec::new())
        }
    }

    fn collect_instance(&mut self, d: &InstanceDef) {
        // Validate class exists
        let class_def = match self.class_defs.get(&d.class_name) {
            Some(cd) => cd.clone(),
            None => {
                self.error(format!("unknown typeclass '{}'", d.class_name), d.span);
                return;
            }
        };

        // Check type arg count
        if d.type_args.len() != class_def.type_params.len() {
            self.error(
                format!(
                    "typeclass '{}' expects {} type arguments, got {}",
                    d.class_name,
                    class_def.type_params.len(),
                    d.type_args.len()
                ),
                d.span,
            );
            return;
        }

        let saved = self.type_param_scope.clone();
        self.type_param_scope.clear();

        // Resolve instance type args (e.g., i32, or (Option 'a))
        let instance_type_args: Vec<Ty> = d
            .type_args
            .iter()
            .map(|&t| self.resolve_type_expr(t))
            .collect();

        // Kind-check each instance type arg against what the class expects
        for (i, inst_ty) in instance_type_args.iter().enumerate() {
            let expected_kind = &class_def.type_param_kinds[i];
            let actual_kind = self.kind_of(inst_ty);
            if *expected_kind != actual_kind {
                self.error(
                    format!(
                        "kind mismatch in instance of '{}': expected kind `{}`, got `{}`",
                        d.class_name, expected_kind, actual_kind
                    ),
                    d.span,
                );
                self.type_param_scope = saved;
                return;
            }
        }

        // Resolve instance constraints (e.g., (Eq 'a))
        let instance_constraints: Vec<ClassConstraint> = d
            .constraints
            .iter()
            .map(|c| ClassConstraint {
                class_name: c.class_name.clone(),
                type_args: c
                    .type_args
                    .iter()
                    .map(|&t| self.resolve_type_expr(t))
                    .collect(),
            })
            .collect();

        // Check for duplicate instance
        for existing in &self.instances {
            if existing.class_name == d.class_name {
                // Simple duplicate check: same concrete head type
                let existing_head = existing.type_args.first();
                let new_head = instance_type_args.first();
                if let (Some(eh), Some(nh)) = (existing_head, new_head) {
                    if self.types_structurally_equal(eh, nh) {
                        self.error(
                            format!("duplicate instance of {} for {}", d.class_name, nh),
                            d.span,
                        );
                        self.type_param_scope = saved;
                        return;
                    }
                }
            }
        }

        // Orphan check: in cross-file mode, either the class or at least one
        // type argument must be defined in this file.
        if !self.external_names.is_empty() && self.external_names.contains(&d.class_name) {
            let has_local_type = instance_type_args.iter().any(|ty| self.is_type_local(ty));
            if !has_local_type {
                self.error(
                    format!(
                        "orphan instance: neither class '{}' nor its type arguments are defined in this file",
                        d.class_name
                    ),
                    d.span,
                );
                self.type_param_scope = saved;
                return;
            }
        }

        // Build the type param mapping from class params to instance types
        let class_param_mapping: HashMap<SmolStr, Ty> = class_def
            .type_params
            .iter()
            .zip(instance_type_args.iter())
            .map(|(p, t)| (p.clone(), t.clone()))
            .collect();

        let mut method_impls = HashMap::new();

        // Type-check each method implementation
        for method_defn in &d.methods {
            let class_method = class_def
                .methods
                .iter()
                .find(|(n, _)| *n == method_defn.name);
            if class_method.is_none() {
                self.error(
                    format!(
                        "'{}' is not a method of typeclass '{}'",
                        method_defn.name, d.class_name
                    ),
                    method_defn.span,
                );
                continue;
            }

            // Generate mangled name for this instance method
            let mangled = format!(
                "{}#{}#{}",
                d.class_name,
                instance_type_args
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(","),
                method_defn.name
            );
            let mangled = SmolStr::new(&mangled);

            // Register the method defn as a regular function under the mangled name
            self.collect_instance_method_sig(method_defn, &mangled);

            method_impls.insert(method_defn.name.clone(), mangled);
        }

        self.instances.push(InstanceEntry {
            class_name: d.class_name.clone(),
            type_args: instance_type_args,
            constraints: instance_constraints,
            methods: method_impls,
        });

        let _ = class_param_mapping; // used for future type-checking of method bodies against class sig
        self.type_param_scope = saved;
    }

    /// Register an instance method under a mangled name.
    fn collect_instance_method_sig(&mut self, d: &Defn, mangled_name: &SmolStr) {
        let saved = self.type_param_scope.clone();
        // Don't clear — we need the instance type param scope

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

        let quantified: Vec<TyVarId> = self
            .type_param_scope
            .values()
            .filter_map(|ty| {
                if let Ty::Var(id) = ty {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        self.fn_schemes.insert(
            mangled_name.clone(),
            FnScheme {
                quantified,
                constraints: Vec::new(),
                param_types,
                return_type,
            },
        );

        self.type_param_scope = saved;
    }

    /// Determine the kind of a concrete type.
    /// Primitive types have kind `*`. Named type constructors look up `type_kinds`.
    /// A bare type constructor name (e.g., `Option` with no args applied) gets its
    /// registered kind. A fully-applied constructor (e.g., `(Option i32)`) has kind `*`.
    fn kind_of(&self, ty: &Ty) -> TyKind {
        match ty {
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
            | Ty::Bool
            | Ty::Str
            | Ty::Unit
            | Ty::Error => TyKind::Star,
            Ty::Con(name, args) => {
                if let Some(registered) = self.type_kinds.get(name) {
                    // Strip applied args from the kind
                    let mut kind = registered.clone();
                    for _ in 0..args.len() {
                        if let TyKind::Arrow(_, result) = kind {
                            kind = *result;
                        } else {
                            // Over-applied — kind is `*` (error elsewhere)
                            return TyKind::Star;
                        }
                    }
                    kind
                } else {
                    // Unknown — assume `*`
                    TyKind::Star
                }
            }
            Ty::Fn(_, _) => TyKind::Star,
            Ty::Vector(_) | Ty::Map(_, _) => TyKind::Star, // fully applied
            Ty::Var(_) => TyKind::Star,                    // type variables default to kind `*`
            Ty::App(_, _) => TyKind::Star,                 // applied HKT is kind `*`
        }
    }

    /// Infer the arity of a type variable from its usage as a type constructor.
    /// If `var_id` appears as the function in `App(Var(var_id), args)`, return `args.len()`.
    /// Otherwise return 0 (kind `*`).
    fn infer_var_arity(&self, var_id: TyVarId, ty: &Ty) -> usize {
        match ty {
            Ty::App(func, args) => {
                let from_func = if let Ty::Var(id) = func.as_ref() {
                    if *id == var_id {
                        args.len()
                    } else {
                        0
                    }
                } else {
                    self.infer_var_arity(var_id, func)
                };
                let from_args = args
                    .iter()
                    .map(|a| self.infer_var_arity(var_id, a))
                    .max()
                    .unwrap_or(0);
                from_func.max(from_args)
            }
            Ty::Con(_, args) => args
                .iter()
                .map(|a| self.infer_var_arity(var_id, a))
                .max()
                .unwrap_or(0),
            Ty::Fn(params, ret) => {
                let from_params = params
                    .iter()
                    .map(|p| self.infer_var_arity(var_id, p))
                    .max()
                    .unwrap_or(0);
                from_params.max(self.infer_var_arity(var_id, ret))
            }
            Ty::Vector(e) => self.infer_var_arity(var_id, e),
            Ty::Map(k, v) => self
                .infer_var_arity(var_id, k)
                .max(self.infer_var_arity(var_id, v)),
            _ => 0,
        }
    }

    /// Check if two types are structurally equal (ignoring unification variables).
    fn types_structurally_equal(&self, a: &Ty, b: &Ty) -> bool {
        match (a, b) {
            (Ty::I8, Ty::I8)
            | (Ty::I16, Ty::I16)
            | (Ty::I32, Ty::I32)
            | (Ty::I64, Ty::I64)
            | (Ty::U8, Ty::U8)
            | (Ty::U16, Ty::U16)
            | (Ty::U32, Ty::U32)
            | (Ty::U64, Ty::U64)
            | (Ty::F32, Ty::F32)
            | (Ty::F64, Ty::F64)
            | (Ty::Bool, Ty::Bool)
            | (Ty::Str, Ty::Str)
            | (Ty::Unit, Ty::Unit) => true,
            (Ty::Con(n1, a1), Ty::Con(n2, a2)) => {
                n1 == n2
                    && a1.len() == a2.len()
                    && a1
                        .iter()
                        .zip(a2.iter())
                        .all(|(x, y)| self.types_structurally_equal(x, y))
            }
            _ => false,
        }
    }

    /// Check whether a type is "local" to the current file for orphan-rule purposes.
    /// Primitives are always local, type constructors are local if defined in this file,
    /// and type variables are considered local (polymorphic instances aren't rejected).
    fn is_type_local(&self, ty: &Ty) -> bool {
        match ty {
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
            | Ty::Bool
            | Ty::Str
            | Ty::Unit => true,
            Ty::Con(name, _) => {
                self.type_defs.contains_key(name) || self.struct_defs.contains_key(name)
            }
            Ty::Var(_) => true,
            _ => false,
        }
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

        // Register kinds for built-in type constructors
        // Vector : * -> *, Map : * -> * -> *
        self.type_kinds
            .insert(SmolStr::new("Vector"), TyKind::from_param_count(1));
        self.type_kinds
            .insert(SmolStr::new("Map"), TyKind::from_param_count(2));
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

        // Check each top-level function
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

        // Check instance method bodies
        let instance_items: Vec<_> = self
            .module
            .items
            .iter()
            .filter_map(|(item, _)| {
                if let Item::Instance(d) = item {
                    Some(d.clone())
                } else {
                    None
                }
            })
            .collect();

        for inst in &instance_items {
            self.check_instance_methods(inst);
        }
    }

    fn check_defn(&mut self, d: &Defn) {
        self.check_defn_with_name(&d.name, d);
    }

    fn check_defn_with_name(&mut self, name: &SmolStr, d: &Defn) {
        let scheme = match self.fn_schemes.get(name) {
            Some(s) => s.clone(),
            None => return,
        };

        self.push_scope();

        // Set current function name for dependency tracking
        let prev_fn_name = self.current_fn_name.take();
        self.current_fn_name = Some(name.clone());

        // Set current function return type for ? operator checking
        let prev_return_type = self.current_fn_return_type.take();
        self.current_fn_return_type = Some(scheme.return_type.clone());

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

        // Restore previous return type and function name
        self.current_fn_return_type = prev_return_type;
        self.current_fn_name = prev_fn_name;

        self.pop_scope();

        // Default unresolved numeric literals first so constraints have concrete types
        self.default_numeric_vars();

        // Resolve deferred constraints
        self.resolve_constraints();
    }

    fn check_instance_methods(&mut self, inst: &InstanceDef) {
        let instance_type_args_str = inst
            .type_args
            .iter()
            .map(|&t| {
                let saved = self.type_param_scope.clone();
                self.type_param_scope.clear();
                let ty = self.resolve_type_expr(t);
                self.type_param_scope = saved;
                format!("{}", ty)
            })
            .collect::<Vec<_>>()
            .join(",");

        for method_defn in &inst.methods {
            let mangled = SmolStr::new(format!(
                "{}#{}#{}",
                inst.class_name, instance_type_args_str, method_defn.name
            ));
            self.check_defn_with_name(&mangled, method_defn);
        }
    }

    /// Resolve all deferred typeclass constraints.
    fn resolve_constraints(&mut self) {
        let constraints: Vec<_> = self.deferred_constraints.drain(..).collect();
        for (constraint, expr_id, span) in constraints {
            let resolved = ClassConstraint {
                class_name: constraint.class_name.clone(),
                type_args: constraint.type_args.iter().map(|t| self.apply(t)).collect(),
            };

            // Check if any type args are still unresolved variables
            let has_vars = resolved.type_args.iter().any(|t| matches!(t, Ty::Var(_)));

            if has_vars {
                // Can't resolve yet — might be inside a polymorphic function
                // If this function itself has the constraint in its scheme, that's fine
                continue;
            }

            match self.find_instance(&resolved) {
                Some(mangled_method) => {
                    self.method_resolutions.insert(expr_id, mangled_method);
                }
                None => {
                    // Check if there's an error type in the args — suppress error
                    let has_error = resolved.type_args.iter().any(|t| matches!(t, Ty::Error));
                    if !has_error {
                        self.error(
                            format!(
                                "no instance of {} for {}",
                                resolved.class_name,
                                resolved
                                    .type_args
                                    .iter()
                                    .map(|t| format!("{}", t))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            span,
                        );
                    }
                }
            }
        }
    }

    /// Find a matching instance for a concrete constraint.
    /// Returns the mangled method name if found.
    fn find_instance(&self, constraint: &ClassConstraint) -> Option<SmolStr> {
        for inst in &self.instances {
            if inst.class_name != constraint.class_name {
                continue;
            }
            if inst.type_args.len() != constraint.type_args.len() {
                continue;
            }

            // Check if the instance type args match the constraint type args
            let matches =
                inst.type_args.iter().zip(constraint.type_args.iter()).all(
                    |(inst_ty, constraint_ty)| self.instance_type_matches(inst_ty, constraint_ty),
                );

            if matches {
                // For now, return the first method name found — caller will need
                // to resolve specific method. We just confirm the instance exists.
                // The actual method resolution happens during call checking.
                return Some(SmolStr::new(format!(
                    "{}#{}",
                    inst.class_name,
                    inst.type_args
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(",")
                )));
            }
        }
        None
    }

    /// Check if an instance type matches a concrete constraint type.
    fn instance_type_matches(&self, inst_ty: &Ty, constraint_ty: &Ty) -> bool {
        match (inst_ty, constraint_ty) {
            // Instance has a type variable — matches anything (conditional instance)
            (Ty::Var(_), _) => true,
            // Both concrete — must be equal
            (a, b) => self.types_structurally_equal(a, b),
        }
    }

    fn check_body(&mut self, body: &[ExprId]) -> Ty {
        let mut ty = Ty::Unit;
        for &expr_id in body {
            ty = self.check_expr(expr_id);
        }
        ty
    }

    // ── Arena provenance ───────────────────────────────────────

    fn is_heap_type(&self, ty: &Ty) -> bool {
        matches!(ty, Ty::Str | Ty::Vector(_) | Ty::Fn(_, _) | Ty::Map(_, _) | Ty::Con(_, _))
    }

    fn compute_provenance(&self, expr_id: ExprId, ty: &Ty) -> u32 {
        let expr = &self.module.exprs[expr_id];
        match &expr.kind {
            // Heap allocations inside arena get current arena depth
            ExprKind::VectorLit(_) | ExprKind::Lambda { .. } | ExprKind::MapLit(_) => {
                self.arena_depth
            }
            // Variable reference inherits provenance from the variable
            ExprKind::Var(name) => self.lookup_provenance(name),
            // Call returning a heap type inside arena gets arena provenance (conservative)
            ExprKind::Call { .. } => {
                if self.is_heap_type(ty) {
                    self.arena_depth
                } else {
                    0
                }
            }
            // Let inherits provenance from body's last expression
            ExprKind::Let { body, .. } => {
                if let Some(&last) = body.last() {
                    self.expr_provenance.get(last).copied().unwrap_or(0)
                } else {
                    0
                }
            }
            // Do inherits provenance from body's last expression
            ExprKind::Do { body } | ExprKind::Unsafe { body } => {
                if let Some(&last) = body.last() {
                    self.expr_provenance.get(last).copied().unwrap_or(0)
                } else {
                    0
                }
            }
            // If/Cond — take the max provenance from branches
            ExprKind::If { then_branch, else_branch, .. } => {
                let then_prov = self.expr_provenance.get(*then_branch).copied().unwrap_or(0);
                let else_prov = else_branch
                    .and_then(|e| self.expr_provenance.get(e).copied())
                    .unwrap_or(0);
                then_prov.max(else_prov)
            }
            // with-arena block itself returns provenance 0 (verified by escape check)
            ExprKind::WithArena { .. } => 0,
            _ => 0,
        }
    }

    // ── Expression checking ──────────────────────────────────────

    fn check_expr(&mut self, expr_id: ExprId) -> Ty {
        let ty = self.check_expr_inner(expr_id);
        self.expr_types.insert(expr_id, ty.clone());

        // Compute arena provenance for this expression
        if self.arena_depth > 0 {
            let prov = self.compute_provenance(expr_id, &ty);
            if prov > 0 {
                self.expr_provenance.insert(expr_id, prov);
            }
        }

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
                    self.record_type_dep(&info.0);
                    return self.instantiate_constructor(&info.0, &info.1, &info.2);
                }
                // Check if it's a known function (instantiate its scheme)
                if let Some(scheme) = self.fn_schemes.get(name).cloned() {
                    let (params, ret, _constraints) = self.instantiate(&scheme);
                    return Ty::Fn(params, Box::new(ret));
                }
                if let Some(ty) = self.lookup_var(name) {
                    ty
                } else if self.external_names.contains(name.as_str()) {
                    // Known external symbol — return a fresh type variable
                    self.fresh_var()
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
                    // Track arena provenance for the binding
                    if self.arena_depth > 0 {
                        let prov = self.expr_provenance.get(b.value).copied().unwrap_or(0);
                        if prov > 0 {
                            self.var_provenance.last_mut().unwrap().insert(b.name.clone(), prov);
                        }
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

                // Arena escape check: can't assign arena value to outer variable
                if self.arena_depth > 0 {
                    let val_prov = self.expr_provenance.get(*value).copied().unwrap_or(0);
                    if val_prov > 0 {
                        let place_expr = &self.module.exprs[*place];
                        if let ExprKind::Var(name) = &place_expr.kind {
                            let target_prov = self.lookup_provenance(name);
                            if target_prov < val_prov {
                                self.error(
                                    "cannot assign arena-allocated value to outer variable".to_string(),
                                    span,
                                );
                            }
                        }
                    }
                }

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

            ExprKind::WithArena { name: _, body } => {
                self.arena_depth += 1;
                let body_ty = self.check_body(body);

                // Check that the result type doesn't have arena provenance
                if let Some(&last_id) = body.last() {
                    let prov = self.expr_provenance.get(last_id).copied().unwrap_or(0);
                    if prov >= self.arena_depth {
                        self.error(
                            "arena-allocated value cannot escape with-arena block".to_string(),
                            span,
                        );
                    }
                }

                self.arena_depth -= 1;
                body_ty
            }

            ExprKind::Try(inner) => {
                let inner_ty = self.check_expr(*inner);
                let applied = self.apply(&inner_ty);

                // Verify the inner expression is a Result type
                match &applied {
                    Ty::Con(name, args) if name == "Result" && args.len() == 2 => {
                        let ok_ty = args[0].clone();
                        let err_ty = args[1].clone();

                        // Verify enclosing function returns a Result with matching error type
                        if let Some(ret_ty) = &self.current_fn_return_type {
                            let ret_applied = self.apply(ret_ty);
                            match &ret_applied {
                                Ty::Con(rname, rargs) if rname == "Result" && rargs.len() == 2 => {
                                    self.unify(&err_ty, &rargs[1], span);
                                }
                                Ty::Var(_) => {
                                    // Return type not yet resolved — unify with Result
                                    let fresh_ok = self.fresh_var();
                                    let result_ty = Ty::Con(
                                        SmolStr::new("Result"),
                                        vec![fresh_ok, err_ty.clone()],
                                    );
                                    self.unify(&ret_applied, &result_ty, span);
                                }
                                _ => {
                                    self.error(
                                        format!(
                                            "? operator requires enclosing function to return Result, got {}",
                                            ret_applied
                                        ),
                                        span,
                                    );
                                }
                            }
                        } else {
                            self.error("? operator used outside of a function", span);
                        }

                        ok_ty
                    }
                    Ty::Var(_) => {
                        // Type not yet resolved — constrain it to be Result
                        let ok_ty = self.fresh_var();
                        let err_ty = self.fresh_var();
                        let result_ty =
                            Ty::Con(SmolStr::new("Result"), vec![ok_ty.clone(), err_ty.clone()]);
                        self.unify(&applied, &result_ty, span);

                        // Also constrain enclosing function return type
                        if let Some(ret_ty) = &self.current_fn_return_type {
                            let ret_applied = self.apply(ret_ty);
                            let fresh_ok2 = self.fresh_var();
                            let ret_result =
                                Ty::Con(SmolStr::new("Result"), vec![fresh_ok2, err_ty]);
                            self.unify(&ret_applied, &ret_result, span);
                        }

                        ok_ty
                    }
                    Ty::Error => Ty::Error,
                    _ => {
                        self.error(
                            format!("? operator requires a Result type, got {}", applied),
                            span,
                        );
                        Ty::Error
                    }
                }
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

        // Check for comparison operators (<, >, <=, >=) — these defer an Ord constraint
        if let ExprKind::Var(name) = &func_expr.kind {
            if self.is_comparison_op(name) {
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();
                if arg_tys.len() != 2 {
                    self.error("comparison requires exactly 2 arguments", span);
                    return Ty::Error;
                }
                self.unify(&arg_tys[0], &arg_tys[1], span);
                let func_ty = Ty::Fn(arg_tys.clone(), Box::new(Ty::Bool));
                self.expr_types.insert(func_id, func_ty);

                // If Ord class is in scope, defer an Ord constraint on the arg type
                if self.class_defs.contains_key("Ord") {
                    let constraint = ClassConstraint {
                        class_name: SmolStr::new("Ord"),
                        type_args: vec![arg_tys[0].clone()],
                    };
                    self.deferred_constraints.push((constraint, func_id, span));
                }
                return Ty::Bool;
            }
        }

        // Check for builtin call
        if let ExprKind::Var(name) = &func_expr.kind {
            if self.is_builtin(name) {
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();
                let ret_ty = self.check_builtin_call(name, &arg_tys, span);
                // Record the builtin's function type so hover works on the Var expression
                let func_ty = Ty::Fn(arg_tys, Box::new(ret_ty.clone()));
                self.expr_types.insert(func_id, func_ty);
                return ret_ty;
            }

            // Check for ADT constructor call
            if let Some(info) = self.constructors.get(name).cloned() {
                // Record type dependency: this function uses this type
                self.record_type_dep(&info.0);
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
                self.record_type_dep(&sdef.name);
                return self.check_struct_construction(&sdef, args, span);
            }

            // Regular function call — instantiate the scheme
            if let Some(scheme) = self.fn_schemes.get(name).cloned() {
                self.record_call_dep(name);
                let (param_tys, ret_ty, constraints) = self.instantiate(&scheme);
                let arg_tys: Vec<Ty> = args.iter().map(|a| self.check_expr(a.value)).collect();

                // Record the function's type so hover works on the Var expression
                let func_ty = Ty::Fn(param_tys.clone(), Box::new(ret_ty.clone()));
                self.expr_types.insert(func_id, func_ty);

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

                // Defer any constraints for resolution after body check
                for c in constraints {
                    self.deferred_constraints.push((c, func_id, span));
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
                // Record type dependency for field access
                self.record_type_dep(name);
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
                    // Record type dependency for pattern matching
                    self.record_type_dep(&info.0);
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
                } else if self.external_names.contains(name.as_str()) {
                    // Known external constructor — unify expected with a fresh var
                    let ext_ty = self.fresh_var();
                    self.unify(expected, &ext_ty, span);
                    // Bind sub-pattern variables to fresh vars
                    let mut bindings = Vec::new();
                    for sub_pat in &args {
                        let arg_ty = self.fresh_var();
                        bindings.extend(self.check_pattern(*sub_pat, &arg_ty));
                    }
                    bindings
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
                        // Record type dependency for struct destructuring
                        self.record_type_dep(name);
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

    fn is_comparison_op(&self, name: &str) -> bool {
        matches!(name, "<" | ">" | "<=" | ">=")
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

    // ── Generic functions (Phase 8a) ────────────────────────────

    #[test]
    fn generic_identity() {
        check_ok(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () (let ((a (id 42)) (b (id \"hello\"))) a))",
        );
    }

    #[test]
    fn generic_function_calling_generic_constructor() {
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defn wrap ((x : 'a)) : (Option 'a) (Some x))
             (defn main () (wrap 42))",
        );
    }

    #[test]
    fn generic_multiple_type_params() {
        check_ok(
            "(defn const ((x : 'a) (y : 'b)) : 'a x)
             (defn main () (const 1 \"hello\"))",
        );
    }

    #[test]
    fn error_type_mismatch_through_generic() {
        let msg = check_err(
            "(defn id ((x : 'a)) : 'a x)
             (defn main () : i32 (id \"hello\"))",
        );
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch, got: {}",
            msg
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

    // ── Typeclasses (Phase 8b) ──────────────────────────────────

    #[test]
    fn typeclass_basic_instance() {
        check_ok(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () (== 1 2))",
        );
    }

    #[test]
    fn typeclass_missing_instance() {
        let msg = check_err(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (defn main () (== 1 2))",
        );
        assert!(
            msg.contains("no instance of Eq"),
            "expected 'no instance', got: {}",
            msg
        );
    }

    #[test]
    fn typeclass_constrained_function() {
        check_ok(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (declare equal? (=> (Eq 'a) (Fn ['a 'a] Bool)))
             (defn equal? ((x : 'a) (y : 'a)) : Bool (== x y))
             (defn main () (equal? 1 2))",
        );
    }

    #[test]
    fn typeclass_multiple_methods() {
        check_ok(
            "(defclass (Show 'a)
               (show : (Fn ['a] String)))
             (instance (Show i32)
               (defn show ((x : i32)) : String (str x)))
             (defn main () (show 42))",
        );
    }

    #[test]
    fn error_duplicate_instance() {
        let msg = check_err(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (instance (Eq i32)
               (defn == ((x : i32) (y : i32)) : Bool (= x y)))
             (defn main () (== 1 2))",
        );
        assert!(
            msg.contains("duplicate instance"),
            "expected 'duplicate instance', got: {}",
            msg
        );
    }

    #[test]
    fn error_unknown_typeclass() {
        let msg = check_err(
            "(instance (Foo i32)
               (defn bar ((x : i32)) : i32 x))
             (defn main () 42)",
        );
        assert!(
            msg.contains("unknown typeclass"),
            "expected 'unknown typeclass', got: {}",
            msg
        );
    }

    // ── Ord typeclass ─────────────────────────────────────────────

    #[test]
    fn test_ord_typeclass_resolves() {
        check_ok(
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
             (defn main ()
               (println (min-of 10 3))
               (println (max-of 10 3)))",
        );
    }

    // ── Kind system + HKTs (Phase 8d) ────────────────────────────

    #[test]
    fn kind_functor_class() {
        // Functor uses 'f as a type constructor (* -> *), applied as ('f 'a)
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor Option)
               (defn fmap ((f : (Fn ['a] 'b)) (opt : (Option 'a))) : (Option 'b)
                 (match opt
                   ((Some x) (Some (f x)))
                   (None None))))
             (defn main () (fmap (fn (x) (+ x 1)) (Some 5)))",
        );
    }

    #[test]
    fn error_kind_mismatch_star_for_hkt() {
        // i32 has kind *, but Functor expects * -> *
        let msg = check_err(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor i32)
               (defn fmap ((f : (Fn ['a] 'b)) (x : i32)) : i32 x))
             (defn main () 42)",
        );
        assert!(
            msg.contains("kind mismatch"),
            "expected 'kind mismatch', got: {}",
            msg
        );
    }

    #[test]
    fn kind_type_constructor_registration() {
        // Option has kind * -> * (1 type param)
        // Using it bare in an instance that expects * -> * should work
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Wrapper 'f)
               (wrap : (Fn ['a] ('f 'a))))
             (instance (Wrapper Option)
               (defn wrap ((x : 'a)) : (Option 'a) (Some x)))
             (defn main () (wrap 42))",
        );
    }

    #[test]
    fn hkt_type_application_unification() {
        // HKT type application should unify correctly:
        // ('f 'a) with (Option i32) should bind 'f=Option, 'a=i32
        check_ok(
            "(deftype (Option 'a) (Some 'a) None)
             (defclass (Functor 'f)
               (fmap : (Fn [(Fn ['a] 'b) ('f 'a)] ('f 'b))))
             (instance (Functor Option)
               (defn fmap ((f : (Fn ['a] 'b)) (opt : (Option 'a))) : (Option 'b)
                 (match opt
                   ((Some x) (Some (f x)))
                   (None None))))
             (defn main () : (Option i32)
               (fmap (fn (x) (+ x 1)) (Some 5)))",
        );
    }

    // ── External symbol awareness ────────────────────────────────

    fn check_with_ext(source: &str, externals: &[&str]) -> Vec<TypeError> {
        let (module, parse_errors) = weir_parser::parse(source);
        assert!(parse_errors.is_empty(), "parse errors: {:?}", parse_errors);
        let ext: HashSet<SmolStr> = externals.iter().map(|s| SmolStr::from(*s)).collect();
        let result = check_with_externals(&module, &ext);
        result.errors
    }

    #[test]
    fn external_variable_no_error() {
        // Calling an external function should not produce "undefined variable"
        let errors = check_with_ext("(defn main () (square 5))", &["square"]);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn unknown_variable_still_errors_without_external() {
        // Without declaring it external, the error should remain
        let errors = check_with_ext("(defn main () (square 5))", &[]);
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("undefined variable 'square'"));
    }

    #[test]
    fn external_constructor_in_pattern_no_error() {
        // Matching on an external constructor should not error
        let errors = check_with_ext(
            "(defn main ((x : i32))
               (match x
                 ((ExternalCon y) y)
                 (_ 0)))",
            &["ExternalCon"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn external_constructor_in_expr_no_error() {
        // Using an external constructor as a value should not error
        let errors = check_with_ext("(defn main () (Some 42))", &["Some"]);
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn mixed_local_and_external() {
        // Local definitions coexist with external names
        let errors = check_with_ext(
            "(defn helper () 42)
             (defn main () (+ (helper) (remote-fn 10)))",
            &["remote-fn"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    // ── Orphan instance coherence ─────────────────────────────────

    #[test]
    fn orphan_instance_class_local_ok() {
        // Class defined locally, type is external → allowed (class owner)
        let errors = check_with_ext(
            "(defclass (MyClass 'a)
               (my-method : (Fn ['a] i32)))
             (instance (MyClass ExternalType)
               (defn my-method ((x : ExternalType)) : i32 42))
             (defn main () 0)",
            &["ExternalType"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_type_local_ok() {
        // Class is external, type defined locally → allowed (type owner)
        let errors = check_with_ext(
            "(deftype Color Red Green Blue)
             (defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show Color)
               (defn show ((c : Color)) : i32 0))
             (defn main () 0)",
            &["Show"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_primitive_ok() {
        // Class is external, type is i32 (primitive) → allowed
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show i32)
               (defn show ((x : i32)) : i32 x))
             (defn main () 0)",
            &["Show"],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    #[test]
    fn orphan_instance_rejected() {
        // Class is external, type is external → orphan
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show ExternalType)
               (defn show ((x : ExternalType)) : i32 42))
             (defn main () 0)",
            &["Show", "ExternalType"],
        );
        assert!(!errors.is_empty());
        assert!(
            errors[0].message.contains("orphan instance"),
            "expected 'orphan instance', got: {}",
            errors[0].message
        );
    }

    #[test]
    fn orphan_rule_single_file_no_check() {
        // No externals → single-file mode → always passes
        let errors = check_with_ext(
            "(defclass (Show 'a)
               (show : (Fn ['a] i32)))
             (instance (Show i32)
               (defn show ((x : i32)) : i32 x))
             (defn main () 0)",
            &[],
        );
        assert!(errors.is_empty(), "unexpected errors: {:?}", errors);
    }

    // ── From typeclass ────────────────────────────────────────────

    #[test]
    fn from_typeclass_resolves() {
        check_ok(
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
             (defn main ()
               (let (((r : Primary) (from Red)))
                 (println r)))",
        );
    }

    #[test]
    fn from_typeclass_missing_instance_error() {
        let msg = check_err(
            "(deftype Color Red Green Blue)
             (deftype Primary Yes No)
             (defclass (From 'a 'b)
               (from : (Fn ['a] 'b)))
             (defn main ()
               (let (((r : Primary) (from Red)))
                 (println r)))",
        );
        assert!(
            msg.contains("no instance"),
            "expected 'no instance' error, got: {}",
            msg
        );
    }

    // ── Result type + ? operator tests ──────────────────────────

    #[test]
    fn result_type_basic() {
        check_ok(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn wrap ((x : i32)) : (Result i32 i32) (Ok x))
             (defn unwrap ((r : (Result i32 i32))) : i32
               (match r
                 ((Ok val) val)
                 ((Err _) 0)))
             (defn main () (println (unwrap (wrap 42))))",
        );
    }

    #[test]
    fn try_operator_unwraps_ok() {
        check_ok(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn safe-div ((x : i32) (y : i32)) : (Result i32 i32)
               (if (= y 0) (Err y) (Ok (/ x y))))
             (defn use-try ((x : i32) (y : i32)) : (Result i32 i32)
               (let ((result (safe-div x y)?))
                 (Ok (+ result 1))))
             (defn main () (println 0))",
        );
    }

    #[test]
    fn try_operator_error_on_non_result() {
        let msg = check_err(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn id ((x : i32)) : i32 x)
             (defn bad ((x : i32)) : (Result i32 i32) (let ((y (id x)?)) (Ok y)))
             (defn main () (println 0))",
        );
        assert!(
            msg.contains("? operator requires a Result type"),
            "expected '? operator requires a Result type' error, got: {}",
            msg
        );
    }

    #[test]
    fn try_operator_error_type_must_match() {
        let msg = check_err(
            "(deftype (Result 'ok 'err) (Ok 'ok) (Err 'err))
             (defn f () : (Result i32 Bool) (Ok 42))
             (defn g () : (Result i32 i32) (let ((x (f)?)) (Ok x)))
             (defn main () (println 0))",
        );
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch error, got: {}",
            msg
        );
    }

    // ── Type-error fixture tests ────────────────────────────────

    fn fixture_check_err(name: &str) -> String {
        let path = format!(
            "{}/tests/fixtures/type-errors/{}.weir",
            env!("CARGO_MANIFEST_DIR").replace("/crates/weir-typeck", ""),
            name
        );
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("could not read fixture {}: {}", path, e));
        check_err(&source)
    }

    #[test]
    fn fixture_type_error_mismatched_return() {
        let msg = fixture_check_err("mismatched-return");
        assert!(
            msg.contains("type mismatch"),
            "expected type mismatch error, got: {}",
            msg
        );
    }

    #[test]
    fn fixture_type_error_non_exhaustive_match() {
        let msg = fixture_check_err("non-exhaustive-match");
        assert!(
            msg.contains("non-exhaustive"),
            "expected non-exhaustive match error, got: {}",
            msg
        );
    }

    #[test]
    fn fixture_type_error_unknown_variable() {
        let msg = fixture_check_err("unknown-variable");
        assert!(
            msg.contains("undefined variable"),
            "expected undefined variable error, got: {}",
            msg
        );
    }

    #[test]
    fn test_comparison_without_ord_still_works() {
        // Without an Ord class in scope, < still works on primitives (no constraint deferred)
        check_ok(
            "(defn main () : Bool (< 1 2))",
        );
    }

    #[test]
    fn test_comparison_with_ord_defers_constraint() {
        // With Ord in scope, < on i32 defers and resolves the constraint
        check_ok(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (instance (Ord i32)
               (defn compare ((x : i32) (y : i32)) : Ordering
                 (if (< x y) LT (if (> x y) GT EQ))))
             (defn main () : Bool (< 1 2))",
        );
    }

    #[test]
    fn test_comparison_custom_type_without_ord_instance_errors() {
        let msg = check_err(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (deftype Color Red Green Blue)
             (defn main () : Bool (< Red Green))",
        );
        assert!(
            msg.contains("no instance of Ord for Color"),
            "expected 'no instance of Ord' error, got: {}",
            msg
        );
    }

    #[test]
    fn test_comparison_custom_type_with_ord_instance_passes() {
        check_ok(
            "(deftype Ordering LT EQ GT)
             (defclass (Ord 'a)
               (compare : (Fn ['a 'a] Ordering)))
             (deftype Priority Low High)
             (instance (Ord Priority)
               (defn compare ((a : Priority) (b : Priority)) : Ordering
                 LT))
             (defn main () : Bool (< Low High))",
        );
    }

    // ── Arena escape analysis tests ─────────────────────────────

    #[test]
    fn arena_escape_vector() {
        let msg = check_err(
            "(defn main () (with-arena a [1 2 3]))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_escape_lambda() {
        let msg = check_err(
            "(defn main () (with-arena a (fn (x) x)))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_ok_returns_non_heap() {
        check_ok(
            "(defn main ()
               (with-arena a
                 (let ((v [1 2 3]))
                   (len v))))",
        );
    }

    #[test]
    fn arena_ok_returns_pre_existing_value() {
        check_ok(
            "(defn main ()
               (let ((x 42))
                 (with-arena a x)))",
        );
    }

    #[test]
    fn arena_escape_via_set() {
        let msg = check_err(
            "(defn main ()
               (let ((mut x [1]))
                 (with-arena a
                   (set! x [2 3]))))",
        );
        assert!(msg.contains("cannot assign arena-allocated value to outer variable"), "got: {}", msg);
    }

    #[test]
    fn arena_nested_inner_escape() {
        let msg = check_err(
            "(defn main ()
               (with-arena a
                 (let ((v [1 2 3]))
                   (with-arena b [4 5]))))",
        );
        assert!(msg.contains("arena-allocated value cannot escape"), "got: {}", msg);
    }

    #[test]
    fn arena_ok_returns_pre_existing_gc_vector() {
        check_ok(
            "(defn main ()
               (let ((v [1 2]))
                 (with-arena a v)))",
        );
    }

    #[test]
    fn arena_provenance_does_not_leak_across_scopes() {
        // After a let scope containing arena-provenance `x` pops,
        // a new `x` in the outer scope should not inherit stale provenance.
        check_ok(
            "(defn main () : i64
               (with-arena a
                 (let ((x [1 2 3]))
                   (len x))
                 (let ((x 42))
                   x)))",
        );
    }

    #[test]
    fn arena_shadowed_variable_independent_provenance() {
        // Inner `v` has arena provenance, but outer `v` (GC) should be
        // independently tracked and safe to return from the arena block.
        check_ok(
            "(defn main ()
               (let ((v [1 2]))
                 (with-arena a
                   (let ((v [3 4]))
                     (len v))
                   v)))",
        );
    }

    // ── Property-based tests ────────────────────────────────────

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn typeck_never_panics_on_parsed_input(s in "\\PC{0,200}") {
                let (module, _errors) = weir_parser::parse(&s);
                let _ = check(&module);
            }

            #[test]
            fn typeck_never_panics_on_lispy_input(
                s in proptest::string::string_regex(
                    r"[\(\)\[\] a-z0-9\+\-\*/:;'\n ]{0,150}"
                ).unwrap()
            ) {
                let (module, _errors) = weir_parser::parse(&s);
                let _ = check(&module);
            }

            #[test]
            fn typeck_is_deterministic(
                s in proptest::string::string_regex(
                    r"\(defn main \(\) \([\+\-\*] [0-9]{1,3} [0-9]{1,3}\)\)"
                ).unwrap()
            ) {
                let (module, errors) = weir_parser::parse(&s);
                if errors.is_empty() {
                    let r1 = check(&module);
                    let r2 = check(&module);
                    prop_assert_eq!(r1.errors.len(), r2.errors.len());
                }
            }
        }
    }

    // ── Dependency graph tests ────────────────────────────────────

    #[test]
    fn dep_graph_call_chain() {
        // A calls B calls C → forward and reverse deps
        let source = "(defn c () : i32 42)
                      (defn b () : i32 (c))
                      (defn a () : i32 (b))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        // Forward: a→{b}, b→{c}
        assert!(deps.call_deps.get("a").unwrap().contains("b"));
        assert!(deps.call_deps.get("b").unwrap().contains("c"));
        // Reverse: c is called by {b}, b is called by {a}
        assert!(deps.callers.get("c").unwrap().contains("b"));
        assert!(deps.callers.get("b").unwrap().contains("a"));
    }

    #[test]
    fn dep_graph_type_usage() {
        // Function uses a deftype constructor
        let source = "(deftype Color (Red) (Green) (Blue))
                      (defn pick () : Color Red)";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        // pick uses type Color
        assert!(deps.type_deps.get("pick").unwrap().contains("Color"));
        // Reverse: Color is used by pick
        assert!(deps.type_users.get("Color").unwrap().contains("pick"));
    }

    #[test]
    fn dep_graph_diamond() {
        // A→B, A→C, B→D, C→D (diamond)
        let source = "(defn d () : i32 1)
                      (defn b () : i32 (d))
                      (defn c () : i32 (d))
                      (defn a () : i32 (+ (b) (c)))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        let a_deps = deps.call_deps.get("a").unwrap();
        assert!(a_deps.contains("b"));
        assert!(a_deps.contains("c"));
        assert!(deps.call_deps.get("b").unwrap().contains("d"));
        assert!(deps.call_deps.get("c").unwrap().contains("d"));
        // Reverse: d is called by {b, c}
        let d_callers = deps.callers.get("d").unwrap();
        assert!(d_callers.contains("b"));
        assert!(d_callers.contains("c"));
    }

    #[test]
    fn dep_graph_pattern_match_type() {
        let source = "(deftype Option (Some i32) (None))
                      (defn unwrap ((x : Option)) : i32
                        (match x
                          ((Some v) v)
                          (None 0)))";
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty());
        let result = check(&module);
        assert!(result.errors.is_empty());

        let deps = &result.deps;
        assert!(deps.type_deps.get("unwrap").unwrap().contains("Option"));
        assert!(deps.type_users.get("Option").unwrap().contains("unwrap"));
    }
}
