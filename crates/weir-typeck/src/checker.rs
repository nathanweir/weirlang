use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use weir_ast::*;

use crate::defs::*;
use crate::error::TypeError;
use crate::result::DependencyGraph;
use crate::types::{Ty, TyVarId};

// ── Type checker ─────────────────────────────────────────────────

pub(crate) struct TypeChecker<'a> {
    pub(crate) module: &'a Module,

    // Substitution: index = TyVarId, value = resolved type (or None)
    subst: Vec<Option<Ty>>,
    var_kinds: Vec<VarKind>,

    // Definitions
    type_defs: HashMap<SmolStr, TypeDef>,
    struct_defs: HashMap<SmolStr, StructDef>,
    pub(crate) fn_schemes: HashMap<SmolStr, FnScheme>,
    /// Constructor name → (type_name, type_params, field_type_exprs)
    constructors: HashMap<SmolStr, (SmolStr, Vec<SmolStr>, Vec<TypeExprId>)>,

    // Typeclass definitions and instances
    class_defs: HashMap<SmolStr, ClassDef>,
    pub(crate) instances: Vec<InstanceEntry>,
    /// Deferred constraints to check after function body is fully typed
    deferred_constraints: Vec<(ClassConstraint, ExprId, Span)>,

    // Kind system
    /// Maps type constructor names to their kinds (e.g., "Option" → `* -> *`)
    type_kinds: HashMap<SmolStr, TyKind>,

    // Scope stack for variable types
    scopes: Vec<HashMap<SmolStr, Ty>>,

    // Type parameter scope: maps 'a → Ty::Var(id) during resolution
    type_param_scope: HashMap<SmolStr, Ty>,

    pub(crate) errors: Vec<TypeError>,

    /// Records the type of every expression (pre-substitution).
    pub(crate) expr_types: ArenaMap<ExprId, Ty>,

    /// For each call site that invokes a class method, the resolved concrete function name.
    pub(crate) method_resolutions: HashMap<ExprId, SmolStr>,

    /// Known symbol names from other workspace files.
    /// Variables/constructors in this set produce fresh type variables instead of errors.
    external_names: HashSet<SmolStr>,

    /// Return type of the function currently being checked (for `?` operator).
    current_fn_return_type: Option<Ty>,

    /// Name of the function currently being checked (for dependency tracking).
    current_fn_name: Option<SmolStr>,
    /// Dependency graph built during typechecking.
    pub(crate) dep_graph: DependencyGraph,

    /// Current arena nesting depth (0 = not inside with-arena)
    arena_depth: u32,
    task_depth: u32,
    /// Per-variable arena provenance depth (0 = GC/stack, >0 = arena), scope-aware
    var_provenance: Vec<HashMap<SmolStr, u32>>,
    /// Per-expression arena provenance depth
    expr_provenance: ArenaMap<ExprId, u32>,
}

impl<'a> TypeChecker<'a> {
    pub(crate) fn new(module: &'a Module, externals: &HashSet<SmolStr>) -> Self {
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
            task_depth: 0,
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

    pub(crate) fn compute_reverse_deps(&mut self) {
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

    pub(crate) fn apply(&self, ty: &Ty) -> Ty {
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
            "Bool" => Ty::Bool,
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

    pub(crate) fn collect_definitions(&mut self) {
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

        // Pass 6: register built-in Shareable marker typeclass + instances
        self.register_shareable_typeclass();
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
            Ty::Vector(_) | Ty::Map(_, _) | Ty::Atom(_) | Ty::Channel(_) => TyKind::Star,
            Ty::Var(_) => TyKind::Star,   // type variables default to kind `*`
            Ty::App(_, _) => TyKind::Star, // applied HKT is kind `*`
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
        self.type_kinds
            .insert(SmolStr::new("Atom"), TyKind::from_param_count(1));
        self.type_kinds
            .insert(SmolStr::new("Channel"), TyKind::from_param_count(1));
    }

    fn register_shareable_typeclass(&mut self) {
        // Shareable is a built-in marker typeclass (no methods).
        // Types that are Shareable can be shared across threads.
        self.class_defs.insert(
            SmolStr::new("Shareable"),
            ClassDef {
                name: SmolStr::new("Shareable"),
                type_params: vec![SmolStr::new("a")],
                type_param_kinds: vec![TyKind::from_param_count(0)],
                methods: vec![],
            },
        );

        // Auto-register instances for primitive types
        let shareable_primitives: Vec<Ty> = vec![
            Ty::I8, Ty::I16, Ty::I32, Ty::I64,
            Ty::U8, Ty::U16, Ty::U32, Ty::U64,
            Ty::F32, Ty::F64,
            Ty::Bool, Ty::Unit, Ty::Str,
        ];
        for ty in shareable_primitives {
            self.instances.push(InstanceEntry {
                class_name: SmolStr::new("Shareable"),
                type_args: vec![ty],
                constraints: vec![],
                methods: HashMap::new(),
            });
        }

        // (Atom T) is Shareable if T is Shareable
        let atom_var = self.fresh_var();
        self.instances.push(InstanceEntry {
            class_name: SmolStr::new("Shareable"),
            type_args: vec![Ty::Con(SmolStr::new("Atom"), vec![atom_var.clone()])],
            constraints: vec![ClassConstraint {
                class_name: SmolStr::new("Shareable"),
                type_args: vec![atom_var],
            }],
            methods: HashMap::new(),
        });
    }

    // ── Second pass: check items ─────────────────────────────────

    pub(crate) fn check_items(&mut self) {
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

            ExprKind::SwapBang { atom, func } => {
                let atom_ty = self.check_expr(*atom);
                let func_ty = self.check_expr(*func);

                // atom must be (Atom T)
                let inner_ty = self.fresh_var();
                let expected_atom = Ty::Con(SmolStr::new("Atom"), vec![inner_ty.clone()]);
                self.unify(&atom_ty, &expected_atom, span);

                // func must be (Fn [T] T)
                let expected_fn = Ty::Fn(vec![inner_ty.clone()], Box::new(inner_ty.clone()));
                self.unify(&func_ty, &expected_fn, span);

                // T must be Shareable (atoms are shared across threads)
                self.deferred_constraints.push((
                    ClassConstraint {
                        class_name: SmolStr::new("Shareable"),
                        type_args: vec![inner_ty.clone()],
                    },
                    expr_id,
                    span,
                ));

                inner_ty
            }

            ExprKind::WithTasks { body } => {
                self.task_depth += 1;
                let body_ty = self.check_body(body);
                self.task_depth -= 1;
                body_ty
            }

            ExprKind::Spawn(inner) => {
                if self.task_depth == 0 {
                    self.error("spawn must be inside a with-tasks block", span);
                }
                let _inner_ty = self.check_expr(*inner);
                Ty::Unit
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
                | "atom"
                | "deref"
                | "channel"
                | "send"
                | "recv"
                | "par-map"
                | "par-for-each"
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
            "atom" => {
                if arg_tys.len() != 1 {
                    self.error("atom requires exactly 1 argument", span);
                    return Ty::Error;
                }
                Ty::Con(SmolStr::new("Atom"), vec![arg_tys[0].clone()])
            }
            "deref" => {
                if arg_tys.len() != 1 {
                    self.error("deref requires exactly 1 argument", span);
                    return Ty::Error;
                }
                let inner = self.fresh_var();
                let atom_ty = Ty::Con(SmolStr::new("Atom"), vec![inner.clone()]);
                self.unify(&arg_tys[0], &atom_ty, span);
                inner
            }
            "channel" => {
                if !arg_tys.is_empty() {
                    self.error("channel takes no arguments", span);
                    return Ty::Error;
                }
                let elem = self.fresh_var();
                Ty::Con(SmolStr::new("Channel"), vec![elem])
            }
            "send" => {
                if arg_tys.len() != 2 {
                    self.error("send requires exactly 2 arguments (channel, value)", span);
                    return Ty::Error;
                }
                let elem = self.fresh_var();
                let ch_ty = Ty::Con(SmolStr::new("Channel"), vec![elem.clone()]);
                self.unify(&arg_tys[0], &ch_ty, span);
                self.unify(&arg_tys[1], &elem, span);
                Ty::Unit
            }
            "recv" => {
                if arg_tys.len() != 1 {
                    self.error("recv requires exactly 1 argument", span);
                    return Ty::Error;
                }
                let elem = self.fresh_var();
                let ch_ty = Ty::Con(SmolStr::new("Channel"), vec![elem.clone()]);
                self.unify(&arg_tys[0], &ch_ty, span);
                elem
            }
            "par-map" => {
                if arg_tys.len() != 2 {
                    self.error("par-map requires exactly 2 arguments (func, vector)", span);
                    return Ty::Error;
                }
                let t = self.fresh_var();
                let u = self.fresh_var();
                let fn_ty = Ty::Fn(vec![t.clone()], Box::new(u.clone()));
                self.unify(&arg_tys[0], &fn_ty, span);
                let vec_t = Ty::Vector(Box::new(t));
                self.unify(&arg_tys[1], &vec_t, span);
                Ty::Vector(Box::new(u))
            }
            "par-for-each" => {
                if arg_tys.len() != 2 {
                    self.error("par-for-each requires exactly 2 arguments (func, vector)", span);
                    return Ty::Error;
                }
                let t = self.fresh_var();
                let fn_ty = Ty::Fn(vec![t.clone()], Box::new(Ty::Unit));
                self.unify(&arg_tys[0], &fn_ty, span);
                let vec_t = Ty::Vector(Box::new(t));
                self.unify(&arg_tys[1], &vec_t, span);
                Ty::Unit
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
