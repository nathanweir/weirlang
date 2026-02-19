use weir_ast::*;
use weir_lexer::Span;

// ── Public types ─────────────────────────────────────────────────

type ScopeId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RefKind {
    Definition,
    Reference,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Type,
    Variant,
    Struct,
    Class,
    Parameter,
    LetBinding,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeKind {
    Module,
    Function,
    Let,
    Lambda,
    MatchArm,
}

#[derive(Debug, Clone)]
struct ScopedDef {
    name: String,
    kind: SymbolKind,
    name_span: Span,
    full_span: Span,
    docstring: Option<String>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Scope {
    kind: ScopeKind,
    span: Span,
    parent: Option<ScopeId>,
    definitions: Vec<ScopedDef>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Occurrence {
    name: String,
    name_span: Span,
    scope_id: ScopeId,
    kind: RefKind,
}

/// Result of resolving a symbol at a cursor position.
#[derive(Debug)]
pub struct Resolution {
    pub name: String,
    pub kind: SymbolKind,
    pub def_name_span: Span,
    pub def_full_span: Span,
    pub docstring: Option<String>,
    /// All name-only spans that resolve to the same definition (def + refs).
    pub all_name_spans: Vec<Span>,
}

// ── SymbolIndex ──────────────────────────────────────────────────

#[derive(Debug)]
pub struct SymbolIndex {
    scopes: Vec<Scope>,
    /// Sorted by name_span.start
    occurrences: Vec<Occurrence>,
}

impl SymbolIndex {
    /// Build a symbol index from a parsed module.
    pub fn build(module: &Module) -> Self {
        let mut builder = IndexBuilder {
            module,
            scopes: Vec::new(),
            occurrences: Vec::new(),
        };
        builder.build();
        // Sort occurrences by start offset for binary search
        builder.occurrences.sort_by_key(|o| o.name_span.start);
        SymbolIndex {
            scopes: builder.scopes,
            occurrences: builder.occurrences,
        }
    }

    /// Find the symbol name at a given byte offset (binary search).
    pub fn symbol_at(&self, offset: u32) -> Option<&str> {
        let occ = self.occurrence_at(offset)?;
        Some(&occ.name)
    }

    /// Scope-aware resolution: find the definition that a symbol at `offset` resolves to,
    /// and all other occurrences that resolve to the same definition.
    pub fn resolve_at(&self, offset: u32) -> Option<Resolution> {
        let occ = self.occurrence_at(offset)?;
        let name = &occ.name;
        let scope_id = occ.scope_id;

        // Find the defining scope (walk up from the occurrence's scope)
        let def_scope_id = self.find_defining_scope(name, scope_id)?;
        let scope = &self.scopes[def_scope_id];
        let def = scope.definitions.iter().find(|d| d.name == *name)?;

        let def_name_span = def.name_span;
        let def_full_span = def.full_span;
        let docstring = def.docstring.clone();
        let kind = def.kind;

        // Collect all occurrences that resolve to the same definition
        let all_name_spans: Vec<Span> = self
            .occurrences
            .iter()
            .filter(|o| {
                o.name == *name
                    && self.find_defining_scope(&o.name, o.scope_id) == Some(def_scope_id)
            })
            .map(|o| o.name_span)
            .collect();

        Some(Resolution {
            name: name.clone(),
            kind,
            def_name_span,
            def_full_span,
            docstring,
            all_name_spans,
        })
    }

    /// Get all top-level definitions of a name (for backward compat / completion).
    pub fn get_definitions(&self, name: &str) -> Vec<DefinitionInfo> {
        let mut results = Vec::new();
        for scope in &self.scopes {
            for def in &scope.definitions {
                if def.name == name {
                    results.push(DefinitionInfo {
                        name: def.name.clone(),
                        kind: def.kind,
                        name_span: def.name_span,
                        full_span: def.full_span,
                        docstring: def.docstring.clone(),
                    });
                }
            }
        }
        results
    }

    /// Return all definitions visible at `offset`, with inner scopes
    /// shadowing outer scopes.  Each entry is (name, kind).
    pub fn visible_definitions_at(&self, offset: u32) -> Vec<(String, SymbolKind)> {
        // Find the innermost scope containing this offset
        let scope_id = match self.innermost_scope_at(offset) {
            Some(id) => id,
            None => return Vec::new(),
        };

        // Walk up the scope chain collecting definitions; earlier (inner) defs shadow later (outer)
        let mut seen = std::collections::HashSet::new();
        let mut result = Vec::new();
        let mut current = Some(scope_id);
        while let Some(sid) = current {
            let scope = &self.scopes[sid];
            for def in &scope.definitions {
                if seen.insert(def.name.clone()) {
                    result.push((def.name.clone(), def.kind));
                }
            }
            current = scope.parent;
        }
        result
    }

    /// Return all occurrence spans (definitions + references) for a given name.
    /// Used by cross-file references/rename to find all uses of a symbol in a file.
    pub fn get_all_occurrences_of(&self, name: &str) -> Vec<Span> {
        self.occurrences
            .iter()
            .filter(|o| o.name == name)
            .map(|o| o.name_span)
            .collect()
    }

    fn innermost_scope_at(&self, offset: u32) -> Option<ScopeId> {
        let mut best: Option<(ScopeId, u32)> = None;
        for (i, scope) in self.scopes.iter().enumerate() {
            if scope.span.start <= offset && offset <= scope.span.end {
                let size = scope.span.end - scope.span.start;
                let is_better = match best {
                    Some((_, best_size)) => size <= best_size,
                    None => true,
                };
                if is_better {
                    best = Some((i, size));
                }
            }
        }
        best.map(|(id, _)| id)
    }

    // ── internal helpers ─────────────────────────────────────────

    fn occurrence_at(&self, offset: u32) -> Option<&Occurrence> {
        // Binary search for candidate by start offset, then check containment.
        let idx = self
            .occurrences
            .partition_point(|o| o.name_span.start <= offset);
        // Check candidates going backwards from idx
        for i in (0..idx).rev() {
            let o = &self.occurrences[i];
            if o.name_span.start <= offset && offset < o.name_span.end {
                return Some(o);
            }
            // Since sorted by start, once start is too far before offset, stop
            if offset - o.name_span.start > 256 {
                break;
            }
        }
        None
    }

    /// Walk up the scope chain from `scope_id` to find the innermost scope
    /// that defines `name`.
    fn find_defining_scope(&self, name: &str, scope_id: ScopeId) -> Option<ScopeId> {
        let mut current = Some(scope_id);
        while let Some(sid) = current {
            let scope = &self.scopes[sid];
            if scope.definitions.iter().any(|d| d.name == name) {
                return Some(sid);
            }
            current = scope.parent;
        }
        None
    }
}

/// Public definition info (replaces old SymbolDef)
#[derive(Debug, Clone)]
pub struct DefinitionInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub name_span: Span,
    pub full_span: Span,
    pub docstring: Option<String>,
}

// ── Index builder ────────────────────────────────────────────────

struct IndexBuilder<'a> {
    module: &'a Module,
    scopes: Vec<Scope>,
    occurrences: Vec<Occurrence>,
}

impl<'a> IndexBuilder<'a> {
    fn build(&mut self) {
        // Module-level scope covering the entire file
        let module_span = if let Some((_, last_span)) = self.module.items.last() {
            let first_span = self
                .module
                .items
                .first()
                .map(|(_, s)| *s)
                .unwrap_or(*last_span);
            Span::new(first_span.start, last_span.end)
        } else {
            Span::new(0, 0)
        };

        let module_scope = self.push_scope(ScopeKind::Module, module_span, None);

        // First pass: register all top-level definitions
        for (item, item_span) in &self.module.items {
            match item {
                Item::Defn(d) => {
                    self.add_scope_def(
                        module_scope,
                        &d.name,
                        SymbolKind::Function,
                        d.name_span,
                        *item_span,
                        d.docstring.clone(),
                    );
                }
                Item::Deftype(d) => {
                    self.add_scope_def(
                        module_scope,
                        &d.name,
                        SymbolKind::Type,
                        d.name_span,
                        *item_span,
                        None,
                    );
                    for v in &d.variants {
                        self.add_scope_def(
                            module_scope,
                            &v.name,
                            SymbolKind::Variant,
                            v.name_span,
                            v.span,
                            None,
                        );
                    }
                }
                Item::Defstruct(d) => {
                    self.add_scope_def(
                        module_scope,
                        &d.name,
                        SymbolKind::Struct,
                        d.name_span,
                        *item_span,
                        None,
                    );
                }
                Item::Defclass(d) => {
                    self.add_scope_def(
                        module_scope,
                        &d.name,
                        SymbolKind::Class,
                        d.name_span,
                        *item_span,
                        None,
                    );
                }
                Item::Instance(inst) => {
                    for method in &inst.methods {
                        self.add_scope_def(
                            module_scope,
                            &method.name,
                            SymbolKind::Function,
                            method.name_span,
                            method.span,
                            method.docstring.clone(),
                        );
                    }
                }
                _ => {}
            }
        }

        // Second pass: walk into item bodies
        for (item, item_span) in &self.module.items {
            match item {
                Item::Defn(d) => {
                    self.walk_defn(d, module_scope, *item_span);
                }
                Item::Instance(inst) => {
                    for method in &inst.methods {
                        self.walk_defn(method, module_scope, method.span);
                    }
                }
                _ => {}
            }
        }
    }

    fn walk_defn(&mut self, d: &Defn, parent: ScopeId, fn_span: Span) {
        let fn_scope = self.push_scope(ScopeKind::Function, fn_span, Some(parent));
        for p in &d.params {
            self.add_scope_def(
                fn_scope,
                &p.name,
                SymbolKind::Parameter,
                p.name_span,
                p.span,
                None,
            );
        }
        for &body_expr in &d.body {
            self.walk_expr(body_expr, fn_scope);
        }
    }

    fn walk_expr(&mut self, id: ExprId, scope: ScopeId) {
        let expr = &self.module.exprs[id];
        let expr_span = expr.span;
        match &expr.kind {
            ExprKind::Var(name) => {
                self.occurrences.push(Occurrence {
                    name: name.to_string(),
                    name_span: expr_span,
                    scope_id: scope,
                    kind: RefKind::Reference,
                });
            }
            ExprKind::Call { func, args } => {
                self.walk_expr(*func, scope);
                for arg in args {
                    self.walk_expr(arg.value, scope);
                }
            }
            ExprKind::Let { bindings, body } => {
                let let_span = expr_span;
                let let_scope = self.push_scope(ScopeKind::Let, let_span, Some(scope));
                for b in bindings {
                    // Walk value expr in the PARENT scope (or prior let scope);
                    // for simplicity, walk in let_scope (slightly over-scoped but
                    // matching standard let* semantics where earlier bindings are visible)
                    self.walk_expr(b.value, let_scope);
                    self.add_scope_def(
                        let_scope,
                        &b.name,
                        SymbolKind::LetBinding,
                        b.name_span,
                        b.span,
                        None,
                    );
                }
                for &e in body {
                    self.walk_expr(e, let_scope);
                }
            }
            ExprKind::Lambda { params, body, .. } => {
                let lam_scope = self.push_scope(ScopeKind::Lambda, expr_span, Some(scope));
                for p in params {
                    self.add_scope_def(
                        lam_scope,
                        &p.name,
                        SymbolKind::Parameter,
                        p.name_span,
                        p.span,
                        None,
                    );
                }
                for &e in body {
                    self.walk_expr(e, lam_scope);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.walk_expr(*scrutinee, scope);
                for arm in arms {
                    let arm_scope = self.push_scope(ScopeKind::MatchArm, arm.span, Some(scope));
                    self.walk_pattern(arm.pattern, arm_scope);
                    for &e in &arm.body {
                        self.walk_expr(e, arm_scope);
                    }
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.walk_expr(*condition, scope);
                self.walk_expr(*then_branch, scope);
                if let Some(e) = else_branch {
                    self.walk_expr(*e, scope);
                }
            }
            ExprKind::Cond {
                clauses,
                else_clause,
            } => {
                for (test, body) in clauses {
                    self.walk_expr(*test, scope);
                    self.walk_expr(*body, scope);
                }
                if let Some(e) = else_clause {
                    self.walk_expr(*e, scope);
                }
            }
            ExprKind::When { condition, body } | ExprKind::Unless { condition, body } => {
                self.walk_expr(*condition, scope);
                for &e in body {
                    self.walk_expr(e, scope);
                }
            }
            ExprKind::Do { body }
            | ExprKind::Unsafe { body }
            | ExprKind::WithArena { body, .. }
            | ExprKind::WithTasks { body } => {
                for &e in body {
                    self.walk_expr(e, scope);
                }
            }
            ExprKind::SetBang { place, value } => {
                self.walk_expr(*place, scope);
                self.walk_expr(*value, scope);
            }
            ExprKind::Ann { expr, .. } => {
                self.walk_expr(*expr, scope);
            }
            ExprKind::Try(inner) | ExprKind::Spawn(inner) => {
                self.walk_expr(*inner, scope);
            }
            ExprKind::SwapBang { atom, func } => {
                self.walk_expr(*atom, scope);
                self.walk_expr(*func, scope);
            }
            ExprKind::VectorLit(elems) => {
                for &e in elems {
                    self.walk_expr(e, scope);
                }
            }
            ExprKind::MapLit(pairs) => {
                for (k, v) in pairs {
                    self.walk_expr(*k, scope);
                    self.walk_expr(*v, scope);
                }
            }
            ExprKind::Lit(_) | ExprKind::FieldAccess(_) => {}
        }
    }

    fn walk_pattern(&mut self, id: PatternId, scope: ScopeId) {
        let pat = &self.module.patterns[id];
        match &pat.kind {
            PatternKind::Var(name) => {
                // Pattern variable — its span is already just the name token
                let name_span = pat.span;
                self.add_scope_def(
                    scope,
                    name,
                    SymbolKind::LetBinding,
                    name_span,
                    name_span,
                    None,
                );
            }
            PatternKind::Constructor { args, .. } => {
                for &arg in args {
                    self.walk_pattern(arg, scope);
                }
            }
            PatternKind::StructDestructure { fields } => {
                for f in fields {
                    if let Some(ref binding) = f.binding {
                        self.add_scope_def(
                            scope,
                            binding,
                            SymbolKind::LetBinding,
                            f.span,
                            f.span,
                            None,
                        );
                    } else {
                        self.add_scope_def(
                            scope,
                            &f.field_name,
                            SymbolKind::LetBinding,
                            f.span,
                            f.span,
                            None,
                        );
                    }
                }
            }
            PatternKind::Wildcard | PatternKind::Literal(_) => {}
        }
    }

    // ── helpers ──────────────────────────────────────────────────

    fn push_scope(&mut self, kind: ScopeKind, span: Span, parent: Option<ScopeId>) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(Scope {
            kind,
            span,
            parent,
            definitions: Vec::new(),
        });
        id
    }

    fn add_scope_def(
        &mut self,
        scope_id: ScopeId,
        name: &str,
        kind: SymbolKind,
        name_span: Span,
        full_span: Span,
        docstring: Option<String>,
    ) {
        self.scopes[scope_id].definitions.push(ScopedDef {
            name: name.to_string(),
            kind,
            name_span,
            full_span,
            docstring,
        });
        self.occurrences.push(Occurrence {
            name: name.to_string(),
            name_span,
            scope_id,
            kind: RefKind::Definition,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_index(source: &str) -> SymbolIndex {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        SymbolIndex::build(&module)
    }

    #[test]
    fn indexes_function_def_and_call() {
        let idx = build_index(
            "(defn foo () 42)
             (defn main () (foo))",
        );
        let defs = idx.get_definitions("foo");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].kind, SymbolKind::Function);

        let res = idx.resolve_at(6).unwrap(); // "foo" at offset 6
        assert_eq!(res.name, "foo");
        assert_eq!(res.kind, SymbolKind::Function);
        // def + call = 2 spans
        assert_eq!(res.all_name_spans.len(), 2);
    }

    #[test]
    fn indexes_type_and_variants() {
        let idx = build_index(
            "(deftype Color
               Red
               Green
               Blue)",
        );
        assert_eq!(idx.get_definitions("Color").len(), 1);
        assert_eq!(idx.get_definitions("Red").len(), 1);
        assert_eq!(idx.get_definitions("Red")[0].kind, SymbolKind::Variant);
    }

    #[test]
    fn indexes_let_bindings() {
        let idx = build_index("(defn main () (let ((x 1) (y 2)) (+ x y)))");
        assert_eq!(idx.get_definitions("x").len(), 1);
        assert_eq!(idx.get_definitions("x")[0].kind, SymbolKind::LetBinding);
    }

    #[test]
    fn symbol_at_offset() {
        let source = "(defn foo () 42)";
        let idx = build_index(source);
        // "foo" starts after "(defn " = offset 6
        assert_eq!(idx.symbol_at(6), Some("foo"));
        assert_eq!(idx.symbol_at(0), None);
    }

    #[test]
    fn indexes_parameters() {
        let idx = build_index("(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
        let defs = idx.get_definitions("x");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].kind, SymbolKind::Parameter);
    }

    #[test]
    fn indexes_struct() {
        let idx = build_index(
            "(defstruct Point
               (x : f64)
               (y : f64))",
        );
        assert_eq!(idx.get_definitions("Point").len(), 1);
        assert_eq!(idx.get_definitions("Point")[0].kind, SymbolKind::Struct);
    }

    // ── New scope-aware tests ────────────────────────────────────

    #[test]
    fn rename_span_is_name_only() {
        let source = "(defn foo () 42)";
        let idx = build_index(source);
        // Resolve at "foo" (offset 6)
        let res = idx.resolve_at(6).unwrap();
        assert_eq!(res.name, "foo");
        // The definition name_span should cover only "foo" (3 bytes: 6..9)
        assert_eq!(res.def_name_span, Span::new(6, 9));
        // All spans in the rename set should be name-only
        for span in &res.all_name_spans {
            assert_eq!(span.end - span.start, 3, "span {:?} is not 3 bytes", span);
        }
    }

    #[test]
    fn rename_param_span_is_name_only() {
        let source = "(defn add ((x : i32) (y : i32)) (+ x y))";
        let idx = build_index(source);
        // "x" param is at offset 12
        let res = idx.resolve_at(12).unwrap();
        assert_eq!(res.name, "x");
        assert_eq!(res.def_name_span.end - res.def_name_span.start, 1);
        // All x spans should be 1 byte
        for span in &res.all_name_spans {
            assert_eq!(span.end - span.start, 1, "span {:?} is not 1 byte", span);
        }
    }

    #[test]
    fn rename_let_binding_span_is_name_only() {
        let source = "(defn main () (let ((x 1)) x))";
        let idx = build_index(source);
        // "x" in let binding is at offset 21
        let res = idx.resolve_at(21).unwrap();
        assert_eq!(res.name, "x");
        assert_eq!(res.def_name_span.end - res.def_name_span.start, 1);
    }

    #[test]
    fn shadowed_let_bindings() {
        let source = "(defn f () (let ((x 1)) (let ((x 2)) x)))";
        let idx = build_index(source);
        // Outer x def at offset 18, inner x def at offset 31, body ref x at offset 37
        let res = idx.resolve_at(37).unwrap();
        assert_eq!(res.name, "x");
        // Should only include inner def + inner ref, NOT outer def
        assert_eq!(
            res.all_name_spans.len(),
            2,
            "inner x should have exactly 2 occurrences (def + ref)"
        );
        // The outer x should not be included
        for span in &res.all_name_spans {
            assert_ne!(span.start, 18, "outer x should not be in rename set");
        }
    }

    #[test]
    fn shadowed_param_by_let() {
        let source = "(defn f (x) (let ((x 1)) x))";
        let idx = build_index(source);
        // Param x at offset 9, let-bound x at offset 19, body ref x at offset 25
        let res = idx.resolve_at(25).unwrap();
        assert_eq!(res.name, "x");
        // Should include let-bound def + body ref, NOT param
        for span in &res.all_name_spans {
            assert_ne!(span.start, 9, "param x should not be in rename set");
        }
    }

    #[test]
    fn lambda_scope_isolation() {
        let source = "(defn f (x) (fn (x) x))";
        let idx = build_index(source);
        // Lambda param x is at offset 17
        // Lambda body ref x is at offset 20
        // Outer param x is at offset 9
        // Resolve at lambda body x
        let res = idx.resolve_at(20).unwrap();
        assert_eq!(res.name, "x");
        // Should only include lambda param + lambda body ref
        for span in &res.all_name_spans {
            assert_ne!(span.start, 9, "outer param x should not be in rename set");
        }
    }

    #[test]
    fn cross_scope_reference() {
        let source = "(defn f (x) (let ((y 1)) x))";
        let idx = build_index(source);
        // x ref in let body at offset 25
        let res = idx.resolve_at(25).unwrap();
        assert_eq!(res.name, "x");
        // Should resolve to the param x
        assert_eq!(res.kind, SymbolKind::Parameter);
    }

    #[test]
    fn function_def_and_call_rename() {
        let source = "(defn foo () 42) (defn main () (foo))";
        let idx = build_index(source);
        // "foo" call is at offset 32
        let res = idx.resolve_at(32).unwrap();
        assert_eq!(res.name, "foo");
        assert_eq!(res.all_name_spans.len(), 2);
        // Both spans should be exactly 3 bytes (just "foo")
        for span in &res.all_name_spans {
            assert_eq!(span.end - span.start, 3);
        }
    }

    #[test]
    fn match_arm_pattern_bindings() {
        let source = "(defn f (x) (match x ((Some val) val) (None 0)))";
        let idx = build_index(source);
        // "val" in pattern — find its offset
        // Source: (defn f (x) (match x ((Some val) val) (None 0)))
        //         0     6   10       20        30
        // Let's find "val" def in the match arm
        let val_def_offset = source.find("val").unwrap() as u32;
        let res = idx.resolve_at(val_def_offset).unwrap();
        assert_eq!(res.name, "val");
        assert_eq!(res.kind, SymbolKind::LetBinding);
        // Should have 2 occurrences: pattern binding + body ref
        assert_eq!(res.all_name_spans.len(), 2);
    }

    #[test]
    fn no_symbol_at_whitespace() {
        let source = "(defn foo () 42)";
        let idx = build_index(source);
        // Offset 0 is '(', offset 5 is ' ', offset 10 is ' '
        assert!(idx.symbol_at(0).is_none());
        assert!(idx.symbol_at(5).is_none());
        assert!(idx.resolve_at(0).is_none());
    }

    #[test]
    fn indexes_defstruct_name() {
        let source = "(defstruct Point (x : f64) (y : f64))";
        let idx = build_index(source);
        let res = idx.resolve_at(11).unwrap(); // "Point" at offset 11
        assert_eq!(res.name, "Point");
        assert_eq!(res.kind, SymbolKind::Struct);
        assert_eq!(res.def_name_span, Span::new(11, 16));
    }

    #[test]
    fn indexes_deftype_name() {
        let source = "(deftype Color Red Green Blue)";
        let idx = build_index(source);
        let res = idx.resolve_at(9).unwrap(); // "Color" at offset 9
        assert_eq!(res.name, "Color");
        assert_eq!(res.kind, SymbolKind::Type);
    }

    #[test]
    fn indexes_variant_constructors() {
        let source = "(deftype (Option 'a) (Some 'a) None)";
        let idx = build_index(source);
        // Find "Some" — it starts at offset 22
        let some_offset = source.find("Some").unwrap() as u32;
        let res = idx.resolve_at(some_offset).unwrap();
        assert_eq!(res.name, "Some");
        assert_eq!(res.kind, SymbolKind::Variant);
    }

    #[test]
    fn indexes_class() {
        let source = "(defclass (Show 'a) (show : (Fn ['a] String)))";
        let idx = build_index(source);
        let show_offset = source.find("Show").unwrap() as u32;
        let res = idx.resolve_at(show_offset).unwrap();
        assert_eq!(res.name, "Show");
        assert_eq!(res.kind, SymbolKind::Class);
    }

    #[test]
    fn docstring_preserved_in_resolution() {
        let source = "(defn greet () \"Says hello\" (println \"hello\"))";
        let idx = build_index(source);
        let res = idx.resolve_at(6).unwrap(); // "greet"
        assert_eq!(res.docstring.as_deref(), Some("Says hello"));
    }

    #[test]
    fn multiple_functions_independent() {
        let source = "(defn foo () 42) (defn bar () (foo))";
        let idx = build_index(source);
        // Resolving "bar" should only get bar's occurrences
        let bar_offset = source.find("bar").unwrap() as u32;
        let res = idx.resolve_at(bar_offset).unwrap();
        assert_eq!(res.name, "bar");
        assert_eq!(res.all_name_spans.len(), 1); // just the def, no calls to bar
    }

    #[test]
    fn get_all_occurrences_of_returns_def_and_refs() {
        let source = "(defn foo () 42) (defn main () (foo))";
        let idx = build_index(source);
        let spans = idx.get_all_occurrences_of("foo");
        assert_eq!(spans.len(), 2); // def + call
    }

    #[test]
    fn get_all_occurrences_of_unknown_name() {
        let source = "(defn foo () 42)";
        let idx = build_index(source);
        let spans = idx.get_all_occurrences_of("nonexistent");
        assert!(spans.is_empty());
    }
}
