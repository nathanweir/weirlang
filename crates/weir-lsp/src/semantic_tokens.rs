use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType, SemanticTokensLegend};
use weir_ast::*;
use weir_typeck::TypeCheckResult;

use crate::document::LineIndex;

// ── Token type registry ──────────────────────────────────────────

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,    // 0 — function definitions and calls
    SemanticTokenType::VARIABLE,    // 1 — local variables, let bindings
    SemanticTokenType::PARAMETER,   // 2 — function parameters
    SemanticTokenType::TYPE,        // 3 — type names (i32, String, user types)
    SemanticTokenType::ENUM_MEMBER, // 4 — variant constructors (Some, None)
    SemanticTokenType::KEYWORD,     // 5 — keywords (defn, let, if, match, etc.)
    SemanticTokenType::STRING,      // 6 — string literals
    SemanticTokenType::NUMBER,      // 7 — numeric literals
    SemanticTokenType::OPERATOR,    // 8 — operators (+, -, *, etc.)
    SemanticTokenType::COMMENT,     // 9 — comments
    SemanticTokenType::MACRO,       // 10 — macro names
    SemanticTokenType::INTERFACE,   // 11 — typeclass names
    SemanticTokenType::METHOD,      // 12 — typeclass methods
    SemanticTokenType::new("typeParameter"), // 13 — type variables ('a, 'b)
    SemanticTokenType::new("boolean"), // 14 — boolean literals
];

const TK_FUNCTION: u32 = 0;
const TK_VARIABLE: u32 = 1;
const TK_PARAMETER: u32 = 2;
const TK_TYPE: u32 = 3;
const TK_ENUM_MEMBER: u32 = 4;
const TK_KEYWORD: u32 = 5;
const TK_STRING: u32 = 6;
const TK_NUMBER: u32 = 7;
const TK_OPERATOR: u32 = 8;
#[allow(dead_code)]
const TK_COMMENT: u32 = 9;
#[allow(dead_code)]
const TK_MACRO: u32 = 10;
const TK_INTERFACE: u32 = 11;
const TK_METHOD: u32 = 12;
const TK_TYPE_PARAM: u32 = 13;
const TK_BOOLEAN: u32 = 14;

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.to_vec(),
        token_modifiers: vec![],
    }
}

// ── Token collection ─────────────────────────────────────────────

struct RawToken {
    start: u32,
    length: u32,
    token_type: u32,
}

pub fn semantic_tokens(
    module: &Module,
    type_result: &TypeCheckResult,
    source: &str,
    line_index: &LineIndex,
) -> Vec<SemanticToken> {
    let mut collector = TokenCollector {
        module,
        type_result,
        source,
        tokens: Vec::new(),
    };
    collector.collect_all();

    // Sort by position
    collector.tokens.sort_by_key(|t| t.start);

    // Convert to LSP delta format
    to_delta_tokens(&collector.tokens, line_index)
}

struct TokenCollector<'a> {
    module: &'a Module,
    type_result: &'a TypeCheckResult,
    source: &'a str,
    tokens: Vec<RawToken>,
}

impl<'a> TokenCollector<'a> {
    fn push(&mut self, start: u32, length: u32, token_type: u32) {
        if length > 0 {
            self.tokens.push(RawToken {
                start,
                length,
                token_type,
            });
        }
    }

    fn collect_all(&mut self) {
        for (item, item_span) in &self.module.items {
            match item {
                Item::Defn(d) => {
                    self.push_keyword_at_form_start(*item_span, "defn");
                    self.push(
                        d.name_span.start,
                        d.name_span.end - d.name_span.start,
                        TK_FUNCTION,
                    );
                    for p in &d.params {
                        self.push(
                            p.name_span.start,
                            p.name_span.end - p.name_span.start,
                            TK_PARAMETER,
                        );
                        if let Some(type_ann) = p.type_ann {
                            self.collect_type_expr(type_ann);
                        }
                    }
                    if let Some(ret) = d.return_type {
                        self.collect_type_expr(ret);
                    }
                    if let Some(ref doc) = d.docstring {
                        // Find the docstring in source (it's a string literal in the body area)
                        let _ = doc; // docstring highlighting handled by string literal below
                    }
                    for &expr_id in &d.body {
                        self.collect_expr(expr_id);
                    }
                }
                Item::Deftype(d) => {
                    self.push_keyword_at_form_start(*item_span, "deftype");
                    self.push(
                        d.name_span.start,
                        d.name_span.end - d.name_span.start,
                        TK_TYPE,
                    );
                    for tp in &d.type_params {
                        // Find the type param in source to get its span
                        if let Some(offset) = self.find_type_param_in_range(tp, *item_span) {
                            self.push(offset, tp.len() as u32 + 1, TK_TYPE_PARAM);
                            // +1 for '
                        }
                    }
                    for v in &d.variants {
                        self.push(
                            v.name_span.start,
                            v.name_span.end - v.name_span.start,
                            TK_ENUM_MEMBER,
                        );
                        for &field in &v.fields {
                            self.collect_type_expr(field);
                        }
                    }
                }
                Item::Defstruct(d) => {
                    self.push_keyword_at_form_start(*item_span, "defstruct");
                    self.push(
                        d.name_span.start,
                        d.name_span.end - d.name_span.start,
                        TK_TYPE,
                    );
                    for f in &d.fields {
                        self.push(
                            f.name_span.start,
                            f.name_span.end - f.name_span.start,
                            TK_VARIABLE,
                        );
                        self.collect_type_expr(f.type_ann);
                    }
                }
                Item::Defclass(d) => {
                    self.push_keyword_at_form_start(*item_span, "defclass");
                    self.push(
                        d.name_span.start,
                        d.name_span.end - d.name_span.start,
                        TK_INTERFACE,
                    );
                    for m in &d.methods {
                        self.push(
                            m.name_span.start,
                            m.name_span.end - m.name_span.start,
                            TK_METHOD,
                        );
                        self.collect_type_expr(m.type_ann);
                    }
                }
                Item::Instance(inst) => {
                    self.push_keyword_at_form_start(*item_span, "instance");
                    // Class name reference
                    if let Some(offset) = self.find_word_in_range(&inst.class_name, *item_span) {
                        self.push(offset, inst.class_name.len() as u32, TK_INTERFACE);
                    }
                    for type_arg in &inst.type_args {
                        self.collect_type_expr(*type_arg);
                    }
                    for method in &inst.methods {
                        self.push(
                            method.name_span.start,
                            method.name_span.end - method.name_span.start,
                            TK_METHOD,
                        );
                        for p in &method.params {
                            self.push(
                                p.name_span.start,
                                p.name_span.end - p.name_span.start,
                                TK_PARAMETER,
                            );
                            if let Some(type_ann) = p.type_ann {
                                self.collect_type_expr(type_ann);
                            }
                        }
                        if let Some(ret) = method.return_type {
                            self.collect_type_expr(ret);
                        }
                        for &expr_id in &method.body {
                            self.collect_expr(expr_id);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn collect_expr(&mut self, id: ExprId) {
        let expr = &self.module.exprs[id];
        match &expr.kind {
            ExprKind::Lit(lit) => match lit {
                Literal::Int(_) | Literal::Float(_) => {
                    self.push(expr.span.start, expr.span.end - expr.span.start, TK_NUMBER);
                }
                Literal::Bool(_) => {
                    self.push(expr.span.start, expr.span.end - expr.span.start, TK_BOOLEAN);
                }
                Literal::String(_) => {
                    self.push(expr.span.start, expr.span.end - expr.span.start, TK_STRING);
                }
            },
            ExprKind::Var(name) => {
                let token_type = self.classify_var(name, id);
                self.push(expr.span.start, expr.span.end - expr.span.start, token_type);
            }
            ExprKind::Call { func, args } => {
                self.collect_expr(*func);
                for arg in args {
                    self.collect_expr(arg.value);
                }
            }
            ExprKind::Let { bindings, body } => {
                self.push_keyword_at_form_start(expr.span, "let");
                for b in bindings {
                    self.push(
                        b.name_span.start,
                        b.name_span.end - b.name_span.start,
                        TK_VARIABLE,
                    );
                    self.collect_expr(b.value);
                }
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.push_keyword_at_form_start(expr.span, "if");
                self.collect_expr(*condition);
                self.collect_expr(*then_branch);
                if let Some(e) = else_branch {
                    self.collect_expr(*e);
                }
            }
            ExprKind::Cond {
                clauses,
                else_clause,
            } => {
                self.push_keyword_at_form_start(expr.span, "cond");
                for (test, body) in clauses {
                    self.collect_expr(*test);
                    self.collect_expr(*body);
                }
                if let Some(e) = else_clause {
                    self.collect_expr(*e);
                }
            }
            ExprKind::When { condition, body } => {
                self.push_keyword_at_form_start(expr.span, "when");
                self.collect_expr(*condition);
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::Unless { condition, body } => {
                self.push_keyword_at_form_start(expr.span, "unless");
                self.collect_expr(*condition);
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::Match { scrutinee, arms } => {
                self.push_keyword_at_form_start(expr.span, "match");
                self.collect_expr(*scrutinee);
                for arm in arms {
                    self.collect_pattern(arm.pattern);
                    for &e in &arm.body {
                        self.collect_expr(e);
                    }
                }
            }
            ExprKind::Lambda { params, body, .. } => {
                self.push_keyword_at_form_start(expr.span, "fn");
                for p in params {
                    self.push(
                        p.name_span.start,
                        p.name_span.end - p.name_span.start,
                        TK_PARAMETER,
                    );
                    if let Some(type_ann) = p.type_ann {
                        self.collect_type_expr(type_ann);
                    }
                }
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::Do { body } | ExprKind::Unsafe { body } => {
                let kw = if matches!(expr.kind, ExprKind::Do { .. }) {
                    "do"
                } else {
                    "unsafe"
                };
                self.push_keyword_at_form_start(expr.span, kw);
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::WithArena { body, .. } => {
                self.push_keyword_at_form_start(expr.span, "with-arena");
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::SetBang { place, value } => {
                self.push_keyword_at_form_start(expr.span, "set!");
                self.collect_expr(*place);
                self.collect_expr(*value);
            }
            ExprKind::Ann { expr: inner, .. } => {
                self.collect_expr(*inner);
            }
            ExprKind::Try(inner) | ExprKind::Spawn(inner) => {
                self.collect_expr(*inner);
            }
            ExprKind::SwapBang { atom, func } => {
                self.push_keyword_at_form_start(expr.span, "swap!");
                self.collect_expr(*atom);
                self.collect_expr(*func);
            }
            ExprKind::WithTasks { body } => {
                self.push_keyword_at_form_start(expr.span, "with-tasks");
                for &e in body {
                    self.collect_expr(e);
                }
            }
            ExprKind::VectorLit(elems) => {
                for &e in elems {
                    self.collect_expr(e);
                }
            }
            ExprKind::MapLit(pairs) => {
                for (k, v) in pairs {
                    self.collect_expr(*k);
                    self.collect_expr(*v);
                }
            }
            ExprKind::FieldAccess(_) => {}
        }
    }

    fn collect_pattern(&mut self, id: PatternId) {
        let pat = &self.module.patterns[id];
        match &pat.kind {
            PatternKind::Var(name) => {
                // Check if it looks like a constructor (starts uppercase) or variable
                let tk = if name.chars().next().is_some_and(|c| c.is_uppercase()) {
                    TK_ENUM_MEMBER
                } else {
                    TK_VARIABLE
                };
                self.push(pat.span.start, pat.span.end - pat.span.start, tk);
            }
            PatternKind::Constructor { name, args, .. } => {
                // Find constructor name span
                if let Some(offset) = self.find_word_in_range(name, pat.span) {
                    self.push(offset, name.len() as u32, TK_ENUM_MEMBER);
                }
                for &arg in args {
                    self.collect_pattern(arg);
                }
            }
            PatternKind::Literal(lit) => match lit {
                Literal::Int(_) | Literal::Float(_) => {
                    self.push(pat.span.start, pat.span.end - pat.span.start, TK_NUMBER);
                }
                Literal::Bool(_) => {
                    self.push(pat.span.start, pat.span.end - pat.span.start, TK_BOOLEAN);
                }
                Literal::String(_) => {
                    self.push(pat.span.start, pat.span.end - pat.span.start, TK_STRING);
                }
            },
            PatternKind::StructDestructure { fields } => {
                for f in fields {
                    self.push(f.span.start, f.span.end - f.span.start, TK_VARIABLE);
                }
            }
            PatternKind::Wildcard => {
                self.push(pat.span.start, pat.span.end - pat.span.start, TK_KEYWORD);
            }
        }
    }

    fn collect_type_expr(&mut self, id: TypeExprId) {
        let texpr = &self.module.type_exprs[id];
        match &texpr.kind {
            TypeExprKind::Named(name) => {
                let tk = classify_type_name(name);
                self.push(texpr.span.start, texpr.span.end - texpr.span.start, tk);
            }
            TypeExprKind::TypeVar(_) => {
                self.push(
                    texpr.span.start,
                    texpr.span.end - texpr.span.start,
                    TK_TYPE_PARAM,
                );
            }
            TypeExprKind::Applied { constructor, args } => {
                self.collect_type_expr(*constructor);
                for &arg in args {
                    self.collect_type_expr(arg);
                }
            }
            TypeExprKind::Fn {
                params,
                return_type,
            } => {
                // "Fn" keyword
                let fn_offset = texpr.span.start + 1; // skip '('
                if self.source_slice(fn_offset, fn_offset + 2) == Some("Fn") {
                    self.push(fn_offset, 2, TK_KEYWORD);
                }
                for &p in params {
                    self.collect_type_expr(p);
                }
                self.collect_type_expr(*return_type);
            }
            TypeExprKind::Constrained { inner, .. } => {
                self.collect_type_expr(*inner);
            }
        }
    }

    /// Classify a variable reference — is it a function, operator, or local variable?
    fn classify_var(&self, name: &str, _expr_id: ExprId) -> u32 {
        if is_operator(name) {
            return TK_OPERATOR;
        }
        // Check if it's a known function (top-level defn or builtin)
        if self.type_result.fn_types.contains_key(name) {
            return TK_FUNCTION;
        }
        // Check if it's a typeclass method
        if self.type_result.method_resolutions.values().any(|v| {
            // method_resolutions maps ExprId -> "Class#Type", not method name
            // This is a rough heuristic: check if any class defines this method
            v.contains(name)
        }) {
            return TK_METHOD;
        }
        // Check if it references a typeclass method by checking class definitions
        for (item, _) in &self.module.items {
            if let Item::Defclass(d) = item {
                for m in &d.methods {
                    if m.name == name {
                        return TK_METHOD;
                    }
                }
            }
        }
        TK_VARIABLE
    }

    // ── helpers ──────────────────────────────────────────────────

    fn push_keyword_at_form_start(&mut self, span: Span, keyword: &str) {
        // Form starts with '(' then the keyword
        let start = span.start;
        if self.source_slice(start, start + 1) == Some("(") {
            let kw_start = start + 1;
            let kw_len = keyword.len() as u32;
            if self.source_slice(kw_start, kw_start + kw_len) == Some(keyword) {
                self.push(kw_start, kw_len, TK_KEYWORD);
            }
        }
    }

    fn source_slice(&self, start: u32, end: u32) -> Option<&str> {
        let s = start as usize;
        let e = end as usize;
        if e <= self.source.len() {
            Some(&self.source[s..e])
        } else {
            None
        }
    }

    fn find_word_in_range(&self, word: &str, span: Span) -> Option<u32> {
        let range = &self.source[span.start as usize..span.end as usize];
        range.find(word).map(|i| span.start + i as u32)
    }

    fn find_type_param_in_range(&self, name: &str, span: Span) -> Option<u32> {
        let range = &self.source[span.start as usize..span.end as usize];
        let needle = format!("'{}", name);
        range.find(&needle).map(|i| span.start + i as u32)
    }
}

fn is_operator(name: &str) -> bool {
    matches!(
        name,
        "+" | "-" | "*" | "/" | "mod" | "<" | ">" | "<=" | ">=" | "=" | "!=" | "not" | "and" | "or"
    )
}

fn classify_type_name(name: &str) -> u32 {
    match name {
        "Fn" => TK_KEYWORD,
        _ => TK_TYPE,
    }
}

// ── Delta encoding ───────────────────────────────────────────────

fn to_delta_tokens(raw: &[RawToken], line_index: &LineIndex) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(raw.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in raw {
        let pos = line_index.offset_to_position(token.start);
        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_start
        } else {
            pos.character
        };

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length,
            token_type: token.token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = pos.line;
        prev_start = pos.character;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup(source: &str) -> (Module, TypeCheckResult, LineIndex) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        let line_index = LineIndex::new(source);
        (module, type_result, line_index)
    }

    /// Collect raw tokens (before delta encoding) for easier testing
    fn raw_tokens(source: &str) -> Vec<(u32, u32, u32)> {
        let (module, type_result, _) = setup(source);
        let mut collector = TokenCollector {
            module: &module,
            type_result: &type_result,
            source,
            tokens: Vec::new(),
        };
        collector.collect_all();
        collector.tokens.sort_by_key(|t| t.start);
        collector
            .tokens
            .iter()
            .map(|t| (t.start, t.length, t.token_type))
            .collect()
    }

    fn has_token(tokens: &[(u32, u32, u32)], source: &str, text: &str, expected_type: u32) -> bool {
        let offset = source.find(text).unwrap() as u32;
        let len = text.len() as u32;
        tokens
            .iter()
            .any(|&(s, l, t)| s == offset && l == len && t == expected_type)
    }

    #[test]
    fn function_definition_tokens() {
        let source = "(defn foo () 42)";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "defn", TK_KEYWORD));
        assert!(has_token(&tokens, source, "foo", TK_FUNCTION));
        assert!(has_token(&tokens, source, "42", TK_NUMBER));
    }

    #[test]
    fn parameter_tokens() {
        let source = "(defn add ((x : i32) (y : i32)) (+ x y))";
        let tokens = raw_tokens(source);
        // First occurrence of 'x' is the parameter definition
        assert!(tokens.iter().any(|&(_, _, t)| t == TK_PARAMETER));
        assert!(has_token(&tokens, source, "i32", TK_TYPE));
    }

    #[test]
    fn let_binding_tokens() {
        let source = "(defn main () (let ((x 1)) x))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "let", TK_KEYWORD));
        assert!(has_token(&tokens, source, "1", TK_NUMBER));
    }

    #[test]
    fn string_literal_tokens() {
        let source = r#"(defn main () (print "hello"))"#;
        let tokens = raw_tokens(source);
        // The string literal should be classified as TK_STRING
        assert!(tokens.iter().any(|&(_, _, t)| t == TK_STRING));
    }

    #[test]
    fn boolean_tokens() {
        let source = "(defn main () true)";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "true", TK_BOOLEAN));
    }

    #[test]
    fn operator_tokens() {
        let source = "(defn main () (+ 1 2))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "+", TK_OPERATOR));
    }

    #[test]
    fn type_definition_tokens() {
        let source = "(deftype Color Red Green Blue)";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "deftype", TK_KEYWORD));
        assert!(has_token(&tokens, source, "Color", TK_TYPE));
        assert!(has_token(&tokens, source, "Red", TK_ENUM_MEMBER));
        assert!(has_token(&tokens, source, "Green", TK_ENUM_MEMBER));
    }

    #[test]
    fn class_tokens() {
        let source = "(defclass (Eq 'a) (== : (Fn ['a 'a] Bool)))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "defclass", TK_KEYWORD));
        assert!(has_token(&tokens, source, "Eq", TK_INTERFACE));
        assert!(has_token(&tokens, source, "==", TK_METHOD));
    }

    #[test]
    fn delta_encoding_single_line() {
        let source = "(defn foo () 42)";
        let (module, tr, li) = setup(source);
        let tokens = semantic_tokens(&module, &tr, source, &li);
        assert!(!tokens.is_empty());
        // First token should have delta_line=0 since it's on first line
        assert_eq!(tokens[0].delta_line, 0);
    }

    #[test]
    fn delta_encoding_multi_line() {
        let source = "(defn foo ()\n  42)";
        let (module, tr, li) = setup(source);
        let tokens = semantic_tokens(&module, &tr, source, &li);
        assert!(!tokens.is_empty());
        // Find the "42" token — it should be on line 1
        let num_token = tokens.iter().find(|t| t.token_type == TK_NUMBER).unwrap();
        // The delta_line should be > 0 relative to previous token
        assert!(num_token.delta_line > 0 || tokens.iter().any(|t| t.delta_line > 0));
    }

    #[test]
    fn if_keyword_highlighted() {
        let source = "(defn main () (if true 1 2))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "if", TK_KEYWORD));
    }

    #[test]
    fn match_keyword_highlighted() {
        let source = "(defn main () (match 1 (x x)))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "match", TK_KEYWORD));
    }

    #[test]
    fn instance_tokens() {
        let source = "(defclass (Eq 'a) (== : (Fn ['a 'a] Bool))) (instance (Eq i32) (defn == ((x : i32) (y : i32)) : Bool (= x y)))";
        let tokens = raw_tokens(source);
        assert!(has_token(&tokens, source, "instance", TK_KEYWORD));
    }
}
