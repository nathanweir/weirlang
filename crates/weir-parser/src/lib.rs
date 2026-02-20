use smol_str::SmolStr;
use weir_ast::*;
use weir_lexer::{lex, Span, Token};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.start, self.span.end, self.message)
    }
}

pub fn parse(source: &str) -> (Module, Vec<ParseError>) {
    let (tokens, lex_errors) = lex(source);
    let mut parser = Parser::new(tokens);
    let mut errors: Vec<ParseError> = lex_errors
        .into_iter()
        .map(|span| ParseError {
            message: "unexpected character".into(),
            span,
        })
        .collect();
    parser.parse_module();
    errors.append(&mut parser.errors);
    (parser.module, errors)
}

struct Parser {
    tokens: Vec<(Token, Span)>,
    pos: usize,
    module: Module,
    errors: Vec<ParseError>,
}

impl Parser {
    fn new(tokens: Vec<(Token, Span)>) -> Self {
        Self {
            tokens,
            pos: 0,
            module: Module::new(),
            errors: Vec::new(),
        }
    }

    // ── Token helpers ─────────────────────────────────────────────

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn peek_span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|(_, s)| *s)
            .unwrap_or_else(|| {
                self.tokens
                    .last()
                    .map(|(_, s)| Span::new(s.end, s.end))
                    .unwrap_or(Span::new(0, 0))
            })
    }

    fn advance(&mut self) -> (Token, Span) {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    fn check(&self, expected: &Token) -> bool {
        self.peek() == Some(expected)
    }

    fn check_symbol(&self, name: &str) -> bool {
        matches!(self.peek(), Some(Token::Symbol(s)) if s.as_str() == name)
    }

    fn eat(&mut self, expected: &Token) -> bool {
        if self.check(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat_symbol(&mut self, name: &str) -> bool {
        if self.check_symbol(name) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, expected: &Token) -> Option<Span> {
        if self.check(expected) {
            let (_, span) = self.advance();
            Some(span)
        } else {
            let span = self.peek_span();
            self.error(
                format!("expected {:?}, found {:?}", expected, self.peek()),
                span,
            );
            None
        }
    }

    fn expect_symbol(&mut self) -> Option<(SmolStr, Span)> {
        if let Some(Token::Symbol(_)) = self.peek() {
            let (tok, span) = self.advance();
            if let Token::Symbol(s) = tok {
                return Some((s, span));
            }
        }
        let span = self.peek_span();
        self.error(format!("expected symbol, found {:?}", self.peek()), span);
        None
    }

    fn error(&mut self, message: String, span: Span) {
        self.errors.push(ParseError { message, span });
    }

    /// Skip tokens until we reach a `)` at depth 0, consuming it.
    fn recover_to_close_paren(&mut self) {
        let mut depth = 1;
        while !self.at_end() && depth > 0 {
            match self.peek() {
                Some(Token::LParen | Token::LBracket | Token::LBrace) => {
                    depth += 1;
                    self.advance();
                }
                Some(Token::RParen | Token::RBracket | Token::RBrace) => {
                    depth -= 1;
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    // ── Allocators ────────────────────────────────────────────────

    fn alloc_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        self.module.exprs.alloc(Expr { kind, span })
    }

    fn alloc_type(&mut self, kind: TypeExprKind, span: Span) -> TypeExprId {
        self.module.type_exprs.alloc(TypeExpr { kind, span })
    }

    fn alloc_pattern(&mut self, kind: PatternKind, span: Span) -> PatternId {
        self.module.patterns.alloc(Pattern { kind, span })
    }

    // ── Module parsing ────────────────────────────────────────────

    fn parse_module(&mut self) {
        while !self.at_end() {
            if let Some(item) = self.parse_item() {
                self.module.items.push(item);
            }
        }
    }

    fn parse_item(&mut self) -> Option<(Item, Span)> {
        let start = self.peek_span();
        if !self.eat(&Token::LParen) {
            let span = self.peek_span();
            self.error("expected '(' to start a top-level form".into(), span);
            self.advance();
            return None;
        }

        let is_pub = self.eat_symbol("pub");

        let result = if self.check_symbol("defn") {
            self.advance();
            self.parse_defn(is_pub).map(Item::Defn)
        } else if self.check_symbol("deftype") {
            self.advance();
            self.parse_deftype(is_pub).map(Item::Deftype)
        } else if self.check_symbol("defstruct") {
            self.advance();
            self.parse_defstruct(is_pub).map(Item::Defstruct)
        } else if self.check_symbol("defclass") {
            self.advance();
            self.parse_defclass(is_pub).map(Item::Defclass)
        } else if self.check_symbol("instance") {
            self.advance();
            self.parse_instance().map(Item::Instance)
        } else if self.check_symbol("import") {
            self.advance();
            self.parse_import().map(Item::Import)
        } else if self.check_symbol("declare") {
            self.advance();
            self.parse_declare().map(Item::Declare)
        } else if self.check_symbol("extern") {
            self.advance();
            self.parse_extern_c().map(Item::ExternC)
        } else {
            let span = self.peek_span();
            self.error(
                format!(
                    "expected top-level form (defn, deftype, ...), found {:?}",
                    self.peek()
                ),
                span,
            );
            self.recover_to_close_paren();
            return None;
        };

        let end = self.peek_span();
        if !self.eat(&Token::RParen) {
            self.error("expected ')' to close top-level form".into(), end);
            self.recover_to_close_paren();
        }

        let span = start.merge(
            self.tokens
                .get(self.pos.saturating_sub(1))
                .map(|(_, s)| *s)
                .unwrap_or(end),
        );
        result.map(|item| (item, span))
    }

    // ── defn ──────────────────────────────────────────────────────

    fn parse_defn(&mut self, is_pub: bool) -> Option<Defn> {
        let start = self.peek_span();
        let (name, name_span) = self.expect_symbol()?;

        let (params, params_end) = self.parse_params()?;

        let return_type = if self.eat(&Token::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };

        let docstring = if let Some(Token::String(_)) = self.peek() {
            let (tok, _) = self.advance();
            if let Token::String(s) = tok {
                Some(s)
            } else {
                None
            }
        } else {
            None
        };

        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(expr) = self.try_parse_expr() {
                body.push(expr);
            } else {
                break;
            }
        }

        let end = self.peek_span();
        Some(Defn {
            name,
            name_span,
            params,
            params_end,
            return_type,
            body,
            is_pub,
            docstring,
            span: start.merge(end),
        })
    }

    /// Parse a param list `(params...)`, returning the params and the byte
    /// offset of the end of the closing `)`.
    fn parse_params(&mut self) -> Option<(Vec<Param>, u32)> {
        self.expect(&Token::LParen)?;
        let mut params = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(param) = self.parse_param() {
                params.push(param);
            } else {
                break;
            }
        }
        let close_span = self.expect(&Token::RParen)?;
        Some((params, close_span.end))
    }

    fn parse_param(&mut self) -> Option<Param> {
        let start = self.peek_span();

        if self.check(&Token::LParen) {
            // Annotated param: (name : type), (mut name : type), (ref name : type)
            self.advance();
            let is_mut = self.eat_symbol("mut");
            let is_ref = if !is_mut {
                self.eat_symbol("ref")
            } else {
                false
            };
            let (name, name_span) = self.expect_symbol()?;
            let type_ann = if self.eat(&Token::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            Some(Param {
                name,
                name_span,
                type_ann,
                is_mut,
                is_ref,
                span: start.merge(end),
            })
        } else if let Some(Token::Symbol(_)) = self.peek() {
            // Bare param: name
            let (name, name_span) = self.expect_symbol()?;
            Some(Param {
                name,
                name_span,
                type_ann: None,
                is_mut: false,
                is_ref: false,
                span: name_span,
            })
        } else {
            let span = self.peek_span();
            self.error(format!("expected parameter, found {:?}", self.peek()), span);
            None
        }
    }

    // ── deftype ───────────────────────────────────────────────────

    fn parse_deftype(&mut self, is_pub: bool) -> Option<Deftype> {
        let start = self.peek_span();
        let (name, name_span, type_params) = self.parse_type_name()?;

        let mut variants = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(v) = self.parse_variant() {
                variants.push(v);
            } else {
                break;
            }
        }

        let end = self.peek_span();
        Some(Deftype {
            name,
            name_span,
            type_params,
            variants,
            is_pub,
            span: start.merge(end),
        })
    }

    /// Parse `Name` or `(Name 'a 'b)` for type/struct name with optional type params.
    /// Returns `(name, name_span, type_params)`.
    fn parse_type_name(&mut self) -> Option<(SmolStr, Span, Vec<SmolStr>)> {
        if self.check(&Token::LParen) {
            self.advance();
            let (name, name_span) = self.expect_symbol()?;
            let mut type_params = Vec::new();
            while let Some(Token::TypeVar(_)) = self.peek() {
                let (tok, _) = self.advance();
                if let Token::TypeVar(s) = tok {
                    type_params.push(s);
                }
            }
            self.expect(&Token::RParen)?;
            Some((name, name_span, type_params))
        } else {
            let (name, name_span) = self.expect_symbol()?;
            Some((name, name_span, Vec::new()))
        }
    }

    fn parse_variant(&mut self) -> Option<Variant> {
        let start = self.peek_span();
        if self.check(&Token::LParen) {
            // Variant with fields: (Name type1 type2)
            self.advance();
            let (name, name_span) = self.expect_symbol()?;
            let mut fields = Vec::new();
            while !self.at_end() && !self.check(&Token::RParen) {
                fields.push(self.parse_type_expr()?);
            }
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            Some(Variant {
                name,
                name_span,
                fields,
                span: start.merge(end),
            })
        } else if let Some(Token::Symbol(_)) = self.peek() {
            // Nullary variant: Name
            let (name, name_span) = self.expect_symbol()?;
            Some(Variant {
                name,
                name_span,
                fields: Vec::new(),
                span: name_span,
            })
        } else {
            let span = self.peek_span();
            self.error(format!("expected variant, found {:?}", self.peek()), span);
            None
        }
    }

    // ── defstruct ─────────────────────────────────────────────────

    fn parse_defstruct(&mut self, is_pub: bool) -> Option<Defstruct> {
        let start = self.peek_span();
        let (name, name_span, type_params) = self.parse_type_name()?;

        let mut fields = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            fields.push(self.parse_struct_field()?);
        }

        let end = self.peek_span();
        Some(Defstruct {
            name,
            name_span,
            type_params,
            fields,
            is_pub,
            span: start.merge(end),
        })
    }

    fn parse_struct_field(&mut self) -> Option<StructField> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;
        let (name, name_span) = self.expect_symbol()?;
        self.expect(&Token::Colon)?;
        let type_ann = self.parse_type_expr()?;
        let end = self.peek_span();
        self.expect(&Token::RParen)?;
        Some(StructField {
            name,
            name_span,
            type_ann,
            span: start.merge(end),
        })
    }

    // ── defclass ──────────────────────────────────────────────────

    fn parse_defclass(&mut self, is_pub: bool) -> Option<Defclass> {
        let start = self.peek_span();
        // Parse class head: (Name 'a) or (=> (Super 'a) (Name 'a))
        self.expect(&Token::LParen)?;

        let (superclasses, name, name_span, type_params) = if self.check_symbol("=>") {
            self.advance();
            // Collect all constraint-like forms; last one is the class itself
            let mut items = Vec::new();
            while !self.at_end() && !self.check(&Token::RParen) {
                self.expect(&Token::LParen)?;
                let (n, n_span) = self.expect_symbol()?;
                let mut args = Vec::new();
                while !self.at_end() && !self.check(&Token::RParen) {
                    args.push(self.parse_type_expr()?);
                }
                let end = self.peek_span();
                self.expect(&Token::RParen)?;
                items.push((n, n_span, args, start.merge(end)));
            }
            let (class_name, class_name_span, class_args, _) = items.pop()?;
            let superclasses: Vec<Constraint> = items
                .into_iter()
                .map(|(name, _name_span, type_args, span)| Constraint {
                    class_name: name,
                    type_args,
                    span,
                })
                .collect();
            // Extract type param names from the class args (they should be TypeVar)
            let type_params: Vec<SmolStr> = class_args
                .iter()
                .filter_map(|&id| {
                    if let TypeExprKind::TypeVar(ref name) = self.module.type_exprs[id].kind {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
                .collect();
            (superclasses, class_name, class_name_span, type_params)
        } else {
            let (name, name_span) = self.expect_symbol()?;
            let mut type_params = Vec::new();
            while let Some(Token::TypeVar(_)) = self.peek() {
                let (tok, _) = self.advance();
                if let Token::TypeVar(s) = tok {
                    type_params.push(s);
                }
            }
            (Vec::new(), name, name_span, type_params)
        };

        self.expect(&Token::RParen)?;

        // Parse methods
        let mut methods = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(m) = self.parse_method_sig() {
                methods.push(m);
            } else {
                break;
            }
        }

        let end = self.peek_span();
        Some(Defclass {
            name,
            name_span,
            type_params,
            superclasses,
            methods,
            is_pub,
            span: start.merge(end),
        })
    }

    fn parse_method_sig(&mut self) -> Option<MethodSig> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;
        let (name, name_span) = self.expect_symbol()?;
        self.expect(&Token::Colon)?;
        let type_ann = self.parse_type_expr()?;
        let end = self.peek_span();
        self.expect(&Token::RParen)?;
        Some(MethodSig {
            name,
            name_span,
            type_ann,
            span: start.merge(end),
        })
    }

    // ── instance ──────────────────────────────────────────────────

    fn parse_instance(&mut self) -> Option<InstanceDef> {
        let start = self.peek_span();
        // Parse instance head: (Class Type) or (=> (Constraint 'a) (Class Type))
        self.expect(&Token::LParen)?;

        let (constraints, class_name, type_args) = if self.check_symbol("=>") {
            self.advance();
            let mut items = Vec::new();
            while !self.at_end() && !self.check(&Token::RParen) {
                self.expect(&Token::LParen)?;
                let (n, _) = self.expect_symbol()?;
                let mut args = Vec::new();
                while !self.at_end() && !self.check(&Token::RParen) {
                    args.push(self.parse_type_expr()?);
                }
                let end = self.peek_span();
                self.expect(&Token::RParen)?;
                items.push((n, args, start.merge(end)));
            }
            let (class_name, class_args, _) = items.pop()?;
            let constraints: Vec<Constraint> = items
                .into_iter()
                .map(|(name, type_args, span)| Constraint {
                    class_name: name,
                    type_args,
                    span,
                })
                .collect();
            (constraints, class_name, class_args)
        } else {
            let (name, _) = self.expect_symbol()?;
            let mut args = Vec::new();
            while !self.at_end() && !self.check(&Token::RParen) {
                args.push(self.parse_type_expr()?);
            }
            (Vec::new(), name, args)
        };

        self.expect(&Token::RParen)?;

        // Parse method implementations (defn forms)
        let mut methods = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            self.expect(&Token::LParen)?;
            if !self.eat_symbol("defn") {
                let span = self.peek_span();
                self.error("expected 'defn' in instance body".into(), span);
                self.recover_to_close_paren();
                continue;
            }
            if let Some(defn) = self.parse_defn(false) {
                methods.push(defn);
            }
            self.expect(&Token::RParen)?;
        }

        let end = self.peek_span();
        Some(InstanceDef {
            class_name,
            type_args,
            constraints,
            methods,
            span: start.merge(end),
        })
    }

    // ── import ────────────────────────────────────────────────────

    fn parse_import(&mut self) -> Option<Import> {
        let start = self.peek_span();
        let (module_path, _) = self.expect_symbol()?;

        let kind = if self.check(&Token::LParen) {
            // (import m (a b c))
            self.advance();
            let mut names = Vec::new();
            while !self.at_end() && !self.check(&Token::RParen) {
                let (name, _) = self.expect_symbol()?;
                names.push(name);
            }
            self.expect(&Token::RParen)?;
            ImportKind::Names(names)
        } else if self.check(&Token::Keyword(SmolStr::new("as"))) {
            // (import m :as alias)
            self.advance();
            let (alias, _) = self.expect_symbol()?;
            ImportKind::Alias(alias)
        } else if self.check(&Token::Keyword(SmolStr::new("all"))) {
            // (import m :all)
            self.advance();
            ImportKind::All
        } else {
            let span = self.peek_span();
            self.error("expected import specifier".into(), span);
            ImportKind::Names(Vec::new())
        };

        let end = self.peek_span();
        Some(Import {
            module_path,
            kind,
            span: start.merge(end),
        })
    }

    // ── declare ───────────────────────────────────────────────────

    fn parse_declare(&mut self) -> Option<Declare> {
        let start = self.peek_span();
        let (name, _) = self.expect_symbol()?;
        let type_ann = self.parse_type_expr()?;
        let end = self.peek_span();
        Some(Declare {
            name,
            type_ann,
            span: start.merge(end),
        })
    }

    // ── extern "C" ───────────────────────────────────────────────

    fn parse_extern_c(&mut self) -> Option<ExternC> {
        let start = self.peek_span();
        // Expect string "C"
        if let Some(Token::String(_)) = self.peek() {
            let (tok, _) = self.advance();
            if let Token::String(ref s) = tok {
                if s != "C" {
                    self.error(format!("expected \"C\", found {:?}", s), start);
                }
            }
        } else {
            self.error("expected \"C\" after extern".into(), start);
        }

        let mut declarations = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            self.expect(&Token::LParen)?;
            if !self.eat_symbol("defn") {
                let span = self.peek_span();
                self.error("expected 'defn' in extern block".into(), span);
                self.recover_to_close_paren();
                continue;
            }
            if let Some(defn) = self.parse_defn(false) {
                declarations.push(defn);
            }
            self.expect(&Token::RParen)?;
        }

        let end = self.peek_span();
        Some(ExternC {
            declarations,
            span: start.merge(end),
        })
    }

    // ── Expressions ───────────────────────────────────────────────

    fn try_parse_expr(&mut self) -> Option<ExprId> {
        if self.at_end() {
            return None;
        }
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> Option<ExprId> {
        let expr = self.parse_primary_expr()?;
        // Check for postfix ?
        let mut expr = expr;
        while self.check(&Token::Question) {
            let (_, qspan) = self.advance();
            let inner_span = self.module.exprs[expr].span;
            expr = self.alloc_expr(ExprKind::Try(expr), inner_span.merge(qspan));
        }
        Some(expr)
    }

    fn parse_primary_expr(&mut self) -> Option<ExprId> {
        let start = self.peek_span();
        match self.peek()? {
            Token::LParen => self.parse_list_expr(),
            Token::LBracket => self.parse_vector_lit(),
            Token::LBrace => self.parse_map_lit(),
            Token::Int(_) => {
                let (tok, span) = self.advance();
                if let Token::Int(n) = tok {
                    Some(self.alloc_expr(ExprKind::Lit(Literal::Int(n)), span))
                } else {
                    None
                }
            }
            Token::Float(_) => {
                let (tok, span) = self.advance();
                if let Token::Float(f) = tok {
                    Some(self.alloc_expr(ExprKind::Lit(Literal::Float(f)), span))
                } else {
                    None
                }
            }
            Token::String(_) => {
                let (tok, span) = self.advance();
                if let Token::String(s) = tok {
                    Some(self.alloc_expr(ExprKind::Lit(Literal::String(s)), span))
                } else {
                    None
                }
            }
            Token::True => {
                let (_, span) = self.advance();
                Some(self.alloc_expr(ExprKind::Lit(Literal::Bool(true)), span))
            }
            Token::False => {
                let (_, span) = self.advance();
                Some(self.alloc_expr(ExprKind::Lit(Literal::Bool(false)), span))
            }
            Token::Symbol(_) => {
                let (tok, span) = self.advance();
                if let Token::Symbol(s) = tok {
                    Some(self.alloc_expr(ExprKind::Var(s), span))
                } else {
                    None
                }
            }
            Token::FieldAccess(_) => {
                let (tok, span) = self.advance();
                if let Token::FieldAccess(s) = tok {
                    Some(self.alloc_expr(ExprKind::FieldAccess(s), span))
                } else {
                    None
                }
            }
            Token::Keyword(_) => {
                // Keywords in expression position become literals (used in map keys, named args)
                let (tok, span) = self.advance();
                if let Token::Keyword(s) = tok {
                    Some(self.alloc_expr(ExprKind::Lit(Literal::String(format!(":{}", s))), span))
                } else {
                    None
                }
            }
            _ => {
                self.error(format!("unexpected token {:?}", self.peek()), start);
                self.advance();
                None
            }
        }
    }

    fn parse_list_expr(&mut self) -> Option<ExprId> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;

        if self.check(&Token::RParen) {
            // Empty parens - error
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            self.error("empty parentheses".into(), start.merge(end));
            return None;
        }

        let result = if self.check_symbol("let") {
            self.advance();
            self.parse_let_body(start)
        } else if self.check_symbol("if") {
            self.advance();
            self.parse_if_body(start)
        } else if self.check_symbol("cond") {
            self.advance();
            self.parse_cond_body(start)
        } else if self.check_symbol("when") {
            self.advance();
            self.parse_when_body(start)
        } else if self.check_symbol("unless") {
            self.advance();
            self.parse_unless_body(start)
        } else if self.check_symbol("match") {
            self.advance();
            self.parse_match_body(start)
        } else if self.check_symbol("fn") {
            self.advance();
            self.parse_lambda_body(start)
        } else if self.check_symbol("do") {
            self.advance();
            self.parse_do_body(start)
        } else if self.check_symbol("set!") {
            self.advance();
            self.parse_set_bang_body(start)
        } else if self.check_symbol("ann") {
            self.advance();
            self.parse_ann_body(start)
        } else if self.check_symbol("unsafe") {
            self.advance();
            self.parse_unsafe_body(start)
        } else if self.check_symbol("with-arena") {
            self.advance();
            self.parse_with_arena_body(start)
        } else if self.check_symbol("swap!") {
            self.advance();
            self.parse_swap_bang_body(start)
        } else if self.check_symbol("with-tasks") {
            self.advance();
            self.parse_with_tasks_body(start)
        } else if self.check_symbol("spawn") {
            self.advance();
            self.parse_spawn_body(start)
        } else if self.check_symbol("target") {
            self.advance();
            self.parse_target_body(start)
        } else {
            self.parse_call_body(start)
        };

        let end = self.peek_span();
        self.expect(&Token::RParen)?;
        result.map(|kind| self.alloc_expr(kind, start.merge(end)))
    }

    fn parse_let_body(&mut self, _start: Span) -> Option<ExprKind> {
        self.expect(&Token::LParen)?;
        let mut bindings = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(b) = self.parse_let_binding() {
                bindings.push(b);
            } else {
                break;
            }
        }
        self.expect(&Token::RParen)?;

        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(expr) = self.try_parse_expr() {
                body.push(expr);
            } else {
                break;
            }
        }

        Some(ExprKind::Let { bindings, body })
    }

    fn parse_let_binding(&mut self) -> Option<LetBinding> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;

        if self.check(&Token::LParen) {
            // Annotated: ((name : type) value) or ((mut name : type) value)
            self.advance();
            let is_mut = self.eat_symbol("mut");
            let (name, name_span) = self.expect_symbol()?;
            let type_ann = if self.eat(&Token::Colon) {
                Some(self.parse_type_expr()?)
            } else {
                None
            };
            self.expect(&Token::RParen)?;
            let value = self.parse_expr()?;
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            Some(LetBinding {
                name,
                name_span,
                type_ann,
                value,
                is_mut,
                span: start.merge(end),
            })
        } else if self.check_symbol("mut") {
            // Mutable: (mut name value)
            self.advance();
            let (name, name_span) = self.expect_symbol()?;
            let value = self.parse_expr()?;
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            Some(LetBinding {
                name,
                name_span,
                type_ann: None,
                value,
                is_mut: true,
                span: start.merge(end),
            })
        } else {
            // Simple: (name value)
            let (name, name_span) = self.expect_symbol()?;
            let value = self.parse_expr()?;
            let end = self.peek_span();
            self.expect(&Token::RParen)?;
            Some(LetBinding {
                name,
                name_span,
                type_ann: None,
                value,
                is_mut: false,
                span: start.merge(end),
            })
        }
    }

    fn parse_if_body(&mut self, _start: Span) -> Option<ExprKind> {
        let condition = self.parse_expr()?;
        let then_branch = self.parse_expr()?;
        let else_branch = if !self.check(&Token::RParen) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        Some(ExprKind::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_cond_body(&mut self, _start: Span) -> Option<ExprKind> {
        let mut clauses = Vec::new();
        let mut else_clause = None;

        while !self.at_end() && !self.check(&Token::RParen) {
            self.expect(&Token::LParen)?;
            if self.check_symbol("else") {
                self.advance();
                else_clause = Some(self.parse_expr()?);
                self.expect(&Token::RParen)?;
                break;
            }
            let test = self.parse_expr()?;
            let body = self.parse_expr()?;
            self.expect(&Token::RParen)?;
            clauses.push((test, body));
        }

        Some(ExprKind::Cond {
            clauses,
            else_clause,
        })
    }

    fn parse_when_body(&mut self, _start: Span) -> Option<ExprKind> {
        let condition = self.parse_expr()?;
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::When { condition, body })
    }

    fn parse_unless_body(&mut self, _start: Span) -> Option<ExprKind> {
        let condition = self.parse_expr()?;
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::Unless { condition, body })
    }

    fn parse_match_body(&mut self, _start: Span) -> Option<ExprKind> {
        let scrutinee = self.parse_expr()?;
        let mut arms = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(arm) = self.parse_match_arm() {
                arms.push(arm);
            } else {
                break;
            }
        }
        Some(ExprKind::Match { scrutinee, arms })
    }

    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let start = self.peek_span();
        self.expect(&Token::LParen)?;
        let pattern = self.parse_pattern()?;
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        let end = self.peek_span();
        self.expect(&Token::RParen)?;
        Some(MatchArm {
            pattern,
            body,
            span: start.merge(end),
        })
    }

    fn parse_lambda_body(&mut self, _start: Span) -> Option<ExprKind> {
        let (params, _) = self.parse_params()?;
        let return_type = if self.eat(&Token::Colon) {
            Some(self.parse_type_expr()?)
        } else {
            None
        };
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::Lambda {
            params,
            return_type,
            body,
        })
    }

    fn parse_do_body(&mut self, _start: Span) -> Option<ExprKind> {
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::Do { body })
    }

    fn parse_set_bang_body(&mut self, _start: Span) -> Option<ExprKind> {
        let place = self.parse_expr()?;
        let value = self.parse_expr()?;
        Some(ExprKind::SetBang { place, value })
    }

    fn parse_ann_body(&mut self, _start: Span) -> Option<ExprKind> {
        let type_ann = self.parse_type_expr()?;
        let expr = self.parse_expr()?;
        Some(ExprKind::Ann { type_ann, expr })
    }

    fn parse_unsafe_body(&mut self, _start: Span) -> Option<ExprKind> {
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::Unsafe { body })
    }

    fn parse_with_arena_body(&mut self, _start: Span) -> Option<ExprKind> {
        let (name, _) = self.expect_symbol()?;
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::WithArena { name, body })
    }

    fn parse_swap_bang_body(&mut self, _start: Span) -> Option<ExprKind> {
        let atom = self.parse_expr()?;
        let func = self.parse_expr()?;
        Some(ExprKind::SwapBang { atom, func })
    }

    fn parse_with_tasks_body(&mut self, _start: Span) -> Option<ExprKind> {
        let mut body = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            body.push(self.parse_expr()?);
        }
        Some(ExprKind::WithTasks { body })
    }

    fn parse_spawn_body(&mut self, _start: Span) -> Option<ExprKind> {
        let inner = self.parse_expr()?;
        Some(ExprKind::Spawn(inner))
    }

    fn parse_target_body(&mut self, _start: Span) -> Option<ExprKind> {
        let mut branches = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            // Each branch: (:keyword expr)
            self.expect(&Token::LParen)?;
            let keyword = if let Some(Token::Keyword(_)) = self.peek() {
                let (tok, _) = self.advance();
                if let Token::Keyword(k) = tok {
                    k
                } else {
                    unreachable!()
                }
            } else {
                let span = self.peek_span();
                self.error("expected keyword (e.g. :native, :wasm) in target branch".into(), span);
                self.recover_to_close_paren();
                continue;
            };
            let expr = self.parse_expr()?;
            self.expect(&Token::RParen)?;
            branches.push((keyword, expr));
        }
        if branches.is_empty() {
            let span = self.peek_span();
            self.error("target requires at least one branch".into(), span);
            return None;
        }
        Some(ExprKind::Target { branches })
    }

    fn parse_call_body(&mut self, _start: Span) -> Option<ExprKind> {
        let func = self.parse_expr()?;
        let mut args = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            if let Some(Token::Keyword(_)) = self.peek() {
                // Named argument: :name value
                let (tok, span) = self.advance();
                if let Token::Keyword(name) = tok {
                    let value = self.parse_expr()?;
                    let val_span = self.module.exprs[value].span;
                    args.push(Arg {
                        name: Some(name),
                        value,
                        span: span.merge(val_span),
                    });
                }
            } else {
                let value = self.parse_expr()?;
                let val_span = self.module.exprs[value].span;
                args.push(Arg {
                    name: None,
                    value,
                    span: val_span,
                });
            }
        }
        Some(ExprKind::Call { func, args })
    }

    fn parse_vector_lit(&mut self) -> Option<ExprId> {
        let start = self.peek_span();
        self.expect(&Token::LBracket)?;
        let mut elems = Vec::new();
        while !self.at_end() && !self.check(&Token::RBracket) {
            elems.push(self.parse_expr()?);
        }
        let end = self.peek_span();
        self.expect(&Token::RBracket)?;
        Some(self.alloc_expr(ExprKind::VectorLit(elems), start.merge(end)))
    }

    fn parse_map_lit(&mut self) -> Option<ExprId> {
        let start = self.peek_span();
        self.expect(&Token::LBrace)?;
        let mut pairs = Vec::new();
        while !self.at_end() && !self.check(&Token::RBrace) {
            let key = self.parse_expr()?;
            let value = self.parse_expr()?;
            pairs.push((key, value));
        }
        let end = self.peek_span();
        self.expect(&Token::RBrace)?;
        Some(self.alloc_expr(ExprKind::MapLit(pairs), start.merge(end)))
    }

    // ── Patterns ──────────────────────────────────────────────────

    fn parse_pattern(&mut self) -> Option<PatternId> {
        let start = self.peek_span();
        match self.peek()? {
            Token::LParen => {
                // Constructor pattern: (Name args...)
                self.advance();
                let (name, _) = self.expect_symbol()?;
                let mut args = Vec::new();
                while !self.at_end() && !self.check(&Token::RParen) {
                    args.push(self.parse_pattern()?);
                }
                let end = self.peek_span();
                self.expect(&Token::RParen)?;
                Some(self.alloc_pattern(PatternKind::Constructor { name, args }, start.merge(end)))
            }
            Token::LBrace => {
                // Struct destructure: {:field1 :field2} or {:field1 bind1 :field2 bind2}
                // Keywords (:name) denote field names; a bare symbol after a keyword
                // is an explicit binding name. Without one, binding = field name.
                self.advance();
                let mut fields = Vec::new();
                while !self.at_end() && !self.check(&Token::RBrace) {
                    // Expect a keyword for the field name
                    let fspan = self.peek_span();
                    let field_name = if let Some(Token::Keyword(_)) = self.peek() {
                        let (tok, _) = self.advance();
                        if let Token::Keyword(name) = tok {
                            name
                        } else {
                            unreachable!()
                        }
                    } else {
                        self.error(
                            "expected keyword (:field-name) in struct destructure".into(),
                            fspan,
                        );
                        return None;
                    };
                    // Check for an explicit binding name (bare symbol)
                    let binding = if !self.check(&Token::RBrace)
                        && matches!(self.peek(), Some(Token::Symbol(_)))
                    {
                        let (b, _) = self.expect_symbol()?;
                        Some(b)
                    } else {
                        None
                    };
                    fields.push(FieldPattern {
                        field_name,
                        binding,
                        span: fspan,
                    });
                }
                let end = self.peek_span();
                self.expect(&Token::RBrace)?;
                Some(
                    self.alloc_pattern(PatternKind::StructDestructure { fields }, start.merge(end)),
                )
            }
            Token::Symbol(s) if s.as_str() == "_" => {
                let (_, span) = self.advance();
                Some(self.alloc_pattern(PatternKind::Wildcard, span))
            }
            Token::Symbol(s) => {
                let first_char = s.chars().next().unwrap_or('a');
                let (tok, span) = self.advance();
                if let Token::Symbol(s) = tok {
                    if first_char.is_uppercase() {
                        // Nullary constructor
                        Some(self.alloc_pattern(
                            PatternKind::Constructor {
                                name: s,
                                args: Vec::new(),
                            },
                            span,
                        ))
                    } else {
                        // Variable binding
                        Some(self.alloc_pattern(PatternKind::Var(s), span))
                    }
                } else {
                    None
                }
            }
            Token::Int(_) => {
                let (tok, span) = self.advance();
                if let Token::Int(n) = tok {
                    Some(self.alloc_pattern(PatternKind::Literal(Literal::Int(n)), span))
                } else {
                    None
                }
            }
            Token::Float(_) => {
                let (tok, span) = self.advance();
                if let Token::Float(f) = tok {
                    Some(self.alloc_pattern(PatternKind::Literal(Literal::Float(f)), span))
                } else {
                    None
                }
            }
            Token::String(_) => {
                let (tok, span) = self.advance();
                if let Token::String(s) = tok {
                    Some(self.alloc_pattern(PatternKind::Literal(Literal::String(s)), span))
                } else {
                    None
                }
            }
            Token::True => {
                let (_, span) = self.advance();
                Some(self.alloc_pattern(PatternKind::Literal(Literal::Bool(true)), span))
            }
            Token::False => {
                let (_, span) = self.advance();
                Some(self.alloc_pattern(PatternKind::Literal(Literal::Bool(false)), span))
            }
            _ => {
                self.error(format!("expected pattern, found {:?}", self.peek()), start);
                None
            }
        }
    }

    // ── Type expressions ──────────────────────────────────────────

    fn parse_type_expr(&mut self) -> Option<TypeExprId> {
        let start = self.peek_span();
        match self.peek()? {
            Token::Symbol(_) => {
                let (tok, span) = self.advance();
                if let Token::Symbol(s) = tok {
                    Some(self.alloc_type(TypeExprKind::Named(s), span))
                } else {
                    None
                }
            }
            Token::TypeVar(_) => {
                let (tok, span) = self.advance();
                if let Token::TypeVar(s) = tok {
                    Some(self.alloc_type(TypeExprKind::TypeVar(s), span))
                } else {
                    None
                }
            }
            Token::LParen => {
                self.advance();
                let result = if self.check_symbol("Fn") {
                    self.advance();
                    self.parse_fn_type(start)
                } else if self.check_symbol("=>") {
                    self.advance();
                    self.parse_constrained_type(start)
                } else {
                    // Applied type: (Constructor args...)
                    let constructor = self.parse_type_expr()?;
                    let mut args = Vec::new();
                    while !self.at_end() && !self.check(&Token::RParen) {
                        args.push(self.parse_type_expr()?);
                    }
                    if args.is_empty() {
                        // Just a parenthesized type, return inner
                        Some(constructor)
                    } else {
                        let end = self.peek_span();
                        Some(self.alloc_type(
                            TypeExprKind::Applied { constructor, args },
                            start.merge(end),
                        ))
                    }
                };
                self.expect(&Token::RParen)?;
                result
            }
            _ => {
                self.error(format!("expected type, found {:?}", self.peek()), start);
                None
            }
        }
    }

    fn parse_fn_type(&mut self, start: Span) -> Option<TypeExprId> {
        // After eating "Fn"
        self.expect(&Token::LBracket)?;
        let mut params = Vec::new();
        while !self.at_end() && !self.check(&Token::RBracket) {
            params.push(self.parse_type_expr()?);
        }
        self.expect(&Token::RBracket)?;
        let return_type = self.parse_type_expr()?;
        let end = self.peek_span();
        Some(self.alloc_type(
            TypeExprKind::Fn {
                params,
                return_type,
            },
            start.merge(end),
        ))
    }

    fn parse_constrained_type(&mut self, start: Span) -> Option<TypeExprId> {
        // After eating "=>"
        // Parse everything; last item is the inner type, rest are constraints
        let mut type_items = Vec::new();
        while !self.at_end() && !self.check(&Token::RParen) {
            type_items.push(self.parse_type_expr()?);
        }
        if type_items.is_empty() {
            self.error("expected type after =>".into(), start);
            return None;
        }
        let inner = type_items.pop().unwrap();
        let constraints: Vec<Constraint> = type_items
            .into_iter()
            .map(|id| {
                let ty = &self.module.type_exprs[id];
                let span = ty.span;
                match &ty.kind {
                    TypeExprKind::Applied { constructor, args } => {
                        let class_name = match &self.module.type_exprs[*constructor].kind {
                            TypeExprKind::Named(n) => n.clone(),
                            _ => SmolStr::new("<unknown>"),
                        };
                        Constraint {
                            class_name,
                            type_args: args.clone(),
                            span,
                        }
                    }
                    TypeExprKind::Named(n) => Constraint {
                        class_name: n.clone(),
                        type_args: Vec::new(),
                        span,
                    },
                    _ => Constraint {
                        class_name: SmolStr::new("<unknown>"),
                        type_args: Vec::new(),
                        span,
                    },
                }
            })
            .collect();

        let end = self.peek_span();
        Some(self.alloc_type(
            TypeExprKind::Constrained { constraints, inner },
            start.merge(end),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_print(source: &str) -> String {
        let (module, errors) = parse(source);
        if !errors.is_empty() {
            let mut result = String::from("ERRORS:\n");
            for e in &errors {
                result.push_str(&format!("  {}\n", e));
            }
            result.push('\n');
            result.push_str(&pretty_print(&module));
            result
        } else {
            pretty_print(&module)
        }
    }

    #[test]
    fn test_parse_simple_defn() {
        let result = parse_and_print("(defn add ((x : i32) (y : i32)) : i32\n  (+ x y))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_defn_no_types() {
        let result = parse_and_print("(defn greet (name) (println name))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_deftype() {
        let result = parse_and_print("(deftype (Option 'a)\n  (Some 'a)\n  None)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_defstruct() {
        let result = parse_and_print("(defstruct Vec2\n  (x : f32)\n  (y : f32))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_let() {
        let result =
            parse_and_print("(defn main () : Unit\n  (let ((x 5)\n        (y 10))\n    (+ x y)))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_if() {
        let result = parse_and_print("(defn abs ((x : i32)) : i32\n  (if (< x 0) (- 0 x) x))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_match() {
        let result = parse_and_print(
            "(defn show-option (opt)\n  (match opt\n    ((Some val) (println val))\n    (None (println \"none\"))))",
        );
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_lambda() {
        let result = parse_and_print(
            "(defn main () : Unit\n  (let ((f (fn (x) (+ x 1))))\n    (println (f 5))))",
        );
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_vector_map() {
        let result = parse_and_print(
            "(defn main () : Unit\n  (let ((v [1 2 3])\n        (m {:name \"Alice\" :age 30}))\n    (println v)))",
        );
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_threading_as_call() {
        // After Phase 7, -> is desugared by the macro expander before parsing.
        // If it reaches the parser, it's treated as a regular call.
        let result = parse_and_print("(defn main () : Unit\n  (-> enemy .pos .x))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_defclass() {
        let result = parse_and_print("(defclass (Show 'a)\n  (show : (Fn ['a] String)))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_instance() {
        let result = parse_and_print("(instance (Show i32)\n  (defn show (x) (str x)))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_import() {
        let result = parse_and_print("(import math.vec2 (add sub))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_pub() {
        let result = parse_and_print("(pub defn add ((x : i32) (y : i32)) : i32\n  (+ x y))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_mut_let() {
        let result =
            parse_and_print("(defn main () : Unit\n  (let ((mut x 5))\n    (set! x 10)\n    x))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_cond() {
        let result = parse_and_print(
            "(defn classify ((x : i32)) : String\n  (cond\n    ((< x 0) \"negative\")\n    ((= x 0) \"zero\")\n    (else \"positive\")))",
        );
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_try_operator() {
        let result = parse_and_print(
            "(defn load ((path : String)) : (Result Data Error)\n  (let ((data (read-file path)?))\n    (Ok data)))",
        );
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_named_args() {
        let result =
            parse_and_print("(defn main () : Unit\n  (spawn-enemy :pos origin :health 50))");
        insta::assert_snapshot!(result);
    }

    // ── Error recovery tests ─────────────────────────────────────

    #[test]
    fn test_error_empty_input() {
        let result = parse_and_print("");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_unclosed_paren() {
        let result = parse_and_print("(defn foo (x) (+ x 1)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_unexpected_close_paren() {
        let result = parse_and_print(")");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_bare_token() {
        let result = parse_and_print("hello");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_incomplete_defn() {
        let result = parse_and_print("(defn)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_defn_no_params() {
        let result = parse_and_print("(defn foo)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_empty_parens_in_expr() {
        let result = parse_and_print("(defn main () ())");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_bad_param_list() {
        let result = parse_and_print("(defn foo (42) x)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_bad_variant_in_deftype() {
        let result = parse_and_print("(deftype Color 42)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_unclosed_nested() {
        let result = parse_and_print("(if (= 1 (+ 2");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_multiple_errors() {
        let result = parse_and_print("(defn) (defn)");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_unexpected_token_in_expr() {
        let result = parse_and_print("(defn main () (+ 1 ] 2))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_error_invalid_character() {
        let result = parse_and_print("(defn main () (+ 1 \\2))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_with_arena() {
        let result = parse_and_print("(defn main () (with-arena foo (+ 1 2)))");
        insta::assert_snapshot!(result);
    }

    // ── Property-based tests ────────────────────────────────────

    mod proptests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            #[test]
            fn parse_never_panics_on_ascii(s in "\\PC{0,200}") {
                let _ = parse(&s);
            }

            #[test]
            fn parse_never_panics_on_bytes(bytes in proptest::collection::vec(any::<u8>(), 0..200)) {
                if let Ok(s) = std::str::from_utf8(&bytes) {
                    let _ = parse(s);
                }
            }

            #[test]
            fn parse_never_panics_on_lispy_input(
                s in proptest::string::string_regex(r"[\(\)\[\]\{\} a-z0-9\+\-\*/:;'\n ]{0,150}")
                    .unwrap()
            ) {
                let _ = parse(&s);
            }
        }
    }

    // ── Atom / concurrency parsing ──────────────────────────────────

    #[test]
    fn test_parse_swap_bang() {
        let result = parse_and_print("(defn main () : Unit (swap! a inc))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_with_tasks() {
        let result = parse_and_print("(defn main () : Unit (with-tasks (spawn (println 1)) (spawn (println 2))))");
        insta::assert_snapshot!(result);
    }

    #[test]
    fn test_parse_spawn() {
        let result = parse_and_print("(defn main () : Unit (with-tasks (spawn (+ 1 2))))");
        insta::assert_snapshot!(result);
    }
}
