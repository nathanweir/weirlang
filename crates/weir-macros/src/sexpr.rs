use crate::MacroError;
use weir_lexer::{Span, Token};

/// S-expression: structural intermediate representation between tokens and AST.
#[derive(Debug, Clone)]
pub enum SExpr {
    /// A single token (identifier, literal, keyword, etc.)
    Atom(Token, Span),
    /// Parenthesized list: `(...)`.
    List(Vec<SExpr>, Span),
    /// Bracketed vector: `[...]`.
    Vector(Vec<SExpr>, Span),
    /// Braced map: `{...}`.
    Map(Vec<SExpr>, Span),
    /// Quasiquote: `` `expr ``
    Quasiquote(Box<SExpr>, Span),
    /// Unquote: `,expr`
    Unquote(Box<SExpr>, Span),
    /// Splice-unquote: `,@expr`
    Splice(Box<SExpr>, Span),
}

impl SExpr {
    pub fn span(&self) -> Span {
        match self {
            SExpr::Atom(_, s)
            | SExpr::List(_, s)
            | SExpr::Vector(_, s)
            | SExpr::Map(_, s)
            | SExpr::Quasiquote(_, s)
            | SExpr::Unquote(_, s)
            | SExpr::Splice(_, s) => *s,
        }
    }

    /// Returns true if this is a Symbol atom with the given name.
    pub fn is_symbol(&self, name: &str) -> bool {
        matches!(self, SExpr::Atom(Token::Symbol(s), _) if s.as_str() == name)
    }

    /// If this is a Symbol atom, return its name.
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            SExpr::Atom(Token::Symbol(s), _) => Some(s.as_str()),
            _ => None,
        }
    }
}

/// Parse a token stream into a list of S-expressions.
pub fn read(tokens: &[(Token, Span)]) -> (Vec<SExpr>, Vec<MacroError>) {
    let mut reader = Reader {
        tokens,
        pos: 0,
        errors: Vec::new(),
    };
    let mut result = Vec::new();
    while !reader.at_end() {
        if let Some(sexpr) = reader.read_sexpr() {
            result.push(sexpr);
        }
    }
    (result, reader.errors)
}

/// Convert S-expressions back into a flat token stream.
pub fn flatten(sexprs: &[SExpr]) -> Vec<(Token, Span)> {
    let mut tokens = Vec::new();
    for sexpr in sexprs {
        flatten_one(sexpr, &mut tokens);
    }
    tokens
}

fn flatten_one(sexpr: &SExpr, tokens: &mut Vec<(Token, Span)>) {
    match sexpr {
        SExpr::Atom(token, span) => {
            tokens.push((token.clone(), *span));
        }
        SExpr::List(children, span) => {
            tokens.push((Token::LParen, Span::new(span.start, span.start + 1)));
            for child in children {
                flatten_one(child, tokens);
            }
            tokens.push((
                Token::RParen,
                Span::new(span.end.saturating_sub(1), span.end),
            ));
        }
        SExpr::Vector(children, span) => {
            tokens.push((Token::LBracket, Span::new(span.start, span.start + 1)));
            for child in children {
                flatten_one(child, tokens);
            }
            tokens.push((
                Token::RBracket,
                Span::new(span.end.saturating_sub(1), span.end),
            ));
        }
        SExpr::Map(children, span) => {
            tokens.push((Token::LBrace, Span::new(span.start, span.start + 1)));
            for child in children {
                flatten_one(child, tokens);
            }
            tokens.push((
                Token::RBrace,
                Span::new(span.end.saturating_sub(1), span.end),
            ));
        }
        SExpr::Quasiquote(_, span) | SExpr::Unquote(_, span) | SExpr::Splice(_, span) => {
            // These should have been eliminated during expansion.
            // If they remain, it's a bug — just skip them and emit a synthetic error token.
            // In practice, the expander should handle them.
            tokens.push((
                Token::Symbol(smol_str::SmolStr::new("<unexpanded-quasiquote>")),
                *span,
            ));
        }
    }
}

struct Reader<'a> {
    tokens: &'a [(Token, Span)],
    pos: usize,
    errors: Vec<MacroError>,
}

impl<'a> Reader<'a> {
    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn advance(&mut self) -> (Token, Span) {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    fn read_sexpr(&mut self) -> Option<SExpr> {
        if self.at_end() {
            return None;
        }

        match self.peek()? {
            Token::LParen => {
                let (_, start_span) = self.advance();
                let mut children = Vec::new();
                while !self.at_end() && !matches!(self.peek(), Some(Token::RParen)) {
                    if let Some(child) = self.read_sexpr() {
                        children.push(child);
                    } else {
                        break;
                    }
                }
                if self.at_end() || !matches!(self.peek(), Some(Token::RParen)) {
                    self.errors.push(MacroError {
                        message: "unclosed '('".into(),
                        span: start_span,
                    });
                    return None;
                }
                let (_, end_span) = self.advance();
                Some(SExpr::List(children, start_span.merge(end_span)))
            }

            Token::LBracket => {
                let (_, start_span) = self.advance();
                let mut children = Vec::new();
                while !self.at_end() && !matches!(self.peek(), Some(Token::RBracket)) {
                    if let Some(child) = self.read_sexpr() {
                        children.push(child);
                    } else {
                        break;
                    }
                }
                if self.at_end() || !matches!(self.peek(), Some(Token::RBracket)) {
                    self.errors.push(MacroError {
                        message: "unclosed '['".into(),
                        span: start_span,
                    });
                    return None;
                }
                let (_, end_span) = self.advance();
                Some(SExpr::Vector(children, start_span.merge(end_span)))
            }

            Token::LBrace => {
                let (_, start_span) = self.advance();
                let mut children = Vec::new();
                while !self.at_end() && !matches!(self.peek(), Some(Token::RBrace)) {
                    if let Some(child) = self.read_sexpr() {
                        children.push(child);
                    } else {
                        break;
                    }
                }
                if self.at_end() || !matches!(self.peek(), Some(Token::RBrace)) {
                    self.errors.push(MacroError {
                        message: "unclosed '{'".into(),
                        span: start_span,
                    });
                    return None;
                }
                let (_, end_span) = self.advance();
                Some(SExpr::Map(children, start_span.merge(end_span)))
            }

            Token::RParen | Token::RBracket | Token::RBrace => {
                let (_, span) = self.advance();
                self.errors.push(MacroError {
                    message: "unexpected closing delimiter".into(),
                    span,
                });
                None
            }

            Token::Backtick => {
                let (_, start_span) = self.advance();
                let inner = self.read_sexpr()?;
                let end_span = inner.span();
                Some(SExpr::Quasiquote(
                    Box::new(inner),
                    start_span.merge(end_span),
                ))
            }

            Token::Comma => {
                let (_, start_span) = self.advance();
                let inner = self.read_sexpr()?;
                let end_span = inner.span();
                Some(SExpr::Unquote(Box::new(inner), start_span.merge(end_span)))
            }

            Token::CommaSplice => {
                let (_, start_span) = self.advance();
                let inner = self.read_sexpr()?;
                let end_span = inner.span();
                Some(SExpr::Splice(Box::new(inner), start_span.merge(end_span)))
            }

            // Comments should have been filtered by lex(), but skip defensively
            Token::Comment(_) => {
                self.advance();
                self.read_sexpr()
            }

            // All other tokens are atoms
            _ => {
                let (token, span) = self.advance();
                Some(SExpr::Atom(token, span))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_source(source: &str) -> Vec<SExpr> {
        let (tokens, lex_errors) = weir_lexer::lex(source);
        assert!(lex_errors.is_empty(), "lex errors: {:?}", lex_errors);
        let (sexprs, read_errors) = read(&tokens);
        assert!(read_errors.is_empty(), "read errors: {:?}", read_errors);
        sexprs
    }

    fn roundtrip_tokens(source: &str) -> Vec<Token> {
        let sexprs = read_source(source);
        let flat = flatten(&sexprs);
        flat.into_iter().map(|(t, _)| t).collect()
    }

    fn original_tokens(source: &str) -> Vec<Token> {
        let (tokens, _) = weir_lexer::lex(source);
        tokens.into_iter().map(|(t, _)| t).collect()
    }

    #[test]
    fn roundtrip_simple_list() {
        let source = "(+ 1 2)";
        assert_eq!(roundtrip_tokens(source), original_tokens(source));
    }

    #[test]
    fn roundtrip_nested() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y))";
        assert_eq!(roundtrip_tokens(source), original_tokens(source));
    }

    #[test]
    fn roundtrip_vector_and_map() {
        let source = "(let ((v [1 2 3]) (m {:name \"Alice\"})) v)";
        assert_eq!(roundtrip_tokens(source), original_tokens(source));
    }

    #[test]
    fn roundtrip_threading() {
        let source = "(-> x f (g a))";
        assert_eq!(roundtrip_tokens(source), original_tokens(source));
    }

    #[test]
    fn roundtrip_deftype() {
        let source = "(deftype (Option 'a) (Some 'a) None)";
        assert_eq!(roundtrip_tokens(source), original_tokens(source));
    }

    #[test]
    fn read_quasiquote() {
        let sexprs = read_source("`(list ,a ,@rest)");
        assert_eq!(sexprs.len(), 1);
        assert!(
            matches!(&sexprs[0], SExpr::Quasiquote(inner, _) if matches!(inner.as_ref(), SExpr::List(children, _) if children.len() == 3))
        );
    }

    #[test]
    fn read_atoms() {
        let sexprs = read_source("42 2.72 \"hello\" true false :key 'a .field foo");
        assert_eq!(sexprs.len(), 9);
        assert!(matches!(&sexprs[0], SExpr::Atom(Token::Int(42), _)));
        assert!(matches!(&sexprs[1], SExpr::Atom(Token::Float(f), _) if *f == 2.72));
    }
}
