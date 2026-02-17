mod builtins;
mod expander;
mod hygiene;
mod sexpr;

use weir_lexer::Span;

#[derive(Debug, Clone)]
pub struct MacroError {
    pub message: String,
    pub span: Span,
}

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}:{}] {}",
            self.span.start, self.span.end, self.message
        )
    }
}

pub struct ExpandResult {
    pub source: String,
    pub errors: Vec<MacroError>,
}

/// Expand macros in source code.
///
/// Pipeline: lex → read(SExpr) → expand → flatten(tokens) → reconstruct source.
pub fn expand(source: &str) -> ExpandResult {
    let (tokens, lex_errors) = weir_lexer::lex(source);

    let mut errors: Vec<MacroError> = lex_errors
        .into_iter()
        .map(|span| MacroError {
            message: "unexpected character".into(),
            span,
        })
        .collect();

    let (sexprs, read_errors) = sexpr::read(&tokens);
    errors.extend(read_errors);

    if !errors.is_empty() {
        return ExpandResult {
            source: source.to_string(),
            errors,
        };
    }

    let (expanded, expand_errors) = expander::expand_toplevel(sexprs);
    errors.extend(expand_errors);

    if !errors.is_empty() {
        return ExpandResult {
            source: source.to_string(),
            errors,
        };
    }

    let flat_tokens = sexpr::flatten(&expanded);
    let reconstructed = reconstruct(&flat_tokens, source);

    ExpandResult {
        source: reconstructed,
        errors,
    }
}

/// Reconstruct source text from tokens, preserving spacing where possible.
fn reconstruct(tokens: &[(weir_lexer::Token, Span)], _original: &str) -> String {
    use weir_lexer::Token;

    let mut buf = String::new();
    for (i, (token, _span)) in tokens.iter().enumerate() {
        if i > 0 {
            // Add space between tokens, except after open delimiters and before close delimiters
            let prev = &tokens[i - 1].0;
            let needs_space = !matches!(prev, Token::LParen | Token::LBracket | Token::LBrace)
                && !matches!(token, Token::RParen | Token::RBracket | Token::RBrace);
            if needs_space {
                buf.push(' ');
            }
        }
        match token {
            Token::LParen => buf.push('('),
            Token::RParen => buf.push(')'),
            Token::LBracket => buf.push('['),
            Token::RBracket => buf.push(']'),
            Token::LBrace => buf.push('{'),
            Token::RBrace => buf.push('}'),
            Token::Question => buf.push('?'),
            Token::Colon => buf.push(':'),
            Token::Backtick => buf.push('`'),
            Token::Comma => buf.push(','),
            Token::CommaSplice => buf.push_str(",@"),
            Token::Float(f) => {
                if f.fract() == 0.0 && !f.is_infinite() && !f.is_nan() {
                    buf.push_str(&format!("{:.1}", f));
                } else {
                    buf.push_str(&format!("{}", f));
                }
            }
            Token::Int(n) => buf.push_str(&n.to_string()),
            Token::String(s) => {
                buf.push('"');
                for c in s.chars() {
                    match c {
                        '\n' => buf.push_str("\\n"),
                        '\t' => buf.push_str("\\t"),
                        '\r' => buf.push_str("\\r"),
                        '\\' => buf.push_str("\\\\"),
                        '"' => buf.push_str("\\\""),
                        '\0' => buf.push_str("\\0"),
                        c => buf.push(c),
                    }
                }
                buf.push('"');
            }
            Token::True => buf.push_str("true"),
            Token::False => buf.push_str("false"),
            Token::Keyword(k) => {
                buf.push(':');
                buf.push_str(k);
            }
            Token::TypeVar(t) => {
                buf.push('\'');
                buf.push_str(t);
            }
            Token::FieldAccess(f) => {
                buf.push('.');
                buf.push_str(f);
            }
            Token::Symbol(s) => buf.push_str(s),
            Token::Comment(c) => buf.push_str(c),
        }
    }
    buf
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expand_passthrough() {
        let source = "(defn main () (println 42))";
        let result = expand(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        // Should parse identically
        let (m1, e1) = weir_parser::parse(source);
        let (m2, e2) = weir_parser::parse(&result.source);
        assert!(e1.is_empty());
        assert!(e2.is_empty());
        assert_eq!(weir_ast::pretty_print(&m1), weir_ast::pretty_print(&m2));
    }
}
