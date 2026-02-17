use logos::Logos;
use smol_str::SmolStr;

/// Source span as byte offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

fn parse_string(lex: &mut logos::Lexer<Token>) -> Option<String> {
    let slice = lex.slice();
    let inner = &slice[1..slice.len() - 1];
    let mut result = String::new();
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next()? {
                'n' => result.push('\n'),
                't' => result.push('\t'),
                'r' => result.push('\r'),
                '\\' => result.push('\\'),
                '"' => result.push('"'),
                '0' => result.push('\0'),
                other => {
                    result.push('\\');
                    result.push(other);
                }
            }
        } else {
            result.push(c);
        }
    }
    Some(result)
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\r\n]+")]
#[logos(skip r";[^\n]*")]
pub enum Token {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,
    #[token("`")]
    Backtick,
    #[token(",@", priority = 2)]
    CommaSplice,
    #[token(",")]
    Comma,

    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 3, callback = |lex| lex.slice().parse::<f64>().ok())]
    Float(f64),

    #[regex(r"-?[0-9]+", priority = 2, callback = |lex| lex.slice().parse::<i64>().ok())]
    Int(i64),

    #[regex(r#""([^"\\]|\\.)*""#, callback = parse_string)]
    String(String),

    #[token("true")]
    True,
    #[token("false")]
    False,

    /// Keyword literal: `:name`, `:health`
    #[regex(r":[a-zA-Z_][a-zA-Z0-9_\-!?]*", callback = |lex| SmolStr::new(&lex.slice()[1..]))]
    Keyword(SmolStr),

    /// Type variable: `'a`, `'elem`
    #[regex(r"'[a-zA-Z_][a-zA-Z0-9_\-!?]*", callback = |lex| SmolStr::new(&lex.slice()[1..]))]
    TypeVar(SmolStr),

    /// Field accessor: `.field`, `.pos`
    #[regex(r"\.[a-zA-Z_][a-zA-Z0-9_\-!?]*", callback = |lex| SmolStr::new(&lex.slice()[1..]))]
    FieldAccess(SmolStr),

    /// Symbol (identifiers and operators): `foo`, `+`, `set!`, `spawn-enemy`, `module.name`
    #[regex(r"[a-zA-Z_+\-*/<>=!&|^~][a-zA-Z0-9_+\-*/<>=!&|^~.?]*", priority = 1, callback = |lex| SmolStr::new(lex.slice()))]
    Symbol(SmolStr),
}

/// Lex source code into a list of (token, span) pairs.
pub fn lex(source: &str) -> (Vec<(Token, Span)>, Vec<Span>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut lexer = Token::lexer(source);

    while let Some(result) = lexer.next() {
        let range = lexer.span();
        let span = Span::new(range.start as u32, range.end as u32);
        match result {
            Ok(token) => tokens.push((token, span)),
            Err(_) => errors.push(span),
        }
    }

    (tokens, errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_tokens(source: &str) -> Vec<Token> {
        let (tokens, errors) = lex(source);
        assert!(errors.is_empty(), "unexpected lex errors: {:?}", errors);
        tokens.into_iter().map(|(t, _)| t).collect()
    }

    #[test]
    fn test_delimiters() {
        assert_eq!(
            lex_tokens("( ) [ ] { }"),
            vec![
                Token::LParen,
                Token::RParen,
                Token::LBracket,
                Token::RBracket,
                Token::LBrace,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_integers() {
        assert_eq!(lex_tokens("42"), vec![Token::Int(42)]);
        assert_eq!(lex_tokens("0"), vec![Token::Int(0)]);
        assert_eq!(lex_tokens("-7"), vec![Token::Int(-7)]);
    }

    #[test]
    fn test_floats() {
        assert_eq!(lex_tokens("3.25"), vec![Token::Float(3.25)]);
        assert_eq!(lex_tokens("-0.5"), vec![Token::Float(-0.5)]);
        assert_eq!(lex_tokens("1.0e10"), vec![Token::Float(1.0e10)]);
    }

    #[test]
    fn test_strings() {
        assert_eq!(
            lex_tokens(r#""hello""#),
            vec![Token::String("hello".into())]
        );
        assert_eq!(
            lex_tokens(r#""hello\nworld""#),
            vec![Token::String("hello\nworld".into())]
        );
        assert_eq!(lex_tokens(r#""""#), vec![Token::String("".into())]);
    }

    #[test]
    fn test_booleans() {
        assert_eq!(lex_tokens("true false"), vec![Token::True, Token::False]);
    }

    #[test]
    fn test_keywords() {
        assert_eq!(
            lex_tokens(":name :health"),
            vec![
                Token::Keyword("name".into()),
                Token::Keyword("health".into()),
            ]
        );
    }

    #[test]
    fn test_type_vars() {
        assert_eq!(
            lex_tokens("'a 'elem"),
            vec![Token::TypeVar("a".into()), Token::TypeVar("elem".into()),]
        );
    }

    #[test]
    fn test_field_access() {
        assert_eq!(
            lex_tokens(".pos .health"),
            vec![
                Token::FieldAccess("pos".into()),
                Token::FieldAccess("health".into()),
            ]
        );
    }

    #[test]
    fn test_symbols() {
        assert_eq!(
            lex_tokens("foo bar spawn-enemy"),
            vec![
                Token::Symbol("foo".into()),
                Token::Symbol("bar".into()),
                Token::Symbol("spawn-enemy".into()),
            ]
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            lex_tokens("+ - * / < > = <= >= => -> ->>"),
            vec![
                Token::Symbol("+".into()),
                Token::Symbol("-".into()),
                Token::Symbol("*".into()),
                Token::Symbol("/".into()),
                Token::Symbol("<".into()),
                Token::Symbol(">".into()),
                Token::Symbol("=".into()),
                Token::Symbol("<=".into()),
                Token::Symbol(">=".into()),
                Token::Symbol("=>".into()),
                Token::Symbol("->".into()),
                Token::Symbol("->>".into()),
            ]
        );
    }

    #[test]
    fn test_special_symbols() {
        assert_eq!(
            lex_tokens("set! alive? defn let if"),
            vec![
                Token::Symbol("set!".into()),
                Token::Symbol("alive?".into()),
                Token::Symbol("defn".into()),
                Token::Symbol("let".into()),
                Token::Symbol("if".into()),
            ]
        );
    }

    #[test]
    fn test_colon_standalone() {
        assert_eq!(
            lex_tokens("x : i32"),
            vec![
                Token::Symbol("x".into()),
                Token::Colon,
                Token::Symbol("i32".into()),
            ]
        );
    }

    #[test]
    fn test_question_mark() {
        assert_eq!(
            lex_tokens("(foo)?"),
            vec![
                Token::LParen,
                Token::Symbol("foo".into()),
                Token::RParen,
                Token::Question,
            ]
        );
    }

    #[test]
    fn test_comments_skipped() {
        assert_eq!(lex_tokens("; this is a comment\n42"), vec![Token::Int(42)]);
    }

    #[test]
    fn test_simple_expression() {
        assert_eq!(
            lex_tokens("(+ 1 2)"),
            vec![
                Token::LParen,
                Token::Symbol("+".into()),
                Token::Int(1),
                Token::Int(2),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_defn() {
        let tokens = lex_tokens("(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
        assert_eq!(tokens[0], Token::LParen);
        assert_eq!(tokens[1], Token::Symbol("defn".into()));
        assert_eq!(tokens[2], Token::Symbol("add".into()));
        assert_eq!(tokens[tokens.len() - 1], Token::RParen);
    }

    #[test]
    fn test_collection_literals() {
        assert_eq!(
            lex_tokens("[1 2 3]"),
            vec![
                Token::LBracket,
                Token::Int(1),
                Token::Int(2),
                Token::Int(3),
                Token::RBracket,
            ]
        );
        assert_eq!(
            lex_tokens("{:name \"Alice\"}"),
            vec![
                Token::LBrace,
                Token::Keyword("name".into()),
                Token::String("Alice".into()),
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_quasiquote_tokens() {
        assert_eq!(
            lex_tokens("` , ,@"),
            vec![Token::Backtick, Token::Comma, Token::CommaSplice]
        );
    }

    #[test]
    fn test_comma_splice_priority() {
        // ,@ should tokenize as a single CommaSplice, not Comma + Symbol(@)
        assert_eq!(
            lex_tokens(",@foo"),
            vec![Token::CommaSplice, Token::Symbol("foo".into())]
        );
        assert_eq!(
            lex_tokens(",foo"),
            vec![Token::Comma, Token::Symbol("foo".into())]
        );
    }

    #[test]
    fn test_quasiquote_in_context() {
        assert_eq!(
            lex_tokens("`(list ,a ,@rest)"),
            vec![
                Token::Backtick,
                Token::LParen,
                Token::Symbol("list".into()),
                Token::Comma,
                Token::Symbol("a".into()),
                Token::CommaSplice,
                Token::Symbol("rest".into()),
                Token::RParen,
            ]
        );
    }

    #[test]
    fn test_spans() {
        let (tokens, _) = lex("(+ 1 2)");
        assert_eq!(tokens[0], (Token::LParen, Span::new(0, 1)));
        assert_eq!(tokens[1], (Token::Symbol("+".into()), Span::new(1, 2)));
        assert_eq!(tokens[2], (Token::Int(1), Span::new(3, 4)));
        assert_eq!(tokens[3], (Token::Int(2), Span::new(5, 6)));
        assert_eq!(tokens[4], (Token::RParen, Span::new(6, 7)));
    }
}
