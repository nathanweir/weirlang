use weir_lexer::{Span, Token};

// ---------------------------------------------------------------------------
// Form IR — structured representation of the token stream
// ---------------------------------------------------------------------------

#[derive(Debug)]
enum Form {
    Atom(String),
    Comment {
        text: String,
        trailing: bool,
    },
    List {
        open: &'static str,
        close: &'static str,
        children: Vec<Form>,
    },
    BlankLine,
}

/// Classify the head symbol for indentation.
#[derive(Debug, Clone, Copy, PartialEq)]
enum IndentStyle {
    DefnLike,
    LetLike,
    BodyIndent,
    Call,
}

fn classify_head(name: &str) -> IndentStyle {
    match name {
        "defn" | "deftype" | "defstruct" | "defclass" | "instance" | "defmacro" | "declare"
        | "import" | "extern" => IndentStyle::DefnLike,
        "let" => IndentStyle::LetLike,
        "if" | "cond" | "when" | "unless" | "match" | "do" | "fn" | "unsafe" | "set!" => {
            IndentStyle::BodyIndent
        }
        _ => IndentStyle::Call,
    }
}

// ---------------------------------------------------------------------------
// Token stream → Form tree
// ---------------------------------------------------------------------------

struct FormReader<'a> {
    tokens: &'a [(Token, Span)],
    source: &'a str,
    pos: usize,
}

impl<'a> FormReader<'a> {
    fn new(tokens: &'a [(Token, Span)], source: &'a str) -> Self {
        Self {
            tokens,
            source,
            pos: 0,
        }
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn advance(&mut self) -> &(Token, Span) {
        let tok = &self.tokens[self.pos];
        self.pos += 1;
        tok
    }

    /// Determine the line number (0-based) for a byte offset.
    fn line_of(&self, offset: u32) -> usize {
        self.source[..offset as usize]
            .bytes()
            .filter(|&b| b == b'\n')
            .count()
    }

    /// Check if there's a blank line (2+ newlines gap) between two byte offsets.
    fn has_blank_line_between(&self, prev_end: u32, next_start: u32) -> bool {
        let gap = &self.source[prev_end as usize..next_start as usize];
        gap.bytes().filter(|&b| b == b'\n').count() >= 2
    }

    /// Determine if a comment is trailing (there was a non-comment token earlier on the same line).
    fn is_trailing_comment(&self, comment_span: &Span) -> bool {
        let comment_line = self.line_of(comment_span.start);
        // Look backward for a non-comment token on the same line
        for i in (0..self.pos.saturating_sub(1)).rev() {
            let (tok, span) = &self.tokens[i];
            let tok_line = self.line_of(span.start);
            if tok_line < comment_line {
                break;
            }
            if tok_line == comment_line && !matches!(tok, Token::Comment(_)) {
                return true;
            }
        }
        false
    }

    fn read_all(&mut self) -> Vec<Form> {
        let mut forms = Vec::new();
        let mut prev_end: Option<u32> = None;

        while !self.at_end() {
            // Check for blank line between previous form and this token
            if let Some(pe) = prev_end {
                let next_start = self.tokens[self.pos].1.start;
                if self.has_blank_line_between(pe, next_start) {
                    forms.push(Form::BlankLine);
                }
            }

            if let Some(form) = self.read_form() {
                prev_end = Some(self.form_end_offset(&form));
                forms.push(form);
            }
        }

        forms
    }

    fn form_end_offset(&self, _form: &Form) -> u32 {
        // Use the span of the last consumed token
        if self.pos > 0 {
            self.tokens[self.pos - 1].1.end
        } else {
            0
        }
    }

    fn read_form(&mut self) -> Option<Form> {
        if self.at_end() {
            return None;
        }

        match self.peek()? {
            Token::Comment(_) => {
                let (tok, span) = self.advance().clone();
                if let Token::Comment(text) = tok {
                    let trailing = self.is_trailing_comment(&span);
                    Some(Form::Comment {
                        text: text.to_string(),
                        trailing,
                    })
                } else {
                    unreachable!()
                }
            }
            Token::LParen => self.read_delimited("(", ")", |t| matches!(t, Token::RParen)),
            Token::LBracket => self.read_delimited("[", "]", |t| matches!(t, Token::RBracket)),
            Token::LBrace => self.read_delimited("{", "}", |t| matches!(t, Token::RBrace)),
            Token::RParen | Token::RBracket | Token::RBrace => {
                // Unexpected close — skip
                self.advance();
                self.read_form()
            }
            _ => {
                let (tok, _) = self.advance().clone();
                Some(Form::Atom(token_to_string(&tok)))
            }
        }
    }

    fn read_delimited(
        &mut self,
        open: &'static str,
        close: &'static str,
        is_close: impl Fn(&Token) -> bool,
    ) -> Option<Form> {
        self.advance(); // consume opening delimiter
        let mut children = Vec::new();
        let mut prev_end: Option<u32> = None;

        while !self.at_end() {
            if is_close(self.peek().unwrap()) {
                self.advance(); // consume closing delimiter
                return Some(Form::List {
                    open,
                    close,
                    children,
                });
            }

            // Check for blank line between children
            if let Some(pe) = prev_end {
                let next_start = self.tokens[self.pos].1.start;
                if self.has_blank_line_between(pe, next_start) {
                    children.push(Form::BlankLine);
                }
            }

            if let Some(form) = self.read_form() {
                prev_end = Some(if self.pos > 0 {
                    self.tokens[self.pos - 1].1.end
                } else {
                    0
                });
                children.push(form);
            }
        }

        // Unclosed — emit what we have
        Some(Form::List {
            open,
            close,
            children,
        })
    }
}

fn token_to_string(tok: &Token) -> String {
    match tok {
        Token::LParen => "(".into(),
        Token::RParen => ")".into(),
        Token::LBracket => "[".into(),
        Token::RBracket => "]".into(),
        Token::LBrace => "{".into(),
        Token::RBrace => "}".into(),
        Token::Question => "?".into(),
        Token::Colon => ":".into(),
        Token::Backtick => "`".into(),
        Token::Comma => ",".into(),
        Token::CommaSplice => ",@".into(),
        Token::Float(f) => {
            if f.fract() == 0.0 && !f.is_infinite() && !f.is_nan() {
                format!("{:.1}", f)
            } else {
                format!("{}", f)
            }
        }
        Token::Int(n) => n.to_string(),
        Token::String(s) => {
            let mut buf = String::from('"');
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
            buf
        }
        Token::True => "true".into(),
        Token::False => "false".into(),
        Token::Keyword(k) => format!(":{}", k),
        Token::TypeVar(t) => format!("'{}", t),
        Token::FieldAccess(f) => format!(".{}", f),
        Token::Symbol(s) => s.to_string(),
        Token::Comment(c) => c.to_string(),
    }
}

// ---------------------------------------------------------------------------
// Doc IR — Wadler-Lindig pretty-printing
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Doc {
    Text(String),
    /// Soft break: space if flat, newline+indent if broken.
    Line,
    /// Always newline+indent.
    HardLine,
    Concat(Vec<Doc>),
    /// Try to render flat; if exceeds width, break.
    Group(Box<Doc>),
    /// Increase indent for inner doc.
    Indent(usize, Box<Doc>),
}

impl Doc {
    fn text(s: impl Into<String>) -> Doc {
        Doc::Text(s.into())
    }

    fn concat(docs: Vec<Doc>) -> Doc {
        Doc::Concat(docs)
    }

    fn group(inner: Doc) -> Doc {
        Doc::Group(Box::new(inner))
    }

    fn indent(n: usize, inner: Doc) -> Doc {
        Doc::Indent(n, Box::new(inner))
    }
}

// ---------------------------------------------------------------------------
// Form → Doc conversion
// ---------------------------------------------------------------------------

fn form_to_doc(form: &Form) -> Doc {
    match form {
        Form::Atom(s) => Doc::text(s),
        Form::Comment { text, trailing: _ } => Doc::text(text),
        Form::BlankLine => Doc::HardLine,
        Form::List {
            open,
            close,
            children,
        } => list_to_doc(open, close, children),
    }
}

fn list_to_doc(open: &str, close: &str, children: &[Form]) -> Doc {
    if children.is_empty() {
        return Doc::text(format!("{}{}", open, close));
    }

    // Determine indent style from head symbol
    let style = match &children[0] {
        Form::Atom(name) => classify_head(name),
        _ => IndentStyle::Call,
    };

    match style {
        IndentStyle::DefnLike => defn_like_doc(open, close, children),
        IndentStyle::LetLike => let_like_doc(open, close, children),
        IndentStyle::BodyIndent => body_indent_doc(open, close, children),
        IndentStyle::Call => call_doc(open, close, children),
    }
}

/// Determine where the "body" starts for a defn-like form.
/// Returns the index (into `children`) of the first body form.
/// Signature parts are children[0..body_start], body is children[body_start..].
fn defn_body_start(children: &[Form]) -> usize {
    if children.is_empty() {
        return 0;
    }

    let head = match &children[0] {
        Form::Atom(name) => name.as_str(),
        _ => return 1,
    };

    match head {
        // declare/import/extern: all signature, no body
        "declare" | "import" | "extern" => children.len(),

        // defn/defmacro: skip name, skip params (first List), skip optional `: return-type`
        "defn" | "defmacro" => {
            let mut i = 1;
            // skip name
            if i < children.len() {
                i += 1;
            }
            // skip params (first List)
            if i < children.len() && matches!(&children[i], Form::List { open: "(", .. }) {
                i += 1;
            }
            // skip optional `: return-type`
            if i < children.len() && matches!(&children[i], Form::Atom(s) if s == ":") {
                i += 1; // skip ":"
                if i < children.len() {
                    i += 1; // skip return type
                }
            }
            i
        }

        // deftype/defstruct/defclass/instance: skip one header form, rest is body
        "deftype" | "defstruct" | "defclass" | "instance" => {
            let mut i = 1;
            // skip the header form (name or (Name 'a))
            if i < children.len() {
                i += 1;
            }
            i
        }

        _ => 1,
    }
}

/// DefnLike: signature parts joined with spaces (never break),
/// body indented 2 with HardLine for 2+ body forms.
fn defn_like_doc(open: &str, close: &str, children: &[Form]) -> Doc {
    let mut parts = Vec::new();
    parts.push(Doc::text(open));

    if children.is_empty() {
        parts.push(Doc::text(close));
        return Doc::concat(parts);
    }

    let body_start = defn_body_start(children);
    let sig = &children[..body_start];
    let body = &children[body_start..];

    // Signature parts: joined with Text(" ") — never break
    for (i, child) in sig.iter().enumerate() {
        if i > 0 {
            parts.push(Doc::text(" "));
        }
        parts.push(form_to_doc(child));
    }

    // Body forms
    if !body.is_empty() {
        let break_doc = if body.len() >= 2 {
            Doc::HardLine
        } else {
            Doc::Line
        };

        let mut body_parts = Vec::new();
        for child in body.iter() {
            body_parts.push(break_doc.clone());
            body_parts.push(form_to_doc(child));
        }
        parts.push(Doc::indent(2, Doc::concat(body_parts)));
    }

    parts.push(Doc::text(close));
    Doc::group(Doc::concat(parts))
}

/// LetLike: (let (bindings) body...)
/// Bindings stay on same line as `let`. Body uses HardLine for 2+ forms.
fn let_like_doc(open: &str, close: &str, children: &[Form]) -> Doc {
    let mut parts = Vec::new();
    parts.push(Doc::text(open));
    parts.push(form_to_doc(&children[0])); // "let"

    if children.len() == 1 {
        parts.push(Doc::text(close));
        return Doc::group(Doc::concat(parts));
    }

    parts.push(Doc::text(" "));
    // Bindings block (typically a list)
    parts.push(form_to_doc(&children[1]));

    // Body forms
    let body = &children[2..];
    if !body.is_empty() {
        let break_doc = if body.len() >= 2 {
            Doc::HardLine
        } else {
            Doc::Line
        };

        let mut body_parts = Vec::new();
        for child in body.iter() {
            body_parts.push(break_doc.clone());
            body_parts.push(form_to_doc(child));
        }
        parts.push(Doc::indent(2, Doc::concat(body_parts)));
    }

    parts.push(Doc::text(close));
    Doc::group(Doc::concat(parts))
}

/// BodyIndent: all sub-forms indented 2
/// (if cond
///   then
///   else)
fn body_indent_doc(open: &str, close: &str, children: &[Form]) -> Doc {
    let mut parts = Vec::new();
    parts.push(Doc::text(open));
    parts.push(form_to_doc(&children[0])); // head

    if children.len() > 1 {
        let mut body = Vec::new();
        for (i, child) in children[1..].iter().enumerate() {
            if i == 0 {
                body.push(Doc::text(" "));
            } else {
                body.push(Doc::Line);
            }
            body.push(form_to_doc(child));
        }
        parts.push(Doc::indent(2, Doc::concat(body)));
    }

    parts.push(Doc::text(close));
    Doc::group(Doc::concat(parts))
}

/// Call: try one line; if too long, args indented 2
/// (func arg1 arg2)
/// or
/// (func arg1
///   arg2
///   arg3)
fn call_doc(open: &str, close: &str, children: &[Form]) -> Doc {
    let mut parts = Vec::new();
    parts.push(Doc::text(open));

    let mut inner = Vec::new();
    for (i, child) in children.iter().enumerate() {
        if i > 0 {
            inner.push(Doc::Line);
        }
        inner.push(form_to_doc(child));
    }

    parts.push(Doc::indent(2, Doc::concat(inner)));
    parts.push(Doc::text(close));
    Doc::group(Doc::concat(parts))
}

fn forms_to_doc(forms: &[Form]) -> Doc {
    let mut parts = Vec::new();
    let mut had_item = false;
    let mut pending_blank = false;

    for form in forms.iter() {
        match form {
            Form::BlankLine => {
                if had_item {
                    pending_blank = true;
                }
            }
            Form::Comment {
                text,
                trailing: true,
            } => {
                // Trailing comment: space then comment on same line
                parts.push(Doc::text(" "));
                parts.push(Doc::text(text));
            }
            _ => {
                if had_item {
                    parts.push(Doc::HardLine);
                    if pending_blank {
                        parts.push(Doc::HardLine);
                        pending_blank = false;
                    }
                }
                parts.push(form_to_doc(form));
                had_item = true;
            }
        }
    }

    Doc::concat(parts)
}

// ---------------------------------------------------------------------------
// Doc renderer (Wadler-Lindig)
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Flat,
    Break,
}

struct Renderer {
    width: usize,
    output: String,
    col: usize,
}

impl Renderer {
    fn new(width: usize) -> Self {
        Self {
            width,
            output: String::new(),
            col: 0,
        }
    }

    fn render(&mut self, doc: &Doc) {
        // Work queue: (indent, mode, doc)
        let mut stack: Vec<(usize, Mode, &Doc)> = vec![(0, Mode::Break, doc)];

        while let Some((indent, mode, doc)) = stack.pop() {
            match doc {
                Doc::Text(s) => {
                    self.output.push_str(s);
                    self.col += s.len();
                }
                Doc::Line => match mode {
                    Mode::Flat => {
                        self.output.push(' ');
                        self.col += 1;
                    }
                    Mode::Break => {
                        self.output.push('\n');
                        for _ in 0..indent {
                            self.output.push(' ');
                        }
                        self.col = indent;
                    }
                },
                Doc::HardLine => {
                    self.output.push('\n');
                    for _ in 0..indent {
                        self.output.push(' ');
                    }
                    self.col = indent;
                }
                Doc::Concat(docs) => {
                    // Push in reverse order so first doc is processed first
                    for d in docs.iter().rev() {
                        stack.push((indent, mode, d));
                    }
                }
                Doc::Group(inner) => {
                    // Try flat first
                    let flat_len = measure_flat(inner, self.col, self.width);
                    if flat_len <= self.width {
                        stack.push((indent, Mode::Flat, inner));
                    } else {
                        stack.push((indent, Mode::Break, inner));
                    }
                }
                Doc::Indent(n, inner) => {
                    stack.push((indent + n, mode, inner));
                }
            }
        }
    }
}

/// Measure how wide a doc would be when rendered flat, starting from `col`.
/// Returns the final column. If it exceeds `limit`, returns limit + 1 as early exit.
fn measure_flat(doc: &Doc, start_col: usize, limit: usize) -> usize {
    let mut stack = vec![doc];
    let mut col = start_col;

    while let Some(doc) = stack.pop() {
        if col > limit {
            return col;
        }
        match doc {
            Doc::Text(s) => col += s.len(),
            Doc::Line => col += 1,             // space in flat mode
            Doc::HardLine => return limit + 1, // can't be flat
            Doc::Concat(docs) => {
                for d in docs.iter().rev() {
                    stack.push(d);
                }
            }
            Doc::Group(inner) => stack.push(inner),
            Doc::Indent(_, inner) => stack.push(inner),
        }
    }

    col
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

pub fn format_document(source: &str, line_width: usize) -> String {
    let (tokens, _errors) = weir_lexer::lex_with_comments(source);

    if tokens.is_empty() {
        return source.to_string();
    }

    let mut reader = FormReader::new(&tokens, source);
    let forms = reader.read_all();

    let doc = forms_to_doc(&forms);

    let mut renderer = Renderer::new(line_width);
    renderer.render(&doc);

    let mut result = renderer.output;
    // Ensure trailing newline
    if !result.ends_with('\n') {
        result.push('\n');
    }
    result
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(source: &str) -> String {
        format_document(source, 80)
    }

    // ----- defn-like forms -----

    #[test]
    fn defn_signature_stays_together() {
        // Bug 1 regression: signature parts (name, params) must not break apart
        let input = "(defn main ()\n             (let ((result (add 1 2))\n        (name \"weir\"))\n    (println result)\n    (println name)))";
        insta::assert_snapshot!(fmt(input), @r#"
        (defn main ()
          (let ((result (add 1 2)) (name "weir"))
            (println result)
            (println name)))
        "#);
    }

    #[test]
    fn defn_single_body_fits_one_line() {
        let input = "(defn   add  ((x : i32)   (y : i32))  : i32  (+ x y))";
        insta::assert_snapshot!(fmt(input), @"(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
    }

    #[test]
    fn defn_multi_body_always_breaks() {
        let input = "(defn f () (println 1) (println 2))";
        insta::assert_snapshot!(fmt(input), @"
        (defn f ()
          (println 1)
          (println 2))
        ");
    }

    #[test]
    fn defn_with_return_type() {
        let input = "(defn add ((x : i32) (y : i32)) : i32\n  (+ x y))";
        insta::assert_snapshot!(fmt(input), @"(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
    }

    #[test]
    fn defn_line_breaking_long() {
        let input = "(defn a-very-long-function-name ((parameter-one : i32) (parameter-two : i32) (parameter-three : i32)) : i32 (+ parameter-one parameter-two))";
        let result = fmt(input);
        assert!(result.contains('\n'), "Should break: {}", result);
        let trimmed = result.trim();
        assert!(trimmed.starts_with("(defn"));
        assert!(trimmed.ends_with(')'));
    }

    #[test]
    fn defn_empty_body() {
        let input = "(defn main () ())";
        insta::assert_snapshot!(fmt(input), @"(defn main () ())");
    }

    // ----- let-like forms -----

    #[test]
    fn let_single_body_fits_one_line() {
        let input = "(let   ((x  1)  (y  2))   (+ x y))";
        insta::assert_snapshot!(fmt(input), @"(let ((x 1) (y 2)) (+ x y))");
    }

    #[test]
    fn let_multi_body_always_breaks() {
        // Bug 2 regression: 2+ body forms must use HardLine
        let input = "(let ((x 1)) (println x) (+ x 1))";
        insta::assert_snapshot!(fmt(input), @"
        (let ((x 1))
          (println x)
          (+ x 1))
        ");
    }

    // ----- deftype / defstruct -----

    #[test]
    fn deftype_body_breaks() {
        let input = "(deftype (Option 'a) (Some 'a) None)";
        insta::assert_snapshot!(fmt(input), @"
        (deftype (Option 'a)
          (Some 'a)
          None)
        ");
    }

    #[test]
    fn defstruct_fields() {
        let input = "(defstruct Vec2 (x : f32) (y : f32))";
        insta::assert_snapshot!(fmt(input), @"
        (defstruct Vec2
          (x : f32)
          (y : f32))
        ");
    }

    // ----- nested forms (original user bug report) -----

    #[test]
    fn nested_defn_let() {
        let input = "(defn main ()\n             (let ((result (add 1 2))\n        (name \"weir\"))\n    (println result)\n    (println name)))";
        insta::assert_snapshot!(fmt(input), @r#"
        (defn main ()
          (let ((result (add 1 2)) (name "weir"))
            (println result)
            (println name)))
        "#);
    }

    // ----- body-indent forms -----

    #[test]
    fn body_indent_if() {
        let input = "(if (> x 0) (print \"positive\") (print \"non-positive\"))";
        insta::assert_snapshot!(fmt(input), @r#"(if (> x 0) (print "positive") (print "non-positive"))"#);
    }

    // ----- comments -----

    #[test]
    fn leading_comment_preserved() {
        let input = "; Adds two numbers\n(defn add ((x : i32) (y : i32)) : i32 (+ x y))";
        insta::assert_snapshot!(fmt(input), @"
        ; Adds two numbers
        (defn add ((x : i32) (y : i32)) : i32 (+ x y))
        ");
    }

    #[test]
    fn trailing_comment_preserved() {
        let input = "(defn main () (+ x y)) ; entry point";
        insta::assert_snapshot!(fmt(input), @"(defn main () (+ x y)) ; entry point");
    }

    #[test]
    fn comment_between_items() {
        let input = "(defn foo () 1)\n\n; Helper function\n(defn bar () 2)";
        let result = fmt(input);
        assert!(
            result.contains("(defn foo () 1)\n\n; Helper function\n(defn bar () 2)"),
            "result: {:?}",
            result
        );
    }

    #[test]
    fn comment_only_file() {
        let input = "; This is a comment\n; Another comment";
        insta::assert_snapshot!(fmt(input), @"
        ; This is a comment
        ; Another comment
        ");
    }

    #[test]
    fn nested_comments() {
        let input = "(let ((x 1))\n  ; compute result\n  (+ x 2))";
        let result = fmt(input);
        assert!(result.contains("; compute result"), "result: {:?}", result);
    }

    // ----- blank lines -----

    #[test]
    fn blank_line_preservation() {
        let input = "(defn foo () 1)\n\n\n(defn bar () 2)";
        let result = fmt(input);
        assert!(result.contains("1)\n\n(defn bar"), "result: {:?}", result);
    }

    // ----- collections -----

    #[test]
    fn vectors() {
        let input = "[1   2   3]";
        insta::assert_snapshot!(fmt(input), @"[1 2 3]");
    }

    // ----- idempotency -----

    #[test]
    fn idempotent() {
        let inputs = [
            "(defn add ((x : i32) (y : i32)) : i32 (+ x y))",
            "; comment\n(defn foo () 1)",
            "(let ((x 1)) (+ x 2))",
            "(if true 1 2)",
            "; just a comment",
            "(defn foo () 1)\n\n(defn bar () 2)",
            "(defn f () (println 1) (println 2))",
            "(let ((x 1)) (println x) (+ x 1))",
            "(deftype (Option 'a) (Some 'a) None)",
            "(defstruct Vec2 (x : f32) (y : f32))",
            "(defn main ()\n  (let ((result (add 1 2)) (name \"weir\"))\n    (println result)\n    (println name)))",
        ];
        for input in inputs {
            let first = fmt(input);
            let second = fmt(&first);
            assert_eq!(first, second, "Not idempotent for input: {:?}", input);
        }
    }
}
