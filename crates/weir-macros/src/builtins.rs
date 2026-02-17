use crate::sexpr::SExpr;
use crate::MacroError;
use weir_lexer::{Span, Token};

/// Expand a built-in macro.
pub fn expand_builtin(name: &str, args: &[SExpr], span: Span) -> Result<SExpr, MacroError> {
    match name {
        "->" => expand_thread_first(args, span),
        "->>" => expand_thread_last(args, span),
        _ => Err(MacroError {
            message: format!("unknown built-in macro '{}'", name),
            span,
        }),
    }
}

/// Check if a name is a built-in macro.
pub fn is_builtin_macro(name: &str) -> bool {
    matches!(name, "->" | "->>")
}

/// `(-> x f (g a) .h)` → `(.h (g (f x) a))`
fn expand_thread_first(args: &[SExpr], span: Span) -> Result<SExpr, MacroError> {
    if args.is_empty() {
        return Err(MacroError {
            message: "-> requires at least one argument".into(),
            span,
        });
    }

    let mut acc = args[0].clone();
    for step in &args[1..] {
        acc = thread_step(step, acc, true, span)?;
    }
    Ok(acc)
}

/// `(->> x f (g a) .h)` → `(.h (g a (f x)))`
fn expand_thread_last(args: &[SExpr], span: Span) -> Result<SExpr, MacroError> {
    if args.is_empty() {
        return Err(MacroError {
            message: "->> requires at least one argument".into(),
            span,
        });
    }

    let mut acc = args[0].clone();
    for step in &args[1..] {
        acc = thread_step(step, acc, false, span)?;
    }
    Ok(acc)
}

fn thread_step(step: &SExpr, acc: SExpr, is_first: bool, span: Span) -> Result<SExpr, MacroError> {
    match step {
        // Symbol: `f` → `(f acc)`
        SExpr::Atom(Token::Symbol(_), step_span) => {
            Ok(SExpr::List(vec![step.clone(), acc], span.merge(*step_span)))
        }

        // Field access: `.field` → `(.field acc)`
        SExpr::Atom(Token::FieldAccess(_), step_span) => {
            Ok(SExpr::List(vec![step.clone(), acc], span.merge(*step_span)))
        }

        // Call: `(g a b)` → `(g acc a b)` (first) or `(g a b acc)` (last)
        SExpr::List(children, step_span) => {
            if children.is_empty() {
                return Err(MacroError {
                    message: "empty list in threading step".into(),
                    span: *step_span,
                });
            }

            let mut new_children = Vec::with_capacity(children.len() + 1);
            if is_first {
                new_children.push(children[0].clone());
                new_children.push(acc);
                new_children.extend_from_slice(&children[1..]);
            } else {
                new_children.extend_from_slice(children);
                new_children.push(acc);
            }

            Ok(SExpr::List(new_children, span.merge(*step_span)))
        }

        _ => Err(MacroError {
            message: "threading step must be a symbol, field access, or call".into(),
            span: step.span(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sexpr;

    fn read_one(source: &str) -> SExpr {
        let (tokens, _) = weir_lexer::lex(source);
        let (sexprs, _) = sexpr::read(&tokens);
        assert_eq!(sexprs.len(), 1);
        sexprs.into_iter().next().unwrap()
    }

    fn read_args(source: &str) -> Vec<SExpr> {
        let sexpr = read_one(source);
        if let SExpr::List(children, _) = sexpr {
            children[1..].to_vec()
        } else {
            panic!("expected list");
        }
    }

    fn flatten_to_source(sexpr: &SExpr) -> String {
        let tokens = sexpr::flatten(std::slice::from_ref(sexpr));
        crate::reconstruct(&tokens, "")
    }

    #[test]
    fn thread_first_simple() {
        let args = read_args("(-> x f g)");
        let result = expand_thread_first(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "(g (f x))");
    }

    #[test]
    fn thread_first_with_calls() {
        let args = read_args("(-> x (f a) (g b c))");
        let result = expand_thread_first(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "(g (f x a) b c)");
    }

    #[test]
    fn thread_first_with_field() {
        let args = read_args("(-> obj .pos .x)");
        let result = expand_thread_first(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "(.x (.pos obj))");
    }

    #[test]
    fn thread_last_simple() {
        let args = read_args("(->> x f g)");
        let result = expand_thread_last(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "(g (f x))");
    }

    #[test]
    fn thread_last_with_calls() {
        let args = read_args("(->> x (f a) (g b))");
        let result = expand_thread_last(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "(g b (f a x))");
    }

    #[test]
    fn thread_first_single_arg() {
        let args = read_args("(-> x)");
        let result = expand_thread_first(&args, Span::new(0, 0)).unwrap();
        let source = flatten_to_source(&result);
        assert_eq!(source, "x");
    }
}
