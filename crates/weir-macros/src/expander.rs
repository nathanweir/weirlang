use crate::builtins;
use crate::hygiene;
use crate::sexpr::SExpr;
use crate::MacroError;
use smol_str::SmolStr;
use std::collections::HashMap;
use weir_lexer::{Span, Token};

const MAX_EXPANSION_DEPTH: usize = 256;

/// A user-defined macro.
#[derive(Debug, Clone)]
struct MacroDef {
    params: Vec<MacroParam>,
    body: SExpr,
}

/// A macro parameter.
#[derive(Debug, Clone)]
enum MacroParam {
    Required(SmolStr),
    Rest(SmolStr),
}

/// Expand all macros in a top-level list of S-expressions.
pub fn expand_toplevel(sexprs: Vec<SExpr>) -> (Vec<SExpr>, Vec<MacroError>) {
    let mut macros: HashMap<SmolStr, MacroDef> = HashMap::new();
    let mut errors = Vec::new();

    // First pass: collect defmacro definitions
    let mut remaining = Vec::new();
    for sexpr in sexprs {
        if is_defmacro(&sexpr) {
            match parse_defmacro(&sexpr) {
                Ok((name, def)) => {
                    macros.insert(name, def);
                }
                Err(err) => errors.push(err),
            }
        } else {
            remaining.push(sexpr);
        }
    }

    // Second pass: expand macro calls
    let mut expanded = Vec::new();
    for sexpr in remaining {
        match expand_sexpr(sexpr, &macros, 0) {
            Ok(result) => expanded.push(result),
            Err(err) => errors.push(err),
        }
    }

    (expanded, errors)
}

fn is_defmacro(sexpr: &SExpr) -> bool {
    if let SExpr::List(children, _) = sexpr {
        if let Some(first) = children.first() {
            return first.is_symbol("defmacro");
        }
    }
    false
}

fn parse_defmacro(sexpr: &SExpr) -> Result<(SmolStr, MacroDef), MacroError> {
    let SExpr::List(children, span) = sexpr else {
        return Err(MacroError {
            message: "expected defmacro form".into(),
            span: sexpr.span(),
        });
    };

    if children.len() < 4 {
        return Err(MacroError {
            message: "defmacro requires name, params, and body".into(),
            span: *span,
        });
    }

    let name = children[1].as_symbol().ok_or_else(|| MacroError {
        message: "defmacro name must be a symbol".into(),
        span: children[1].span(),
    })?;

    let params = parse_macro_params(&children[2])?;
    let body = children[3].clone();

    Ok((SmolStr::new(name), MacroDef { params, body }))
}

fn parse_macro_params(sexpr: &SExpr) -> Result<Vec<MacroParam>, MacroError> {
    let SExpr::List(children, _) = sexpr else {
        return Err(MacroError {
            message: "macro params must be a list".into(),
            span: sexpr.span(),
        });
    };

    let mut params = Vec::new();
    let mut i = 0;
    while i < children.len() {
        if children[i].is_symbol("&rest") {
            i += 1;
            if i >= children.len() {
                return Err(MacroError {
                    message: "&rest must be followed by a parameter name".into(),
                    span: children[i - 1].span(),
                });
            }
            let name = children[i].as_symbol().ok_or_else(|| MacroError {
                message: "&rest parameter must be a symbol".into(),
                span: children[i].span(),
            })?;
            params.push(MacroParam::Rest(SmolStr::new(name)));
            i += 1;
            if i < children.len() {
                return Err(MacroError {
                    message: "&rest parameter must be last".into(),
                    span: children[i].span(),
                });
            }
        } else {
            let name = children[i].as_symbol().ok_or_else(|| MacroError {
                message: "macro parameter must be a symbol".into(),
                span: children[i].span(),
            })?;
            params.push(MacroParam::Required(SmolStr::new(name)));
            i += 1;
        }
    }
    Ok(params)
}

/// Recursively expand macro calls in an S-expression.
fn expand_sexpr(
    sexpr: SExpr,
    macros: &HashMap<SmolStr, MacroDef>,
    depth: usize,
) -> Result<SExpr, MacroError> {
    if depth > MAX_EXPANSION_DEPTH {
        return Err(MacroError {
            message: "macro expansion depth exceeded (possible infinite loop)".into(),
            span: sexpr.span(),
        });
    }

    match sexpr {
        SExpr::List(children, span) => {
            if children.is_empty() {
                return Ok(SExpr::List(children, span));
            }

            if let Some(name) = children[0].as_symbol() {
                // Check for built-in macros first
                if builtins::is_builtin_macro(name) {
                    let args: Vec<SExpr> = children[1..].to_vec();
                    let expanded = builtins::expand_builtin(name, &args, span)?;
                    return expand_sexpr(expanded, macros, depth + 1);
                }

                // Check for user-defined macros
                if let Some(mac) = macros.get(name) {
                    let mac = mac.clone();
                    let args: Vec<SExpr> = children[1..].to_vec();

                    let bindings = bind_params(&mac.params, &args, span)?;
                    let template = apply_hygiene(&mac.body, &bindings);
                    let expanded = eval_template(&template, &bindings)?;
                    return expand_sexpr(expanded, macros, depth + 1);
                }
            }

            // Not a macro call — recursively expand children
            let mut expanded_children = Vec::with_capacity(children.len());
            for child in children {
                expanded_children.push(expand_sexpr(child, macros, depth)?);
            }
            Ok(SExpr::List(expanded_children, span))
        }

        SExpr::Vector(children, span) => {
            let mut expanded = Vec::with_capacity(children.len());
            for child in children {
                expanded.push(expand_sexpr(child, macros, depth)?);
            }
            Ok(SExpr::Vector(expanded, span))
        }

        SExpr::Map(children, span) => {
            let mut expanded = Vec::with_capacity(children.len());
            for child in children {
                expanded.push(expand_sexpr(child, macros, depth)?);
            }
            Ok(SExpr::Map(expanded, span))
        }

        other => Ok(other),
    }
}

fn bind_params(
    params: &[MacroParam],
    args: &[SExpr],
    span: Span,
) -> Result<HashMap<SmolStr, SExpr>, MacroError> {
    let mut bindings = HashMap::new();

    let required_count = params
        .iter()
        .filter(|p| matches!(p, MacroParam::Required(_)))
        .count();
    let has_rest = params.iter().any(|p| matches!(p, MacroParam::Rest(_)));

    if has_rest {
        if args.len() < required_count {
            return Err(MacroError {
                message: format!(
                    "macro expects at least {} arguments, got {}",
                    required_count,
                    args.len()
                ),
                span,
            });
        }
    } else if args.len() != required_count {
        return Err(MacroError {
            message: format!(
                "macro expects {} arguments, got {}",
                required_count,
                args.len()
            ),
            span,
        });
    }

    let mut arg_idx = 0;
    for param in params {
        match param {
            MacroParam::Required(name) => {
                bindings.insert(name.clone(), args[arg_idx].clone());
                arg_idx += 1;
            }
            MacroParam::Rest(name) => {
                let rest_args: Vec<SExpr> = args[arg_idx..].to_vec();
                bindings.insert(name.clone(), SExpr::List(rest_args, span));
            }
        }
    }

    Ok(bindings)
}

fn apply_hygiene(template: &SExpr, bindings: &HashMap<SmolStr, SExpr>) -> SExpr {
    let mut renames: HashMap<SmolStr, SmolStr> = HashMap::new();
    collect_binding_symbols(template, bindings, &mut renames);

    if renames.is_empty() {
        return template.clone();
    }

    rename_symbols(template, &renames)
}

fn collect_binding_symbols(
    sexpr: &SExpr,
    bindings: &HashMap<SmolStr, SExpr>,
    renames: &mut HashMap<SmolStr, SmolStr>,
) {
    if let SExpr::List(children, _) = sexpr {
        if children.len() >= 2 {
            if let Some(head) = children[0].as_symbol() {
                if head == "let" {
                    if let SExpr::List(let_bindings, _) = &children[1] {
                        for binding in let_bindings {
                            if let SExpr::List(parts, _) = binding {
                                if parts.len() >= 2 {
                                    let binding_name = if let Some(name) = parts[0].as_symbol() {
                                        if name != "mut" {
                                            Some(SmolStr::new(name))
                                        } else if parts.len() >= 3 {
                                            parts[1].as_symbol().map(SmolStr::new)
                                        } else {
                                            None
                                        }
                                    } else if let SExpr::List(inner, _) = &parts[0] {
                                        if !inner.is_empty() {
                                            let first =
                                                if inner[0].is_symbol("mut") && inner.len() > 1 {
                                                    inner[1].as_symbol()
                                                } else {
                                                    inner[0].as_symbol()
                                                };
                                            first.map(SmolStr::new)
                                        } else {
                                            None
                                        }
                                    } else {
                                        None
                                    };

                                    if let Some(name) = binding_name {
                                        if !bindings.contains_key(&name)
                                            && !renames.contains_key(&name)
                                        {
                                            renames.insert(
                                                name.clone(),
                                                SmolStr::new(hygiene::gensym(&name)),
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        for child in children {
            collect_binding_symbols(child, bindings, renames);
        }
    }
}

fn rename_symbols(sexpr: &SExpr, renames: &HashMap<SmolStr, SmolStr>) -> SExpr {
    match sexpr {
        SExpr::Atom(Token::Symbol(name), span) => {
            if let Some(new_name) = renames.get(name) {
                SExpr::Atom(Token::Symbol(new_name.clone()), *span)
            } else {
                sexpr.clone()
            }
        }
        SExpr::List(children, span) => SExpr::List(
            children
                .iter()
                .map(|c| rename_symbols(c, renames))
                .collect(),
            *span,
        ),
        SExpr::Vector(children, span) => SExpr::Vector(
            children
                .iter()
                .map(|c| rename_symbols(c, renames))
                .collect(),
            *span,
        ),
        SExpr::Map(children, span) => SExpr::Map(
            children
                .iter()
                .map(|c| rename_symbols(c, renames))
                .collect(),
            *span,
        ),
        SExpr::Quasiquote(inner, span) => {
            SExpr::Quasiquote(Box::new(rename_symbols(inner, renames)), *span)
        }
        SExpr::Unquote(inner, span) => {
            SExpr::Unquote(Box::new(rename_symbols(inner, renames)), *span)
        }
        SExpr::Splice(inner, span) => {
            SExpr::Splice(Box::new(rename_symbols(inner, renames)), *span)
        }
        _ => sexpr.clone(),
    }
}

fn eval_template(
    template: &SExpr,
    bindings: &HashMap<SmolStr, SExpr>,
) -> Result<SExpr, MacroError> {
    match template {
        SExpr::Quasiquote(inner, _) => eval_quasiquote(inner, bindings),
        _ => eval_quasiquote(template, bindings),
    }
}

fn eval_quasiquote(sexpr: &SExpr, bindings: &HashMap<SmolStr, SExpr>) -> Result<SExpr, MacroError> {
    match sexpr {
        SExpr::Unquote(inner, _) => {
            if let Some(name) = inner.as_symbol() {
                bindings.get(name).cloned().ok_or_else(|| MacroError {
                    message: format!("unbound variable '{}' in macro template", name),
                    span: inner.span(),
                })
            } else {
                eval_quasiquote(inner, bindings)
            }
        }

        SExpr::Splice(_, span) => Err(MacroError {
            message: ",@ splice can only appear inside a list".into(),
            span: *span,
        }),

        SExpr::List(children, span) => {
            let mut result = Vec::new();
            for child in children {
                match child {
                    SExpr::Splice(inner, splice_span) => {
                        let name = inner.as_symbol().ok_or_else(|| MacroError {
                            message: ",@ splice target must be a symbol".into(),
                            span: *splice_span,
                        })?;
                        let value = bindings.get(name).ok_or_else(|| MacroError {
                            message: format!("unbound variable '{}' in splice", name),
                            span: inner.span(),
                        })?;
                        if let SExpr::List(elements, _) = value {
                            result.extend(elements.iter().cloned());
                        } else {
                            return Err(MacroError {
                                message: format!(
                                    ",@ splice requires a list, got non-list for '{}'",
                                    name
                                ),
                                span: *splice_span,
                            });
                        }
                    }
                    SExpr::Unquote(inner, _) => {
                        if let Some(name) = inner.as_symbol() {
                            let value = bindings.get(name).ok_or_else(|| MacroError {
                                message: format!("unbound variable '{}' in macro template", name),
                                span: inner.span(),
                            })?;
                            result.push(value.clone());
                        } else {
                            result.push(eval_quasiquote(inner, bindings)?);
                        }
                    }
                    _ => {
                        result.push(eval_quasiquote(child, bindings)?);
                    }
                }
            }
            Ok(SExpr::List(result, *span))
        }

        SExpr::Vector(children, span) => {
            let mut result = Vec::new();
            for child in children {
                result.push(eval_quasiquote(child, bindings)?);
            }
            Ok(SExpr::Vector(result, *span))
        }

        SExpr::Map(children, span) => {
            let mut result = Vec::new();
            for child in children {
                result.push(eval_quasiquote(child, bindings)?);
            }
            Ok(SExpr::Map(result, *span))
        }

        // Implicit unquote: symbol atoms that match bindings get substituted
        SExpr::Atom(Token::Symbol(name), _) => {
            if let Some(value) = bindings.get(name.as_str()) {
                Ok(value.clone())
            } else {
                Ok(sexpr.clone())
            }
        }

        _ => Ok(sexpr.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sexpr;

    fn expand_source(source: &str) -> (Vec<SExpr>, Vec<MacroError>) {
        let (tokens, _) = weir_lexer::lex(source);
        let (sexprs, _) = sexpr::read(&tokens);
        expand_toplevel(sexprs)
    }

    fn expand_to_source(source: &str) -> String {
        let result = crate::expand(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        result.source
    }

    #[test]
    fn test_defmacro_basic() {
        let source = r#"
            (defmacro when-not (cond body)
              (if (not ,cond) ,body))
            (defn main ()
              (when-not false (println "hello")))
        "#;
        let result = expand_to_source(source);
        assert!(result.contains("if"), "result: {}", result);
        assert!(result.contains("not"), "result: {}", result);
        assert!(!result.contains("when-not"), "result: {}", result);
    }

    #[test]
    fn test_defmacro_with_rest() {
        let source = r#"
            (defmacro my-do (&rest body)
              (do ,@body))
            (defn main ()
              (my-do (println "a") (println "b")))
        "#;
        let result = expand_to_source(source);
        assert!(result.contains("do"), "result: {}", result);
        assert!(!result.contains("my-do"), "result: {}", result);
    }

    #[test]
    fn test_thread_first_builtin() {
        let source = "(defn main () (-> 5 f (g a)))";
        let result = expand_to_source(source);
        assert!(!result.contains("->"), "result: {}", result);
    }

    #[test]
    fn test_thread_last_builtin() {
        let source = "(defn main () (->> 5 f (g a)))";
        let result = expand_to_source(source);
        assert!(!result.contains("->>"), "result: {}", result);
    }

    #[test]
    fn test_wrong_arity_error() {
        let source = r#"
            (defmacro my-macro (a b) (+ ,a ,b))
            (defn main () (my-macro 1))
        "#;
        let (_, errors) = expand_source(source);
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("expects 2 arguments"));
    }

    #[test]
    fn test_expansion_depth_limit() {
        let source = r#"
            (defmacro inf (x) (inf ,x))
            (defn main () (inf 1))
        "#;
        let (_, errors) = expand_source(source);
        assert!(!errors.is_empty());
        assert!(errors[0].message.contains("depth exceeded"));
    }

    #[test]
    fn test_passthrough_no_macros() {
        let source = "(defn main () (+ 1 2))";
        let (expanded, errors) = expand_source(source);
        assert!(errors.is_empty());
        assert_eq!(expanded.len(), 1);
    }

    #[test]
    fn test_nested_expansion() {
        // A macro that expands to another macro call
        let source = r#"
            (defmacro inc (x) (+ ,x 1))
            (defmacro double-inc (x) (+ (inc ,x) (inc ,x)))
            (defn main () (double-inc 5))
        "#;
        let result = expand_to_source(source);
        assert!(!result.contains("inc"), "result: {}", result);
        assert!(!result.contains("double-inc"), "result: {}", result);
    }
}
