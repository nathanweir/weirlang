use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind};
use weir_ast::{ExprId, Module};
use weir_typeck::TypeCheckResult;

use crate::document::LineIndex;

/// Find the expression with the tightest span containing `offset`, then look up its type.
pub fn hover_at(
    module: &Module,
    type_result: &TypeCheckResult,
    expanded_source: &str,
    line_index: &LineIndex,
    offset: u32,
) -> Option<Hover> {
    let expr_id = find_tightest_expr(module, offset)?;
    let ty = type_result.expr_types.get(expr_id)?;

    let expr = &module.exprs[expr_id];
    let start = expr.span.start as usize;
    let end = (expr.span.end as usize).min(expanded_source.len());
    let source_fragment = &expanded_source[start..end];

    // Truncate long fragments
    let fragment = if source_fragment.len() > 60 {
        format!("{}...", &source_fragment[..57])
    } else {
        source_fragment.to_string()
    };

    let mut contents = format!("```weir\n{}\n```\n**Type:** `{}`", fragment, ty);

    // If this is a Var referencing a function with a docstring, show it
    if let weir_ast::ExprKind::Var(name) = &expr.kind {
        for (item, _) in &module.items {
            if let weir_ast::Item::Defn(d) = item {
                if d.name == *name {
                    if let Some(ref doc) = d.docstring {
                        contents.push_str("\n\n---\n\n");
                        contents.push_str(doc);
                    }
                    break;
                }
            }
        }
    }

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: contents,
        }),
        range: Some(line_index.span_to_range(expr.span)),
    })
}

/// Find the ExprId whose span contains `offset` and is tightest (smallest span).
fn find_tightest_expr(module: &Module, offset: u32) -> Option<ExprId> {
    let mut best: Option<(ExprId, u32)> = None; // (id, span_size)

    for (id, expr) in module.exprs.iter() {
        if expr.span.start <= offset && offset < expr.span.end {
            let size = expr.span.end - expr.span.start;
            match best {
                Some((_, best_size)) if size < best_size => {
                    best = Some((id, size));
                }
                None => {
                    best = Some((id, size));
                }
                _ => {}
            }
        }
    }

    best.map(|(id, _)| id)
}

/// Get hover info for a top-level item name at `offset`.
pub fn hover_item_at(
    module: &Module,
    type_result: &TypeCheckResult,
    expanded_source: &str,
    line_index: &LineIndex,
    offset: u32,
) -> Option<Hover> {
    // First try expression hover
    if let Some(hover) = hover_at(module, type_result, expanded_source, line_index, offset) {
        return Some(hover);
    }

    // Check if we're on a top-level item name (use name_span, not full item span)
    for (item, _span) in &module.items {
        let (info, name_span) = match item {
            weir_ast::Item::Defn(d) if d.name_span.start <= offset && offset < d.name_span.end => {
                let params: Vec<String> = d.params.iter().map(|p| p.name.to_string()).collect();
                let mut text = format!("**function** `{}`({})", d.name, params.join(", "));
                if let Some(ref doc) = d.docstring {
                    text.push_str("\n\n---\n\n");
                    text.push_str(doc);
                }
                (Some(text), d.name_span)
            }
            weir_ast::Item::Deftype(d)
                if d.name_span.start <= offset && offset < d.name_span.end =>
            {
                (Some(format!("**type** `{}`", d.name)), d.name_span)
            }
            weir_ast::Item::Defstruct(d)
                if d.name_span.start <= offset && offset < d.name_span.end =>
            {
                let fields: Vec<String> = d.fields.iter().map(|f| f.name.to_string()).collect();
                (
                    Some(format!(
                        "**struct** `{}` {{ {} }}",
                        d.name,
                        fields.join(", ")
                    )),
                    d.name_span,
                )
            }
            weir_ast::Item::Defclass(d)
                if d.name_span.start <= offset && offset < d.name_span.end =>
            {
                (Some(format!("**class** `{}`", d.name)), d.name_span)
            }
            _ => (None, weir_ast::Span::new(0, 0)),
        };

        if let Some(value) = info {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range: Some(line_index.span_to_range(name_span)),
            });
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::LineIndex;

    fn setup(source: &str) -> (Module, TypeCheckResult, LineIndex) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        let line_index = LineIndex::new(source);
        (module, type_result, line_index)
    }

    fn hover_value(hover: &Hover) -> &str {
        match &hover.contents {
            HoverContents::Markup(m) => &m.value,
            _ => panic!("expected markup hover"),
        }
    }

    #[test]
    fn hover_on_integer_literal() {
        let source = "(defn main () 42)";
        let (module, tr, li) = setup(source);
        // "42" is at offset 14..16
        let hover = hover_at(&module, &tr, source, &li, 14).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("42"), "should show source fragment");
        assert!(val.contains("i32"), "integer literal should default to i32");
    }

    #[test]
    fn hover_on_string_literal() {
        // Use print so the string is an argument, not a potential docstring
        let source = r#"(defn main () (print "hello"))"#;
        let (module, tr, li) = setup(source);
        // "(defn main () (print "hello"))"
        //  0         1         2
        //  0123456789012345678901234567890
        // "hello" starts at offset 21
        let hover = hover_at(&module, &tr, source, &li, 22).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("String"), "should show String type");
    }

    #[test]
    fn hover_on_variable_reference() {
        // "(defn f ((x : i32)) x)"
        //  0         1         2
        let source = "(defn f ((x : i32)) x)";
        let (module, tr, li) = setup(source);
        // "x" ref at offset 20
        let hover = hover_at(&module, &tr, source, &li, 20).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("i32"), "variable x should have type i32");
    }

    #[test]
    fn hover_on_builtin_function() {
        // "(defn main () (print \"hi\"))"
        //  0         1    ^15
        let source = "(defn main () (print \"hi\"))";
        let (module, tr, li) = setup(source);
        // "print" starts at offset 15
        let hover = hover_at(&module, &tr, source, &li, 15).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("Fn"), "builtin should show function type");
        assert!(val.contains("Unit"), "print returns Unit");
    }

    #[test]
    fn hover_on_function_definition_name() {
        // "(defn foo (x) x)"
        //  0     ^6
        let source = "(defn foo (x) x)";
        let (module, tr, li) = setup(source);
        // "foo" at offset 6 — this is a definition name, not an expr
        let hover = hover_item_at(&module, &tr, source, &li, 6).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("**function**"), "should show function info");
        assert!(val.contains("foo"), "should show function name");
    }

    #[test]
    fn hover_on_function_name_shows_params() {
        let source = "(defn add ((x : i32) (y : i32)) (+ x y))";
        let (module, tr, li) = setup(source);
        // "add" at offset 6
        let hover = hover_item_at(&module, &tr, source, &li, 6).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("x"), "should show param names");
        assert!(val.contains("y"), "should show param names");
    }

    #[test]
    fn hover_on_function_name_shows_docstring() {
        let source = "(defn greet () \"Says hello\" (println \"hello\"))";
        let (module, tr, li) = setup(source);
        let hover = hover_item_at(&module, &tr, source, &li, 6).unwrap();
        let val = hover_value(&hover);
        assert!(
            val.contains("Says hello"),
            "should include docstring, got: {}",
            val
        );
    }

    #[test]
    fn hover_on_deftype_name() {
        let source = "(deftype Color Red Green Blue)";
        let (module, tr, li) = setup(source);
        // "Color" at offset 9
        let hover = hover_item_at(&module, &tr, source, &li, 9).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("**type**"), "should show type info");
        assert!(val.contains("Color"), "should show type name");
    }

    #[test]
    fn hover_on_defstruct_name() {
        let source = "(defstruct Point (x : f64) (y : f64))";
        let (module, tr, li) = setup(source);
        // "Point" at offset 11
        let hover = hover_item_at(&module, &tr, source, &li, 11).unwrap();
        let val = hover_value(&hover);
        assert!(val.contains("**struct**"), "should show struct info");
        assert!(val.contains("Point"), "should show struct name");
        assert!(val.contains("x"), "should show field names");
        assert!(val.contains("y"), "should show field names");
    }

    #[test]
    fn hover_on_whitespace_returns_none() {
        let source = "(defn foo () 42)";
        let (module, tr, li) = setup(source);
        // offset 5 is space between "defn" and "foo"
        assert!(hover_item_at(&module, &tr, source, &li, 5).is_none());
    }

    #[test]
    fn hover_on_var_referencing_function_shows_docstring() {
        let source = "(defn greet () \"Says hello\" (println \"hello\")) (defn main () (greet))";
        let (module, tr, li) = setup(source);
        // "greet" call — find its offset (rfind to get the call, not the def)
        let call_offset = source.rfind("greet").unwrap() as u32;
        let hover = hover_item_at(&module, &tr, source, &li, call_offset).unwrap();
        let val = hover_value(&hover);
        assert!(
            val.contains("Says hello"),
            "should show docstring on var hover, got: {}",
            val
        );
    }

    #[test]
    fn hover_does_not_show_entire_function() {
        // Regression test: hovering over "print" in a function body should NOT
        // show the enclosing function's hover
        let source = "(defn thing () (print \"hi\"))";
        let (module, tr, li) = setup(source);
        // "print" at offset 16
        let hover = hover_item_at(&module, &tr, source, &li, 16).unwrap();
        let val = hover_value(&hover);
        // Should show print's type, NOT "**function** thing()"
        assert!(
            !val.contains("**function**"),
            "should not show enclosing function hover, got: {}",
            val
        );
        assert!(val.contains("print"), "should show the print expression");
    }
}
