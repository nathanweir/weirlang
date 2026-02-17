use tower_lsp::lsp_types::*;
use weir_ast::*;
use weir_typeck::{Ty, TypeCheckResult};

use crate::document::LineIndex;

/// Returns true if the type should be suppressed from inlay hints.
fn should_skip(ty: &Ty) -> bool {
    matches!(ty, Ty::Error | Ty::Var(_))
}

/// Returns true if the position falls within the requested range.
fn in_range(pos: Position, range: &Range) -> bool {
    (pos.line > range.start.line
        || (pos.line == range.start.line && pos.character >= range.start.character))
        && (pos.line < range.end.line
            || (pos.line == range.end.line && pos.character <= range.end.character))
}

/// Compute inlay hints for the visible range of a document.
pub fn inlay_hints(
    module: &Module,
    type_result: &TypeCheckResult,
    line_index: &LineIndex,
    range: Range,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();

    for (item, _span) in &module.items {
        match item {
            Item::Defn(d) => {
                collect_defn_hints(d, module, type_result, line_index, &range, &mut hints);
            }
            Item::Instance(inst) => {
                for method in &inst.methods {
                    collect_defn_hints(method, module, type_result, line_index, &range, &mut hints);
                }
            }
            _ => {}
        }
    }

    hints
}

fn collect_defn_hints(
    d: &Defn,
    module: &Module,
    type_result: &TypeCheckResult,
    line_index: &LineIndex,
    range: &Range,
    hints: &mut Vec<InlayHint>,
) {
    if let Some(fn_type) = type_result.fn_types.get(&d.name) {
        // Param type hints
        for (i, param) in d.params.iter().enumerate() {
            if param.type_ann.is_some() {
                continue;
            }
            if let Some(ty) = fn_type.param_types.get(i) {
                if should_skip(ty) {
                    continue;
                }
                let pos = line_index.offset_to_position(param.name_span.end);
                if in_range(pos, range) {
                    hints.push(InlayHint {
                        position: pos,
                        label: InlayHintLabel::String(format!(": {}", ty)),
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: None,
                        padding_right: None,
                        data: None,
                    });
                }
            }
        }

        // Return type hint
        if d.return_type.is_none() {
            let ret_ty = &fn_type.return_type;
            if !should_skip(ret_ty) && *ret_ty != Ty::Unit {
                // Position: just before the body start — avoids colliding with the
                // last param's type hint (which shares the same offset).
                let hint_offset = if let Some(&first_body) = d.body.first() {
                    module.exprs[first_body].span.start
                } else {
                    0
                };

                if hint_offset > 0 {
                    let pos = line_index.offset_to_position(hint_offset);
                    if in_range(pos, range) {
                        hints.push(InlayHint {
                            position: pos,
                            label: InlayHintLabel::String(format!("→ {}", ret_ty)),
                            kind: Some(InlayHintKind::TYPE),
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: None,
                            data: None,
                        });
                    }
                }
            }
        }
    }

    // Walk body expressions for let bindings and lambdas
    for &expr_id in &d.body {
        collect_expr_hints(expr_id, module, type_result, line_index, range, hints);
    }
}

fn collect_expr_hints(
    expr_id: ExprId,
    module: &Module,
    type_result: &TypeCheckResult,
    line_index: &LineIndex,
    range: &Range,
    hints: &mut Vec<InlayHint>,
) {
    let expr = &module.exprs[expr_id];
    match &expr.kind {
        ExprKind::Let { bindings, body } => {
            for binding in bindings {
                if binding.type_ann.is_none() {
                    if let Some(ty) = type_result.expr_types.get(binding.value) {
                        if !should_skip(ty) {
                            let pos = line_index.offset_to_position(binding.name_span.end);
                            if in_range(pos, range) {
                                hints.push(InlayHint {
                                    position: pos,
                                    label: InlayHintLabel::String(format!(": {}", ty)),
                                    kind: Some(InlayHintKind::TYPE),
                                    text_edits: None,
                                    tooltip: None,
                                    padding_left: None,
                                    padding_right: None,
                                    data: None,
                                });
                            }
                        }
                    }
                }
                // Recurse into binding values (e.g. lambdas)
                collect_expr_hints(binding.value, module, type_result, line_index, range, hints);
            }
            for &b in body {
                collect_expr_hints(b, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::Lambda { params, body, .. } => {
            // Extract param types from the lambda's type (Ty::Fn(params, ret))
            if let Some(Ty::Fn(param_tys, _)) = type_result.expr_types.get(expr_id) {
                for (i, param) in params.iter().enumerate() {
                    if param.type_ann.is_some() {
                        continue;
                    }
                    if let Some(ty) = param_tys.get(i) {
                        if should_skip(ty) {
                            continue;
                        }
                        let pos = line_index.offset_to_position(param.name_span.end);
                        if in_range(pos, range) {
                            hints.push(InlayHint {
                                position: pos,
                                label: InlayHintLabel::String(format!(": {}", ty)),
                                kind: Some(InlayHintKind::TYPE),
                                text_edits: None,
                                tooltip: None,
                                padding_left: None,
                                padding_right: None,
                                data: None,
                            });
                        }
                    }
                }
            }
            for &b in body {
                collect_expr_hints(b, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expr_hints(*condition, module, type_result, line_index, range, hints);
            collect_expr_hints(*then_branch, module, type_result, line_index, range, hints);
            if let Some(e) = else_branch {
                collect_expr_hints(*e, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::Cond {
            clauses,
            else_clause,
        } => {
            for &(test, body) in clauses {
                collect_expr_hints(test, module, type_result, line_index, range, hints);
                collect_expr_hints(body, module, type_result, line_index, range, hints);
            }
            if let Some(e) = else_clause {
                collect_expr_hints(*e, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::Match { scrutinee, arms } => {
            collect_expr_hints(*scrutinee, module, type_result, line_index, range, hints);
            for arm in arms {
                for &b in &arm.body {
                    collect_expr_hints(b, module, type_result, line_index, range, hints);
                }
            }
        }

        ExprKind::When { condition, body } | ExprKind::Unless { condition, body } => {
            collect_expr_hints(*condition, module, type_result, line_index, range, hints);
            for &b in body {
                collect_expr_hints(b, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::Do { body } | ExprKind::Unsafe { body } => {
            for &b in body {
                collect_expr_hints(b, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::Call { func, args } => {
            collect_expr_hints(*func, module, type_result, line_index, range, hints);
            for arg in args {
                collect_expr_hints(arg.value, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::SetBang { place, value } => {
            collect_expr_hints(*place, module, type_result, line_index, range, hints);
            collect_expr_hints(*value, module, type_result, line_index, range, hints);
        }

        ExprKind::Ann { expr, .. } => {
            collect_expr_hints(*expr, module, type_result, line_index, range, hints);
        }

        ExprKind::Try(inner) => {
            collect_expr_hints(*inner, module, type_result, line_index, range, hints);
        }

        ExprKind::VectorLit(elems) => {
            for &e in elems {
                collect_expr_hints(e, module, type_result, line_index, range, hints);
            }
        }

        ExprKind::MapLit(pairs) => {
            for &(k, v) in pairs {
                collect_expr_hints(k, module, type_result, line_index, range, hints);
                collect_expr_hints(v, module, type_result, line_index, range, hints);
            }
        }

        // Leaves — no sub-expressions to recurse into
        ExprKind::Lit(_) | ExprKind::Var(_) | ExprKind::FieldAccess(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::document::LineIndex;

    /// Parse source, typecheck, and return inlay hints for the full document.
    fn hints_for(source: &str) -> Vec<InlayHint> {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        let line_index = LineIndex::new(source);
        let full_range = Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 10000,
                character: 0,
            },
        };
        inlay_hints(&module, &type_result, &line_index, full_range)
    }

    fn hint_label(hint: &InlayHint) -> &str {
        match &hint.label {
            InlayHintLabel::String(s) => s,
            _ => panic!("expected string label"),
        }
    }

    #[test]
    fn let_binding_type_hint() {
        let hints = hints_for("(defn main () (let ((x 5)) x))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            labels.contains(&": i32"),
            "expected `: i32` hint for x, got {:?}",
            labels
        );
    }

    #[test]
    fn annotated_let_binding_no_hint() {
        let hints = hints_for("(defn main () (let (((x : i32) 5)) x))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        // Should not have a `: i32` hint for x since it's annotated
        // (the function may still get a return type hint)
        assert!(
            !labels.iter().any(|l| l.starts_with(": ")),
            "should not emit type hint for annotated binding, got {:?}",
            labels
        );
    }

    #[test]
    fn function_param_type_hint() {
        let hints = hints_for("(defn add (x y) (+ x y)) (defn main () (add 1 2))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        // x and y should both get `: i32` hints
        let i32_count = labels.iter().filter(|l| **l == ": i32").count();
        assert!(
            i32_count >= 2,
            "expected at least 2 `: i32` hints for params, got {:?}",
            labels
        );
    }

    #[test]
    fn annotated_param_no_hint() {
        let hints = hints_for("(defn f ((x : i32)) x)");
        let param_hints: Vec<&InlayHint> = hints
            .iter()
            .filter(|h| hint_label(h).starts_with(": "))
            .collect();
        assert!(
            param_hints.is_empty(),
            "should not emit hint for annotated param, got {:?}",
            param_hints
                .iter()
                .map(|h| hint_label(h))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn function_return_type_hint() {
        let hints = hints_for("(defn f () 42)");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            labels.iter().any(|l| l.contains('→')),
            "expected return type hint with →, got {:?}",
            labels
        );
    }

    #[test]
    fn annotated_return_no_hint() {
        let hints = hints_for("(defn f () : i32 42)");
        let return_hints: Vec<&str> = hints
            .iter()
            .map(hint_label)
            .filter(|l| l.contains('→'))
            .collect();
        assert!(
            return_hints.is_empty(),
            "should not emit return type hint for annotated return, got {:?}",
            return_hints
        );
    }

    #[test]
    fn unit_return_type_suppressed() {
        let hints = hints_for("(defn f () (println \"hi\"))");
        let return_hints: Vec<&str> = hints
            .iter()
            .map(hint_label)
            .filter(|l| l.contains('→'))
            .collect();
        assert!(
            return_hints.is_empty(),
            "should not emit return type hint for Unit return, got {:?}",
            return_hints
        );
    }

    #[test]
    fn error_type_suppressed() {
        // Reference undefined variable — type should be Error, no hint
        let hints = hints_for("(defn main () (let ((x undefined_var)) x))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            !labels.iter().any(|l| l.contains("error")),
            "should not emit hint for error type, got {:?}",
            labels
        );
    }

    #[test]
    fn multiple_let_bindings() {
        let hints = hints_for("(defn main () (let ((x 1) (y \"hello\")) x))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            labels.contains(&": i32"),
            "expected `: i32` for x, got {:?}",
            labels
        );
        assert!(
            labels.contains(&": String"),
            "expected `: String` for y, got {:?}",
            labels
        );
    }

    #[test]
    fn lambda_param_hint() {
        let hints = hints_for("(defn main () (let ((f (fn (x) (+ x 1)))) (f 10)))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        // Lambda param x should get a type hint
        let i32_hints: Vec<&&str> = labels.iter().filter(|l| **l == ": i32").collect();
        assert!(
            !i32_hints.is_empty(),
            "expected type hint for lambda param, got {:?}",
            labels
        );
    }

    #[test]
    fn nested_let_hints() {
        let hints = hints_for("(defn main () (let ((x 1)) (let ((y x)) y)))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        let i32_count = labels.iter().filter(|l| **l == ": i32").count();
        assert!(
            i32_count >= 2,
            "expected hints for both x and y, got {:?}",
            labels
        );
    }

    #[test]
    fn string_type_hint() {
        let hints = hints_for("(defn main () (let ((s \"hello\")) s))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            labels.contains(&": String"),
            "expected `: String` for s, got {:?}",
            labels
        );
    }

    #[test]
    fn complex_type_hint() {
        let hints = hints_for("(defn main () (let ((v [1 2 3])) v))");
        let labels: Vec<&str> = hints.iter().map(hint_label).collect();
        assert!(
            labels
                .iter()
                .any(|l| l.contains("Vector") && l.contains("i32")),
            "expected `(Vector i32)` hint, got {:?}",
            labels
        );
    }
}
