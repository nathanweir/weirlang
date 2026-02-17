use tower_lsp::lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureInformation,
};
use weir_ast::{ExprId, ExprKind, Module};
use weir_typeck::{Ty, TypeCheckResult};

/// Provide signature help at `offset` — when inside a function call, show parameter info.
pub fn signature_help(
    module: &Module,
    type_result: &TypeCheckResult,
    offset: u32,
) -> Option<SignatureHelp> {
    // Find the call expression containing offset, preferring the tightest fit.
    let (func_id, args, active_param) = find_enclosing_call(module, offset)?;

    // Look up the function type
    let func_ty = type_result.expr_types.get(func_id)?;
    let (param_tys, ret_ty) = match func_ty {
        Ty::Fn(params, ret) => (params.as_slice(), ret.as_ref()),
        _ => return None,
    };

    // Get the function name for the label
    let func_expr = &module.exprs[func_id];
    let func_name = match &func_expr.kind {
        ExprKind::Var(name) => name.to_string(),
        _ => "fn".to_string(),
    };

    // Build parameter info
    let parameters: Vec<ParameterInformation> = param_tys
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            // Try to get parameter name from the function definition
            let param_name = find_param_name(module, &func_name, i);
            let label = match param_name {
                Some(name) => format!("{}: {}", name, ty),
                None => format!("{}", ty),
            };
            ParameterInformation {
                label: ParameterLabel::Simple(label),
                documentation: None,
            }
        })
        .collect();

    let param_labels: Vec<String> = parameters
        .iter()
        .map(|p| match &p.label {
            ParameterLabel::Simple(s) => s.clone(),
            _ => String::new(),
        })
        .collect();

    let signature_label = format!("({} {}) -> {}", func_name, param_labels.join(" "), ret_ty);

    // Clamp active parameter to valid range
    let active_parameter = if parameters.is_empty() {
        None
    } else {
        Some((active_param as u32).min(parameters.len() as u32 - 1))
    };

    let _ = args; // used only to find active param index

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label: signature_label,
            documentation: None,
            parameters: Some(parameters),
            active_parameter,
        }],
        active_signature: Some(0),
        active_parameter,
    })
}

/// Find the call expression containing `offset`, returning (func_id, args, active_param_index).
fn find_enclosing_call(module: &Module, offset: u32) -> Option<(ExprId, Vec<ExprId>, usize)> {
    let mut best: Option<(ExprId, Vec<ExprId>, usize, u32)> = None; // (func, args, active, span_size)

    for (_id, expr) in module.exprs.iter() {
        if expr.span.start <= offset && offset <= expr.span.end {
            if let ExprKind::Call { func, args } = &expr.kind {
                let span_size = expr.span.end - expr.span.start;

                // Determine active parameter by counting which arg spans we're past
                let active = compute_active_param(module, args, offset);

                let is_better = match best {
                    Some((_, _, _, best_size)) => span_size < best_size,
                    None => true,
                };

                if is_better {
                    let arg_ids: Vec<ExprId> = args.iter().map(|a| a.value).collect();
                    best = Some((*func, arg_ids, active, span_size));
                }
            }
        }
    }

    best.map(|(func, args, active, _)| (func, args, active))
}

/// Determine which parameter is active based on cursor position within args.
fn compute_active_param(module: &Module, args: &[weir_ast::Arg], offset: u32) -> usize {
    for (i, arg) in args.iter().enumerate() {
        let arg_expr = &module.exprs[arg.value];
        if offset <= arg_expr.span.start {
            // Cursor is before this arg — active param is this one
            return i;
        }
        if offset >= arg_expr.span.start && offset <= arg_expr.span.end {
            // Cursor is inside this arg
            return i;
        }
    }
    // Cursor is past all args — next parameter
    args.len()
}

/// Try to find the parameter name for parameter `index` of function `func_name`.
fn find_param_name(module: &Module, func_name: &str, index: usize) -> Option<String> {
    for (item, _) in &module.items {
        if let weir_ast::Item::Defn(d) = item {
            if d.name == func_name {
                return d.params.get(index).map(|p| p.name.to_string());
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use weir_typeck::TypeCheckResult;

    fn setup(source: &str) -> (Module, TypeCheckResult) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        (module, type_result)
    }

    #[test]
    fn basic_function_call() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y)) (defn main () (add 1 2))";
        let (module, tr) = setup(source);
        // Cursor inside the call `(add 1 2)` — after "add "
        let add_offset = source.rfind("add").unwrap() as u32;
        let result = signature_help(&module, &tr, add_offset + 4); // after "add "
        assert!(result.is_some());
        let sh = result.unwrap();
        assert_eq!(sh.signatures.len(), 1);
        let sig = &sh.signatures[0];
        assert!(sig.label.contains("add"));
        assert!(sig.label.contains("i32"));
        let params = sig.parameters.as_ref().unwrap();
        assert_eq!(params.len(), 2);
    }

    #[test]
    fn active_parameter_first() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y)) (defn main () (add 1 2))";
        let (module, tr) = setup(source);
        // Cursor on the "1" — first argument
        let one_offset = source.rfind('1').unwrap() as u32;
        let sh = signature_help(&module, &tr, one_offset).unwrap();
        assert_eq!(sh.active_parameter, Some(0));
    }

    #[test]
    fn active_parameter_second() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y)) (defn main () (add 1 2))";
        let (module, tr) = setup(source);
        // Cursor on the "2" — second argument
        let two_offset = source.rfind('2').unwrap() as u32;
        let sh = signature_help(&module, &tr, two_offset).unwrap();
        assert_eq!(sh.active_parameter, Some(1));
    }

    #[test]
    fn param_names_shown() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y)) (defn main () (add 1 2))";
        let (module, tr) = setup(source);
        let offset = source.rfind("add").unwrap() as u32 + 4;
        let sh = signature_help(&module, &tr, offset).unwrap();
        let params = sh.signatures[0].parameters.as_ref().unwrap();
        match &params[0].label {
            ParameterLabel::Simple(s) => assert!(s.contains("x"), "should contain param name x"),
            _ => panic!("expected simple label"),
        }
        match &params[1].label {
            ParameterLabel::Simple(s) => assert!(s.contains("y"), "should contain param name y"),
            _ => panic!("expected simple label"),
        }
    }

    #[test]
    fn no_help_outside_call() {
        let source = "(defn main () 42)";
        let (module, tr) = setup(source);
        // Cursor on the literal — not a call, but 42 is inside (defn ...) which is not a Call
        // Actually the top-level isn't a call. Let's try offset 0
        let result = signature_help(&module, &tr, 0);
        assert!(result.is_none());
    }

    #[test]
    fn builtin_function_call() {
        let source = "(defn main () (+ 1 2))";
        let (module, tr) = setup(source);
        // Cursor after "+" inside the call
        let plus_offset = source.find("+ ").unwrap() as u32;
        let result = signature_help(&module, &tr, plus_offset + 2);
        assert!(result.is_some());
        let sh = result.unwrap();
        assert_eq!(sh.signatures.len(), 1);
    }

    #[test]
    fn return_type_shown_in_label() {
        let source = "(defn add ((x : i32) (y : i32)) : i32 (+ x y)) (defn main () (add 1 2))";
        let (module, tr) = setup(source);
        let offset = source.rfind("add").unwrap() as u32 + 4;
        let sh = signature_help(&module, &tr, offset).unwrap();
        assert!(
            sh.signatures[0].label.contains("i32"),
            "label should contain return type"
        );
    }
}
