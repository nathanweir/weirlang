use la_arena::Arena;
use std::collections::HashSet;

use crate::{Expr, ExprId, ExprKind};

/// Collect all ExprIds that are in tail position within a body.
pub fn mark_tail_positions(body: &[ExprId], exprs: &Arena<Expr>) -> HashSet<ExprId> {
    let mut tails = HashSet::new();
    if let Some(&last) = body.last() {
        mark_tail(last, exprs, &mut tails);
    }
    tails
}

fn mark_tail(expr_id: ExprId, exprs: &Arena<Expr>, tails: &mut HashSet<ExprId>) {
    tails.insert(expr_id);
    let expr = &exprs[expr_id];
    match &expr.kind {
        ExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            mark_tail(*then_branch, exprs, tails);
            if let Some(e) = else_branch {
                mark_tail(*e, exprs, tails);
            }
        }
        ExprKind::Let { body, .. } => {
            if let Some(&last) = body.last() {
                mark_tail(last, exprs, tails);
            }
        }
        ExprKind::Cond {
            clauses,
            else_clause,
        } => {
            for (_test, body) in clauses {
                mark_tail(*body, exprs, tails);
            }
            if let Some(e) = else_clause {
                mark_tail(*e, exprs, tails);
            }
        }
        ExprKind::Match { arms, .. } => {
            for arm in arms {
                if let Some(&last) = arm.body.last() {
                    mark_tail(last, exprs, tails);
                }
            }
        }
        ExprKind::Do { body } | ExprKind::When { body, .. } | ExprKind::Unless { body, .. } => {
            if let Some(&last) = body.last() {
                mark_tail(last, exprs, tails);
            }
        }
        // Leaf expressions (Call, Var, Lit, etc.) — already marked above
        _ => {}
    }
}

/// Check if any tail-position expression is a self-recursive call to `fn_name`.
pub fn has_self_tail_calls(body: &[ExprId], fn_name: &str, exprs: &Arena<Expr>) -> bool {
    let tails = mark_tail_positions(body, exprs);
    tails.iter().any(|&id| {
        if let ExprKind::Call { func, .. } = &exprs[id].kind {
            if let ExprKind::Var(callee) = &exprs[*func].kind {
                return callee == fn_name;
            }
        }
        false
    })
}
