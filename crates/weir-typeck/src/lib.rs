mod checker;
mod defs;
mod error;
mod result;
mod types;

#[cfg(test)]
mod tests;

pub use error::TypeError;
pub use result::{DependencyGraph, FnType, Specialization, StructInfo, TypeCheckResult};
pub use types::{Ty, TyVarId};

use la_arena::ArenaMap;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use weir_ast::*;

use checker::TypeChecker;
use result::has_type_var;

// ── Public API ───────────────────────────────────────────────────

pub fn check(module: &Module) -> TypeCheckResult {
    check_with_externals(module, &HashSet::new())
}

/// Type-check a module, treating `externals` as known symbols from other files.
/// Variables and constructors in `externals` won't produce "undefined" errors;
/// they'll be assigned fresh type variables (opaque types).
pub fn check_with_externals(module: &Module, externals: &HashSet<SmolStr>) -> TypeCheckResult {
    let mut checker = TypeChecker::new(module, externals);
    checker.collect_definitions();
    checker.check_items();
    checker.compute_reverse_deps();

    // Apply final substitution to all recorded expression types
    let mut expr_types = ArenaMap::default();
    for (id, ty) in checker.expr_types.iter() {
        expr_types.insert(id, checker.apply(ty));
    }

    // Apply final substitution to all function schemes
    let mut fn_types = HashMap::new();
    for (name, scheme) in &checker.fn_schemes {
        let param_types = scheme
            .param_types
            .iter()
            .map(|t| checker.apply(t))
            .collect();
        let return_type = checker.apply(&scheme.return_type);
        fn_types.insert(
            name.clone(),
            FnType {
                param_types,
                return_type,
            },
        );
    }

    // Collect specializations from instance methods.
    // Map type checker instances to AST item indices (they're collected in the same order).
    let instance_item_indices: Vec<usize> = module
        .items
        .iter()
        .enumerate()
        .filter_map(|(i, (item, _))| {
            if matches!(item, Item::Instance(_)) {
                Some(i)
            } else {
                None
            }
        })
        .collect();

    let mut specializations = Vec::new();
    for (inst_idx, inst) in checker.instances.iter().enumerate() {
        let item_index = instance_item_indices.get(inst_idx).copied();
        for (method_name, mangled_name) in &inst.methods {
            if let Some(scheme) = checker.fn_schemes.get(mangled_name) {
                let param_types = scheme
                    .param_types
                    .iter()
                    .map(|t| checker.apply(t))
                    .collect();
                let return_type = checker.apply(&scheme.return_type);
                specializations.push(Specialization {
                    original_name: method_name.clone(),
                    mangled_name: mangled_name.clone(),
                    param_types,
                    return_type,
                    instance_item_index: item_index,
                });
            }
        }
    }

    // Collect specializations for generic user function calls.
    // Walk all expressions looking for calls where the function type has concrete types
    // but the function's scheme has quantified type variables.
    let mut seen_specs: HashMap<SmolStr, HashSet<SmolStr>> = HashMap::new();
    for (id, _ty) in expr_types.iter() {
        let expr = &module.exprs[id];
        if let ExprKind::Call { func, .. } = &expr.kind {
            let func_expr = &module.exprs[*func];
            if let ExprKind::Var(name) = &func_expr.kind {
                if let Some(scheme) = checker.fn_schemes.get(name) {
                    if scheme.quantified.is_empty() || !scheme.constraints.is_empty() {
                        continue; // not a generic user function (or is a class method)
                    }
                    // Get the instantiated type at this call site
                    if let Some(Ty::Fn(param_tys, ret_ty)) = expr_types.get(*func) {
                        // Check all types are concrete (no remaining Var)
                        let all_concrete =
                            param_tys.iter().all(|t| !has_type_var(t)) && !has_type_var(ret_ty);
                        if !all_concrete {
                            continue;
                        }
                        // Build mangled name from concrete types.
                        // Safety: comma is a valid separator because Ty::Display never
                        // produces commas (Con uses spaces, Fn uses brackets).
                        let type_key: String = param_tys
                            .iter()
                            .chain(std::iter::once(ret_ty.as_ref()))
                            .map(|t| format!("{}", t))
                            .collect::<Vec<_>>()
                            .join(",");
                        let mangled = SmolStr::new(format!("{}#{}", name, type_key));

                        // Avoid duplicate specializations
                        let entry = seen_specs.entry(name.clone()).or_default();
                        if entry.insert(mangled.clone()) {
                            specializations.push(Specialization {
                                original_name: name.clone(),
                                mangled_name: mangled,
                                param_types: param_tys.clone(),
                                return_type: *ret_ty.clone(),
                                instance_item_index: None,
                            });
                        }
                    }
                }
            }
        }
    }

    let struct_defs = checker.export_struct_defs();

    TypeCheckResult {
        errors: checker.errors,
        expr_types,
        fn_types,
        method_resolutions: checker.method_resolutions,
        specializations,
        deps: checker.dep_graph,
        struct_defs,
    }
}
