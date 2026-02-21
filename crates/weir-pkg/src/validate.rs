use std::collections::{HashMap, HashSet};

use smol_str::SmolStr;
use weir_ast::{ImportKind, Item, Module};

use crate::error::PkgError;

/// Extract the set of public symbol names from a parsed module.
///
/// Collects names from: defn, deftype (type + variants), defstruct,
/// extern "C" declarations, defclass, declare.
pub fn extract_exports(module: &Module) -> HashSet<SmolStr> {
    let mut exports = HashSet::new();

    for (item, _span) in &module.items {
        match item {
            Item::Defn(d) => {
                exports.insert(d.name.clone());
            }
            Item::Deftype(d) => {
                exports.insert(d.name.clone());
                for variant in &d.variants {
                    exports.insert(variant.name.clone());
                }
            }
            Item::Defstruct(d) => {
                exports.insert(d.name.clone());
            }
            Item::ExternC(ext) => {
                for decl in &ext.declarations {
                    exports.insert(decl.name.clone());
                }
            }
            Item::Defclass(d) => {
                exports.insert(d.name.clone());
                for method in &d.methods {
                    exports.insert(method.name.clone());
                }
            }
            Item::Declare(d) => {
                exports.insert(d.name.clone());
            }
            Item::Defglobal(g) => {
                exports.insert(g.name.clone());
            }
            Item::Instance(_) | Item::Import(_) => {}
        }
    }

    exports
}

/// Validate that all imports in the given modules reference real modules and symbols.
///
/// `module_exports` maps module name → set of exported symbol names.
/// `parsed_modules` maps module name → parsed AST.
///
/// Returns a list of errors (empty if all imports are valid).
pub fn validate_imports(
    module_exports: &HashMap<SmolStr, HashSet<SmolStr>>,
    parsed_modules: &HashMap<SmolStr, Module>,
) -> Vec<PkgError> {
    let mut errors = Vec::new();

    for (module_name, module) in parsed_modules {
        for (item, _span) in &module.items {
            if let Item::Import(import) = item {
                let target_module = &import.module_path;

                // Check that the target module exists
                if !module_exports.contains_key(target_module) {
                    errors.push(PkgError::UnknownModule(format!(
                        "in module `{}`: unknown module `{}`",
                        module_name, target_module
                    )));
                    continue;
                }

                let target_exports = &module_exports[target_module];

                // Check imported names exist in target module
                match &import.kind {
                    ImportKind::Names(names) => {
                        for name in names {
                            if !target_exports.contains(name) {
                                errors.push(PkgError::UnknownSymbol {
                                    module: target_module.to_string(),
                                    symbol: name.to_string(),
                                });
                            }
                        }
                    }
                    ImportKind::All | ImportKind::Alias(_) => {
                        // :all and :as don't need symbol-level validation
                    }
                }
            }
        }
    }

    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use weir_ast::*;

    fn make_module_with_defn(name: &str) -> Module {
        let mut module = Module::new();
        module.items.push((
            Item::Defn(Defn {
                name: SmolStr::new(name),
                name_span: Span::new(0, 0),
                params: vec![],
                params_end: 0,
                return_type: None,
                body: vec![],
                is_pub: false,
                docstring: None,
                span: Span::new(0, 0),
            }),
            Span::new(0, 0),
        ));
        module
    }

    #[test]
    fn extract_exports_from_defn() {
        let module = make_module_with_defn("my-func");
        let exports = extract_exports(&module);
        assert!(exports.contains("my-func"));
    }

    #[test]
    fn extract_exports_from_deftype_includes_variants() {
        let mut module = Module::new();
        module.items.push((
            Item::Deftype(Deftype {
                name: SmolStr::new("Color"),
                name_span: Span::new(0, 0),
                type_params: vec![],
                variants: vec![
                    Variant {
                        name: SmolStr::new("Red"),
                        name_span: Span::new(0, 0),
                        fields: vec![],
                        span: Span::new(0, 0),
                    },
                    Variant {
                        name: SmolStr::new("Blue"),
                        name_span: Span::new(0, 0),
                        fields: vec![],
                        span: Span::new(0, 0),
                    },
                ],
                is_pub: false,
                span: Span::new(0, 0),
            }),
            Span::new(0, 0),
        ));
        let exports = extract_exports(&module);
        assert!(exports.contains("Color"));
        assert!(exports.contains("Red"));
        assert!(exports.contains("Blue"));
    }

    #[test]
    fn validate_imports_catches_unknown_module() {
        let module_exports: HashMap<SmolStr, HashSet<SmolStr>> = HashMap::new();

        let mut importing_module = Module::new();
        importing_module.items.push((
            Item::Import(Import {
                module_path: SmolStr::new("nonexistent"),
                kind: ImportKind::Names(vec![SmolStr::new("foo")]),
                span: Span::new(0, 0),
            }),
            Span::new(0, 0),
        ));

        let mut parsed = HashMap::new();
        parsed.insert(SmolStr::new("my-app"), importing_module);

        let errors = validate_imports(&module_exports, &parsed);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].to_string().contains("nonexistent"));
    }

    #[test]
    fn validate_imports_catches_unknown_symbol() {
        let mut module_exports: HashMap<SmolStr, HashSet<SmolStr>> = HashMap::new();
        let mut lib_exports = HashSet::new();
        lib_exports.insert(SmolStr::new("real-fn"));
        module_exports.insert(SmolStr::new("test-lib"), lib_exports);

        let mut importing_module = Module::new();
        importing_module.items.push((
            Item::Import(Import {
                module_path: SmolStr::new("test-lib"),
                kind: ImportKind::Names(vec![SmolStr::new("nonexistent-fn")]),
                span: Span::new(0, 0),
            }),
            Span::new(0, 0),
        ));

        let mut parsed = HashMap::new();
        parsed.insert(SmolStr::new("my-app"), importing_module);

        let errors = validate_imports(&module_exports, &parsed);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].to_string().contains("nonexistent-fn"));
    }

    #[test]
    fn validate_imports_passes_valid_imports() {
        let mut module_exports: HashMap<SmolStr, HashSet<SmolStr>> = HashMap::new();
        let mut lib_exports = HashSet::new();
        lib_exports.insert(SmolStr::new("add-nums"));
        module_exports.insert(SmolStr::new("test-lib"), lib_exports);

        let mut importing_module = Module::new();
        importing_module.items.push((
            Item::Import(Import {
                module_path: SmolStr::new("test-lib"),
                kind: ImportKind::Names(vec![SmolStr::new("add-nums")]),
                span: Span::new(0, 0),
            }),
            Span::new(0, 0),
        ));

        let mut parsed = HashMap::new();
        parsed.insert(SmolStr::new("my-app"), importing_module);

        let errors = validate_imports(&module_exports, &parsed);
        assert!(errors.is_empty());
    }
}
