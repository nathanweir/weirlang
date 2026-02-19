use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};
use weir_ast::{Item, Module};
use weir_typeck::TypeCheckResult;

use crate::index::{SymbolIndex, SymbolKind};
use crate::workspace::WorkspaceSymbol;

/// Collect completion items from module definitions, builtins, keywords,
/// local variables visible at `offset`, and workspace symbols from other files.
pub fn completions(
    module: &Module,
    type_result: Option<&TypeCheckResult>,
    symbol_index: Option<&SymbolIndex>,
    offset: Option<u32>,
    workspace_symbols: Option<&[WorkspaceSymbol]>,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Top-level definitions
    for (item, _span) in &module.items {
        match item {
            Item::Defn(d) => {
                let detail = type_result
                    .and_then(|tr| tr.fn_types.get(d.name.as_str()))
                    .map(|ft| format_fn_type(&ft.param_types, &ft.return_type));
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail,
                    ..Default::default()
                });
            }
            Item::Deftype(d) => {
                let detail = if d.type_params.is_empty() {
                    None
                } else {
                    Some(format!(
                        "({})",
                        std::iter::once(d.name.as_str())
                            .chain(d.type_params.iter().map(|p| p.as_str()))
                            .collect::<Vec<_>>()
                            .join(" ")
                    ))
                };
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::ENUM),
                    detail,
                    ..Default::default()
                });
                // Variant constructors
                for v in &d.variants {
                    let detail = if v.fields.is_empty() {
                        Some(d.name.to_string())
                    } else {
                        Some(format!(
                            "{} -> {}",
                            v.fields
                                .iter()
                                .map(|f| format_type_expr(module, *f))
                                .collect::<Vec<_>>()
                                .join(", "),
                            d.name
                        ))
                    };
                    items.push(CompletionItem {
                        label: v.name.to_string(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        detail,
                        ..Default::default()
                    });
                }
            }
            Item::Defstruct(d) => {
                let fields: String = d
                    .fields
                    .iter()
                    .map(|f| f.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::STRUCT),
                    detail: Some(format!("{{ {} }}", fields)),
                    ..Default::default()
                });
            }
            Item::Defclass(d) => {
                let methods: String = d
                    .methods
                    .iter()
                    .map(|m| m.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::INTERFACE),
                    detail: Some(format!("class: {}", methods)),
                    ..Default::default()
                });
                // Class methods as completion items
                for method in &d.methods {
                    let detail = type_result
                        .and_then(|tr| tr.fn_types.get(method.name.as_str()))
                        .map(|ft| format_fn_type(&ft.param_types, &ft.return_type));
                    items.push(CompletionItem {
                        label: method.name.to_string(),
                        kind: Some(CompletionItemKind::METHOD),
                        detail,
                        ..Default::default()
                    });
                }
            }
            _ => {}
        }
    }

    // Builtin functions
    for &(name, sig) in BUILTINS {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(sig.to_string()),
            ..Default::default()
        });
    }

    // Primitive / prelude types
    for name in PRIMITIVE_TYPES {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::CLASS),
            detail: Some("primitive type".into()),
            ..Default::default()
        });
    }

    // Language keywords
    for name in KEYWORDS {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..Default::default()
        });
    }

    // Scope-aware local variables (params, let bindings, match bindings)
    if let (Some(idx), Some(off)) = (symbol_index, offset) {
        let existing: std::collections::HashSet<String> =
            items.iter().map(|i| i.label.clone()).collect();
        for (name, kind) in idx.visible_definitions_at(off) {
            if existing.contains(&name) {
                continue;
            }
            let (item_kind, detail) = match kind {
                SymbolKind::Parameter => (CompletionItemKind::VARIABLE, Some("parameter".into())),
                SymbolKind::LetBinding => {
                    (CompletionItemKind::VARIABLE, Some("let binding".into()))
                }
                _ => continue, // top-level defs already added above
            };
            items.push(CompletionItem {
                label: name,
                kind: Some(item_kind),
                detail,
                sort_text: Some("0".into()), // sort locals first
                ..Default::default()
            });
        }
    }

    // Workspace symbols from other files
    if let Some(ws_symbols) = workspace_symbols {
        let existing: std::collections::HashSet<String> =
            items.iter().map(|i| i.label.clone()).collect();
        for sym in ws_symbols {
            if existing.contains(&sym.name) {
                continue;
            }
            let item_kind = match sym.kind {
                SymbolKind::Function => CompletionItemKind::FUNCTION,
                SymbolKind::Type => CompletionItemKind::ENUM,
                SymbolKind::Variant => CompletionItemKind::ENUM_MEMBER,
                SymbolKind::Struct => CompletionItemKind::STRUCT,
                SymbolKind::Class => CompletionItemKind::INTERFACE,
                SymbolKind::Parameter | SymbolKind::LetBinding => continue,
            };
            let filename = sym.uri.path().rsplit('/').next().unwrap_or("").to_string();
            items.push(CompletionItem {
                label: sym.name.clone(),
                kind: Some(item_kind),
                detail: Some(format!("from {}", filename)),
                sort_text: Some("2".into()), // after locals "0" and in-file defs
                ..Default::default()
            });
        }
    }

    items
}

fn format_type_expr(module: &Module, id: weir_ast::TypeExprId) -> String {
    let texpr = &module.type_exprs[id];
    match &texpr.kind {
        weir_ast::TypeExprKind::Named(name) => name.to_string(),
        weir_ast::TypeExprKind::TypeVar(name) => format!("'{}", name),
        weir_ast::TypeExprKind::Applied {
            constructor, args, ..
        } => {
            let ctor = format_type_expr(module, *constructor);
            let arg_strs: Vec<String> = args.iter().map(|a| format_type_expr(module, *a)).collect();
            format!("({} {})", ctor, arg_strs.join(" "))
        }
        weir_ast::TypeExprKind::Fn {
            params,
            return_type,
        } => {
            let param_strs: Vec<String> = params
                .iter()
                .map(|p| format_type_expr(module, *p))
                .collect();
            let ret = format_type_expr(module, *return_type);
            format!("(Fn [{}] {})", param_strs.join(" "), ret)
        }
        weir_ast::TypeExprKind::Constrained {
            constraints: _,
            inner,
        } => format_type_expr(module, *inner),
    }
}

fn format_fn_type(params: &[weir_typeck::Ty], ret: &weir_typeck::Ty) -> String {
    let params_str: String = params
        .iter()
        .map(|t| format!("{}", t))
        .collect::<Vec<_>>()
        .join(" ");
    format!("(Fn [{}] {})", params_str, ret)
}

const BUILTINS: &[(&str, &str)] = &[
    ("+", "(Fn [Num Num ...] Num)"),
    ("-", "(Fn [Num Num ...] Num)"),
    ("*", "(Fn [Num Num ...] Num)"),
    ("/", "(Fn [Num Num] Num)"),
    ("mod", "(Fn [Int Int] Int)"),
    ("<", "(Fn [Num Num] Bool)"),
    (">", "(Fn [Num Num] Bool)"),
    ("<=", "(Fn [Num Num] Bool)"),
    (">=", "(Fn [Num Num] Bool)"),
    ("=", "(Fn [a a] Bool)"),
    ("!=", "(Fn [a a] Bool)"),
    ("not", "(Fn [Bool] Bool)"),
    ("and", "(Fn [Bool Bool ...] Bool)"),
    ("or", "(Fn [Bool Bool ...] Bool)"),
    ("println", "(Fn [a] Unit)"),
    ("print", "(Fn [a] Unit)"),
    ("str", "(Fn [a] String)"),
    ("len", "(Fn [Collection] i32)"),
    ("nth", "(Fn [Collection i32] a)"),
    ("append", "(Fn [Collection a] Collection)"),
    ("type-of", "(Fn [a] String)"),
    ("sleep", "(Fn [i64] Unit)"),
    // Math — Cranelift native
    ("sqrt", "(Fn [f64] f64)"),
    ("abs", "(Fn [Num] Num)"),
    ("floor", "(Fn [f64] f64)"),
    ("ceil", "(Fn [f64] f64)"),
    ("min", "(Fn [Num Num] Num)"),
    ("max", "(Fn [Num Num] Num)"),
    // Math — runtime (libm)
    ("sin", "(Fn [f64] f64)"),
    ("cos", "(Fn [f64] f64)"),
    ("tan", "(Fn [f64] f64)"),
    ("asin", "(Fn [f64] f64)"),
    ("acos", "(Fn [f64] f64)"),
    ("atan", "(Fn [f64] f64)"),
    ("atan2", "(Fn [f64 f64] f64)"),
    ("exp", "(Fn [f64] f64)"),
    ("log", "(Fn [f64] f64)"),
    ("pow", "(Fn [f64 f64] f64)"),
    ("round", "(Fn [f64] f64)"),
    // Type conversions
    ("to-f64", "(Fn [Num] f64)"),
    ("to-i64", "(Fn [Num] i64)"),
    ("to-f32", "(Fn [Num] f32)"),
    ("to-i32", "(Fn [Num] i32)"),
    // Random
    ("random", "(Fn [] f64)"),
    ("random-int", "(Fn [i64] i64)"),
    ("random-seed", "(Fn [i64] Unit)"),
    // String operations
    ("string-length", "(Fn [String] i64)"),
    ("substring", "(Fn [String i64 i64] String)"),
    ("string-ref", "(Fn [String i64] i64)"),
    ("string-contains", "(Fn [String String] Bool)"),
    ("string-upcase", "(Fn [String] String)"),
    ("string-downcase", "(Fn [String] String)"),
    ("string-trim", "(Fn [String] String)"),
    ("char-to-string", "(Fn [i64] String)"),
    // File I/O
    ("read-file", "(Fn [String] String)"),
    ("write-file", "(Fn [String String] Unit)"),
    // Vector update
    ("set-nth", "(Fn [(Vector a) i64 a] (Vector a))"),
    // Time
    ("time-ms", "(Fn [] i64)"),
    // Terminal I/O
    ("term-init", "(Fn [] Unit)"),
    ("term-restore", "(Fn [] Unit)"),
    ("read-key", "(Fn [] i64)"),
];

const PRIMITIVE_TYPES: &[&str] = &[
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "Bool", "String", "Unit",
];

const KEYWORDS: &[&str] = &[
    "defn",
    "deftype",
    "defstruct",
    "defclass",
    "instance",
    "import",
    "declare",
    "extern",
    "let",
    "if",
    "cond",
    "when",
    "unless",
    "match",
    "fn",
    "do",
    "set!",
    "true",
    "false",
    "mut",
    "ref",
    "pub",
    "unsafe",
];

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_module(source: &str) -> (Module, TypeCheckResult) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        (module, type_result)
    }

    fn labels(items: &[CompletionItem]) -> Vec<String> {
        items.iter().map(|i| i.label.clone()).collect()
    }

    #[test]
    fn includes_user_defined_functions() {
        let (module, tr) = parse_module("(defn foo () 42) (defn bar () 0)");
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"foo".to_string()));
        assert!(l.contains(&"bar".to_string()));
    }

    #[test]
    fn function_detail_shows_type_signature() {
        let (module, tr) = parse_module("(defn add ((x : i32) (y : i32)) : i32 (+ x y))");
        let items = completions(&module, Some(&tr), None, None, None);
        let add = items.iter().find(|i| i.label == "add").unwrap();
        assert_eq!(add.detail.as_deref(), Some("(Fn [i32 i32] i32)"));
    }

    #[test]
    fn includes_user_defined_types_and_variants() {
        let (module, tr) = parse_module("(deftype Color Red Green Blue)");
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"Color".to_string()));
        assert!(l.contains(&"Red".to_string()));
        assert!(l.contains(&"Green".to_string()));
        assert!(l.contains(&"Blue".to_string()));
    }

    #[test]
    fn user_type_has_correct_kind() {
        let (module, tr) = parse_module("(deftype Color Red)");
        let items = completions(&module, Some(&tr), None, None, None);
        let color = items.iter().find(|i| i.label == "Color").unwrap();
        assert_eq!(color.kind, Some(CompletionItemKind::ENUM));
        let red = items.iter().find(|i| i.label == "Red").unwrap();
        assert_eq!(red.kind, Some(CompletionItemKind::ENUM_MEMBER));
    }

    #[test]
    fn variant_detail_shows_parent_type() {
        let (module, tr) = parse_module("(deftype Color Red Green Blue)");
        let items = completions(&module, Some(&tr), None, None, None);
        let red = items.iter().find(|i| i.label == "Red").unwrap();
        assert_eq!(red.detail.as_deref(), Some("Color"));
    }

    #[test]
    fn includes_user_defined_structs() {
        let (module, tr) = parse_module("(defstruct Point (x : f64) (y : f64))");
        let items = completions(&module, Some(&tr), None, None, None);
        let point = items.iter().find(|i| i.label == "Point").unwrap();
        assert_eq!(point.kind, Some(CompletionItemKind::STRUCT));
        assert_eq!(point.detail.as_deref(), Some("{ x, y }"));
    }

    #[test]
    fn includes_user_defined_classes() {
        let (module, tr) = parse_module("(defclass (Show 'a) (show : (Fn ['a] String)))");
        let items = completions(&module, Some(&tr), None, None, None);
        let show_class = items.iter().find(|i| i.label == "Show").unwrap();
        assert_eq!(show_class.kind, Some(CompletionItemKind::INTERFACE));
        assert_eq!(show_class.detail.as_deref(), Some("class: show"));
    }

    #[test]
    fn class_methods_appear_in_completions() {
        let (module, tr) = parse_module(
            "(defclass (Eq 'a)
               (== : (Fn ['a 'a] Bool)))",
        );
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"==".to_string()));
        let eq_method = items.iter().find(|i| i.label == "==").unwrap();
        assert_eq!(eq_method.kind, Some(CompletionItemKind::METHOD));
    }

    #[test]
    fn includes_builtin_functions() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"print".to_string()));
        assert!(l.contains(&"println".to_string()));
        assert!(l.contains(&"+".to_string()));
        assert!(l.contains(&"len".to_string()));
    }

    #[test]
    fn builtins_have_type_signatures() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let print = items.iter().find(|i| i.label == "println").unwrap();
        assert_eq!(print.detail.as_deref(), Some("(Fn [a] Unit)"));
        let add = items.iter().find(|i| i.label == "+").unwrap();
        assert!(add.detail.as_deref().unwrap().starts_with("(Fn"));
    }

    #[test]
    fn includes_primitive_types() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"i32".to_string()), "should include i32");
        assert!(l.contains(&"i64".to_string()), "should include i64");
        assert!(l.contains(&"f32".to_string()), "should include f32");
        assert!(l.contains(&"f64".to_string()), "should include f64");
        assert!(l.contains(&"Bool".to_string()), "should include Bool");
        assert!(l.contains(&"String".to_string()), "should include String");
        assert!(l.contains(&"Unit".to_string()), "should include Unit");
    }

    #[test]
    fn primitive_types_have_correct_detail() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let string = items.iter().find(|i| i.label == "String").unwrap();
        assert_eq!(string.detail.as_deref(), Some("primitive type"));
        assert_eq!(string.kind, Some(CompletionItemKind::CLASS));
    }

    #[test]
    fn includes_keywords() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"defn".to_string()));
        assert!(l.contains(&"let".to_string()));
        assert!(l.contains(&"if".to_string()));
        assert!(l.contains(&"match".to_string()));
        assert!(l.contains(&"fn".to_string()));
    }

    #[test]
    fn keywords_have_correct_kind() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let defn = items.iter().find(|i| i.label == "defn").unwrap();
        assert_eq!(defn.kind, Some(CompletionItemKind::KEYWORD));
    }

    #[test]
    fn empty_module_still_has_builtins_types_and_keywords() {
        let (module, tr) = parse_module("");
        let items = completions(&module, Some(&tr), None, None, None);
        let expected_min = BUILTINS.len() + PRIMITIVE_TYPES.len() + KEYWORDS.len();
        assert!(
            items.len() >= expected_min,
            "expected at least {} items, got {}",
            expected_min,
            items.len()
        );
    }

    #[test]
    fn works_without_type_result() {
        let (module, _errors) = weir_parser::parse("(defn foo () 42)");
        let items = completions(&module, None, None, None, None);
        let l = labels(&items);
        assert!(l.contains(&"foo".to_string()));
        // Without type result, detail should be None for user fns
        let foo = items.iter().find(|i| i.label == "foo").unwrap();
        assert!(foo.detail.is_none());
    }

    // ── Scope-aware completion tests ────────────────────────────

    fn setup_with_index(source: &str) -> (Module, TypeCheckResult, SymbolIndex) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let type_result = weir_typeck::check(&module);
        let idx = SymbolIndex::build(&module);
        (module, type_result, idx)
    }

    #[test]
    fn scope_aware_includes_params() {
        let source = "(defn add ((x : i32) (y : i32)) (+ x y))";
        let (module, tr, idx) = setup_with_index(source);
        // Cursor inside the body, at "+" (offset ~33)
        let offset = source.find("+ x").unwrap() as u32;
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), None);
        let l = labels(&items);
        assert!(l.contains(&"x".to_string()), "should include param x");
        assert!(l.contains(&"y".to_string()), "should include param y");
    }

    #[test]
    fn scope_aware_includes_let_bindings() {
        let source = "(defn main () (let ((count 42)) count))";
        let (module, tr, idx) = setup_with_index(source);
        // Cursor in the let body (at "count" reference)
        let body_offset = source.rfind("count").unwrap() as u32;
        let items = completions(&module, Some(&tr), Some(&idx), Some(body_offset), None);
        let l = labels(&items);
        assert!(
            l.contains(&"count".to_string()),
            "should include let binding 'count'"
        );
    }

    #[test]
    fn scope_aware_param_has_variable_kind() {
        let source = "(defn f (x) x)";
        let (module, tr, idx) = setup_with_index(source);
        let offset = source.rfind('x').unwrap() as u32;
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), None);
        let x = items.iter().find(|i| i.label == "x").unwrap();
        assert_eq!(x.kind, Some(CompletionItemKind::VARIABLE));
        assert_eq!(x.detail.as_deref(), Some("parameter"));
    }

    #[test]
    fn scope_aware_let_binding_has_variable_kind() {
        let source = "(defn main () (let ((val 1)) val))";
        let (module, tr, idx) = setup_with_index(source);
        let offset = source.rfind("val").unwrap() as u32;
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), None);
        let val = items.iter().find(|i| i.label == "val").unwrap();
        assert_eq!(val.kind, Some(CompletionItemKind::VARIABLE));
        assert_eq!(val.detail.as_deref(), Some("let binding"));
    }

    #[test]
    fn scope_aware_no_locals_outside_function() {
        // Extra whitespace after the defn to ensure offset is truly outside
        let source = "(defn f (x) x)   ";
        let (module, tr, idx) = setup_with_index(source);
        // Offset 17 is in trailing whitespace, outside any item
        let items = completions(&module, Some(&tr), Some(&idx), Some(17), None);
        let l = labels(&items);
        assert!(
            !l.contains(&"x".to_string()),
            "param x should not appear outside function"
        );
    }

    #[test]
    fn scope_aware_nested_let() {
        let source = "(defn main () (let ((a 1)) (let ((b 2)) (+ a b))))";
        let (module, tr, idx) = setup_with_index(source);
        // Cursor in the inner let body
        let offset = source.find("(+ a b)").unwrap() as u32 + 3;
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), None);
        let l = labels(&items);
        assert!(
            l.contains(&"a".to_string()),
            "should see outer let binding a"
        );
        assert!(
            l.contains(&"b".to_string()),
            "should see inner let binding b"
        );
    }

    #[test]
    fn scope_aware_locals_sorted_first() {
        let source = "(defn main (myvar) myvar)";
        let (module, tr, idx) = setup_with_index(source);
        let offset = source.rfind("myvar").unwrap() as u32;
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), None);
        let myvar = items.iter().find(|i| i.label == "myvar").unwrap();
        assert_eq!(myvar.sort_text.as_deref(), Some("0"));
    }

    // ── Workspace completion tests ──────────────────────────────

    fn make_ws_symbol(name: &str, kind: SymbolKind, uri_str: &str) -> WorkspaceSymbol {
        WorkspaceSymbol {
            name: name.to_string(),
            kind,
            name_span: weir_lexer::Span::new(0, name.len() as u32),
            uri: tower_lsp::lsp_types::Url::parse(uri_str).unwrap(),
        }
    }

    #[test]
    fn workspace_symbols_in_completion() {
        let (module, tr) = parse_module("(defn local-fn () 1)");
        let ws_syms = vec![make_ws_symbol(
            "remote-fn",
            SymbolKind::Function,
            "file:///other.weir",
        )];
        let items = completions(&module, Some(&tr), None, None, Some(&ws_syms));
        let l = labels(&items);
        assert!(l.contains(&"remote-fn".to_string()));
        let remote = items.iter().find(|i| i.label == "remote-fn").unwrap();
        assert_eq!(remote.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(remote.detail.as_deref(), Some("from other.weir"));
    }

    #[test]
    fn workspace_symbols_shadowed_by_local() {
        let (module, tr) = parse_module("(defn foo () 1)");
        let ws_syms = vec![make_ws_symbol(
            "foo",
            SymbolKind::Function,
            "file:///other.weir",
        )];
        let items = completions(&module, Some(&tr), None, None, Some(&ws_syms));
        // "foo" should only appear once (from local module, not workspace)
        let foo_count = items.iter().filter(|i| i.label == "foo").count();
        assert_eq!(foo_count, 1);
    }

    #[test]
    fn workspace_symbols_sorted_after_locals() {
        let source = "(defn main (myvar) myvar)";
        let (module, tr, idx) = setup_with_index(source);
        let offset = source.rfind("myvar").unwrap() as u32;
        let ws_syms = vec![make_ws_symbol(
            "remote-fn",
            SymbolKind::Function,
            "file:///other.weir",
        )];
        let items = completions(&module, Some(&tr), Some(&idx), Some(offset), Some(&ws_syms));
        let local = items.iter().find(|i| i.label == "myvar").unwrap();
        let remote = items.iter().find(|i| i.label == "remote-fn").unwrap();
        assert_eq!(local.sort_text.as_deref(), Some("0"));
        assert_eq!(remote.sort_text.as_deref(), Some("2"));
    }
}
