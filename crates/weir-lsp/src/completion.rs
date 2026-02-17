use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};
use weir_ast::{Item, Module};

/// Collect completion items from module definitions, builtins, and keywords.
pub fn completions(module: &Module) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // Top-level definitions
    for (item, _span) in &module.items {
        match item {
            Item::Defn(d) => {
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    ..Default::default()
                });
            }
            Item::Deftype(d) => {
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::ENUM),
                    ..Default::default()
                });
                // Variant constructors
                for v in &d.variants {
                    items.push(CompletionItem {
                        label: v.name.to_string(),
                        kind: Some(CompletionItemKind::ENUM_MEMBER),
                        ..Default::default()
                    });
                }
            }
            Item::Defstruct(d) => {
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::STRUCT),
                    ..Default::default()
                });
            }
            Item::Defclass(d) => {
                items.push(CompletionItem {
                    label: d.name.to_string(),
                    kind: Some(CompletionItemKind::INTERFACE),
                    ..Default::default()
                });
            }
            _ => {}
        }
    }

    // Builtin functions
    for name in BUILTINS {
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("builtin".into()),
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

    items
}

const BUILTINS: &[&str] = &[
    "+", "-", "*", "/", "mod", "<", ">", "<=", ">=", "=", "!=", "not", "and", "or", "println",
    "print", "str", "len", "nth", "append", "type-of",
];

const PRIMITIVE_TYPES: &[&str] = &["i32", "i64", "f32", "f64", "Bool", "String", "Unit"];

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

    fn parse_module(source: &str) -> Module {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        module
    }

    fn labels(items: &[CompletionItem]) -> Vec<String> {
        items.iter().map(|i| i.label.clone()).collect()
    }

    #[test]
    fn includes_user_defined_functions() {
        let module = parse_module("(defn foo () 42) (defn bar () 0)");
        let items = completions(&module);
        let l = labels(&items);
        assert!(l.contains(&"foo".to_string()));
        assert!(l.contains(&"bar".to_string()));
    }

    #[test]
    fn includes_user_defined_types_and_variants() {
        let module = parse_module("(deftype Color Red Green Blue)");
        let items = completions(&module);
        let l = labels(&items);
        assert!(l.contains(&"Color".to_string()));
        assert!(l.contains(&"Red".to_string()));
        assert!(l.contains(&"Green".to_string()));
        assert!(l.contains(&"Blue".to_string()));
    }

    #[test]
    fn user_type_has_correct_kind() {
        let module = parse_module("(deftype Color Red)");
        let items = completions(&module);
        let color = items.iter().find(|i| i.label == "Color").unwrap();
        assert_eq!(color.kind, Some(CompletionItemKind::ENUM));
        let red = items.iter().find(|i| i.label == "Red").unwrap();
        assert_eq!(red.kind, Some(CompletionItemKind::ENUM_MEMBER));
    }

    #[test]
    fn includes_user_defined_structs() {
        let module = parse_module("(defstruct Point (x : f64) (y : f64))");
        let items = completions(&module);
        let point = items.iter().find(|i| i.label == "Point").unwrap();
        assert_eq!(point.kind, Some(CompletionItemKind::STRUCT));
    }

    #[test]
    fn includes_user_defined_classes() {
        let module = parse_module("(defclass (Show 'a) (show : (Fn ['a] String)))");
        let items = completions(&module);
        let show = items.iter().find(|i| i.label == "Show").unwrap();
        assert_eq!(show.kind, Some(CompletionItemKind::INTERFACE));
    }

    #[test]
    fn includes_builtin_functions() {
        let module = parse_module("");
        let items = completions(&module);
        let l = labels(&items);
        assert!(l.contains(&"print".to_string()));
        assert!(l.contains(&"println".to_string()));
        assert!(l.contains(&"+".to_string()));
        assert!(l.contains(&"len".to_string()));
    }

    #[test]
    fn builtins_have_correct_detail() {
        let module = parse_module("");
        let items = completions(&module);
        let print = items.iter().find(|i| i.label == "print").unwrap();
        assert_eq!(print.detail.as_deref(), Some("builtin"));
    }

    #[test]
    fn includes_primitive_types() {
        let module = parse_module("");
        let items = completions(&module);
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
        let module = parse_module("");
        let items = completions(&module);
        let string = items.iter().find(|i| i.label == "String").unwrap();
        assert_eq!(string.detail.as_deref(), Some("primitive type"));
        assert_eq!(string.kind, Some(CompletionItemKind::CLASS));
    }

    #[test]
    fn includes_keywords() {
        let module = parse_module("");
        let items = completions(&module);
        let l = labels(&items);
        assert!(l.contains(&"defn".to_string()));
        assert!(l.contains(&"let".to_string()));
        assert!(l.contains(&"if".to_string()));
        assert!(l.contains(&"match".to_string()));
        assert!(l.contains(&"fn".to_string()));
    }

    #[test]
    fn keywords_have_correct_kind() {
        let module = parse_module("");
        let items = completions(&module);
        let defn = items.iter().find(|i| i.label == "defn").unwrap();
        assert_eq!(defn.kind, Some(CompletionItemKind::KEYWORD));
    }

    #[test]
    fn empty_module_still_has_builtins_types_and_keywords() {
        let module = parse_module("");
        let items = completions(&module);
        // At minimum: BUILTINS + PRIMITIVE_TYPES + KEYWORDS
        let expected_min = BUILTINS.len() + PRIMITIVE_TYPES.len() + KEYWORDS.len();
        assert!(
            items.len() >= expected_min,
            "expected at least {} items, got {}",
            expected_min,
            items.len()
        );
    }
}
