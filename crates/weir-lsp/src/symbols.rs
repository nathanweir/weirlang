use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};
use weir_ast::{Item, Module};

use crate::document::LineIndex;

#[allow(deprecated)] // DocumentSymbol.deprecated is deprecated but required by the struct
pub fn document_symbols(module: &Module, line_index: &LineIndex) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();

    for (item, span) in &module.items {
        let range = line_index.span_to_range(*span);
        match item {
            Item::Defn(d) => {
                let selection_range = line_index.span_to_range(d.name_span);
                symbols.push(DocumentSymbol {
                    name: d.name.to_string(),
                    detail: None,
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: None,
                });
            }
            Item::Deftype(d) => {
                let selection_range = line_index.span_to_range(d.name_span);
                let children: Vec<DocumentSymbol> = d
                    .variants
                    .iter()
                    .map(|v| {
                        let vrange = line_index.span_to_range(v.span);
                        let vsel = line_index.span_to_range(v.name_span);
                        DocumentSymbol {
                            name: v.name.to_string(),
                            detail: None,
                            kind: SymbolKind::ENUM_MEMBER,
                            tags: None,
                            deprecated: None,
                            range: vrange,
                            selection_range: vsel,
                            children: None,
                        }
                    })
                    .collect();
                symbols.push(DocumentSymbol {
                    name: d.name.to_string(),
                    detail: None,
                    kind: SymbolKind::ENUM,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            Item::Defstruct(d) => {
                let selection_range = line_index.span_to_range(d.name_span);
                let children: Vec<DocumentSymbol> = d
                    .fields
                    .iter()
                    .map(|f| {
                        let frange = line_index.span_to_range(f.span);
                        let fsel = line_index.span_to_range(f.name_span);
                        DocumentSymbol {
                            name: f.name.to_string(),
                            detail: None,
                            kind: SymbolKind::FIELD,
                            tags: None,
                            deprecated: None,
                            range: frange,
                            selection_range: fsel,
                            children: None,
                        }
                    })
                    .collect();
                symbols.push(DocumentSymbol {
                    name: d.name.to_string(),
                    detail: None,
                    kind: SymbolKind::STRUCT,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            Item::Defclass(d) => {
                let selection_range = line_index.span_to_range(d.name_span);
                let children: Vec<DocumentSymbol> = d
                    .methods
                    .iter()
                    .map(|m| {
                        let mrange = line_index.span_to_range(m.span);
                        let msel = line_index.span_to_range(m.name_span);
                        DocumentSymbol {
                            name: m.name.to_string(),
                            detail: None,
                            kind: SymbolKind::METHOD,
                            tags: None,
                            deprecated: None,
                            range: mrange,
                            selection_range: msel,
                            children: None,
                        }
                    })
                    .collect();
                symbols.push(DocumentSymbol {
                    name: d.name.to_string(),
                    detail: None,
                    kind: SymbolKind::INTERFACE,
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range,
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                });
            }
            _ => {}
        }
    }

    symbols
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::Position;

    fn setup(source: &str) -> (Module, LineIndex) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let line_index = LineIndex::new(source);
        (module, line_index)
    }

    #[allow(deprecated)]
    fn assert_selection_within_range(sym: &DocumentSymbol) {
        // selection_range must be contained within range
        assert!(
            sym.selection_range.start.line >= sym.range.start.line
                || (sym.selection_range.start.line == sym.range.start.line
                    && sym.selection_range.start.character >= sym.range.start.character),
            "selection_range start {:?} is before range start {:?} for '{}'",
            sym.selection_range.start,
            sym.range.start,
            sym.name
        );
        assert!(
            sym.selection_range.end.line <= sym.range.end.line
                || (sym.selection_range.end.line == sym.range.end.line
                    && sym.selection_range.end.character <= sym.range.end.character),
            "selection_range end {:?} is after range end {:?} for '{}'",
            sym.selection_range.end,
            sym.range.end,
            sym.name
        );
    }

    #[test]
    #[allow(deprecated)]
    fn function_symbol() {
        let source = "(defn foo () 42)";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "foo");
        assert_eq!(syms[0].kind, SymbolKind::FUNCTION);
        // selection_range should be just "foo" (3 chars), not the whole form
        assert_eq!(
            syms[0].selection_range.start,
            Position {
                line: 0,
                character: 6
            }
        );
        assert_eq!(
            syms[0].selection_range.end,
            Position {
                line: 0,
                character: 9
            }
        );
        assert_selection_within_range(&syms[0]);
    }

    #[test]
    #[allow(deprecated)]
    fn type_with_variants() {
        let source = "(deftype Color Red Green Blue)";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Color");
        assert_eq!(syms[0].kind, SymbolKind::ENUM);
        assert_selection_within_range(&syms[0]);

        let children = syms[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].name, "Red");
        assert_eq!(children[0].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(children[1].name, "Green");
        assert_eq!(children[2].name, "Blue");
        // Variant selection_range should be name-only
        for child in children {
            assert_selection_within_range(child);
        }
    }

    #[test]
    #[allow(deprecated)]
    fn struct_with_fields() {
        let source = "(defstruct Point (x : f64) (y : f64))";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Point");
        assert_eq!(syms[0].kind, SymbolKind::STRUCT);
        assert_selection_within_range(&syms[0]);
        // selection_range should be just "Point"
        assert_eq!(
            syms[0].selection_range.start,
            Position {
                line: 0,
                character: 11
            }
        );
        assert_eq!(
            syms[0].selection_range.end,
            Position {
                line: 0,
                character: 16
            }
        );

        let children = syms[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "x");
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[1].name, "y");
        // Field selection_range should be name-only
        for child in children {
            assert_selection_within_range(child);
        }
    }

    #[test]
    #[allow(deprecated)]
    fn class_with_methods() {
        let source = "(defclass (Show 'a) (show : (Fn ['a] String)))";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 1);
        assert_eq!(syms[0].name, "Show");
        assert_eq!(syms[0].kind, SymbolKind::INTERFACE);
        assert_selection_within_range(&syms[0]);

        let children = syms[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 1);
        assert_eq!(children[0].name, "show");
        assert_eq!(children[0].kind, SymbolKind::METHOD);
        assert_selection_within_range(&children[0]);
    }

    #[test]
    #[allow(deprecated)]
    fn multiple_items() {
        let source = "(defn foo () 42) (defn bar () 0)";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 2);
        assert_eq!(syms[0].name, "foo");
        assert_eq!(syms[1].name, "bar");
    }

    #[test]
    fn empty_module_returns_no_symbols() {
        let source = "";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert!(syms.is_empty());
    }

    #[test]
    #[allow(deprecated)]
    fn selection_range_is_subset_of_range_for_all_item_types() {
        let source = "\
(defn greet () 42)
(deftype Color Red Green)
(defstruct Point (x : f64))
(defclass (Eq 'a) (eq : (Fn ['a 'a] Bool)))";
        let (module, li) = setup(source);
        let syms = document_symbols(&module, &li);
        assert_eq!(syms.len(), 4);
        for sym in &syms {
            assert_selection_within_range(sym);
            if let Some(children) = &sym.children {
                for child in children {
                    assert_selection_within_range(child);
                }
            }
        }
    }
}
