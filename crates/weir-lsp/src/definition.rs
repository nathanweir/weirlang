use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Url};

use crate::document::LineIndex;
use crate::index::SymbolIndex;

/// Find the definition of the symbol at `offset`. Same-file only.
pub fn goto_definition(
    symbol_index: &SymbolIndex,
    line_index: &LineIndex,
    offset: u32,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let res = symbol_index.resolve_at(offset)?;

    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range: line_index.span_to_range(res.def_name_span),
    }))
}

/// Find all references to the symbol at `offset`. Same-file only.
pub fn find_references(
    symbol_index: &SymbolIndex,
    line_index: &LineIndex,
    offset: u32,
    uri: &Url,
    include_declaration: bool,
) -> Vec<Location> {
    let res = match symbol_index.resolve_at(offset) {
        Some(r) => r,
        None => return vec![],
    };

    res.all_name_spans
        .iter()
        .filter(|span| include_declaration || **span != res.def_name_span)
        .map(|span| Location {
            uri: uri.clone(),
            range: line_index.span_to_range(*span),
        })
        .collect()
}

/// Prepare rename: check that the symbol at `offset` exists and is renameable.
pub fn prepare_rename(
    symbol_index: &SymbolIndex,
    line_index: &LineIndex,
    offset: u32,
) -> Option<tower_lsp::lsp_types::Range> {
    let res = symbol_index.resolve_at(offset)?;

    // Find the specific occurrence at this offset to return its range
    for span in &res.all_name_spans {
        if span.start <= offset && offset < span.end {
            return Some(line_index.span_to_range(*span));
        }
    }

    // Fallback: return the definition name span
    Some(line_index.span_to_range(res.def_name_span))
}

/// Compute rename edits: replace all occurrences of the symbol at `offset` with `new_name`.
pub fn rename(
    symbol_index: &SymbolIndex,
    line_index: &LineIndex,
    offset: u32,
    uri: &Url,
    new_name: &str,
) -> Option<tower_lsp::lsp_types::WorkspaceEdit> {
    let res = symbol_index.resolve_at(offset)?;

    let edits: Vec<tower_lsp::lsp_types::TextEdit> = res
        .all_name_spans
        .iter()
        .map(|span| tower_lsp::lsp_types::TextEdit {
            range: line_index.span_to_range(*span),
            new_text: new_name.to_string(),
        })
        .collect();

    if edits.is_empty() {
        return None;
    }

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(tower_lsp::lsp_types::WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tower_lsp::lsp_types::Position;

    fn setup(source: &str) -> (SymbolIndex, LineIndex, Url) {
        let (module, errors) = weir_parser::parse(source);
        assert!(errors.is_empty(), "parse errors: {:?}", errors);
        let idx = SymbolIndex::build(&module);
        let line_index = LineIndex::new(source);
        let uri = Url::parse("file:///test.weir").unwrap();
        (idx, line_index, uri)
    }

    #[test]
    fn goto_definition_from_call_site() {
        // "(defn foo () 42) (defn main () (foo))"
        //  0123456789...                   ^32
        let source = "(defn foo () 42) (defn main () (foo))";
        let (idx, li, uri) = setup(source);
        let result = goto_definition(&idx, &li, 32, &uri).unwrap();
        if let GotoDefinitionResponse::Scalar(loc) = result {
            // Should point to "foo" definition at offset 6..9
            assert_eq!(
                loc.range.start,
                Position {
                    line: 0,
                    character: 6
                }
            );
            assert_eq!(
                loc.range.end,
                Position {
                    line: 0,
                    character: 9
                }
            );
        } else {
            panic!("expected scalar response");
        }
    }

    #[test]
    fn goto_definition_from_definition_site() {
        let source = "(defn foo () 42)";
        let (idx, li, uri) = setup(source);
        // Resolve at "foo" definition itself (offset 6)
        let result = goto_definition(&idx, &li, 6, &uri).unwrap();
        if let GotoDefinitionResponse::Scalar(loc) = result {
            assert_eq!(
                loc.range.start,
                Position {
                    line: 0,
                    character: 6
                }
            );
            assert_eq!(
                loc.range.end,
                Position {
                    line: 0,
                    character: 9
                }
            );
        } else {
            panic!("expected scalar response");
        }
    }

    #[test]
    fn goto_definition_parameter() {
        // "(defn f (x) x)"
        //  0123456789...^13
        let source = "(defn f (x) x)";
        let (idx, li, uri) = setup(source);
        // x ref at offset 12
        let result = goto_definition(&idx, &li, 12, &uri).unwrap();
        if let GotoDefinitionResponse::Scalar(loc) = result {
            // Should point to param x at offset 9
            assert_eq!(
                loc.range.start,
                Position {
                    line: 0,
                    character: 9
                }
            );
            assert_eq!(
                loc.range.end,
                Position {
                    line: 0,
                    character: 10
                }
            );
        } else {
            panic!("expected scalar response");
        }
    }

    #[test]
    fn goto_definition_at_whitespace_returns_none() {
        let source = "(defn foo () 42)";
        let (idx, li, uri) = setup(source);
        // offset 5 is the space before "foo"
        assert!(goto_definition(&idx, &li, 5, &uri).is_none());
    }

    #[test]
    fn find_references_includes_declaration() {
        // "(defn foo () 42) (defn main () (foo))"
        let source = "(defn foo () 42) (defn main () (foo))";
        let (idx, li, uri) = setup(source);
        let refs = find_references(&idx, &li, 32, &uri, true);
        assert_eq!(refs.len(), 2); // def + call
    }

    #[test]
    fn find_references_excludes_declaration() {
        let source = "(defn foo () 42) (defn main () (foo))";
        let (idx, li, uri) = setup(source);
        let refs = find_references(&idx, &li, 32, &uri, false);
        assert_eq!(refs.len(), 1); // call only
    }

    #[test]
    fn find_references_at_whitespace_returns_empty() {
        let source = "(defn foo () 42)";
        let (idx, li, uri) = setup(source);
        let refs = find_references(&idx, &li, 5, &uri, true);
        assert!(refs.is_empty());
    }

    #[test]
    fn prepare_rename_returns_name_span() {
        let source = "(defn foo () 42)";
        let (idx, li, _) = setup(source);
        let range = prepare_rename(&idx, &li, 6).unwrap();
        assert_eq!(
            range.start,
            Position {
                line: 0,
                character: 6
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 0,
                character: 9
            }
        );
    }

    #[test]
    fn prepare_rename_at_call_site() {
        let source = "(defn foo () 42) (defn main () (foo))";
        let (idx, li, _) = setup(source);
        // Call site "foo" at offset 32
        let range = prepare_rename(&idx, &li, 32).unwrap();
        assert_eq!(
            range.start,
            Position {
                line: 0,
                character: 32
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 0,
                character: 35
            }
        );
    }

    #[test]
    fn prepare_rename_at_whitespace_returns_none() {
        let source = "(defn foo () 42)";
        let (idx, li, _) = setup(source);
        assert!(prepare_rename(&idx, &li, 5).is_none());
    }

    #[test]
    fn rename_function() {
        let source = "(defn foo () 42) (defn main () (foo))";
        let (idx, li, uri) = setup(source);
        let edit = rename(&idx, &li, 6, &uri, "bar").unwrap();
        let changes = edit.changes.unwrap();
        let edits = changes.get(&uri).unwrap();
        assert_eq!(edits.len(), 2);
        for e in edits {
            assert_eq!(e.new_text, "bar");
        }
    }

    #[test]
    fn rename_shadowed_variable_only_affects_inner_scope() {
        // "(defn f () (let ((x 1)) (let ((x 2)) x)))"
        //  0         1         2         3
        //  0123456789012345678901234567890123456789012
        //                    ^18          ^31   ^37
        let source = "(defn f () (let ((x 1)) (let ((x 2)) x)))";
        let (idx, li, uri) = setup(source);
        // Inner x ref at offset 37
        let edit = rename(&idx, &li, 37, &uri, "y").unwrap();
        let changes = edit.changes.unwrap();
        let edits = changes.get(&uri).unwrap();
        // Should only rename inner x (def at 31) + ref at 37, NOT outer x at 18
        assert_eq!(edits.len(), 2);
        for e in edits {
            assert_ne!(
                e.range.start,
                Position {
                    line: 0,
                    character: 18
                },
                "outer x should not be renamed"
            );
        }
    }

    #[test]
    fn rename_let_binding_includes_all_refs() {
        // "(defn f () (let ((x 1)) (+ x x)))"
        //  0         1         2         3
        let source = "(defn f () (let ((x 1)) (+ x x)))";
        let (idx, li, uri) = setup(source);
        // x at offset 18
        let edit = rename(&idx, &li, 18, &uri, "y").unwrap();
        let changes = edit.changes.unwrap();
        let edits = changes.get(&uri).unwrap();
        // def + 2 refs = 3
        assert_eq!(edits.len(), 3);
    }
}
