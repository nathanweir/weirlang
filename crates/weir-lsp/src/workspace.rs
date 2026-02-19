use std::collections::HashMap;
use std::path::PathBuf;

use tower_lsp::lsp_types::Url;
use weir_ast::Item;
use weir_lexer::Span;

use crate::document::LineIndex;
use crate::expand;
use crate::index::SymbolKind;

/// A top-level symbol discovered from a workspace file.
#[derive(Debug, Clone)]
pub struct WorkspaceSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub name_span: Span,
    pub uri: Url,
}

/// Per-file info stored in the workspace index.
#[derive(Debug)]
pub struct WorkspaceFileInfo {
    pub uri: Url,
    pub symbols: Vec<WorkspaceSymbol>,
    pub line_index: LineIndex,
    pub is_open: bool,
}

/// Workspace-wide index of top-level symbols from all `.weir` files.
#[derive(Debug)]
pub struct WorkspaceIndex {
    pub root: PathBuf,
    pub files: HashMap<Url, WorkspaceFileInfo>,
}

impl WorkspaceIndex {
    pub fn new(root: PathBuf) -> Self {
        Self {
            root,
            files: HashMap::new(),
        }
    }

    /// Walk the workspace root and return URIs for all `.weir` files.
    pub fn discover_files(&self) -> Vec<PathBuf> {
        let mut result = Vec::new();
        self.walk_dir(&self.root, &mut result);
        result
    }

    fn walk_dir(&self, dir: &std::path::Path, out: &mut Vec<PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                // Skip hidden directories and common non-source dirs
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if name.starts_with('.') || name == "target" || name == "node_modules" {
                        continue;
                    }
                }
                self.walk_dir(&path, out);
            } else if path.extension().and_then(|e| e.to_str()) == Some("weir") {
                out.push(path);
            }
        }
    }

    /// Analyze a file's text and store its top-level symbols.
    pub fn analyze_file(&mut self, uri: Url, text: &str, is_open: bool) {
        let (symbols, line_index) = extract_symbols(&uri, text);
        self.files.insert(
            uri.clone(),
            WorkspaceFileInfo {
                uri,
                symbols,
                line_index,
                is_open,
            },
        );
    }

    /// Remove a file from the index.
    pub fn remove_file(&mut self, uri: &Url) {
        self.files.remove(uri);
    }

    /// Find all workspace symbols matching a name.
    pub fn find_symbols_by_name(&self, name: &str) -> Vec<&WorkspaceSymbol> {
        let mut results = Vec::new();
        for file_info in self.files.values() {
            for sym in &file_info.symbols {
                if sym.name == name {
                    results.push(sym);
                }
            }
        }
        results
    }

    /// Return all top-level symbols across the workspace.
    pub fn all_top_level_symbols(&self) -> Vec<&WorkspaceSymbol> {
        self.files.values().flat_map(|f| f.symbols.iter()).collect()
    }

    /// Get the LineIndex for a file, if indexed.
    pub fn line_index_for(&self, uri: &Url) -> Option<&LineIndex> {
        self.files.get(uri).map(|f| &f.line_index)
    }
}

/// Extract top-level symbols from source text.
/// Returns symbols with spans that match the returned LineIndex.
fn extract_symbols(uri: &Url, text: &str) -> (Vec<WorkspaceSymbol>, LineIndex) {
    let expanded = expand::expand_source(text);
    let (module, _errors) = weir_parser::parse(&expanded.source);
    let line_index = expanded.line_index;

    let mut symbols = Vec::new();

    for (item, _span) in &module.items {
        match item {
            Item::Defn(d) => {
                symbols.push(WorkspaceSymbol {
                    name: d.name.to_string(),
                    kind: SymbolKind::Function,
                    name_span: d.name_span,
                    uri: uri.clone(),
                });
            }
            Item::Deftype(d) => {
                symbols.push(WorkspaceSymbol {
                    name: d.name.to_string(),
                    kind: SymbolKind::Type,
                    name_span: d.name_span,
                    uri: uri.clone(),
                });
                for v in &d.variants {
                    symbols.push(WorkspaceSymbol {
                        name: v.name.to_string(),
                        kind: SymbolKind::Variant,
                        name_span: v.name_span,
                        uri: uri.clone(),
                    });
                }
            }
            Item::Defstruct(d) => {
                symbols.push(WorkspaceSymbol {
                    name: d.name.to_string(),
                    kind: SymbolKind::Struct,
                    name_span: d.name_span,
                    uri: uri.clone(),
                });
            }
            Item::Defclass(d) => {
                symbols.push(WorkspaceSymbol {
                    name: d.name.to_string(),
                    kind: SymbolKind::Class,
                    name_span: d.name_span,
                    uri: uri.clone(),
                });
            }
            Item::Instance(inst) => {
                for method in &inst.methods {
                    symbols.push(WorkspaceSymbol {
                        name: method.name.to_string(),
                        kind: SymbolKind::Function,
                        name_span: method.name_span,
                        uri: uri.clone(),
                    });
                }
            }
            _ => {}
        }
    }

    (symbols, line_index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn make_temp_dir(test_name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("weir_lsp_test_{}", test_name));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn discover_files_finds_weir_files() {
        let root = make_temp_dir("discover_files");

        // Create nested .weir files
        fs::write(root.join("main.weir"), "(defn main () 42)").unwrap();
        let sub = root.join("lib");
        fs::create_dir(&sub).unwrap();
        fs::write(sub.join("utils.weir"), "(defn helper () 0)").unwrap();

        // Non-.weir file should be ignored
        fs::write(root.join("readme.txt"), "hello").unwrap();

        let index = WorkspaceIndex::new(root.clone());
        let files = index.discover_files();

        assert_eq!(files.len(), 2);
        assert!(files.iter().any(|p| p.ends_with("main.weir")));
        assert!(files.iter().any(|p| p.ends_with("utils.weir")));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn discover_files_skips_hidden_dirs() {
        let root = make_temp_dir("discover_hidden");

        fs::write(root.join("main.weir"), "").unwrap();
        let hidden = root.join(".hidden");
        fs::create_dir(&hidden).unwrap();
        fs::write(hidden.join("secret.weir"), "").unwrap();

        let index = WorkspaceIndex::new(root.clone());
        let files = index.discover_files();

        assert_eq!(files.len(), 1);

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn analyze_file_extracts_symbols() {
        let uri = Url::parse("file:///test.weir").unwrap();
        let source =
            "(defn foo () 42)\n(deftype Color Red Green Blue)\n(defstruct Point (x : f64))";
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri.clone(), source, false);

        let file = index.files.get(&uri).unwrap();
        let names: Vec<&str> = file.symbols.iter().map(|s| s.name.as_str()).collect();
        assert!(names.contains(&"foo"));
        assert!(names.contains(&"Color"));
        assert!(names.contains(&"Red"));
        assert!(names.contains(&"Green"));
        assert!(names.contains(&"Blue"));
        assert!(names.contains(&"Point"));
    }

    #[test]
    fn find_symbols_by_name_multi_file() {
        let uri1 = Url::parse("file:///a.weir").unwrap();
        let uri2 = Url::parse("file:///b.weir").unwrap();
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri1, "(defn foo () 1)", false);
        index.analyze_file(uri2, "(defn bar () (foo))", false);

        let results = index.find_symbols_by_name("foo");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "foo");

        let results = index.find_symbols_by_name("bar");
        assert_eq!(results.len(), 1);
    }

    #[test]
    fn remove_file_clears_symbols() {
        let uri = Url::parse("file:///test.weir").unwrap();
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri.clone(), "(defn foo () 1)", false);
        assert!(index.files.contains_key(&uri));

        index.remove_file(&uri);
        assert!(!index.files.contains_key(&uri));
        assert!(index.find_symbols_by_name("foo").is_empty());
    }

    #[test]
    fn file_with_pub_and_private() {
        let uri = Url::parse("file:///test.weir").unwrap();
        let source = "(defn pub-fn () 1)\n(defn priv-fn () 2)";
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri.clone(), source, false);

        // Both should be indexed (import-aware filtering is deferred)
        let file = index.files.get(&uri).unwrap();
        assert_eq!(file.symbols.len(), 2);
    }

    #[test]
    fn all_top_level_symbols_aggregates() {
        let uri1 = Url::parse("file:///a.weir").unwrap();
        let uri2 = Url::parse("file:///b.weir").unwrap();
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri1, "(defn foo () 1)", false);
        index.analyze_file(uri2, "(defn bar () 2)\n(defn baz () 3)", false);

        let all = index.all_top_level_symbols();
        assert_eq!(all.len(), 3);
    }

    #[test]
    fn symbol_spans_point_to_correct_source_location() {
        // Multi-line source: verify the span for each symbol actually
        // corresponds to the symbol name in the original text.
        let source = ";; a comment\n(defn square ((x : i32)) : i32\n  (* x x))\n\n(deftype Color\n  Red\n  Green\n  Blue)\n";
        let uri = Url::parse("file:///test.weir").unwrap();
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri.clone(), source, false);

        let file = index.files.get(&uri).unwrap();
        for sym in &file.symbols {
            let start = sym.name_span.start as usize;
            let end = sym.name_span.end as usize;
            let slice = &source[start..end];
            assert_eq!(
                slice, sym.name,
                "span {}..{} should be '{}' but was '{}'",
                start, end, sym.name, slice
            );
        }
    }

    #[test]
    fn symbol_span_to_lsp_range_is_correct() {
        // Verify the line_index stored alongside symbols produces correct
        // LSP positions (this catches the original bug where the line_index
        // was built from original text but spans came from expanded source).
        let source = ";; comment\n(defn helper () 42)\n";
        let uri = Url::parse("file:///test.weir").unwrap();
        let mut index = WorkspaceIndex::new(PathBuf::from("/tmp"));
        index.analyze_file(uri.clone(), source, false);

        let file = index.files.get(&uri).unwrap();
        let sym = file.symbols.iter().find(|s| s.name == "helper").unwrap();

        // "helper" is on line 1 (0-indexed), starts at character 6
        let range = file.line_index.span_to_range(sym.name_span);
        assert_eq!(range.start.line, 1, "should be on line 1");
        assert_eq!(range.start.character, 6, "should start at character 6");
        assert_eq!(
            range.end.character - range.start.character,
            6,
            "should span 6 characters ('helper')"
        );
    }
}
