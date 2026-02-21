# LSP Implementation Status

Tracks what's implemented in `crates/weir-lsp/`, known limitations, and candidate next features.

## Implemented Features

### Text Document Sync (`server.rs`)
- Full document sync (TextDocumentSyncKind::FULL)
- did_open, did_change, did_close
- Workspace file watching for `**/*.weir`

### Diagnostics (`diagnostics.rs`, `server.rs`)
- Multi-stage pipeline: macro expand -> parse -> typecheck
- Errors from all three stages reported with correct spans
- Cross-file aware: type checker receives external symbol names from workspace index so cross-file references don't produce false "undefined variable" errors

### Hover (`hover.rs`)
- Expression type on hover
- Top-level item info (functions, types, structs, classes)
- Function docstrings and parameter lists
- Source fragment preview (truncated at 60 chars)

### Go to Definition (`definition.rs`, `server.rs`)
- In-file: functions, types, variants, let bindings, parameters via SymbolIndex
- Cross-file: falls back to workspace index when in-file resolution fails

### Find References (`definition.rs`, `server.rs`)
- All occurrences within current file
- Cross-file: parses other workspace files to find references
- Include/exclude declaration based on LSP context parameter

### Rename (`definition.rs`, `server.rs`)
- prepare_rename validation
- Scope-aware (shadowed variables only rename inner scope)
- Cross-file: produces WorkspaceEdit spanning multiple URIs

### Completion (`completion.rs`, `server.rs`)
- User-defined top-level symbols (functions, types, variants, structs, classes)
- ~24 builtin functions with type signatures
- Primitive types (i8-i64, u8-u64, f32, f64, Bool, String, Unit)
- 24 language keywords
- Scope-aware locals (parameters, let bindings) sorted before globals
- Cross-file workspace symbols with "from {filename}" detail, sorted after locals

### Signature Help (`signature_help.rs`)
- Parameter hints during function calls
- Active parameter tracking
- Return type in label
- Works for user-defined and builtin functions

### Document Formatting (`formatting.rs`)
- Wadler-Lindig pretty printer with width-aware line breaking
- Understands Weir indentation conventions (defn-like, let-like, body, call)
- Preserves comments (leading and trailing)
- Blank line preservation between forms
- 80-character default width

### Semantic Tokens (`semantic_tokens.rs`)
- 15 token types: function, variable, parameter, type, enum_member, keyword, string, number, operator, comment, macro, interface, method, typeParameter, boolean
- Delta encoding for LSP wire format
- Covers expressions, patterns, type expressions, instance methods

### Inlay Hints (`inlay_hints.rs`)
- Unannotated parameter types
- Let binding types
- Return types (arrow notation)
- Lambda parameter types
- Suppresses hints when annotation is explicit, for Unit returns, and for error types
- Range-aware (only computes hints in visible editor range)

### Document Symbols (`symbols.rs`)
- All top-level definitions with correct SymbolKind
- Variants/fields as children of parent types
- selection_range scoped to name only

### Workspace Symbol Search (`server.rs`)
- Case-insensitive substring matching on query
- Returns all top-level symbols across workspace when query is empty
- Correct SymbolKind mapping and location with file name as container_name

### Workspace Index (`workspace.rs`)
- Discovers all `.weir` files recursively (skips hidden dirs, target, node_modules)
- Package-aware: resolves `weir.pkg` manifests to index all package source files (including dependencies outside the workspace root)
- On-demand package discovery: opening any file triggers indexing of its entire package
- Per-file symbol index with name, kind, span, URI
- Tracks open/closed file status
- Per-file LineIndex for correct span-to-LSP-range mapping

## Known Limitations

- **Workspace indexing races with document analysis**: `update_workspace_symbols` spawns asynchronously while `analyze_and_publish` reads the workspace index for external names. During rapid edits, the workspace index may be stale when the type checker runs, producing spurious "undefined variable" errors for cross-file symbols. These resolve on the next edit/save cycle.
- **Cross-file operations re-parse files on each request** (no persistent cache of parsed workspace files)
- **Completion**: no snippet support, static builtins list, no doc tooltips on completion items
- **Hover**: source preview truncated without surrounding context
- **Formatting**: hard-coded 80-char width, not configurable
- **Rename**: no validation that new name is a valid identifier
- **Semantic tokens**: no modifier flags (mutable, declaration), uppercase heuristic for enum members
- **Inlay hints**: no parameter name hints at call sites
- **Diagnostics**: no warnings, only errors

## Not Yet Implemented

Candidate features roughly ordered by impact:

1. **Code Actions** - quick fixes (e.g., "add type annotation", "import from X")
2. **Folding Ranges** - collapse function bodies, type definitions, comments
4. **Go to Implementation** - navigate from typeclass to its instances
5. **Go to Type Definition** - navigate from expression to its type's definition
6. **Call Hierarchy** - incoming/outgoing call graphs
7. **Code Lens** - inline "N references" annotations
8. **Linked Editing Range** - simultaneous editing of matching identifiers
9. **Selection Range** - smart expand/shrink selection based on AST
10. **On Type Formatting** - auto-format after typing specific characters
