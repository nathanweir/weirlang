use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::completion;
use crate::definition;
use crate::diagnostics;
use crate::document::Document;
use crate::expand;
use crate::formatting;
use crate::hover;
use crate::index::SymbolIndex;
use crate::inlay_hints;
use crate::semantic_tokens;
use crate::signature_help;
use crate::symbols;
use crate::workspace::WorkspaceIndex;

pub struct WeirLspBackend {
    pub client: Client,
    pub documents: Mutex<HashMap<Url, Document>>,
    pub workspace: Arc<tokio::sync::RwLock<WorkspaceIndex>>,
}

impl WeirLspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
            workspace: Arc::new(tokio::sync::RwLock::new(
                WorkspaceIndex::new(PathBuf::new()),
            )),
        }
    }

    async fn analyze_and_publish(&self, uri: Url) {
        // 1. Gather external symbol names from other workspace files (async lock)
        let external_names = {
            let ws = self.workspace.read().await;
            ws.all_top_level_symbols()
                .into_iter()
                .filter(|s| s.uri != uri)
                .map(|s| smol_str::SmolStr::from(s.name.as_str()))
                .collect::<std::collections::HashSet<_>>()
        };

        // 2. Run analysis with external names — MutexGuard confined to this block
        let result = {
            let mut docs = self.documents.lock().unwrap();
            let doc = match docs.get_mut(&uri) {
                Some(d) => d,
                None => return,
            };
            let (analysis, diagnostics) =
                diagnostics::analyze(&doc.text, &doc.line_index, &external_names);
            let version = doc.version;
            doc.analysis = Some(analysis);
            Some((diagnostics, version))
        };

        // 3. Publish diagnostics
        if let Some((diagnostics, version)) = result {
            self.client
                .publish_diagnostics(uri, diagnostics, Some(version))
                .await;
        }
    }

    /// Update the workspace index with the current file's symbols.
    fn update_workspace_symbols(&self, uri: Url, text: &str, is_open: bool) {
        let workspace = self.workspace.clone();
        let text = text.to_string();
        tokio::spawn(async move {
            let mut ws = workspace.write().await;
            ws.analyze_file(uri, &text, is_open);
        });
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for WeirLspBackend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Capture workspace root
        if let Some(root_uri) = params.root_uri {
            if let Ok(path) = root_uri.to_file_path() {
                let mut ws = self.workspace.write().await;
                ws.root = path;
            }
        }

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["(".into(), ".".into()]),
                    ..Default::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".into(), " ".into()]),
                    retrigger_characters: Some(vec![" ".into()]),
                    work_done_progress_options: Default::default(),
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_tokens::legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            work_done_progress_options: Default::default(),
                        },
                    ),
                ),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "weir-lsp".into(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "weir-lsp initialized")
            .await;

        // Register for workspace file watching
        let registration = Registration {
            id: "workspace/didChangeWatchedFiles".into(),
            method: "workspace/didChangeWatchedFiles".into(),
            register_options: Some(
                serde_json::to_value(DidChangeWatchedFilesRegistrationOptions {
                    watchers: vec![FileSystemWatcher {
                        glob_pattern: GlobPattern::String("**/*.weir".into()),
                        kind: Some(WatchKind::all()),
                    }],
                })
                .unwrap(),
            ),
        };
        let _ = self.client.register_capability(vec![registration]).await;

        // Background task: discover and analyze all workspace files
        let workspace = self.workspace.clone();
        let client = self.client.clone();
        tokio::spawn(async move {
            let paths = {
                let ws = workspace.read().await;
                ws.discover_files()
            };

            client
                .log_message(
                    MessageType::INFO,
                    format!("Indexing {} workspace files", paths.len()),
                )
                .await;

            for path in paths {
                let uri = match Url::from_file_path(&path) {
                    Ok(u) => u,
                    Err(_) => continue,
                };
                let text = match std::fs::read_to_string(&path) {
                    Ok(t) => t,
                    Err(_) => continue,
                };
                let mut ws = workspace.write().await;
                ws.analyze_file(uri, &text, false);
            }

            client
                .log_message(MessageType::INFO, "Workspace indexing complete")
                .await;
        });
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let text = params.text_document.text.clone();
        let doc = Document::new(params.text_document.text, params.text_document.version);
        self.documents.lock().unwrap().insert(uri.clone(), doc);
        self.analyze_and_publish(uri.clone()).await;
        self.update_workspace_symbols(uri, &text, true);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        // Full text sync — take the last change
        if let Some(change) = params.content_changes.into_iter().last() {
            let text = change.text.clone();
            let doc = Document::new(change.text, params.text_document.version);
            self.documents.lock().unwrap().insert(uri.clone(), doc);
            self.analyze_and_publish(uri.clone()).await;
            self.update_workspace_symbols(uri, &text, true);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.documents.lock().unwrap().remove(&uri);

        // Mark as not open in workspace; re-analyze from disk to keep symbols current
        let workspace = self.workspace.clone();
        let uri_for_ws = uri.clone();
        tokio::spawn(async move {
            if let Ok(path) = uri_for_ws.to_file_path() {
                if let Ok(text) = std::fs::read_to_string(&path) {
                    let mut ws = workspace.write().await;
                    ws.analyze_file(uri_for_ws, &text, false);
                    return;
                }
            }
            // File doesn't exist on disk anymore — remove from index
            let mut ws = workspace.write().await;
            ws.remove_file(&uri_for_ws);
        });

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        let workspace = self.workspace.clone();
        tokio::spawn(async move {
            let mut ws = workspace.write().await;
            for change in params.changes {
                match change.typ {
                    FileChangeType::CREATED | FileChangeType::CHANGED => {
                        // Only re-analyze if the file is not currently open
                        if ws.files.get(&change.uri).is_none_or(|f| !f.is_open) {
                            if let Ok(path) = change.uri.to_file_path() {
                                if let Ok(text) = std::fs::read_to_string(&path) {
                                    ws.analyze_file(change.uri, &text, false);
                                }
                            }
                        }
                    }
                    FileChangeType::DELETED => {
                        ws.remove_file(&change.uri);
                    }
                    _ => {}
                }
            }
        });
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = analysis.expanded_line_index.position_to_offset(position);
        Ok(hover::hover_item_at(
            &analysis.module,
            &analysis.type_result,
            &analysis.expanded_source,
            &analysis.expanded_line_index,
            offset,
        ))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = analysis.expanded_line_index.position_to_offset(position);
        Ok(signature_help::signature_help(
            &analysis.module,
            &analysis.type_result,
            offset,
        ))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;

        // Try in-file resolution first
        let (in_file_result, symbol_name) = {
            let docs = self.documents.lock().unwrap();
            let doc = match docs.get(&uri) {
                Some(d) => d,
                None => return Ok(None),
            };
            let analysis = match &doc.analysis {
                Some(a) => a,
                None => return Ok(None),
            };
            let offset = analysis.expanded_line_index.position_to_offset(position);
            let result = definition::goto_definition(
                &analysis.symbol_index,
                &analysis.expanded_line_index,
                offset,
                &uri,
            );
            let name = analysis.symbol_index.symbol_at(offset).map(String::from);
            (result, name)
        };
        // documents lock dropped here

        if in_file_result.is_some() {
            return Ok(in_file_result);
        }

        // Cross-file fallback: search workspace for the symbol
        let name = match symbol_name {
            Some(n) => n,
            None => return Ok(None),
        };

        let ws = self.workspace.read().await;
        let matches = ws.find_symbols_by_name(&name);
        for sym in matches {
            if sym.uri == uri {
                continue;
            }
            if let Some(line_index) = ws.line_index_for(&sym.uri) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri: sym.uri.clone(),
                    range: line_index.span_to_range(sym.name_span),
                })));
            }
        }

        Ok(None)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let include_declaration = params.context.include_declaration;

        // Get in-file references and symbol name
        let (in_file_refs, symbol_name) = {
            let docs = self.documents.lock().unwrap();
            let doc = match docs.get(&uri) {
                Some(d) => d,
                None => return Ok(None),
            };
            let analysis = match &doc.analysis {
                Some(a) => a,
                None => return Ok(None),
            };
            let offset = analysis.expanded_line_index.position_to_offset(position);
            let refs = definition::find_references(
                &analysis.symbol_index,
                &analysis.expanded_line_index,
                offset,
                &uri,
                include_declaration,
            );
            let name = analysis.symbol_index.symbol_at(offset).map(String::from);
            (refs, name)
        };
        // documents lock dropped here

        let name = match symbol_name {
            Some(n) => n,
            None => {
                if in_file_refs.is_empty() {
                    return Ok(None);
                }
                return Ok(Some(in_file_refs));
            }
        };

        let mut all_refs = in_file_refs;

        // Cross-file: search other workspace files for occurrences
        let ws = self.workspace.read().await;
        for file_uri in ws.files.keys() {
            if *file_uri == uri {
                continue;
            }
            // Quick text check: skip files that don't contain the name
            let text = if let Ok(path) = file_uri.to_file_path() {
                match std::fs::read_to_string(&path) {
                    Ok(t) => t,
                    Err(_) => continue,
                }
            } else {
                continue;
            };

            if !text.contains(&name) {
                continue;
            }

            // Parse with matching line index for correct spans
            let expanded = expand::expand_source(&text);
            let (module, _) = weir_parser::parse(&expanded.source);
            let line_index = expanded.line_index;
            let idx = SymbolIndex::build(&module);

            for span in idx.get_all_occurrences_of(&name) {
                all_refs.push(Location {
                    uri: file_uri.clone(),
                    range: line_index.span_to_range(span),
                });
            }
        }

        if all_refs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(all_refs))
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let position = params.position;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let offset = analysis.expanded_line_index.position_to_offset(position);
        Ok(definition::prepare_rename(
            &analysis.symbol_index,
            &analysis.expanded_line_index,
            offset,
        )
        .map(PrepareRenameResponse::Range))
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let new_name = params.new_name.clone();

        // Get in-file rename edits and symbol name
        let (in_file_edit, symbol_name) = {
            let docs = self.documents.lock().unwrap();
            let doc = match docs.get(&uri) {
                Some(d) => d,
                None => return Ok(None),
            };
            let analysis = match &doc.analysis {
                Some(a) => a,
                None => return Ok(None),
            };
            let offset = analysis.expanded_line_index.position_to_offset(position);
            let edit = definition::rename(
                &analysis.symbol_index,
                &analysis.expanded_line_index,
                offset,
                &uri,
                &new_name,
            );
            let name = analysis.symbol_index.symbol_at(offset).map(String::from);
            (edit, name)
        };
        // documents lock dropped here

        let name = match symbol_name {
            Some(n) => n,
            None => return Ok(in_file_edit),
        };

        let mut all_changes: HashMap<Url, Vec<TextEdit>> = match &in_file_edit {
            Some(edit) => edit.changes.clone().unwrap_or_default(),
            None => HashMap::new(),
        };

        // Cross-file: find occurrences in other workspace files
        let ws = self.workspace.read().await;
        for file_uri in ws.files.keys() {
            if *file_uri == uri {
                continue;
            }
            let text = if let Ok(path) = file_uri.to_file_path() {
                match std::fs::read_to_string(&path) {
                    Ok(t) => t,
                    Err(_) => continue,
                }
            } else {
                continue;
            };

            if !text.contains(&name) {
                continue;
            }

            // Parse with matching line index for correct spans
            let expanded = expand::expand_source(&text);
            let (module, _) = weir_parser::parse(&expanded.source);
            let line_index = expanded.line_index;
            let idx = SymbolIndex::build(&module);

            let edits: Vec<TextEdit> = idx
                .get_all_occurrences_of(&name)
                .iter()
                .map(|span| TextEdit {
                    range: line_index.span_to_range(*span),
                    new_text: new_name.clone(),
                })
                .collect();

            if !edits.is_empty() {
                all_changes.insert(file_uri.clone(), edits);
            }
        }

        if all_changes.is_empty() {
            Ok(None)
        } else {
            Ok(Some(WorkspaceEdit {
                changes: Some(all_changes),
                ..Default::default()
            }))
        }
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let syms = symbols::document_symbols(&analysis.module, &analysis.expanded_line_index);
        Ok(Some(DocumentSymbolResponse::Nested(syms)))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;

        // Gather workspace symbols first (async lock, must be done before sync lock)
        let ws = self.workspace.read().await;
        let ws_symbols: Vec<_> = ws
            .all_top_level_symbols()
            .into_iter()
            .filter(|s| s.uri != uri)
            .cloned()
            .collect();
        drop(ws);

        // Now get in-file data and produce completions
        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(&uri) {
            Some(d) => d,
            None => return Ok(None),
        };
        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };
        let offset = analysis.expanded_line_index.position_to_offset(position);
        let items = completion::completions(
            &analysis.module,
            Some(&analysis.type_result),
            Some(&analysis.symbol_index),
            Some(offset),
            Some(&ws_symbols),
        );
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let line_width = 80;
        let formatted = formatting::format_document(&doc.text, line_width);

        if formatted == doc.text {
            return Ok(None);
        }

        // Replace entire document
        let last_line = doc.text.lines().count().saturating_sub(1) as u32;
        let last_char = doc.text.lines().last().map_or(0, |l| l.len()) as u32;
        let edit = TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: last_line,
                    character: last_char,
                },
            },
            new_text: formatted,
        };

        Ok(Some(vec![edit]))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let hints = inlay_hints::inlay_hints(
            &analysis.module,
            &analysis.type_result,
            &analysis.expanded_line_index,
            params.range,
        );
        Ok(Some(hints))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let tokens = semantic_tokens::semantic_tokens(
            &analysis.module,
            &analysis.type_result,
            &analysis.expanded_source,
            &analysis.expanded_line_index,
        );

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let query = params.query.to_lowercase();
        let ws = self.workspace.read().await;

        #[allow(deprecated)] // SymbolInformation.deprecated is deprecated but required
        let symbols: Vec<SymbolInformation> =
            ws.all_top_level_symbols()
                .into_iter()
                .filter(|s| {
                    if query.is_empty() {
                        return true;
                    }
                    s.name.to_lowercase().contains(&query)
                })
                .filter_map(|s| {
                    let line_index = ws.line_index_for(&s.uri)?;
                    let range = line_index.span_to_range(s.name_span);
                    let lsp_kind = match s.kind {
                        crate::index::SymbolKind::Function => SymbolKind::FUNCTION,
                        crate::index::SymbolKind::Type => SymbolKind::ENUM,
                        crate::index::SymbolKind::Variant => SymbolKind::ENUM_MEMBER,
                        crate::index::SymbolKind::Struct => SymbolKind::STRUCT,
                        crate::index::SymbolKind::Class => SymbolKind::INTERFACE,
                        crate::index::SymbolKind::Parameter
                        | crate::index::SymbolKind::LetBinding => SymbolKind::VARIABLE,
                    };
                    Some(SymbolInformation {
                        name: s.name.clone(),
                        kind: lsp_kind,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: s.uri.clone(),
                            range,
                        },
                        container_name: s.uri.path().rsplit('/').next().map(|f| f.to_string()),
                    })
                })
                .collect();

        Ok(Some(symbols))
    }
}
