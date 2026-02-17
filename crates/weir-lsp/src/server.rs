use std::collections::HashMap;
use std::sync::Mutex;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::completion;
use crate::definition;
use crate::diagnostics;
use crate::document::Document;
use crate::formatting;
use crate::hover;
use crate::inlay_hints;
use crate::symbols;

pub struct WeirLspBackend {
    pub client: Client,
    pub documents: Mutex<HashMap<Url, Document>>,
}

impl WeirLspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Mutex::new(HashMap::new()),
        }
    }

    fn analyze_and_publish(&self, uri: Url) {
        let mut docs = self.documents.lock().unwrap();
        let doc = match docs.get_mut(&uri) {
            Some(d) => d,
            None => return,
        };

        let (analysis, diagnostics) = diagnostics::analyze(&doc.text, &doc.line_index);
        doc.analysis = Some(analysis);

        let version = doc.version;
        let client = self.client.clone();
        let uri_clone = uri.clone();

        // Drop the lock before sending diagnostics
        drop(docs);

        tokio::spawn(async move {
            client
                .publish_diagnostics(uri_clone, diagnostics, Some(version))
                .await;
        });
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for WeirLspBackend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
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
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        let doc = Document::new(params.text_document.text, params.text_document.version);
        self.documents.lock().unwrap().insert(uri.clone(), doc);
        self.analyze_and_publish(uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        // Full text sync — take the last change
        if let Some(change) = params.content_changes.into_iter().last() {
            let doc = Document::new(change.text, params.text_document.version);
            self.documents.lock().unwrap().insert(uri.clone(), doc);
            self.analyze_and_publish(uri);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.clone();
        self.documents.lock().unwrap().remove(&uri);

        // Clear diagnostics
        self.client.publish_diagnostics(uri, vec![], None).await;
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

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
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
        Ok(definition::goto_definition(
            &analysis.symbol_index,
            &analysis.expanded_line_index,
            offset,
            uri,
        ))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

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
        let refs = definition::find_references(
            &analysis.symbol_index,
            &analysis.expanded_line_index,
            offset,
            uri,
            params.context.include_declaration,
        );

        if refs.is_empty() {
            Ok(None)
        } else {
            Ok(Some(refs))
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
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

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
        Ok(definition::rename(
            &analysis.symbol_index,
            &analysis.expanded_line_index,
            offset,
            uri,
            &params.new_name,
        ))
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
        let uri = &params.text_document_position.text_document.uri;

        let docs = self.documents.lock().unwrap();
        let doc = match docs.get(uri) {
            Some(d) => d,
            None => return Ok(None),
        };

        let analysis = match &doc.analysis {
            Some(a) => a,
            None => return Ok(None),
        };

        let items = completion::completions(&analysis.module);
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
}
