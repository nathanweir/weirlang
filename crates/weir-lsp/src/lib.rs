mod completion;
mod definition;
mod diagnostics;
mod document;
pub mod formatting;
mod hover;
pub mod index;
mod inlay_hints;
mod semantic_tokens;
mod server;
mod signature_help;
mod symbols;

use tower_lsp::{LspService, Server};

/// Start the Weir LSP server on stdin/stdout.
pub async fn run_lsp() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(server::WeirLspBackend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
