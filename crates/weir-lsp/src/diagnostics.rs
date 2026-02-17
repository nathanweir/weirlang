use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::document::{AnalysisResult, LineIndex};
use crate::index::SymbolIndex;

/// Check whether macro expansion actually changed the token stream.
/// If the token sequences are identical (ignoring spans), no macros were expanded.
fn macros_changed_source(original: &str, expanded: &str) -> bool {
    let (orig_tokens, _) = weir_lexer::lex(original);
    let (exp_tokens, _) = weir_lexer::lex(expanded);

    if orig_tokens.len() != exp_tokens.len() {
        return true;
    }

    orig_tokens
        .iter()
        .zip(exp_tokens.iter())
        .any(|((t1, _), (t2, _))| t1 != t2)
}

/// Run the full analysis pipeline: expand → parse → typecheck.
/// Returns the analysis result and a list of LSP diagnostics.
pub fn analyze(text: &str, original_line_index: &LineIndex) -> (AnalysisResult, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();

    // 1. Expand macros
    let expand_result = weir_macros::expand(text);
    let macro_errors = expand_result.errors.clone();

    // Macro errors use spans from the original source
    for error in &expand_result.errors {
        diagnostics.push(Diagnostic {
            range: original_line_index.span_to_range(error.span),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("weir-macros".into()),
            message: error.message.clone(),
            ..Default::default()
        });
    }

    // Determine whether to parse original or expanded source.
    // The macro expander's reconstruct() strips newlines, so if no macros were
    // actually expanded, we parse the original source to preserve line info.
    let macros_expanded =
        macro_errors.is_empty() && macros_changed_source(text, &expand_result.source);

    let (parse_source, parse_line_index): (String, LineIndex) = if macros_expanded {
        let idx = LineIndex::new(&expand_result.source);
        (expand_result.source, idx)
    } else {
        (text.to_string(), original_line_index.clone())
    };

    // 2. Parse
    let (module, parse_errors) = weir_parser::parse(&parse_source);

    for error in &parse_errors {
        diagnostics.push(Diagnostic {
            range: parse_line_index.span_to_range(error.span),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("weir-parser".into()),
            message: error.message.clone(),
            ..Default::default()
        });
    }

    // 3. Typecheck only if no macro/parse errors
    let type_result = if macro_errors.is_empty() && parse_errors.is_empty() {
        let result = weir_typeck::check(&module);
        for error in &result.errors {
            diagnostics.push(Diagnostic {
                range: parse_line_index.span_to_range(error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("weir-typeck".into()),
                message: error.message.clone(),
                ..Default::default()
            });
        }
        result
    } else {
        weir_typeck::check(&module)
    };

    let symbol_index = SymbolIndex::build(&module);

    let analysis = AnalysisResult {
        module,
        type_result,
        macro_errors,
        parse_errors,
        expanded_source: parse_source,
        expanded_line_index: parse_line_index,
        symbol_index,
    };

    (analysis, diagnostics)
}
