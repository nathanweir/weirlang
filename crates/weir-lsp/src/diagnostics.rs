use std::collections::HashSet;

use smol_str::SmolStr;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::document::{AnalysisResult, LineIndex};
use crate::expand;
use crate::index::SymbolIndex;

/// Run the full analysis pipeline: expand → parse → typecheck.
/// `external_names` contains known symbols from other workspace files;
/// the type checker will treat them as valid (opaque) rather than erroring.
/// Returns the analysis result and a list of LSP diagnostics.
pub fn analyze(
    text: &str,
    original_line_index: &LineIndex,
    external_names: &HashSet<SmolStr>,
) -> (AnalysisResult, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();

    // 1. Expand macros
    let expanded = expand::expand_source(text);

    // Macro errors use spans from the original source
    for error in &expanded.macro_errors {
        diagnostics.push(Diagnostic {
            range: original_line_index.span_to_range(error.span),
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("weir-macros".into()),
            message: error.message.clone(),
            ..Default::default()
        });
    }

    let macro_errors = expanded.macro_errors;
    let parse_source = expanded.source;
    let parse_line_index = expanded.line_index;

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
        let result = weir_typeck::check_with_externals(&module, external_names);
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
        weir_typeck::check_with_externals(&module, external_names)
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
