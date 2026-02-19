use crate::document::LineIndex;
use weir_macros::MacroError;

/// Check whether macro expansion actually changed the token stream.
/// If the token sequences are identical (ignoring spans), no macros were expanded.
pub fn macros_changed_source(original: &str, expanded: &str) -> bool {
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

/// Result of expanding macros and preparing source for parsing.
pub struct ExpandedSource {
    /// The source text to parse (either original or macro-expanded).
    pub source: String,
    /// Line index matching the source.
    pub line_index: LineIndex,
    /// Any errors from macro expansion.
    pub macro_errors: Vec<MacroError>,
}

/// Expand macros and determine the correct source text and line index for parsing.
///
/// If macros changed the source, returns the expanded source with a fresh LineIndex.
/// If macros didn't change anything (or had errors), returns the original text.
pub fn expand_source(text: &str) -> ExpandedSource {
    let expand_result = weir_macros::expand(text);
    let macro_errors = expand_result.errors;
    let macros_expanded =
        macro_errors.is_empty() && macros_changed_source(text, &expand_result.source);

    if macros_expanded {
        let line_index = LineIndex::new(&expand_result.source);
        ExpandedSource {
            source: expand_result.source,
            line_index,
            macro_errors,
        }
    } else {
        let line_index = LineIndex::new(text);
        ExpandedSource {
            source: text.to_string(),
            line_index,
            macro_errors,
        }
    }
}
