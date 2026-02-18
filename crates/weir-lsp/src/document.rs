use tower_lsp::lsp_types::{Position, Range};
use weir_ast::Module;
use weir_lexer::Span;
use weir_macros::MacroError;
use weir_parser::ParseError;
use weir_typeck::TypeCheckResult;

use crate::index::SymbolIndex;

/// Maps between byte offsets (used by Weir compiler spans) and LSP positions (line/character).
#[derive(Debug, Clone)]
pub struct LineIndex {
    /// Byte offset of the start of each line.
    line_starts: Vec<u32>,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0u32];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push((i + 1) as u32);
            }
        }
        LineIndex { line_starts }
    }

    pub fn offset_to_position(&self, offset: u32) -> Position {
        let line = match self.line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(next_line) => next_line - 1,
        };
        let line_start = self.line_starts[line];
        Position {
            line: line as u32,
            character: offset - line_start,
        }
    }

    pub fn position_to_offset(&self, position: Position) -> u32 {
        let line = position.line as usize;
        if line < self.line_starts.len() {
            self.line_starts[line] + position.character
        } else {
            // Past end of file — return last valid offset
            *self.line_starts.last().unwrap_or(&0)
        }
    }

    pub fn span_to_range(&self, span: Span) -> Range {
        Range {
            start: self.offset_to_position(span.start),
            end: self.offset_to_position(span.end),
        }
    }
}

/// Per-document state tracked by the LSP server.
pub struct Document {
    pub text: String,
    pub line_index: LineIndex,
    pub version: i32,
    pub analysis: Option<AnalysisResult>,
}

impl Document {
    pub fn new(text: String, version: i32) -> Self {
        let line_index = LineIndex::new(&text);
        Self {
            text,
            line_index,
            version,
            analysis: None,
        }
    }
}

/// Result of running the full analysis pipeline on a document.
#[allow(dead_code)]
pub struct AnalysisResult {
    pub module: Module,
    pub type_result: TypeCheckResult,
    pub macro_errors: Vec<MacroError>,
    pub parse_errors: Vec<ParseError>,
    pub expanded_source: String,
    /// LineIndex for expanded source (may differ from original if macros changed text).
    pub expanded_line_index: LineIndex,
    /// Symbol index: definitions and references.
    pub symbol_index: SymbolIndex,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_index_single_line() {
        let idx = LineIndex::new("hello");
        assert_eq!(
            idx.offset_to_position(0),
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            idx.offset_to_position(3),
            Position {
                line: 0,
                character: 3
            }
        );
        assert_eq!(
            idx.offset_to_position(5),
            Position {
                line: 0,
                character: 5
            }
        );
    }

    #[test]
    fn line_index_multi_line() {
        let idx = LineIndex::new("abc\ndef\nghi");
        // Line 0: bytes 0..3 ("abc")
        // Line 1: bytes 4..7 ("def")
        // Line 2: bytes 8..10 ("ghi")
        assert_eq!(
            idx.offset_to_position(0),
            Position {
                line: 0,
                character: 0
            }
        );
        assert_eq!(
            idx.offset_to_position(3),
            Position {
                line: 0,
                character: 3
            }
        );
        assert_eq!(
            idx.offset_to_position(4),
            Position {
                line: 1,
                character: 0
            }
        );
        assert_eq!(
            idx.offset_to_position(6),
            Position {
                line: 1,
                character: 2
            }
        );
        assert_eq!(
            idx.offset_to_position(8),
            Position {
                line: 2,
                character: 0
            }
        );
        assert_eq!(
            idx.offset_to_position(10),
            Position {
                line: 2,
                character: 2
            }
        );
    }

    #[test]
    fn position_to_offset_roundtrip() {
        let text = "first\nsecond\nthird";
        let idx = LineIndex::new(text);
        for offset in 0..text.len() as u32 {
            let pos = idx.offset_to_position(offset);
            let back = idx.position_to_offset(pos);
            assert_eq!(offset, back, "roundtrip failed for offset {}", offset);
        }
    }

    #[test]
    fn span_to_range() {
        // "(defn foo ()\n  42)"
        //  0123456789012 34567
        // line 0: bytes 0..12, line 1: bytes 13..17
        // "42" is at offsets 15..17
        let idx = LineIndex::new("(defn foo ()\n  42)");
        let span = Span::new(15, 17); // "42" on line 1
        let range = idx.span_to_range(span);
        assert_eq!(
            range.start,
            Position {
                line: 1,
                character: 2
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 1,
                character: 4
            }
        );
    }

    #[test]
    fn empty_text() {
        let idx = LineIndex::new("");
        assert_eq!(
            idx.offset_to_position(0),
            Position {
                line: 0,
                character: 0
            }
        );
    }

    #[test]
    fn trailing_newline() {
        let idx = LineIndex::new("abc\n");
        assert_eq!(
            idx.offset_to_position(4),
            Position {
                line: 1,
                character: 0
            }
        );
    }
}
