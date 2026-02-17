use tree_sitter_language::LanguageFn;

unsafe extern "C" {
    fn tree_sitter_weir() -> *const ();
}

/// The tree-sitter [`LanguageFn`] for Weir.
pub const LANGUAGE_WEIR: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_weir) };

/// The content of the [`node-types.json`] file for this grammar.
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

#[cfg(test)]
mod tests {
    use crate::LANGUAGE_WEIR;

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&LANGUAGE_WEIR.into())
            .expect("Error loading weir language");
    }
}
