use std::path::{Path, PathBuf};

use weir_lexer::Token;

use crate::error::PkgError;

/// A dependency specification.
#[derive(Debug, Clone)]
pub struct DepSpec {
    pub name: String,
    pub path: PathBuf,
}

/// Parsed package manifest from a `weir.pkg` file.
#[derive(Debug, Clone)]
pub struct PackageManifest {
    pub name: String,
    pub version: String,
    pub sources: Vec<PathBuf>,
    pub main: Option<PathBuf>,
    pub deps: Vec<DepSpec>,
    pub native_sources: Vec<PathBuf>,
    pub link_libs: Vec<String>,
    /// JS bridge files for WASM builds (from `(target (:wasm (wasm-bridge ...)))` or `(wasm-bridge ...)`).
    pub wasm_bridge_files: Vec<PathBuf>,
}

/// Parse a `weir.pkg` manifest file.
///
/// `manifest_path` is used to resolve relative paths in the manifest.
pub fn parse_manifest(source: &str, manifest_path: &Path) -> Result<PackageManifest, PkgError> {
    let manifest_dir = manifest_path
        .parent()
        .unwrap_or_else(|| Path::new("."));

    let (tokens, lex_errors) = weir_lexer::lex(source);
    if !lex_errors.is_empty() {
        return Err(PkgError::ManifestParse(format!(
            "lexer errors in manifest at {:?}",
            manifest_path
        )));
    }

    let mut pos = 0;
    let toks: Vec<&Token> = tokens.iter().map(|(t, _)| t).collect();

    // Expect opening `(package ...)`
    expect_tok(&toks, &mut pos, &Token::LParen, "expected `(`")?;
    expect_symbol(&toks, &mut pos, "package")?;

    let mut name: Option<String> = None;
    let mut version: Option<String> = None;
    let mut sources: Vec<PathBuf> = Vec::new();
    let mut main: Option<PathBuf> = None;
    let mut deps: Vec<DepSpec> = Vec::new();
    let mut native_sources: Vec<PathBuf> = Vec::new();
    let mut link_libs: Vec<String> = Vec::new();
    let mut wasm_bridge_files: Vec<PathBuf> = Vec::new();

    // Parse fields until closing `)`
    while pos < toks.len() && *toks[pos] != Token::RParen {
        expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for field")?;

        let field_name = read_symbol(&toks, &mut pos)?;

        match field_name.as_str() {
            "name" => {
                name = Some(read_string(&toks, &mut pos)?);
            }
            "version" => {
                version = Some(read_string(&toks, &mut pos)?);
            }
            "sources" => {
                // One or more string literals
                while pos < toks.len() && *toks[pos] != Token::RParen {
                    let s = read_string(&toks, &mut pos)?;
                    sources.push(manifest_dir.join(s));
                }
            }
            "main" => {
                let s = read_string(&toks, &mut pos)?;
                main = Some(manifest_dir.join(&s));
                // Also add main to sources if not already listed
                // (the main file is implicitly a source)
            }
            "deps" => {
                // Each dep: `(name (path "..."))`
                while pos < toks.len() && *toks[pos] != Token::RParen {
                    expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for dep")?;
                    let dep_name = read_symbol(&toks, &mut pos)?;
                    expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for dep path")?;
                    expect_symbol(&toks, &mut pos, "path")?;
                    let dep_path_str = read_string(&toks, &mut pos)?;
                    let dep_path = manifest_dir.join(&dep_path_str);
                    expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` after dep path")?;
                    expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` after dep")?;
                    deps.push(DepSpec {
                        name: dep_name,
                        path: dep_path,
                    });
                }
            }
            "native" => {
                parse_native_section(&toks, &mut pos, manifest_dir, &mut native_sources, &mut link_libs)?;
            }
            "wasm-bridge" => {
                // One or more string literals for JS bridge files
                while pos < toks.len() && *toks[pos] != Token::RParen {
                    let s = read_string(&toks, &mut pos)?;
                    wasm_bridge_files.push(manifest_dir.join(s));
                }
            }
            "target" => {
                // Per-target sections: (:native ...) (:wasm ...)
                while pos < toks.len() && *toks[pos] != Token::RParen {
                    expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for target branch")?;
                    // Read keyword (:native or :wasm)
                    let kw = read_keyword(&toks, &mut pos)?;
                    match kw.as_str() {
                        "native" => {
                            // Inside (:native ...) — parse sub-fields like (native ...)
                            while pos < toks.len() && *toks[pos] != Token::RParen {
                                expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for native sub")?;
                                let sub = read_symbol(&toks, &mut pos)?;
                                match sub.as_str() {
                                    "native" => {
                                        parse_native_section(&toks, &mut pos, manifest_dir, &mut native_sources, &mut link_libs)?;
                                    }
                                    other => {
                                        return Err(PkgError::ManifestParse(format!(
                                            "unknown field `{}` inside target :native",
                                            other
                                        )));
                                    }
                                }
                                expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` for native sub")?;
                            }
                        }
                        "wasm" => {
                            // Inside (:wasm ...) — parse wasm-bridge etc.
                            while pos < toks.len() && *toks[pos] != Token::RParen {
                                expect_tok(&toks, &mut pos, &Token::LParen, "expected `(` for wasm sub")?;
                                let sub = read_symbol(&toks, &mut pos)?;
                                match sub.as_str() {
                                    "wasm-bridge" => {
                                        while pos < toks.len() && *toks[pos] != Token::RParen {
                                            let s = read_string(&toks, &mut pos)?;
                                            wasm_bridge_files.push(manifest_dir.join(s));
                                        }
                                    }
                                    other => {
                                        return Err(PkgError::ManifestParse(format!(
                                            "unknown field `{}` inside target :wasm",
                                            other
                                        )));
                                    }
                                }
                                expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` for wasm sub")?;
                            }
                        }
                        other => {
                            return Err(PkgError::ManifestParse(format!(
                                "unknown target keyword `:{}`",
                                other
                            )));
                        }
                    }
                    expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` for target branch")?;
                }
            }
            other => {
                return Err(PkgError::ManifestParse(format!(
                    "unknown field `{}`",
                    other
                )));
            }
        }

        expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` to close field")?;
    }

    // Closing `)` for `(package ...)`
    expect_tok(&toks, &mut pos, &Token::RParen, "expected `)` to close package")?;

    let name = name.ok_or_else(|| PkgError::MissingField("name".into()))?;
    let version = version.ok_or_else(|| PkgError::MissingField("version".into()))?;

    Ok(PackageManifest {
        name,
        version,
        sources,
        main,
        deps,
        native_sources,
        link_libs,
        wasm_bridge_files,
    })
}

fn parse_native_section(
    toks: &[&Token],
    pos: &mut usize,
    manifest_dir: &Path,
    native_sources: &mut Vec<PathBuf>,
    link_libs: &mut Vec<String>,
) -> Result<(), PkgError> {
    while *pos < toks.len() && *toks[*pos] != Token::RParen {
        expect_tok(toks, pos, &Token::LParen, "expected `(` for native sub-field")?;
        let sub_field = read_symbol(toks, pos)?;
        match sub_field.as_str() {
            "sources" => {
                while *pos < toks.len() && *toks[*pos] != Token::RParen {
                    let s = read_string(toks, pos)?;
                    native_sources.push(manifest_dir.join(s));
                }
            }
            "link" => {
                while *pos < toks.len() && *toks[*pos] != Token::RParen {
                    let s = read_string(toks, pos)?;
                    link_libs.push(s);
                }
            }
            other => {
                return Err(PkgError::ManifestParse(format!(
                    "unknown native sub-field `{}`",
                    other
                )));
            }
        }
        expect_tok(toks, pos, &Token::RParen, "expected `)` for native sub-field")?;
    }
    Ok(())
}

fn expect_tok(toks: &[&Token], pos: &mut usize, expected: &Token, msg: &str) -> Result<(), PkgError> {
    if *pos >= toks.len() {
        return Err(PkgError::ManifestParse(format!("{} (unexpected end of input)", msg)));
    }
    if std::mem::discriminant(toks[*pos]) != std::mem::discriminant(expected) {
        return Err(PkgError::ManifestParse(format!(
            "{}, got {:?}",
            msg, toks[*pos]
        )));
    }
    *pos += 1;
    Ok(())
}

fn expect_symbol(toks: &[&Token], pos: &mut usize, name: &str) -> Result<(), PkgError> {
    if *pos >= toks.len() {
        return Err(PkgError::ManifestParse(format!(
            "expected `{}` (unexpected end of input)",
            name
        )));
    }
    match toks[*pos] {
        Token::Symbol(s) if s.as_str() == name => {
            *pos += 1;
            Ok(())
        }
        other => Err(PkgError::ManifestParse(format!(
            "expected `{}`, got {:?}",
            name, other
        ))),
    }
}

fn read_symbol(toks: &[&Token], pos: &mut usize) -> Result<String, PkgError> {
    if *pos >= toks.len() {
        return Err(PkgError::ManifestParse(
            "expected symbol (unexpected end of input)".into(),
        ));
    }
    match toks[*pos] {
        Token::Symbol(s) => {
            let result = s.to_string();
            *pos += 1;
            Ok(result)
        }
        other => Err(PkgError::ManifestParse(format!(
            "expected symbol, got {:?}",
            other
        ))),
    }
}

fn read_keyword(toks: &[&Token], pos: &mut usize) -> Result<String, PkgError> {
    if *pos >= toks.len() {
        return Err(PkgError::ManifestParse(
            "expected keyword (unexpected end of input)".into(),
        ));
    }
    match toks[*pos] {
        Token::Keyword(s) => {
            let result = s.to_string();
            *pos += 1;
            Ok(result)
        }
        other => Err(PkgError::ManifestParse(format!(
            "expected keyword, got {:?}",
            other
        ))),
    }
}

fn read_string(toks: &[&Token], pos: &mut usize) -> Result<String, PkgError> {
    if *pos >= toks.len() {
        return Err(PkgError::ManifestParse(
            "expected string (unexpected end of input)".into(),
        ));
    }
    match toks[*pos] {
        Token::String(s) => {
            let result = s.clone();
            *pos += 1;
            Ok(result)
        }
        other => Err(PkgError::ManifestParse(format!(
            "expected string, got {:?}",
            other
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn parse_library_manifest() {
        let source = r#"(package
  (name "test-lib")
  (version "0.1.0")
  (sources "lib.weir"))"#;

        let manifest = parse_manifest(source, Path::new("/tmp/weir.pkg")).unwrap();
        assert_eq!(manifest.name, "test-lib");
        assert_eq!(manifest.version, "0.1.0");
        assert_eq!(manifest.sources.len(), 1);
        assert!(manifest.sources[0].ends_with("lib.weir"));
        assert!(manifest.main.is_none());
        assert!(manifest.deps.is_empty());
    }

    #[test]
    fn parse_app_manifest() {
        let source = r#"(package
  (name "test-app")
  (version "0.1.0")
  (deps
    (test-lib (path "../pkg-lib")))
  (main "app.weir"))"#;

        let manifest = parse_manifest(source, Path::new("/project/weir.pkg")).unwrap();
        assert_eq!(manifest.name, "test-app");
        assert_eq!(manifest.version, "0.1.0");
        assert_eq!(manifest.deps.len(), 1);
        assert_eq!(manifest.deps[0].name, "test-lib");
        assert!(manifest.main.is_some());
    }

    #[test]
    fn parse_multiple_sources() {
        let source = r#"(package
  (name "multi")
  (version "0.1.0")
  (sources "lib.weir" "utils.weir" "helpers.weir"))"#;

        let manifest = parse_manifest(source, Path::new("/tmp/weir.pkg")).unwrap();
        assert_eq!(manifest.sources.len(), 3);
    }

    #[test]
    fn parse_native_fields() {
        let source = r#"(package
  (name "native-lib")
  (version "0.1.0")
  (sources "lib.weir")
  (native
    (sources "helper.c" "wrapper.c")
    (link "glfw" "GL" "m")))"#;

        let manifest = parse_manifest(source, Path::new("/tmp/weir.pkg")).unwrap();
        assert_eq!(manifest.native_sources.len(), 2);
        assert_eq!(manifest.link_libs, vec!["glfw", "GL", "m"]);
    }

    #[test]
    fn missing_name_errors() {
        let source = r#"(package (version "0.1.0"))"#;
        let err = parse_manifest(source, Path::new("/tmp/weir.pkg")).unwrap_err();
        assert!(err.to_string().contains("name"), "error: {}", err);
    }

    #[test]
    fn missing_version_errors() {
        let source = r#"(package (name "test"))"#;
        let err = parse_manifest(source, Path::new("/tmp/weir.pkg")).unwrap_err();
        assert!(err.to_string().contains("version"), "error: {}", err);
    }
}
