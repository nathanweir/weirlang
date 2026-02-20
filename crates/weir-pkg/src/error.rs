use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum PkgError {
    #[error("I/O error: {path}: {source}")]
    Io {
        path: PathBuf,
        source: std::io::Error,
    },

    #[error("manifest parse error: {0}")]
    ManifestParse(String),

    #[error("missing required field `{0}` in manifest")]
    MissingField(String),

    #[error("dependency cycle detected: {0}")]
    DependencyCycle(String),

    #[error("dependency not found: {name} (path: {path})")]
    DepNotFound { name: String, path: PathBuf },

    #[error("no weir.pkg manifest found in {0}")]
    NoManifest(PathBuf),

    #[error("no main entry point specified in manifest")]
    NoMainEntry,

    #[error("unknown module `{0}` in import")]
    UnknownModule(String),

    #[error("unknown symbol `{symbol}` in module `{module}`")]
    UnknownSymbol { module: String, symbol: String },

    #[error("source file not found: {0}")]
    SourceNotFound(PathBuf),
}
