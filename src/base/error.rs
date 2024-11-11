/// An error that occurred during compilation.
#[allow(missing_docs)]
#[derive(Debug, thiserror::Error, Clone, PartialEq)]
pub enum Error {
    #[error("FileProviderError: {0}")]
    FileProviderError(#[from] super::FileProviderError),
    #[error(transparent)]
    LexicalError(#[from] crate::lexical::Error),
    #[error(transparent)]
    ParseError(#[from] crate::syntax::error::Error),
    #[error(transparent)]
    SemanticError(#[from] crate::semantic::error::Error),
    #[error(transparent)]
    TranspileError(#[from] crate::transpile::TranspileError),
    #[error("An error occurred: {0}")]
    Other(String),
}

impl Error {
    /// Creates a new error from a string.
    pub fn other<S: Into<String>>(error: S) -> Self {
        Self::Other(error.into())
    }
}

/// A specialized [`Result`] type for this crate.
pub type Result<T> = std::result::Result<T, Error>;
