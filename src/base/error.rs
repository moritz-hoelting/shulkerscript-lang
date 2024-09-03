/// An error that occurred during compilation.
#[allow(missing_docs)]
#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum Error {
    #[error("An error occurred while working with Input/Output: {0}")]
    IoError(String),
    #[error(transparent)]
    Utf8Error(#[from] std::str::Utf8Error),
    #[error("An error occurred while lexing the source code: {0}")]
    LexicalError(#[from] crate::lexical::Error),
    #[error("An error occured while tokenizing the source code: {0}")]
    TokenizeError(#[from] crate::lexical::token::TokenizeError),
    #[error(transparent)]
    ParseError(#[from] crate::syntax::error::Error),
    #[error(transparent)]
    TranspileError(#[from] crate::transpile::TranspileError),
    #[error("An error occurred: {0}")]
    Other(&'static str),
}

/// A specialized [`Result`] type for this crate.
pub type Result<T> = std::result::Result<T, Error>;
