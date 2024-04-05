use std::io;

/// An error that occurred during compilation.
#[allow(missing_docs)]
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("An error occurred while reading the file.")]
    IoError(#[from] io::Error),
    #[error("An error occured while tokenizing the source code.")]
    TokenizeError(#[from] crate::lexical::token::TokenizeError),
    #[error("An error occurred while parsing the source code.")]
    ParseError(#[from] crate::syntax::error::Error),
    #[error("An error occurred while transpiling the source code.")]
    TranspileError(#[from] crate::transpile::error::TranspileError),
    #[error("An error occurred")]
    Other(&'static str),
}

/// A specialized [`Result`] type for this crate.
pub type Result<T> = std::result::Result<T, Error>;
