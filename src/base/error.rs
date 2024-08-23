/// An error that occurred during compilation.
#[allow(missing_docs)]
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("An error occurred while working with Input/Output.")]
    IoError(String),
    #[error("An error occurred while lexing the source code.")]
    LexicalError(#[from] crate::lexical::Error),
    #[error("An error occured while tokenizing the source code.")]
    TokenizeError(#[from] crate::lexical::token::TokenizeError),
    #[error(transparent)]
    ParseError(#[from] crate::syntax::error::Error),
    #[error("An error occurred while transpiling the source code.")]
    TranspileError(#[from] crate::transpile::TranspileError),
    #[error("An error occurred")]
    Other(&'static str),
}

/// A specialized [`Result`] type for this crate.
pub type Result<T> = std::result::Result<T, Error>;
