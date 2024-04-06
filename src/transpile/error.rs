//! Errors that can occur during transpilation.

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone)]
pub enum TranspileError {
    #[error("Function {} was called but never declared.", .0)]
    MissingFunctionDeclaration(String),
}

/// The result of a transpilation operation.
pub type TranspileResult<T> = Result<T, TranspileError>;
