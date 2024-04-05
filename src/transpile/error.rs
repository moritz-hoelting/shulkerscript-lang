//! Errors that can occur during transpilation.

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone, Copy)]
pub enum TranspileError {
    #[error("No main function was found in the source code.")]
    MissingMainFunction,
}
