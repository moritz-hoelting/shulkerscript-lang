//! Errors that can occur during compilation.

/// Errors that can occur during compilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone, Copy)]
pub enum CompileError {
    #[error("No main function was found in the source code.")]
    MissingMainFunction,
}
