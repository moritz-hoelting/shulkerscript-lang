//! Errors that can occur during transpilation.

use crate::{base::source_file::SourceElement, syntax::syntax_tree::expression::Expression};

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone)]
pub enum TranspileError {
    #[error("Function {} was called but never declared.", .0)]
    MissingFunctionDeclaration(String),
    #[error("Unexpected expression: {}", .0.span().str())]
    UnexpectedExpression(Expression),
}

/// The result of a transpilation operation.
pub type TranspileResult<T> = Result<T, TranspileError>;
