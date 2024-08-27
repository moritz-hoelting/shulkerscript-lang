//! Errors that can occur during transpilation.

use std::fmt::Display;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::{SourceElement, Span},
    },
    syntax::syntax_tree::expression::Expression,
};

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone)]
pub enum TranspileError {
    #[error(transparent)]
    MissingFunctionDeclaration(#[from] MissingFunctionDeclaration),
    #[error("Unexpected expression: {}", .0.span().str())]
    UnexpectedExpression(Expression),
    #[error("Lua code evaluation is disabled.")]
    LuaDisabled,
    #[error("Lua runtime error: {}", .0)]
    LuaRuntimeError(String),
}

/// The result of a transpilation operation.
pub type TranspileResult<T> = Result<T, TranspileError>;

/// An error that occurs when a function declaration is missing.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MissingFunctionDeclaration {
    pub span: Span,
}

impl Display for MissingFunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "no matching function declaration found for invocation of function `{}`",
            self.span.str()
        );
        write!(f, "{}", Message::new(Severity::Error, message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.span, Option::<u8>::None)
        )
    }
}

impl std::error::Error for MissingFunctionDeclaration {}
