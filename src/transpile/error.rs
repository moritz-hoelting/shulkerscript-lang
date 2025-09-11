//! Errors that can occur during transpilation.

use std::fmt::Display;

use getset::Getters;

#[cfg(feature = "shulkerbox")]
use oxford_join::OxfordJoin as _;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::Span,
    },
    semantic::error::{ConflictingFunctionNames, InvalidFunctionArguments, UnexpectedExpression},
};

use super::expression::ExpectedType;

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum TranspileError {
    #[error(transparent)]
    UnexpectedExpression(#[from] UnexpectedExpression),
    #[error("Lua code evaluation is disabled.")]
    LuaDisabled,
    #[error(transparent)]
    LuaRuntimeError(#[from] LuaRuntimeError),
    #[error(transparent)]
    ConflictingFunctionNames(#[from] ConflictingFunctionNames),
    #[error(transparent)]
    InvalidFunctionArguments(#[from] InvalidFunctionArguments),
    #[error(transparent)]
    IllegalAnnotationContent(#[from] IllegalAnnotationContent),
    #[error(transparent)]
    MismatchedTypes(#[from] MismatchedTypes),
    #[error(transparent)]
    FunctionArgumentsNotAllowed(#[from] FunctionArgumentsNotAllowed),
    #[error(transparent)]
    AssignmentError(#[from] AssignmentError),
    #[error(transparent)]
    UnknownIdentifier(#[from] UnknownIdentifier),
    #[error(transparent)]
    MissingValue(#[from] MissingValue),
    #[error(transparent)]
    IllegalIndexing(#[from] IllegalIndexing),
    #[error(transparent)]
    InvalidArgument(#[from] InvalidArgument),
    #[error(transparent)]
    NotComptime(#[from] NotComptime),
    #[error(transparent)]
    InfiniteLoop(#[from] InfiniteLoop),
}

/// The result of a transpilation operation.
pub type TranspileResult<T> = Result<T, TranspileError>;

/// An error that occurs when a function declaration is missing.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LuaRuntimeError {
    /// The span of the code block that caused the error.
    pub code_block: Span,
    /// The error message of the Lua runtime.
    pub error_message: String,
}

impl Display for LuaRuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            r#"error during lua code execution: "{}""#,
            self.error_message
        );
        write!(f, "{}", Message::new(Severity::Error, message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.code_block, Option::<u8>::None)
        )
    }
}

impl std::error::Error for LuaRuntimeError {}

#[cfg(feature = "lua")]
impl LuaRuntimeError {
    /// Creates a new Lua runtime error from an mlua error.
    #[must_use]
    pub fn from_lua_err(err: &mlua::Error, span: Span) -> Self {
        let err_string = err.to_string();
        Self {
            error_message: err_string
                .strip_prefix("runtime error: ")
                .unwrap_or(&err_string)
                .to_string(),
            code_block: span,
        }
    }
}

/// An error that occurs when an annotation has an illegal content.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct IllegalAnnotationContent {
    /// The span of the annotation.
    #[get = "pub"]
    pub annotation: Span,
    /// The error message.
    #[get = "pub"]
    pub message: String,
}

impl Display for IllegalAnnotationContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "illegal content in annotation `{}`: {}",
            self.annotation.str(),
            self.message
        );
        write!(f, "{}", Message::new(Severity::Error, message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.annotation, Option::<u8>::None)
        )
    }
}

impl std::error::Error for IllegalAnnotationContent {}

/// An error that occurs when an expression can not evaluate to the wanted type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct MismatchedTypes {
    /// The expression that can not evaluate to the wanted type.
    #[get = "pub"]
    pub expression: Span,
    /// The expected type.
    #[get = "pub"]
    pub expected_type: ExpectedType,
}

impl Display for MismatchedTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!("expression can not evaluate to type {}", self.expected_type);
        write!(f, "{}", Message::new(Severity::Error, message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.expression, Option::<u8>::None)
        )
    }
}

impl std::error::Error for MismatchedTypes {}

/// An error that occurs when an expression can not evaluate to the wanted type.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct FunctionArgumentsNotAllowed {
    /// The arguments that are not allowed.
    #[get = "pub"]
    pub arguments: Span,
    /// The error message.
    #[get = "pub"]
    pub message: String,
}

impl Display for FunctionArgumentsNotAllowed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Message::new(Severity::Error, &self.message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.arguments, Option::<u8>::None)
        )
    }
}

impl std::error::Error for FunctionArgumentsNotAllowed {}

/// An error that occurs when an expression can not evaluate to the wanted type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct AssignmentError {
    /// The identifier that is assigned to.
    #[get = "pub"]
    pub identifier: Span,
    /// The error message.
    #[get = "pub"]
    pub message: String,
}

impl Display for AssignmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Message::new(Severity::Error, &self.message))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.identifier, Option::<u8>::None)
        )
    }
}

impl std::error::Error for AssignmentError {}

/// An error that occurs when an unknown identifier is used.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct UnknownIdentifier {
    /// The unknown identifier.
    #[get = "pub"]
    pub(crate) identifier: Span,
    /// Alternatives to the current identifier
    #[get = "pub"]
    pub(crate) alternatives: Vec<String>,
}

impl UnknownIdentifier {
    #[cfg(feature = "shulkerbox")]
    pub(crate) fn from_scope(
        identifier: Span,
        scope: &std::sync::Arc<super::variables::Scope>,
    ) -> Self {
        use itertools::Itertools as _;

        let own_name = identifier.str();
        let alternatives = scope
            .get_all_variables()
            .iter()
            .filter_map(|(name, _)| {
                let normalized_distance = strsim::normalized_damerau_levenshtein(own_name, name);
                (normalized_distance > 0.8 || strsim::damerau_levenshtein(own_name, name) < 3)
                    .then_some((normalized_distance, name))
            })
            .sorted_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(_, data)| data)
            .take(8)
            .cloned()
            .collect::<Vec<_>>();

        Self {
            identifier,
            alternatives,
        }
    }
}

impl Display for UnknownIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!("The identifier {} is not defined.", self.identifier.str())
            )
        )?;

        let help_message = if self.alternatives.is_empty() {
            None
        } else {
            let message = String::from("did you mean ");
            let inner = self
                .alternatives
                .iter()
                .map(|s| format!("`{s}`"))
                .collect::<Vec<_>>();

            #[cfg(feature = "shulkerbox")]
            let inner = inner.oxford_or();
            #[cfg(not(feature = "shulkerbox"))]
            let inner = std::borrow::Cow::<str>::Owned(inner.join(", "));

            Some(message + &inner + "?")
        };

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.identifier, help_message.as_ref())
        )
    }
}

impl std::error::Error for UnknownIdentifier {}

/// An error that occurs when there is a value expected but none provided.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct MissingValue {
    /// The expression that is missing a value.
    #[get = "pub"]
    pub expression: Span,
}

impl Display for MissingValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                "The expression is expected to return a value, but no value is found."
            )
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.expression, Option::<u8>::None)
        )
    }
}

impl std::error::Error for MissingValue {}

/// An error that occurs when an indexing operation is not permitted.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct IllegalIndexing {
    /// The reason why the indexing operation is not permitted.
    #[get = "pub"]
    pub reason: IllegalIndexingReason,
    /// The expression that is the reason for the indexing being illegal.
    #[get = "pub"]
    pub expression: Span,
}

impl Display for IllegalIndexing {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Message::new(Severity::Error, &self.reason))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.expression, Option::<u8>::None)
        )
    }
}

impl std::error::Error for IllegalIndexing {}

/// The reason why an indexing operation is not permitted.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IllegalIndexingReason {
    /// The expression is not an identifier.
    NotIdentifier,
    /// The expression cannot be indexed.
    NotIndexable,
    /// The expression can only be indexed with a specific type that can be evaluated at compile time.
    InvalidComptimeType {
        /// The expected type.
        expected: ExpectedType,
    },
    /// The index is out of bounds.
    IndexOutOfBounds {
        /// The index that is out of bounds.
        index: usize,
        /// The length indexed object.
        length: usize,
    },
    /// The index provided is invalid
    InvalidIndex {
        /// The invalid index
        index: Span,
    },
}

impl Display for IllegalIndexingReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotIdentifier => {
                write!(f, "The expression is not an identifier.")
            }
            Self::NotIndexable => {
                write!(f, "The expression cannot be indexed.")
            }
            Self::InvalidComptimeType { expected } => {
                write!(f, "The expression can only be indexed with type {expected} that can be evaluated at compile time.")
            }
            Self::IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "The index {index} is out of bounds for the expression with length {length}."
                )
            }
            Self::InvalidIndex { index } => {
                write!(f, "The index {index} is invalid.", index = index.str())
            }
        }
    }
}

/// An error that occurs when an indexing operation is not permitted.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct InvalidArgument {
    /// The span of the argument.
    #[get = "pub"]
    pub span: Span,
    /// The reason why the argument is invalid.
    #[get = "pub"]
    pub reason: String,
}

impl Display for InvalidArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Message::new(Severity::Error, &self.reason))?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.span, Option::<u8>::None)
        )
    }
}

impl std::error::Error for InvalidArgument {}

/// An error that occurs when an indexing operation is not permitted.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct NotComptime {
    /// The expression that cannot be evaluated at compile time.
    #[get = "pub"]
    pub expression: Span,
}

impl Display for NotComptime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                "The expression cannot be evaluated at compile time but is required to."
            )
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.expression, Option::<u8>::None)
        )
    }
}

impl std::error::Error for NotComptime {}

/// An error that occurs when a loop never terminates.
#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
pub struct InfiniteLoop {
    /// The condition making it not terminate.
    pub span: Span,
}

impl Display for InfiniteLoop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(Severity::Error, "Loop never terminates.")
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(
                &self.span,
                Some("You may want to use a separate function with the `#[tick]` annotation.")
            )
        )
    }
}
