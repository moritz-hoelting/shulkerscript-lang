//! Errors that can occur during transpilation.

use std::{collections::BTreeMap, fmt::Display};

use getset::Getters;
use itertools::Itertools;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::{SourceElement, Span},
    },
    syntax::syntax_tree::expression::Expression,
};

use super::transpiler::FunctionData;

/// Errors that can occur during transpilation.
#[allow(clippy::module_name_repetitions, missing_docs)]
#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum TranspileError {
    #[error(transparent)]
    MissingFunctionDeclaration(#[from] MissingFunctionDeclaration),
    #[error(transparent)]
    UnexpectedExpression(#[from] UnexpectedExpression),
    #[error("Lua code evaluation is disabled.")]
    LuaDisabled,
    #[error(transparent)]
    LuaRuntimeError(#[from] LuaRuntimeError),
    #[error(transparent)]
    ConflictingFunctionNames(#[from] ConflictingFunctionNames),
}

/// The result of a transpilation operation.
pub type TranspileResult<T> = Result<T, TranspileError>;

/// An error that occurs when a function declaration is missing.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct MissingFunctionDeclaration {
    #[get = "pub"]
    span: Span,
    #[get = "pub"]
    alternatives: Vec<FunctionData>,
}

impl MissingFunctionDeclaration {
    pub(super) fn from_context(
        identifier_span: Span,
        functions: &BTreeMap<(String, String), FunctionData>,
    ) -> Self {
        let own_name = identifier_span.str();
        let own_program_identifier = identifier_span.source_file().identifier();
        let alternatives = functions
            .iter()
            .filter_map(|((program_identifier, function_name), data)| {
                let normalized_distance =
                    strsim::normalized_damerau_levenshtein(own_name, function_name);
                (program_identifier == own_program_identifier
                    && (normalized_distance > 0.8
                        || strsim::damerau_levenshtein(own_name, function_name) < 3))
                    .then_some((normalized_distance, data))
            })
            .sorted_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal))
            .map(|(_, data)| data)
            .take(8)
            .cloned()
            .collect::<Vec<_>>();

        Self {
            alternatives,
            span: identifier_span,
        }
    }
}

impl Display for MissingFunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = format!(
            "no matching function declaration found for invocation of function `{}`",
            self.span.str()
        );
        write!(f, "{}", Message::new(Severity::Error, message))?;

        let help_message = if self.alternatives.is_empty() {
            None
        } else {
            let mut message = String::from("did you mean ");
            for (i, alternative) in self.alternatives.iter().enumerate() {
                if i > 0 {
                    message.push_str(", ");
                }
                message.push_str(&format!("`{}`", alternative.identifier_span.str()));
            }
            Some(message + "?")
        };

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.span, help_message.as_ref())
        )
    }
}

impl std::error::Error for MissingFunctionDeclaration {}

/// An error that occurs when a function declaration is missing.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LuaRuntimeError {
    pub code_block: Span,
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

/// An error that occurs when a function declaration is missing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnexpectedExpression(pub Expression);

impl Display for UnexpectedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(Severity::Error, "encountered unexpected expression")
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.0.span(), Option::<u8>::None)
        )
    }
}

impl std::error::Error for UnexpectedExpression {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConflictingFunctionNames {
    pub definition: Span,
    pub name: String,
}

impl Display for ConflictingFunctionNames {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!("the following function declaration conflicts with an existing function with name `{}`", self.name)
            )
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.definition, Option::<u8>::None)
        )
    }
}

impl std::error::Error for ConflictingFunctionNames {}
