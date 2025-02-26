//! Errors that can occur during transpilation.

use std::{fmt::Display, sync::Arc};

use getset::Getters;
use itertools::Itertools;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::Span,
    },
    semantic::error::{ConflictingFunctionNames, InvalidFunctionArguments, UnexpectedExpression},
};

use super::{
    variables::{Scope, VariableType},
    FunctionData,
};

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
    #[error(transparent)]
    InvalidFunctionArguments(#[from] InvalidFunctionArguments),
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
    #[cfg_attr(not(feature = "shulkerbox"), expect(unused))]
    pub(super) fn from_scope(identifier_span: Span, scope: &Arc<Scope>) -> Self {
        let own_name = identifier_span.str();
        let alternatives = scope
            .get_variables()
            .read()
            .unwrap()
            .iter()
            .filter_map(|(name, value)| {
                let data = match value.as_ref() {
                    VariableType::Function { function_data, .. } => function_data,
                    _ => return None,
                };

                let normalized_distance = strsim::normalized_damerau_levenshtein(own_name, name);
                (normalized_distance > 0.8 || strsim::damerau_levenshtein(own_name, name) < 3)
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
