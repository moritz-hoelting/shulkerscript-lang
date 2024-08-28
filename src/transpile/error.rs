//! Errors that can occur during transpilation.

use std::{collections::HashMap, fmt::Display};

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
        functions: &HashMap<(String, String), FunctionData>,
    ) -> Self {
        let own_name = identifier_span.str();
        let own_program_identifier = identifier_span.source_file().identifier();
        let alternatives = functions
            .iter()
            .filter_map(|((program_identifier, function_name), data)| {
                let normalized_distance = strsim::normalized_levenshtein(own_name, function_name);
                (program_identifier == own_program_identifier
                    && (normalized_distance > 0.8
                        || strsim::levenshtein(own_name, function_name) < 3))
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
