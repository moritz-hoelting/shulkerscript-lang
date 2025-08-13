//! Error types for the semantic analysis phase of the compiler.

#![allow(missing_docs)]

use std::fmt::Display;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::{SourceElement as _, Span},
    },
    lexical::token::StringLiteral,
    syntax::syntax_tree::expression::Expression,
    transpile::error::{
        AssignmentError, IllegalIndexing, MismatchedTypes, MissingFunctionDeclaration,
        UnknownIdentifier,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    MissingFunctionDeclaration(#[from] MissingFunctionDeclaration),
    #[error(transparent)]
    UnexpectedExpression(#[from] UnexpectedExpression),
    #[error(transparent)]
    ConflictingFunctionNames(#[from] ConflictingFunctionNames),
    #[error(transparent)]
    InvalidNamespaceName(#[from] InvalidNamespaceName),
    #[error(transparent)]
    UnresolvedMacroUsage(#[from] UnresolvedMacroUsage),
    #[error(transparent)]
    IncompatibleFunctionAnnotation(#[from] IncompatibleFunctionAnnotation),
    #[error(transparent)]
    IllegalIndexing(#[from] IllegalIndexing),
    #[error(transparent)]
    MismatchedTypes(#[from] MismatchedTypes),
    #[error(transparent)]
    UnknownIdentifier(#[from] UnknownIdentifier),
    #[error(transparent)]
    AssignmentError(#[from] AssignmentError),
    #[error("Lua is disabled, but a Lua function was used.")]
    LuaDisabled,
    #[error("Other: {0}")]
    Other(String),
}

/// An error that occurs when a function declaration is missing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnexpectedExpression(pub Box<Expression>);

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidNamespaceName {
    pub name: StringLiteral,
    pub invalid_chars: String,
}

impl Display for InvalidNamespaceName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!(
                    "Invalid characters in namespace `{}`. The following characters are not allowed in namespace definitions: `{}`",
                    self.name.str_content(),
                    self.invalid_chars
                )
            )
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.name.span, Option::<u8>::None)
        )
    }
}

impl std::error::Error for InvalidNamespaceName {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnresolvedMacroUsage {
    pub span: Span,
}

impl Display for UnresolvedMacroUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!(
                    "Macro `{}` was used, but could not be resolved.",
                    self.span.str(),
                )
            )
        )?;

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(
                &self.span,
                Some(format!(
                    "You might want to add `{}` to the function parameters.",
                    self.span.str()
                ))
            )
        )
    }
}

impl std::error::Error for UnresolvedMacroUsage {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IncompatibleFunctionAnnotation {
    pub span: Span,
    pub reason: String,
}

impl Display for IncompatibleFunctionAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!(
                    "Annotation `{}` cannot be used here, because {}.",
                    self.span.str(),
                    self.reason
                )
            )
        )?;

        write!(f, "\n{}", SourceCodeDisplay::new(&self.span, None::<u8>))
    }
}

impl std::error::Error for IncompatibleFunctionAnnotation {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidFunctionArguments {
    pub span: Span,
    pub expected: usize,
    pub actual: usize,
}

impl Display for InvalidFunctionArguments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            Message::new(
                Severity::Error,
                format!(
                    "Expected {} arguments, but got {}.",
                    self.expected, self.actual
                )
            )
        )?;

        let help_message = if self.expected > self.actual {
            format!(
                "You might want to add {} more arguments.",
                self.expected - self.actual
            )
        } else {
            format!(
                "You might want to remove {} arguments.",
                self.actual - self.expected
            )
        };

        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.span, Some(help_message))
        )
    }
}

impl std::error::Error for InvalidFunctionArguments {}
