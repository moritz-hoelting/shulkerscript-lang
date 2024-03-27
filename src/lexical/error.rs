use std::fmt::Display;

use getset::Getters;

use crate::base::{
    log::{Message, Severity, SourceCodeDisplay},
    source_file::Span,
};

use super::token_stream::Delimiter;

/// Represents an error that occurred during the lexical analysis of the source code.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, thiserror::Error)]
pub enum Error {
    #[error("Comment is not terminated.")]
    UnterminatedDelimitedComment(#[from] UnterminatedDelimitedComment),
    #[error("Delimiter is not terminated.")]
    UndelimitedDelimiter(#[from] UndelimitedDelimiter),
}

/// Source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters, thiserror::Error)]
pub struct UnterminatedDelimitedComment {
    /// Span of the unclosed `/*` that starts the comment.
    pub span: Span,
}

impl Display for UnterminatedDelimitedComment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "found an unclosed `/*` comment"),
            SourceCodeDisplay::new(&self.span, Option::<i32>::None)
        )
    }
}

/// Delimiter is not closed by its corresponding closing pair.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Getters, thiserror::Error)]
pub struct UndelimitedDelimiter {
    /// Span of the opening delimiter.
    pub opening_span: Span,

    /// Kind of the delimiter.
    pub delimiter: Delimiter,
}

impl Display for UndelimitedDelimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n{}",
            Message::new(Severity::Error, "found an undelimited delimiter"),
            SourceCodeDisplay::new(
                &self.opening_span,
                Some("this delimiter is not closed by its corresponding closing pair")
            )
        )
    }
}
