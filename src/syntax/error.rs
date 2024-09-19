//! Contains the error types that can occur while parsing the syntax of the language.

use std::fmt::Display;

use crate::{
    base::{
        log::{Message, Severity, SourceCodeDisplay},
        source_file::Span,
    },
    lexical::token::{KeywordKind, Token},
};

/// Result type for parsing operations.
pub type ParseResult<T> = Result<T, Error>;

/// An enumeration containing all kinds of syntactic errors that can occur while parsing the
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    UnexpectedSyntax(#[from] UnexpectedSyntax),
    #[error(transparent)]
    InvalidArgument(#[from] InvalidArgument),
}

/// Enumeration containing all kinds of syntax that can be failed to parse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum SyntaxKind {
    Either(&'static [SyntaxKind]),
    Punctuation(char),
    Keyword(KeywordKind),
    Identifier,
    Declaration,
    Numeric,
    StringLiteral,
    Statement,
    Expression,
    Type,
    ExecuteBlock,
    ExecuteBlockTail,
}

impl SyntaxKind {
    fn expected_binding_str(&self) -> String {
        match self {
            Self::Either(variants) => {
                if variants.is_empty() {
                    "end of file".to_string()
                } else if variants.len() == 1 {
                    variants[0].expected_binding_str()
                } else {
                    let comma_range = ..variants.len() - 2;
                    let comma_elements = variants[comma_range]
                        .iter()
                        .map(Self::expected_binding_str)
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!(
                        "{}, or {}",
                        comma_elements,
                        variants.last().unwrap().expected_binding_str()
                    )
                }
            }
            Self::Identifier => "an identifier token".to_string(),
            Self::Punctuation(char) => format!("a punctuation token `{char}`"),
            Self::Keyword(keyword) => format!("a keyword token `{}`", keyword.as_str()),
            Self::Declaration => "a declaration token".to_string(),
            Self::Numeric => "a numeric token".to_string(),
            Self::StringLiteral => "a string literal".to_string(),
            Self::Statement => "a statement syntax".to_string(),
            Self::Expression => "an expression syntax".to_string(),
            Self::Type => "a type syntax".to_string(),
            Self::ExecuteBlock => "an execute block syntax".to_string(),
            Self::ExecuteBlockTail => "an execute block tail syntax".to_string(),
        }
    }
}

/// A syntax/token is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnexpectedSyntax {
    /// The kind of syntax that was expected.
    pub expected: SyntaxKind,

    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl Display for UnexpectedSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expected_binding = self.expected.expected_binding_str();
        let found_binding = match self.found.clone() {
            Some(Token::Comment(..)) => "a comment token".to_string(),
            Some(Token::DocComment(..)) => "a doc comment token".to_string(),
            Some(Token::Identifier(..)) => "an identifier token".to_string(),
            Some(Token::Keyword(keyword)) => {
                format!("a keyword token `{}`", keyword.keyword.as_str())
            }
            Some(Token::WhiteSpaces(..)) => "a white spaces token".to_string(),
            Some(Token::Punctuation(punctuation)) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Some(Token::Numeric(..)) => "a numeric token".to_string(),
            Some(Token::CommandLiteral(..)) => "a literal command token".to_string(),
            Some(Token::StringLiteral(..)) => "a string literal token".to_string(),

            None => "EOF".to_string(),
        };

        let message = format!("expected {expected_binding}, but found {found_binding}");

        write!(f, "{}", Message::new(Severity::Error, message))?;

        self.found.as_ref().map_or(Ok(()), |span| {
            write!(
                f,
                "\n{}",
                SourceCodeDisplay::new(span.span(), Option::<u8>::None)
            )
        })
    }
}

impl std::error::Error for UnexpectedSyntax {}

/// An error that occurred due to an invalid argument.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InvalidArgument {
    /// The error message.
    pub message: String,
    /// The span of the invalid argument.
    pub span: Span,
}

impl Display for InvalidArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Message::new(Severity::Error, &self.message))?;
        write!(
            f,
            "\n{}",
            SourceCodeDisplay::new(&self.span, Option::<u8>::None)
        )
    }
}

impl std::error::Error for InvalidArgument {}
