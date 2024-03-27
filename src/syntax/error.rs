//! Contains the error types that can occur while parsing the syntax of the language.

use std::fmt::Display;

use crate::{
    base::log::{Message, Severity, SourceCodeDisplay},
    lexical::token::{KeywordKind, Token},
};

/// Enumeration containing all kinds of syntax that can be failed to parse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum SyntaxKind {
    Punctuation(char),
    Keyword(KeywordKind),
    Identifier,
    Declaration,
    Numeric,
    Statement,
    Expression,
    Type,
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
        let expected_binding = match self.expected {
            SyntaxKind::Identifier => "an identifier token".to_string(),
            SyntaxKind::Punctuation(char) => format!("a punctuation token `{char}`"),
            SyntaxKind::Keyword(keyword) => format!("a keyword token `{}`", keyword.as_str()),
            SyntaxKind::Declaration => "a declaration token".to_string(),
            SyntaxKind::Numeric => "a numeric token".to_string(),
            SyntaxKind::Statement => "a statement syntax".to_string(),
            SyntaxKind::Expression => "an expression syntax".to_string(),
            SyntaxKind::Type => "a type syntax".to_string(),
        };
        let found_binding = match self.found.clone() {
            Some(Token::Comment(..)) => "a comment token".to_string(),
            Some(Token::Identifier(..)) => "an identifier token".to_string(),
            Some(Token::Keyword(keyword)) => {
                format!("a keyword token `{}`", keyword.keyword.as_str())
            }
            Some(Token::WhiteSpaces(..)) => "a white spaces token".to_string(),
            Some(Token::Punctuation(punctuation)) => {
                format!("a punctuation token `{}`", punctuation.punctuation)
            }
            Some(Token::Numeric(..)) => "a numeric token".to_string(),
            Some(Token::LiteralCommand(..)) => "a literal command token".to_string(),

            None => "EOF".to_string(),
        };

        let message = format!("expected {expected_binding}, but found {found_binding}");

        write!(f, "{}", Message::new(Severity::Error, message))?;

        self.found.as_ref().map_or(Ok(()), |span| {
            write!(
                f,
                "\n{}",
                SourceCodeDisplay::new(span.span(), Option::<i32>::None)
            )
        })
    }
}

impl std::error::Error for UnexpectedSyntax {}

/// An enumeration containing all kinds of syntactic errors that can occur while parsing the
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("{0}")]
    UnexpectedSyntax(#[from] UnexpectedSyntax),
}
