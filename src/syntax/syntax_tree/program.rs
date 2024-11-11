//! The program node of the syntax tree.

use getset::Getters;
use itertools::Itertools;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::token::{Keyword, KeywordKind, Punctuation, StringLiteral, Token},
    syntax::{
        self,
        error::{ParseResult, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::declaration::Declaration;

/// Program is a collection of declarations preceeded by a namespace selector.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ProgramFile {
    /// The namespace selector.
    #[get = "pub"]
    namespace: Namespace,
    /// The declarations within the program.
    #[get = "pub"]
    declarations: Vec<Declaration>,
}

/// Namespace is a namespace selector.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// Namespace:
///    'namespace' StringLiteral ';' ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Namespace {
    /// The `namespace` keyword.
    #[get = "pub"]
    namespace_keyword: Keyword,
    /// The name of the namespace.
    #[get = "pub"]
    namespace_name: StringLiteral,
    /// The semicolon.
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Namespace {
    fn span(&self) -> Span {
        self.namespace_keyword
            .span()
            .join(&self.semicolon.span())
            .expect("Invalid span")
    }
}

impl Namespace {
    /// Dissolves the namespace into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, StringLiteral, Punctuation) {
        (self.namespace_keyword, self.namespace_name, self.semicolon)
    }

    /// Validates a namespace string.
    ///
    /// # Errors
    /// - If the namespace contains invalid characters.
    pub fn validate_str(namespace: &str) -> Result<(), String> {
        const VALID_CHARS: &str = "0123456789abcdefghijklmnopqrstuvwxyz_-.";

        let invalid_chars = namespace
            .chars()
            .filter(|c| !VALID_CHARS.contains(*c))
            .sorted()
            .unique()
            .collect::<String>();

        if invalid_chars.is_empty() {
            Ok(())
        } else {
            Err(invalid_chars)
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`ProgramFile`].
    #[tracing::instrument(level = "debug", skip_all)]
    pub fn parse_program(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<ProgramFile> {
        tracing::debug!("Parsing program");

        let namespace = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(namespace_keyword))
                if namespace_keyword.keyword == KeywordKind::Namespace =>
            {
                // eat the keyword
                self.forward();

                let namespace_name = self.parse_string_literal(handler)?;

                let semicolon = self.parse_punctuation(';', true, handler)?;

                Ok(Namespace {
                    namespace_keyword,
                    namespace_name,
                    semicolon,
                })
            }
            unexpected => {
                let err = syntax::error::Error::from(UnexpectedSyntax {
                    expected: SyntaxKind::Keyword(KeywordKind::Namespace),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }?;

        tracing::debug!(
            "Found namespace '{}', parsing declarations",
            namespace.namespace_name.str_content()
        );

        let mut declarations = Vec::new();

        while !self.is_exhausted() {
            let result = self.parse_declaration(handler);

            #[allow(clippy::option_if_let_else)]
            if let Ok(x) = result {
                declarations.push(x);
            } else {
                self.stop_at(|reading| {
                    matches!(
                        reading,
                        Reading::IntoDelimited(x) if x.punctuation == '{'
                    )
                });

                self.next_token();
            }
        }

        Ok(ProgramFile {
            namespace,
            declarations,
        })
    }
}
