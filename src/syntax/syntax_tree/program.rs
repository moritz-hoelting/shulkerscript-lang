//! The program node of the syntax tree.

use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::token::{Keyword, KeywordKind, Punctuation, StringLiteral, Token},
    syntax::{
        error::{Error, SyntaxKind, UnexpectedSyntax},
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
    #[must_use]
    pub fn validate_str(namespace: &str) -> bool {
        const VALID_CHARS: &str = "0123456789abcdefghijklmnopqrstuvwxyz_-.";

        namespace.chars().all(|c| VALID_CHARS.contains(c))
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`ProgramFile`].
    #[tracing::instrument(level = "debug", skip_all)]
    pub fn parse_program(&mut self, handler: &impl Handler<Error>) -> Option<ProgramFile> {
        tracing::debug!("Parsing program");

        let namespace = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(namespace_keyword))
                if namespace_keyword.keyword == KeywordKind::Namespace =>
            {
                // eat the keyword
                self.forward();

                let namespace_name = self
                    .parse_string_literal(handler)
                    .and_then(|name| Namespace::validate_str(name.str_content()).then_some(name))?;

                let semicolon = self.parse_punctuation(';', true, handler)?;

                Some(Namespace {
                    namespace_keyword,
                    namespace_name,
                    semicolon,
                })
            }
            unexpected => {
                handler.receive(UnexpectedSyntax {
                    expected: SyntaxKind::Keyword(KeywordKind::Namespace),
                    found: unexpected.into_token(),
                });
                None
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
            if let Some(x) = result {
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

        Some(ProgramFile {
            namespace,
            declarations,
        })
    }
}
