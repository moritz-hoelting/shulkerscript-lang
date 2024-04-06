//! Syntax tree nodes for expressions.

use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{Identifier, Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::ConnectedList;

/// Syntax Synopsis:
///
/// ```ebnf
/// Expression:
///     Primary
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Primary(Primary),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primary(primary) => primary.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Primary:
///     FunctionCall
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Primary {
    FunctionCall(FunctionCall),
    StringLiteral(StringLiteral),
}

impl SourceElement for Primary {
    fn span(&self) -> Span {
        match self {
            Self::FunctionCall(function_call) => function_call.span(),
            Self::StringLiteral(string_literal) => string_literal.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// FunctionCall:
///     Identifier '(' (Expression (',' Expression)*)? ')'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionCall {
    /// The identifier of the function.
    #[get = "pub"]
    identifier: Identifier,
    /// The left parenthesis of the function call.
    #[get = "pub"]
    left_parenthesis: Punctuation,
    /// The arguments of the function call.
    #[get = "pub"]
    arguments: Option<ConnectedList<Box<Expression>, Punctuation>>,
    /// The right parenthesis of the function call.
    #[get = "pub"]
    right_parenthesis: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Span {
        self.identifier
            .span()
            .join(&self.right_parenthesis.span)
            .unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`Expression`]
    pub fn parse_expression(&mut self, handler: &impl Handler<Error>) -> Option<Expression> {
        Some(Expression::Primary(self.parse_primary(handler)?))
    }

    /// Parses an [`Primary`]
    pub fn parse_primary(&mut self, handler: &impl Handler<Error>) -> Option<Primary> {
        match self.stop_at_significant() {
            // identifier expression
            Reading::Atomic(Token::Identifier(identifier)) => {
                // eat the identifier
                self.forward();

                // function call
                if matches!(self.stop_at_significant(), Reading::IntoDelimited(punc) if punc.punctuation == '(')
                {
                    let token_tree = self.parse_enclosed_list(
                        Delimiter::Parenthesis,
                        ',',
                        |parser| parser.parse_expression(handler).map(Box::new),
                        handler,
                    )?;

                    Some(Primary::FunctionCall(FunctionCall {
                        identifier,
                        left_parenthesis: token_tree.open,
                        right_parenthesis: token_tree.close,
                        arguments: token_tree.list,
                    }))
                } else {
                    // insert parser for regular identifier here
                    None
                }
            }

            // string literal expression
            Reading::Atomic(Token::StringLiteral(literal)) => {
                // eat the string literal
                self.forward();

                Some(Primary::StringLiteral(literal))
            }

            unexpected => {
                // make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: crate::syntax::error::SyntaxKind::Expression,
                    found: unexpected.into_token(),
                }));

                None
            }
        }
    }
}
