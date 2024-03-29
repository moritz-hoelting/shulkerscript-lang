//! Syntax tree nodes for expressions.

use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

/// Syntax Synopsis:
///
/// ``` ebnf
/// ParenthesizedCondition:
///    '(' Condition ')';
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ParenthesizedCondition {
    /// The opening parenthesis.
    #[get = "pub"]
    pub open_paren: Punctuation,
    /// The condition within the parenthesis.
    #[get = "pub"]
    pub condition: Box<Condition>,
    /// The closing parenthesis.
    #[get = "pub"]
    pub close_paren: Punctuation,
}

impl ParenthesizedCondition {
    /// Dissolves the parenthesized condition into its components
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Condition, Punctuation) {
        (self.open_paren, *self.condition, self.close_paren)
    }
}

impl SourceElement for ParenthesizedCondition {
    fn span(&self) -> Span {
        self.open_paren
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the parenthesis is invalid.")
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Condition: StringLiteral;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Condition {
    /// The value of the condition.
    #[get = "pub"]
    pub value: StringLiteral,
}

impl SourceElement for Condition {
    fn span(&self) -> Span {
        self.value.span()
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`Condition`].
    pub fn parse_condition(&mut self, handler: &impl Handler<Error>) -> Option<Condition> {
        match self.next_significant_token() {
            Reading::Atomic(Token::StringLiteral(s)) => Some(Condition { value: s }),
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

    /// Parses a [`ParenthesizedCondition`].
    pub fn parse_parenthesized_condition(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<ParenthesizedCondition> {
        let token_tree = self.step_into(
            Delimiter::Parenthesis,
            |parser| parser.parse_condition(handler),
            handler,
        )?;

        Some(ParenthesizedCondition {
            open_paren: token_tree.open,
            condition: Box::new(token_tree.tree?),
            close_paren: token_tree.close,
        })
    }
}
