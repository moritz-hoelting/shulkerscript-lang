//! Syntax tree nodes for conditions.

use std::{cmp::Ordering, collections::VecDeque};

use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Dummy, Handler,
    },
    lexical::{
        token::{Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

/// Syntax Synopsis:
///
/// ``` ebnf
/// Expression:
///     Prefix
///     | Parenthesized
///     | StringLiteral
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PrimaryCondition {
    Prefix(ConditionalPrefix),
    Parenthesized(ParenthesizedCondition),
    StringLiteral(StringLiteral),
}

impl SourceElement for PrimaryCondition {
    fn span(&self) -> Span {
        match self {
            Self::Prefix(prefix) => prefix.span(),
            Self::Parenthesized(parenthesized) => parenthesized.span(),
            Self::StringLiteral(literal) => literal.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// BinaryCondition:
///     Condition ConditionalBinaryOperator Condition
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct BinaryCondition {
    /// The left operand of the binary condition.
    #[get = "pub"]
    left_operand: Box<Condition>,
    /// The operator of the binary condition.
    #[get = "pub"]
    operator: ConditionalBinaryOperator,
    /// The right operand of the binary condition.
    #[get = "pub"]
    right_operand: Box<Condition>,
}

impl SourceElement for BinaryCondition {
    fn span(&self) -> Span {
        self.left_operand
            .span()
            .join(&self.right_operand.span())
            .unwrap()
    }
}

impl BinaryCondition {
    /// Dissolves the binary condition into its components
    #[must_use]
    pub fn dissolve(self) -> (Condition, ConditionalBinaryOperator, Condition) {
        (*self.left_operand, self.operator, *self.right_operand)
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// BinaryOperator:
///     '&&'
///     | '||'
///     ;
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ConditionalBinaryOperator {
    LogicalAnd(Punctuation, Punctuation),
    LogicalOr(Punctuation, Punctuation),
}

impl ConditionalBinaryOperator {
    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    #[must_use]
    pub fn get_precedence(&self) -> u8 {
        match self {
            Self::LogicalOr(..) => 1,
            Self::LogicalAnd(..) => 2,
        }
    }
}

impl SourceElement for ConditionalBinaryOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalAnd(a, b) | Self::LogicalOr(a, b) => a
                .span
                .join(&b.span)
                .expect("Invalid tokens for ConditionalBinaryOperator"),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// ParenthesizedCondition:
///    '(' Condition ')';
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
/// PrefixOperator: '!';
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum ConditionalPrefixOperator {
    LogicalNot(Punctuation),
}

impl SourceElement for ConditionalPrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// Prefix:
///     ConditionalPrefixOperator StringLiteral
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ConditionalPrefix {
    /// The operator of the prefix.
    #[get = "pub"]
    operator: ConditionalPrefixOperator,
    /// The operand of the prefix.
    #[get = "pub"]
    operand: Box<PrimaryCondition>,
}

impl SourceElement for ConditionalPrefix {
    fn span(&self) -> Span {
        self.operator.span().join(&self.operand.span()).unwrap()
    }
}
impl ConditionalPrefix {
    /// Dissolves the conditional prefix into its components
    #[must_use]
    pub fn dissolve(self) -> (ConditionalPrefixOperator, PrimaryCondition) {
        (self.operator, *self.operand)
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Condition: PrimaryCondition;
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Condition {
    Primary(PrimaryCondition),
    Binary(BinaryCondition),
}

impl SourceElement for Condition {
    fn span(&self) -> Span {
        match self {
            Self::Primary(primary) => primary.span(),
            Self::Binary(binary) => binary.span(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`Condition`].
    pub fn parse_condition(&mut self, handler: &impl Handler<Error>) -> Option<Condition> {
        let mut lhs = Condition::Primary(self.parse_primary_condition(handler)?);
        let mut expressions = VecDeque::new();

        // Parses a list of binary operators and expressions
        while let Some(binary_operator) = self.try_parse_conditional_binary_operator() {
            expressions.push_back((
                binary_operator,
                Some(Condition::Primary(self.parse_primary_condition(handler)?)),
            ));
        }

        let mut candidate_index = 0;
        let mut current_precedence;

        while !expressions.is_empty() {
            // reset precedence
            current_precedence = 0;

            for (index, (binary_op, _)) in expressions.iter().enumerate() {
                let new_precedence = binary_op.get_precedence();
                match new_precedence.cmp(&current_precedence) {
                    // Clear the candidate indices and set the current precedence to the
                    // precedence of the current binary operator.
                    Ordering::Greater => {
                        current_precedence = new_precedence;
                        candidate_index = index;
                    }

                    Ordering::Less | Ordering::Equal => (),
                }
            }

            // ASSUMPTION: The assignments have 1 precedence and are right associative.
            assert!(current_precedence > 0);

            if candidate_index == 0 {
                let (binary_op, rhs) = expressions.pop_front().expect("No binary operator found");

                // fold the first expression
                lhs = Condition::Binary(BinaryCondition {
                    left_operand: Box::new(lhs),
                    operator: binary_op,
                    right_operand: Box::new(rhs.unwrap()),
                });
            } else {
                let (binary_op, rhs) = expressions
                    .remove(candidate_index)
                    .expect("No binary operator found");

                // fold the expression at candidate_index
                expressions[candidate_index - 1].1 = Some(Condition::Binary(BinaryCondition {
                    left_operand: Box::new(expressions[candidate_index - 1].1.take().unwrap()),
                    operator: binary_op,
                    right_operand: Box::new(rhs.unwrap()),
                }));
            }
        }

        Some(lhs)
    }

    /// Parses a [`PrimaryCondition`].
    pub fn parse_primary_condition(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<PrimaryCondition> {
        match self.stop_at_significant() {
            // prefixed expression
            Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '!' => {
                // eat prefix operator
                self.forward();

                let operator = match punc.punctuation {
                    '!' => ConditionalPrefixOperator::LogicalNot(punc),
                    _ => unreachable!(),
                };

                let operand = Box::new(self.parse_primary_condition(handler)?);

                Some(PrimaryCondition::Prefix(ConditionalPrefix {
                    operator,
                    operand,
                }))
            }

            // string literal
            Reading::Atomic(Token::StringLiteral(literal)) => {
                self.forward();
                Some(PrimaryCondition::StringLiteral(literal))
            }

            // parenthesized condition
            Reading::IntoDelimited(punc) if punc.punctuation == '(' => self
                .parse_parenthesized_condition(handler)
                .map(PrimaryCondition::Parenthesized),

            unexpected => {
                // make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Expression,
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

    fn try_parse_conditional_binary_operator(&mut self) -> Option<ConditionalBinaryOperator> {
        self.try_parse(|parser| match parser.next_significant_token() {
            Reading::Atomic(Token::Punctuation(punc)) => match punc.punctuation {
                '&' => {
                    let b = parser.parse_punctuation('&', false, &Dummy)?;
                    Some(ConditionalBinaryOperator::LogicalAnd(punc, b))
                }
                '|' => {
                    let b = parser.parse_punctuation('|', false, &Dummy)?;
                    Some(ConditionalBinaryOperator::LogicalOr(punc, b))
                }
                _ => None,
            },
            _ => None,
        })
    }
}
