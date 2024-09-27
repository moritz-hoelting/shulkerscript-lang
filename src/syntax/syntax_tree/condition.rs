//! Syntax tree nodes for conditions.

#![allow(clippy::missing_errors_doc)]

use std::{cmp::Ordering, collections::VecDeque};

use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler, VoidHandler,
    },
    lexical::{
        token::{Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, ParseResult, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

/// Condition that is viewed as a single entity during precedence parsing.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// PrimaryCondition:
///     UnaryCondition
///     | ParenthesizedCondition
///     | StringLiteral
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PrimaryCondition {
    Unary(UnaryCondition),
    Parenthesized(ParenthesizedCondition),
    StringLiteral(StringLiteral),
}

impl SourceElement for PrimaryCondition {
    fn span(&self) -> Span {
        match self {
            Self::Unary(unary) => unary.span(),
            Self::Parenthesized(parenthesized) => parenthesized.span(),
            Self::StringLiteral(literal) => literal.span(),
        }
    }
}

/// Condition that is composed of two conditions and a binary operator.
///
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

/// Operator that is used to combine two conditions.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// ConditionalBinaryOperator:
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

/// Condition that is enclosed in parentheses.
///
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

/// Operator that is used to prefix a condition.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// ConditionalPrefixOperator: '!';
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

/// Condition that is prefixed by an operator.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// UnaryCondition:
///     ConditionalPrefixOperator PrimaryCondition
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct UnaryCondition {
    /// The operator of the prefix.
    #[get = "pub"]
    operator: ConditionalPrefixOperator,
    /// The operand of the prefix.
    #[get = "pub"]
    operand: Box<PrimaryCondition>,
}

impl SourceElement for UnaryCondition {
    fn span(&self) -> Span {
        self.operator.span().join(&self.operand.span()).unwrap()
    }
}
impl UnaryCondition {
    /// Dissolves the conditional prefix into its components
    #[must_use]
    pub fn dissolve(self) -> (ConditionalPrefixOperator, PrimaryCondition) {
        (self.operator, *self.operand)
    }
}

/// Represents a condition in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Condition:
///     PrimaryCondition
///     | BinaryCondition
///     ;
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
    ///
    /// # Precedence of the operators
    /// 1. `!`
    /// 2. `&&`
    /// 3. `||`
    pub fn parse_condition(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Condition> {
        let mut lhs = Condition::Primary(self.parse_primary_condition(handler)?);
        let mut expressions = VecDeque::new();

        // Parses a list of binary operators and expressions
        while let Ok(binary_operator) = self.try_parse_conditional_binary_operator() {
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

        Ok(lhs)
    }

    /// Parses a [`PrimaryCondition`].
    pub fn parse_primary_condition(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<PrimaryCondition> {
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

                Ok(PrimaryCondition::Unary(UnaryCondition {
                    operator,
                    operand,
                }))
            }

            // string literal
            Reading::Atomic(Token::StringLiteral(literal)) => {
                self.forward();
                Ok(PrimaryCondition::StringLiteral(literal))
            }

            // parenthesized condition
            Reading::IntoDelimited(punc) if punc.punctuation == '(' => self
                .parse_parenthesized_condition(handler)
                .map(PrimaryCondition::Parenthesized),

            unexpected => {
                // make progress
                self.forward();

                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Either(&[
                        SyntaxKind::Punctuation('!'),
                        SyntaxKind::StringLiteral,
                        SyntaxKind::Punctuation('('),
                    ]),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());

                Err(err)
            }
        }
    }

    /// Parses a [`ParenthesizedCondition`].
    pub fn parse_parenthesized_condition(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<ParenthesizedCondition> {
        let token_tree = self.step_into(
            Delimiter::Parenthesis,
            |parser| {
                let cond = parser.parse_condition(handler)?;
                parser.stop_at_significant();
                Ok(cond)
            },
            handler,
        )?;

        Ok(ParenthesizedCondition {
            open_paren: token_tree.open,
            condition: Box::new(token_tree.tree?),
            close_paren: token_tree.close,
        })
    }

    fn try_parse_conditional_binary_operator(&mut self) -> ParseResult<ConditionalBinaryOperator> {
        self.try_parse(|parser| match parser.next_significant_token() {
            Reading::Atomic(token) => match token.clone() {
                Token::Punctuation(punc) => match punc.punctuation {
                    '&' => {
                        let b = parser.parse_punctuation('&', false, &VoidHandler)?;
                        Ok(ConditionalBinaryOperator::LogicalAnd(punc, b))
                    }
                    '|' => {
                        let b = parser.parse_punctuation('|', false, &VoidHandler)?;
                        Ok(ConditionalBinaryOperator::LogicalOr(punc, b))
                    }
                    _ => Err(Error::UnexpectedSyntax(UnexpectedSyntax {
                        expected: SyntaxKind::Either(&[
                            SyntaxKind::Punctuation('&'),
                            SyntaxKind::Punctuation('|'),
                        ]),
                        found: Some(token),
                    })),
                },
                unexpected => Err(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Either(&[
                        SyntaxKind::Punctuation('&'),
                        SyntaxKind::Punctuation('|'),
                    ]),
                    found: Some(unexpected),
                })),
            },
            unexpected => Err(Error::UnexpectedSyntax(UnexpectedSyntax {
                expected: SyntaxKind::Either(&[
                    SyntaxKind::Punctuation('&'),
                    SyntaxKind::Punctuation('|'),
                ]),
                found: unexpected.into_token(),
            })),
        })
    }
}
