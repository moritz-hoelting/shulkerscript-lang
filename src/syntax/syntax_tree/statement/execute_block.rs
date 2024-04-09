//! Execute block statement syntax tree.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        DummyHandler, Handler,
    },
    lexical::{
        token::{Keyword, KeywordKind, Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
        syntax_tree::condition::ParenthesizedCondition,
    },
};

use super::Block;

/// Syntax Synopsis:
/// ```ebnf
/// ExecuteBlock:
///    ExecuteBlockHead ExecuteBlockTail
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum ExecuteBlock {
    HeadTail(ExecuteBlockHead, ExecuteBlockTail),
    IfElse(Conditional, Block, Else),
}
impl SourceElement for ExecuteBlock {
    fn span(&self) -> Span {
        match self {
            Self::HeadTail(head, tail) => head.span().join(&tail.span()).unwrap(),
            Self::IfElse(conditional, block, else_) => conditional
                .span()
                .join(&block.span())
                .unwrap()
                .join(&else_.span())
                .unwrap(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ExecuteBlockHead {
    Conditional(Conditional),
    As(As),
}

impl SourceElement for ExecuteBlockHead {
    fn span(&self) -> Span {
        match self {
            Self::Conditional(conditional) => conditional.span(),
            Self::As(as_) => as_.span(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ExecuteBlockTail {
    ExecuteBlock(Punctuation, Box<ExecuteBlock>),
    Block(Block),
}

impl SourceElement for ExecuteBlockTail {
    fn span(&self) -> Span {
        match self {
            Self::ExecuteBlock(punc, execute_block) => punc
                .span
                .join(&execute_block.span())
                .expect("The span of the execute block tail is invalid."),
            Self::Block(block) => block.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Conditional:
/// 'if' ParenthizedCondition
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Conditional {
    /// The `if` keyword.
    #[get = "pub"]
    if_keyword: Keyword,
    /// The condition of the conditional.
    #[get = "pub"]
    condition: ParenthesizedCondition,
}

impl Conditional {
    /// Dissolves the [`Conditional`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, ParenthesizedCondition) {
        (self.if_keyword, self.condition)
    }
}

impl SourceElement for Conditional {
    fn span(&self) -> Span {
        self.if_keyword
            .span()
            .join(&self.condition.span())
            .expect("The span of the conditional is invalid.")
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Else:
///     'else' Block
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Else {
    /// The `else` keyword.
    #[get = "pub"]
    else_keyword: Keyword,
    /// The block of the else statement.
    #[get = "pub"]
    block: Box<Block>,
}

impl Else {
    /// Dissolves the [`Else`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Box<Block>) {
        (self.else_keyword, self.block)
    }
}

impl SourceElement for Else {
    fn span(&self) -> Span {
        self.else_keyword.span().join(&self.block.span()).unwrap()
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// As:
/// 'as' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct As {
    /// The `as` keyword.
    #[get = "pub"]
    as_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the as statement.
    #[get = "pub"]
    as_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}

impl SourceElement for As {
    fn span(&self) -> Span {
        self.as_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the as statement is invalid.")
    }
}
impl As {
    /// Dissolves the [`As`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, StringLiteral, Punctuation) {
        (self.as_keyword, self.as_selector, self.close_paren)
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`ExecuteBlock`].
    pub fn parse_execute_block_statement(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<ExecuteBlock> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(if_keyword))
                if if_keyword.keyword == KeywordKind::If =>
            {
                // eat the if keyword
                self.forward();

                let condition = self.parse_parenthesized_condition(handler)?;

                let conditional = Conditional {
                    if_keyword,
                    condition,
                };

                let else_tail = self.try_parse(|parser| {
                    let block = parser.parse_block(&DummyHandler)?;
                    let (else_keyword, else_block) = match parser.stop_at_significant() {
                        // else statement
                        Reading::Atomic(Token::Keyword(else_keyword))
                            if else_keyword.keyword == KeywordKind::Else =>
                        {
                            // eat the else keyword
                            parser.forward();

                            let else_block = parser.parse_block(handler)?;

                            Some((else_keyword, else_block))
                        }
                        _ => None,
                    }?;

                    Some((
                        block,
                        Else {
                            else_keyword,
                            block: Box::new(else_block),
                        },
                    ))
                });

                if let Some((block, else_tail)) = else_tail {
                    Some(ExecuteBlock::IfElse(conditional, block, else_tail))
                } else {
                    let tail = self.parse_execute_block_tail(handler)?;
                    Some(ExecuteBlock::HeadTail(
                        ExecuteBlockHead::Conditional(conditional),
                        tail,
                    ))
                }
            }

            Reading::Atomic(Token::Keyword(as_keyword))
                if as_keyword.keyword == KeywordKind::As =>
            {
                // eat the as keyword
                self.forward();

                let as_selector = match self.stop_at_significant() {
                    Reading::IntoDelimited(punc) if punc.punctuation == '(' => self.step_into(
                        Delimiter::Parenthesis,
                        |parser| parser.parse_string_literal(handler),
                        handler,
                    ),
                    unexpected => {
                        handler.receive(
                            UnexpectedSyntax {
                                expected: SyntaxKind::Punctuation('('),
                                found: unexpected.into_token(),
                            }
                            .into(),
                        );
                        None
                    }
                }?;

                let tail = self.parse_execute_block_tail(handler)?;

                Some(ExecuteBlock::HeadTail(
                    ExecuteBlockHead::As(As {
                        as_keyword,
                        open_paren: as_selector.open,
                        as_selector: as_selector.tree?,
                        close_paren: as_selector.close,
                    }),
                    tail,
                ))
            }

            // unexpected
            unexpected => {
                handler.receive(
                    UnexpectedSyntax {
                        expected: SyntaxKind::ExecuteBlock,
                        found: unexpected.into_token(),
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn parse_execute_block_tail(
        &mut self,
        handler: &impl Handler<Error>,
    ) -> Option<ExecuteBlockTail> {
        match self.stop_at_significant() {
            // nested execute block
            Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == ',' => {
                // eat the comma
                self.forward();

                let execute_block = self.parse_execute_block_statement(handler)?;

                Some(ExecuteBlockTail::ExecuteBlock(
                    punc,
                    Box::new(execute_block),
                ))
            }

            // end block
            Reading::IntoDelimited(punc) if punc.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Some(ExecuteBlockTail::Block(block))
            }

            unexpected => {
                handler.receive(
                    UnexpectedSyntax {
                        expected: SyntaxKind::ExecuteBlockTail,
                        found: unexpected.into_token(),
                    }
                    .into(),
                );
                None
            }
        }
    }
}
