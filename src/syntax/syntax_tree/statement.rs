//! Syntax tree nodes for statements.

use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{CommandLiteral, DocComment, Keyword, KeywordKind, Punctuation, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::expression::ParenthesizedCondition;

/// Syntax Synopsis:
///
/// ``` ebnf
/// Statement:
///     Block
///     | LiteralCommand
///     | Conditional
///     | DocComment
///     ;
/// ```
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Block(Block),
    LiteralCommand(CommandLiteral),
    Conditional(Conditional),
    DocComment(DocComment),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::LiteralCommand(literal_command) => literal_command.span(),
            Self::Conditional(conditional) => conditional.span(),
            Self::DocComment(doc_comment) => doc_comment.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Block:
///     '{' Statement* '}'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Block {
    /// The opening brace of the block.
    #[get = "pub"]
    pub open_brace: Punctuation,
    /// The statements within the block.
    #[get = "pub"]
    pub statements: Vec<Statement>,
    /// The closing brace of the block.
    #[get = "pub"]
    pub close_brace: Punctuation,
}

impl Block {
    /// Dissolves the [`Block`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<Statement>, Punctuation) {
        (self.open_brace, self.statements, self.close_brace)
    }
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        self.open_brace
            .span()
            .join(&self.close_brace.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Conditional:
/// 'if' ParenthizedCondition Block ('else' Block)?
/// ;
/// ````
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Conditional {
    /// The `if` keyword.
    #[get = "pub"]
    if_keyword: Keyword,
    /// The condition of the conditional.
    #[get = "pub"]
    condition: ParenthesizedCondition,
    /// The block of the conditional.
    #[get = "pub"]
    block: Block,
    /// The `else` statement.
    #[get = "pub"]
    r#else: Option<Else>,
}

impl Conditional {
    /// Dissolves the [`Conditional`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, ParenthesizedCondition, Block, Option<Else>) {
        (self.if_keyword, self.condition, self.block, self.r#else)
    }
}

impl SourceElement for Conditional {
    fn span(&self) -> Span {
        self.r#else.as_ref().map_or_else(
            || {
                self.if_keyword
                    .span()
                    .join(&self.block.span())
                    .expect("The span of the conditional is invalid.")
            },
            |r#else| {
                self.if_keyword
                    .span()
                    .join(&r#else.span())
                    .expect("The span of the else conditional is invalid.")
            },
        )
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Else:
///     'else' Block
///     ;
/// ```
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

impl<'a> Parser<'a> {
    /// Parses a [`Block`].
    pub fn parse_block(&mut self, handler: &impl Handler<Error>) -> Option<Block> {
        let token_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    parser.parse_statement(handler).map_or_else(
                        || {
                            // error recovery
                            parser.stop_at(|reading| matches!(
                                reading,
                                Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == ';'
                            ) || matches!(
                                reading,
                                Reading::IntoDelimited(punc) if punc.punctuation == '{'
                            ));

                            // goes after the semicolon or the open brace
                            parser.forward();
                        },
                        |statement| statements.push(statement),
                    );
                }

                Some(statements)
            },
            handler,
        )?;

        Some(Block {
            open_brace: token_tree.open,
            statements: token_tree.tree?,
            close_brace: token_tree.close,
        })
    }

    /// Parses a [`Statement`].
    pub fn parse_statement(&mut self, handler: &impl Handler<Error>) -> Option<Statement> {
        match self.stop_at_significant() {
            // variable declaration
            Reading::Atomic(Token::CommandLiteral(command)) => {
                self.forward();
                Some(Statement::LiteralCommand(command))
            }
            // block statement
            Reading::IntoDelimited(open_brace) if open_brace.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Some(Statement::Block(block))
            }

            // conditional statement
            Reading::Atomic(Token::Keyword(if_keyword))
                if if_keyword.keyword == KeywordKind::If =>
            {
                // eat the if keyword
                self.forward();

                let condition = self.parse_parenthesized_condition(handler)?;

                let block = self.parse_block(handler)?;

                match self.stop_at_significant() {
                    // else statement
                    Reading::Atomic(Token::Keyword(else_keyword))
                        if else_keyword.keyword == KeywordKind::Else =>
                    {
                        // eat the else keyword
                        self.forward();

                        let else_block = self.parse_block(handler)?;

                        Some(Statement::Conditional(Conditional {
                            if_keyword,
                            condition,
                            block,
                            r#else: Some(Else {
                                else_keyword,
                                block: Box::new(else_block),
                            }),
                        }))
                    }
                    // no else statement
                    _ => Some(Statement::Conditional(Conditional {
                        if_keyword,
                        condition,
                        block,
                        r#else: None,
                    })),
                }
            }

            Reading::Atomic(Token::DocComment(doc_comment)) => {
                self.forward();
                Some(Statement::DocComment(doc_comment))
            }

            // other
            unexpected => {
                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Statement,
                    found: unexpected.into_token(),
                }));

                None
            }
        }
    }
}
