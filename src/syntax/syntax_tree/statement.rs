//! Syntax tree nodes for statements.

pub mod execute_block;

use derive_more::From;
use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{CommandLiteral, DocComment, Keyword, KeywordKind, Punctuation, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::ParseResult,
        parser::{Parser, Reading},
    },
};

use self::execute_block::ExecuteBlock;

use super::expression::Expression;

/// Represents a statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Statement:
///     Block
///     | LiteralCommand
///     | Conditional
///     | Grouping
///     | DocComment
///     | Semicolon
///     | Run
///     ;
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
pub enum Statement {
    Block(Block),
    LiteralCommand(CommandLiteral),
    ExecuteBlock(ExecuteBlock),
    Grouping(Grouping),
    DocComment(DocComment),
    Semicolon(Semicolon),
    Run(Run),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::LiteralCommand(literal_command) => literal_command.span(),
            Self::ExecuteBlock(execute_block) => execute_block.span(),
            Self::Grouping(grouping) => grouping.span(),
            Self::DocComment(doc_comment) => doc_comment.span(),
            Self::Semicolon(semi) => semi.span(),
            Self::Run(run) => run.span(),
        }
    }
}

/// Represents a block in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Block:
///     '{' Statement* '}'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// Represents a run statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Run:
/// 'run' Expression ';'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Run {
    /// The `run` keyword.
    #[get = "pub"]
    run_keyword: Keyword,
    /// The expression of the run statement.
    #[get = "pub"]
    expression: Expression,
    /// The semicolon of the run statement.
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Run {
    fn span(&self) -> Span {
        self.run_keyword
            .span()
            .join(&self.semicolon.span())
            .expect("The span of the run statement is invalid.")
    }
}

impl Run {
    /// Dissolves the [`Run`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Expression, Punctuation) {
        (self.run_keyword, self.expression, self.semicolon)
    }
}

/// Represents a grouping statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Grouping:
/// 'group' Block
/// ;
/// ````
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Grouping {
    /// The `group` keyword.
    #[get = "pub"]
    group_keyword: Keyword,
    /// The block of the conditional.
    #[get = "pub"]
    block: Block,
}

impl Grouping {
    /// Dissolves the [`Grouping`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Block) {
        (self.group_keyword, self.block)
    }
}

impl SourceElement for Grouping {
    fn span(&self) -> Span {
        self.group_keyword
            .span()
            .join(&self.block.span())
            .expect("The span of the grouping is invalid.")
    }
}

/// Represents a statement that ends with a semicolon in the syntax tree.
///
/// Syntax Synopsis:
/// ``` ebnf
/// Semicolon:
///    Expression ';'
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Semicolon {
    /// The expression of the semicolon statement.
    #[get = "pub"]
    expression: Expression,
    /// The semicolon of the semicolon statement.
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Semicolon {
    fn span(&self) -> Span {
        self.expression
            .span()
            .join(&self.semicolon.span())
            .expect("The span of the semicolon statement is invalid.")
    }
}

impl Semicolon {
    /// Dissolves the [`Semicolon`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Expression, Punctuation) {
        (self.expression, self.semicolon)
    }
}

impl<'a> Parser<'a> {
    /// Parses a [`Block`].
    ///
    /// # Errors
    /// - if the parser is not at a block.
    pub fn parse_block(&mut self, handler: &impl Handler<base::Error>) -> ParseResult<Block> {
        let token_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    parser.parse_statement(handler).map_or_else(
                        |_| {
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

                Ok(statements)
            },
            handler,
        )?;

        Ok(Block {
            open_brace: token_tree.open,
            statements: token_tree.tree?,
            close_brace: token_tree.close,
        })
    }

    /// Parses a [`Statement`].
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn parse_statement(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Statement> {
        match self.stop_at_significant() {
            // variable declaration
            Reading::Atomic(Token::CommandLiteral(command)) => {
                self.forward();
                tracing::trace!("Parsed literal command '{}'", command.clean_command());
                Ok(Statement::LiteralCommand(command))
            }
            // block statement
            Reading::IntoDelimited(open_brace) if open_brace.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Ok(Statement::Block(block))
            }

            // execute block
            Reading::Atomic(Token::Keyword(execute_keyword))
                if execute_keyword.keyword.starts_execute_block() =>
            {
                self.parse_execute_block_statement(handler)
                    .map(Statement::ExecuteBlock)
            }

            // doc comment
            Reading::Atomic(Token::DocComment(doc_comment)) => {
                self.forward();
                Ok(Statement::DocComment(doc_comment))
            }

            // grouping statement
            Reading::Atomic(Token::Keyword(group_keyword))
                if group_keyword.keyword == KeywordKind::Group =>
            {
                // eat the group keyword
                self.forward();

                let block = self.parse_block(handler)?;

                tracing::trace!("Parsed group command");

                Ok(Statement::Grouping(Grouping {
                    group_keyword,
                    block,
                }))
            }

            // run statement
            Reading::Atomic(Token::Keyword(run_keyword))
                if run_keyword.keyword == KeywordKind::Run =>
            {
                // eat the run keyword
                self.forward();

                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                tracing::trace!("Parsed run statement: {:?}", expression);

                Ok(Statement::Run(Run {
                    run_keyword,
                    expression,
                    semicolon,
                }))
            }

            // semicolon statement
            _ => {
                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                tracing::trace!("Parsed semicolon statement: {:?}", expression);

                Ok(Statement::Semicolon(Semicolon {
                    expression,
                    semicolon,
                }))
            }
        }
    }
}
