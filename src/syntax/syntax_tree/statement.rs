//! Syntax tree nodes for statements.

use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{LiteralCommand, Punctuation, Token},
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
/// Statement:
///     Block
///     | Conditional
///     ;
/// ```
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Block(Block),
    LiteralCommand(LiteralCommand),
    // Conditional(Conditional),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::LiteralCommand(literal_command) => literal_command.span(),
            //Self::Conditional(conditional) => conditional.span(),
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
            Reading::Atomic(Token::LiteralCommand(command)) => {
                self.forward();
                Some(Statement::LiteralCommand(command))
            }
            // block statement
            Reading::IntoDelimited(open_brace) if open_brace.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Some(Statement::Block(block))
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
