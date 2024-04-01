//! Syntax tree nodes for declarations.

#![allow(missing_docs)]

use getset::Getters;

use crate::{
    base::{
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::{
        token::{Identifier, Keyword, KeywordKind, Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::{statement::Block, ConnectedList};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration {
    Function(Function),
}

impl SourceElement for Declaration {
    fn span(&self) -> Span {
        match self {
            Self::Function(function) => function.span(),
        }
    }
}
/// Syntax Synopsis:
///
/// ``` ebnf
/// Annotation:
///     '#[' Identifier ('=' StringLiteral)? ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Annotation {
    #[get = "pub"]
    pound_sign: Punctuation,
    #[get = "pub"]
    open_bracket: Punctuation,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    value: Option<(Punctuation, StringLiteral)>,
    #[get = "pub"]
    close_bracket: Punctuation,
}

impl Annotation {
    /// Dissolves the [`Annotation`] into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Punctuation,
        Punctuation,
        Identifier,
        Option<(Punctuation, StringLiteral)>,
        Punctuation,
    ) {
        (
            self.pound_sign,
            self.open_bracket,
            self.identifier,
            self.value,
            self.close_bracket,
        )
    }
}
impl SourceElement for Annotation {
    fn span(&self) -> Span {
        self.pound_sign
            .span
            .join(&self.close_bracket.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Function:
///     Annotation* 'fn' Identifier '(' ParameterList? ')' Block
///     ;
///
/// ParameterList:
///     Identifier (',' Identifier)* ','?  
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    annotations: Vec<Annotation>,
    #[get = "pub"]
    function_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    open_paren: Punctuation,
    #[get = "pub"]
    parameters: Option<ConnectedList<Identifier, Punctuation>>,
    #[get = "pub"]
    close_paren: Punctuation,
    #[get = "pub"]
    block: Block,
}

impl Function {
    /// Dissolves the [`Function`] into its components.
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (
        Vec<Annotation>,
        Keyword,
        Identifier,
        Punctuation,
        Option<ConnectedList<Identifier, Punctuation>>,
        Punctuation,
        Block,
    ) {
        (
            self.annotations,
            self.function_keyword,
            self.identifier,
            self.open_paren,
            self.parameters,
            self.close_paren,
            self.block,
        )
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.function_keyword.span.join(&self.block.span()).unwrap()
    }
}

impl<'a> Parser<'a> {
    pub fn parse_annotation(&mut self, handler: &impl Handler<Error>) -> Option<Annotation> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(punctuation)) if punctuation.punctuation == '#' => {
                // eat the pound sign
                self.forward();

                // step into the brackets
                let content = self.step_into(
                    Delimiter::Bracket,
                    |parser| {
                        let identifier = parser.parse_identifier(handler)?;

                        let value = if let Reading::Atomic(Token::Punctuation(punctuation)) =
                            parser.stop_at_significant()
                        {
                            if punctuation.punctuation == '=' {
                                // eat the equals sign
                                parser.forward();

                                // parse the string literal
                                let string_literal = parser
                                    .next_significant_token()
                                    .into_token()?
                                    .into_string_literal()
                                    .ok()?;

                                Some((punctuation, string_literal))
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        Some((identifier, value))
                    },
                    handler,
                )?;

                let (identifier, value) = content.tree?;

                Some(Annotation {
                    pound_sign: punctuation,
                    open_bracket: content.open,
                    identifier,
                    value,
                    close_bracket: content.close,
                })
            }
            _ => None,
        }
    }

    pub fn parse_declaration(&mut self, handler: &impl Handler<Error>) -> Option<Declaration> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                // eat the function keyword
                self.forward();

                // parse the identifier
                let identifier = self.parse_identifier(handler)?;
                let delimited_tree = self.parse_enclosed_list(
                    Delimiter::Parenthesis,
                    ',',
                    |parser: &mut Parser<'_>| parser.parse_identifier(handler),
                    handler,
                )?;

                // parse the block
                let block = self.parse_block(handler)?;

                Some(Declaration::Function(Function {
                    annotations: Vec::new(),
                    function_keyword,
                    identifier,
                    open_paren: delimited_tree.open,
                    parameters: delimited_tree.list,
                    close_paren: delimited_tree.close,
                    block,
                }))
            }

            // parse annotations
            Reading::Atomic(Token::Punctuation(punctuation)) if punctuation.punctuation == '#' => {
                // parse the annotation
                let mut annotations = Vec::new();

                while let Some(annotation) =
                    self.try_parse(|parser| parser.parse_annotation(handler))
                {
                    annotations.push(annotation);
                }

                // parse the function
                self.parse_declaration(handler)
                    .map(|declaration| match declaration {
                        Declaration::Function(mut function) => {
                            function.annotations.extend(annotations);
                            Declaration::Function(function)
                        }
                    })
            }

            unexpected => {
                // make progress
                self.forward();

                handler.receive(Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Declaration,
                    found: unexpected.into_token(),
                }));

                None
            }
        }
    }
}
