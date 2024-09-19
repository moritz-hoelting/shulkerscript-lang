//! Syntax tree nodes for declarations.

#![allow(missing_docs)]

use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler, VoidHandler,
    },
    lexical::{
        token::{Identifier, Keyword, KeywordKind, Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, ParseResult, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::{statement::Block, ConnectedList};

/// Syntax Synopsis:
///
/// ``` ebnf
/// Declaration:
///    Function
///    | Import
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration {
    Function(Function),
    Import(Import),
}

impl SourceElement for Declaration {
    fn span(&self) -> Span {
        match self {
            Self::Function(function) => function.span(),
            Self::Import(import) => import.span(),
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
///     Annotation* 'pub'? 'fn' Identifier '(' ParameterList? ')' Block
///     ;
///
/// ParameterList:
///     Identifier (',' Identifier)* ','?  
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    public_keyword: Option<Keyword>,
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
        Option<Keyword>,
        Vec<Annotation>,
        Keyword,
        Identifier,
        Punctuation,
        Option<ConnectedList<Identifier, Punctuation>>,
        Punctuation,
        Block,
    ) {
        (
            self.public_keyword,
            self.annotations,
            self.function_keyword,
            self.identifier,
            self.open_paren,
            self.parameters,
            self.close_paren,
            self.block,
        )
    }

    /// Returns `true` if the function is public.
    #[must_use]
    pub fn is_public(&self) -> bool {
        self.public_keyword.is_some()
    }
}

impl SourceElement for Function {
    fn span(&self) -> Span {
        self.public_keyword
            .as_ref()
            .map_or_else(|| self.function_keyword.span(), SourceElement::span)
            .join(&self.block.span())
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Import:
///     'from' StringLiteral 'import' ('*' | Identifier (',' Identifier)*) ';'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Import {
    #[get = "pub"]
    from_keyword: Keyword,
    #[get = "pub"]
    module: StringLiteral,
    #[get = "pub"]
    import_keyword: Keyword,
    #[get = "pub"]
    items: ImportItems,
    #[get = "pub"]
    semicolon: Punctuation,
}

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImportItems {
    All(Punctuation),
    Named(ConnectedList<Identifier, Punctuation>),
}

impl Import {
    /// Dissolves the [`Import`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, StringLiteral, Keyword, ImportItems, Punctuation) {
        (
            self.from_keyword,
            self.module,
            self.import_keyword,
            self.items,
            self.semicolon,
        )
    }
}

impl SourceElement for Import {
    fn span(&self) -> Span {
        self.from_keyword
            .span()
            .join(&self.semicolon.span())
            .unwrap()
    }
}

impl<'a> Parser<'a> {
    /// Parses an annotation.
    ///
    /// # Errors
    /// - if the parser position is not at an annotation.
    /// - if the parsing of the annotation fails
    pub fn parse_annotation(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Annotation> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(punctuation)) if punctuation.punctuation == '#' => {
                // eat the pound sign
                self.forward();

                // step into the brackets
                let content = self.step_into(
                    Delimiter::Bracket,
                    |parser| {
                        let identifier = parser.parse_identifier(handler)?;

                        let value = match parser.stop_at_significant() {
                            Reading::Atomic(Token::Punctuation(punc))
                                if punc.punctuation == '=' =>
                            {
                                // eat the equals sign
                                parser.forward();

                                // parse the string literal
                                let string_literal = parser.parse_string_literal(handler)?;

                                Some((punc, string_literal))
                            }
                            _ => None,
                        };

                        Ok((identifier, value))
                    },
                    handler,
                )?;

                let (identifier, value) = content.tree?;

                Ok(Annotation {
                    pound_sign: punctuation,
                    open_bracket: content.open,
                    identifier,
                    value,
                    close_bracket: content.close,
                })
            }
            unexpected => {
                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Punctuation('#'),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }
    }

    #[tracing::instrument(level = "trace", skip_all)]
    pub fn parse_declaration(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Declaration> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(function_keyword))
                if function_keyword.keyword == KeywordKind::Function =>
            {
                let function = self.parse_function(handler)?;

                tracing::trace!("Parsed function '{:?}'", function.identifier.span.str());

                Ok(Declaration::Function(function))
            }

            Reading::Atomic(Token::Keyword(pub_keyword))
                if pub_keyword.keyword == KeywordKind::Pub =>
            {
                let function = self.parse_function(handler)?;

                tracing::trace!("Parsed function '{:?}'", function.identifier.span.str());

                Ok(Declaration::Function(function))
            }

            // parse annotations
            Reading::Atomic(Token::Punctuation(punctuation)) if punctuation.punctuation == '#' => {
                // parse the annotation
                let mut annotations = Vec::new();

                while let Ok(annotation) =
                    self.try_parse(|parser| parser.parse_annotation(&VoidHandler))
                {
                    annotations.push(annotation);
                }

                self.parse_function(handler).map(|mut function| {
                    function.annotations.extend(annotations);
                    Declaration::Function(function)
                })
            }

            Reading::Atomic(Token::Keyword(from_keyword))
                if from_keyword.keyword == KeywordKind::From =>
            {
                // eat the from keyword
                self.forward();

                // parse the module
                let module = self.parse_string_literal(handler)?;

                let import_keyword = self.parse_keyword(KeywordKind::Import, handler)?;

                // TODO: re-enable when the asterisk is supported
                let items = // match self.stop_at_significant() {
                    // Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '*' => {
                        // eat the asterisk
                        // self.forward();

                        // ImportItems::All(punc)
                    // }
                    // _ => 
                    self.try_parse(|parser| parser
                        .parse_connected_list(
                            ',',
                            |parser| parser.parse_identifier(&VoidHandler),
                            handler,
                        )
                        .map(ImportItems::Named)) // ,
                // }
                ;

                if let Ok(items) = items {
                    let semicolon = self.parse_punctuation(';', true, handler)?;

                    tracing::trace!("Parsed import from '{:?}'", module.str_content());

                    Ok(Declaration::Import(Import {
                        from_keyword,
                        module,
                        import_keyword,
                        items,
                        semicolon,
                    }))
                } else {
                    let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                        expected: SyntaxKind::Punctuation('*'),
                        found: self.stop_at_significant().into_token(),
                    });
                    handler.receive(err.clone());

                    Err(err)
                }
            }

            unexpected => {
                // make progress
                self.forward();

                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Declaration,
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());

                Err(err)
            }
        }
    }

    /// Parses a function.
    ///
    /// # Errors
    /// - if the parser is not at a function (not at annotation).
    /// - if the parsing of the function fails.
    pub fn parse_function(&mut self, handler: &impl Handler<base::Error>) -> ParseResult<Function> {
        let pub_keyword =
            self.try_parse(|parser| parser.parse_keyword(KeywordKind::Pub, &VoidHandler));

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

                Ok(Function {
                    public_keyword: pub_keyword.ok(),
                    annotations: Vec::new(),
                    function_keyword,
                    identifier,
                    open_paren: delimited_tree.open,
                    parameters: delimited_tree.list,
                    close_paren: delimited_tree.close,
                    block,
                })
            }
            unexpected => {
                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Keyword(KeywordKind::Function),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }
    }
}
