//! Syntax tree nodes for declarations.

#![allow(missing_docs)]

use std::collections::VecDeque;

use enum_as_inner::EnumAsInner;
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
        error::{Error, InvalidAnnotation, ParseResult, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::{
    statement::{Block, VariableDeclaration},
    Annotation, ConnectedList, DelimitedList,
};

/// Represents a declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Declaration:
///     Function
///     | Import
///     | Tag
///     | ('pub'? VariableDeclaration ';')
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration {
    Function(Function),
    Import(Import),
    Tag(Tag),
    GlobalVariable((Option<Keyword>, VariableDeclaration, Punctuation)),
}

impl SourceElement for Declaration {
    fn span(&self) -> Span {
        match self {
            Self::Function(function) => function.span(),
            Self::Import(import) => import.span(),
            Self::Tag(tag) => tag.span(),
            Self::GlobalVariable((pub_kw, variable, semicolon)) => pub_kw
                .as_ref()
                .map_or_else(|| variable.span(), SourceElement::span)
                .join(&semicolon.span)
                .expect("invalid declaration span"),
        }
    }
}

impl Declaration {
    /// Adds an annotation to the declaration.
    ///
    /// # Errors
    /// - if the annotation is invalid for the target declaration.
    pub fn with_annotation(self, annotation: Annotation) -> ParseResult<Self> {
        match self {
            Self::Function(mut function) => {
                function.annotations.push_front(annotation);

                Ok(Self::Function(function))
            }
            Self::GlobalVariable((pub_kw, var, semi)) => {
                let var_with_annotation = var.with_annotation(annotation)?;

                Ok(Self::GlobalVariable((pub_kw, var_with_annotation, semi)))
            }
            _ => {
                let err = Error::InvalidAnnotation(InvalidAnnotation {
                    annotation: annotation.assignment.identifier.span,
                    target: "declarations except functions".to_string(),
                });

                Err(err)
            }
        }
    }
}

/// Represents a function declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Function:
///     Annotation* 'pub'? 'fn' Identifier '(' ParameterList? ')' Block
///     ;
///
/// ParameterList:
///     FunctionArgument (',' FunctionArgument)* ','?  
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Function {
    #[get = "pub"]
    public_keyword: Option<Keyword>,
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
    #[get = "pub"]
    function_keyword: Keyword,
    #[get = "pub"]
    identifier: Identifier,
    #[get = "pub"]
    open_paren: Punctuation,
    #[get = "pub"]
    parameters: Option<ConnectedList<FunctionParameter, Punctuation>>,
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
        VecDeque<Annotation>,
        Keyword,
        Identifier,
        Punctuation,
        Option<ConnectedList<FunctionParameter, Punctuation>>,
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

// Represents a variable type keyword for function arguments.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// FunctionVariableType:
///     'macro' | 'int' | 'bool'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum FunctionVariableType {
    Macro(Keyword),
    Integer(Keyword),
    Boolean(Keyword),
}

/// Represents a function argument in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// FunctionArgument:
///     FunctionVariableType Identifier
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionParameter {
    #[get = "pub"]
    variable_type: FunctionVariableType,
    #[get = "pub"]
    identifier: Identifier,
}

/// Represents an import declaration in the syntax tree.
///
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

/// Items to import.
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

/// Represents a tag declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// TagDeclaration:
///     'tag' ('<' StringLiteral '>')? StringLiteral 'replace'? '[' (StringLiteral (',' StringLiteral)*)? ']'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Tag {
    #[get = "pub"]
    tag_keyword: Keyword,
    #[get = "pub"]
    of_type: Option<(Punctuation, StringLiteral, Punctuation)>,
    #[get = "pub"]
    name: StringLiteral,
    #[get = "pub"]
    replace: Option<Keyword>,
    #[get = "pub"]
    entries: DelimitedList<StringLiteral>,
}

impl Tag {
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Option<(Punctuation, StringLiteral, Punctuation)>,
        StringLiteral,
        Option<Keyword>,
        DelimitedList<StringLiteral>,
    ) {
        (
            self.tag_keyword,
            self.of_type,
            self.name,
            self.replace,
            self.entries,
        )
    }

    #[cfg(feature = "shulkerbox")]
    #[must_use]
    pub fn tag_type(&self) -> shulkerbox::datapack::tag::TagType {
        use shulkerbox::datapack::tag::TagType;

        self.of_type
            .as_ref()
            .map_or(TagType::Function, |(_, tag_type, _)| {
                match tag_type.str_content().as_ref() {
                    "function" => TagType::Function,
                    "block" => TagType::Block,
                    "entity_type" => TagType::Entity,
                    "fluid" => TagType::Fluid,
                    "game_event" => TagType::GameEvent,
                    "item" => TagType::Item,
                    other => TagType::Other(other.to_string()),
                }
            })
    }
}

impl SourceElement for Tag {
    fn span(&self) -> Span {
        self.tag_keyword
            .span()
            .join(&self.entries.close.span)
            .unwrap()
    }
}

impl Parser<'_> {
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
                if let Ok(function) = self.try_parse(|parser| parser.parse_function(&VoidHandler)) {
                    tracing::trace!("Parsed function '{:?}'", function.identifier.span.str());

                    Ok(Declaration::Function(function))
                } else {
                    // eat the pub keyword
                    self.forward();

                    let var = self.parse_variable_declaration(handler)?;
                    let semi = self.parse_punctuation(';', true, handler)?;

                    Ok(Declaration::GlobalVariable((Some(pub_keyword), var, semi)))
                }
            }

            // parse annotations
            Reading::Atomic(Token::Punctuation(punctuation)) if punctuation.punctuation == '#' => {
                // parse the annotation
                let annotation = self.parse_annotation(handler)?;
                let declaration = self.parse_declaration(handler)?;

                declaration
                    .with_annotation(annotation)
                    .inspect_err(|err| handler.receive(err.clone()))
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

            Reading::Atomic(Token::Keyword(tag_keyword))
                if tag_keyword.keyword == KeywordKind::Tag =>
            {
                // eat the tag keyword
                self.forward();

                let of_type = match self.stop_at_significant() {
                    Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '<' => {
                        // eat the open bracket
                        self.forward();

                        let of_type = self.parse_string_literal(handler)?;

                        // eat the close bracket
                        let closing = self.parse_punctuation('>', true, handler)?;

                        Some((punc, of_type, closing))
                    }
                    _ => None,
                };

                // parse the name
                let name = self.parse_string_literal(handler)?;

                let replace = self
                    .try_parse(|parser| parser.parse_keyword(KeywordKind::Replace, &VoidHandler))
                    .ok();

                let entries = self.parse_enclosed_list(
                    Delimiter::Bracket,
                    ',',
                    |parser| parser.parse_string_literal(handler),
                    handler,
                )?;

                Ok(Declaration::Tag(Tag {
                    tag_keyword,
                    of_type,
                    name,
                    replace,
                    entries,
                }))
            }

            Reading::Atomic(Token::Keyword(keyword))
                if matches!(
                    keyword.keyword,
                    KeywordKind::Int | KeywordKind::Bool | KeywordKind::Val
                ) =>
            {
                let var = self.parse_variable_declaration(handler)?;
                let semi = self.parse_punctuation(';', true, handler)?;

                Ok(Declaration::GlobalVariable((None, var, semi)))
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
                    |parser: &mut Parser<'_>| parser.parse_function_parameter(handler),
                    handler,
                )?;

                // parse the block
                let block = self.parse_block(handler)?;

                Ok(Function {
                    public_keyword: pub_keyword.ok(),
                    annotations: VecDeque::new(),
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

    fn parse_function_parameter(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<FunctionParameter> {
        match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(keyword)) if keyword.keyword == KeywordKind::Int => {
                let variable_type = FunctionVariableType::Integer(keyword);
                self.forward();

                let identifier = self.parse_identifier(handler)?;

                Ok(FunctionParameter {
                    variable_type,
                    identifier,
                })
            }
            Reading::Atomic(Token::Keyword(keyword)) if keyword.keyword == KeywordKind::Bool => {
                let variable_type = FunctionVariableType::Boolean(keyword);
                self.forward();

                let identifier = self.parse_identifier(handler)?;

                Ok(FunctionParameter {
                    variable_type,
                    identifier,
                })
            }
            Reading::Atomic(Token::Keyword(keyword)) if keyword.keyword == KeywordKind::Macro => {
                let variable_type = FunctionVariableType::Macro(keyword);
                self.forward();

                let identifier = self.parse_identifier(handler)?;

                Ok(FunctionParameter {
                    variable_type,
                    identifier,
                })
            }
            unexpected => {
                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Either(&[
                        SyntaxKind::Keyword(KeywordKind::Int),
                        SyntaxKind::Keyword(KeywordKind::Bool),
                        SyntaxKind::Keyword(KeywordKind::Macro),
                    ]),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }
    }
}
