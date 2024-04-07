//! Syntax tree nodes for expressions.

use enum_as_inner::EnumAsInner;
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
        error::{Error, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::ConnectedList;

/// Syntax Synopsis:
///
/// ```ebnf
/// Expression:
///     Primary
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Primary(Primary),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primary(primary) => primary.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// Primary:
///     FunctionCall
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Primary {
    FunctionCall(FunctionCall),
    StringLiteral(StringLiteral),
    Lua(Box<LuaCode>),
}

impl SourceElement for Primary {
    fn span(&self) -> Span {
        match self {
            Self::FunctionCall(function_call) => function_call.span(),
            Self::StringLiteral(string_literal) => string_literal.span(),
            Self::Lua(lua_code) => lua_code.span(),
        }
    }
}

/// Syntax Synopsis:
///
/// ``` ebnf
/// FunctionCall:
///     Identifier '(' (Expression (',' Expression)*)? ')'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct FunctionCall {
    /// The identifier of the function.
    #[get = "pub"]
    identifier: Identifier,
    /// The left parenthesis of the function call.
    #[get = "pub"]
    left_parenthesis: Punctuation,
    /// The arguments of the function call.
    #[get = "pub"]
    arguments: Option<ConnectedList<Box<Expression>, Punctuation>>,
    /// The right parenthesis of the function call.
    #[get = "pub"]
    right_parenthesis: Punctuation,
}

impl SourceElement for FunctionCall {
    fn span(&self) -> Span {
        self.identifier
            .span()
            .join(&self.right_parenthesis.span)
            .unwrap()
    }
}

/// Syntax Synopsis:
///
/// ```ebnf
/// LuaCode:
///     'lua' '(' (Expression (',' Expression)*)? ')' '{' (.*?)* '}'
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct LuaCode {
    /// The `lua` keyword.
    #[get = "pub"]
    lua_keyword: Keyword,
    /// The left parenthesis of the lua code.
    #[get = "pub"]
    left_parenthesis: Punctuation,
    /// The arguments of the lua code.
    #[get = "pub"]
    variables: Option<ConnectedList<Identifier, Punctuation>>,
    /// The right parenthesis of the lua code.
    #[get = "pub"]
    right_parenthesis: Punctuation,
    /// The left brace of the lua code.
    #[get = "pub"]
    left_brace: Punctuation,
    /// The lua code contents.
    #[get = "pub"]
    code: String,
    /// The right brace of the lua code.
    #[get = "pub"]
    right_brace: Punctuation,
}

impl SourceElement for LuaCode {
    fn span(&self) -> Span {
        self.lua_keyword
            .span()
            .join(&self.right_brace.span)
            .expect("Invalid lua code span")
    }
}

impl LuaCode {
    /// Dissolves the [`LuaCode`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, String, Punctuation) {
        (
            self.lua_keyword,
            self.left_brace,
            self.code,
            self.right_brace,
        )
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`Expression`]
    pub fn parse_expression(&mut self, handler: &impl Handler<Error>) -> Option<Expression> {
        Some(Expression::Primary(self.parse_primary(handler)?))
    }

    /// Parses an [`Primary`]
    pub fn parse_primary(&mut self, handler: &impl Handler<Error>) -> Option<Primary> {
        match self.stop_at_significant() {
            // identifier expression
            Reading::Atomic(Token::Identifier(identifier)) => {
                // eat the identifier
                self.forward();

                // function call
                if matches!(self.stop_at_significant(), Reading::IntoDelimited(punc) if punc.punctuation == '(')
                {
                    let token_tree = self.parse_enclosed_list(
                        Delimiter::Parenthesis,
                        ',',
                        |parser| parser.parse_expression(handler).map(Box::new),
                        handler,
                    )?;

                    Some(Primary::FunctionCall(FunctionCall {
                        identifier,
                        left_parenthesis: token_tree.open,
                        right_parenthesis: token_tree.close,
                        arguments: token_tree.list,
                    }))
                } else {
                    // insert parser for regular identifier here
                    None
                }
            }

            // string literal expression
            Reading::Atomic(Token::StringLiteral(literal)) => {
                // eat the string literal
                self.forward();

                Some(Primary::StringLiteral(literal))
            }

            // lua code expression
            Reading::Atomic(Token::Keyword(lua_keyword))
                if lua_keyword.keyword == KeywordKind::Lua =>
            {
                // eat the lua keyword
                self.forward();

                // parse the variable list
                let variables = self.parse_enclosed_list(
                    Delimiter::Parenthesis,
                    ',',
                    |parser| match parser.next_significant_token() {
                        Reading::Atomic(Token::Identifier(identifier)) => {
                            parser.forward();
                            Some(identifier)
                        }
                        _ => None,
                    },
                    handler,
                )?;

                self.stop_at_significant();

                let tree = self.step_into(
                    Delimiter::Brace,
                    |parser| {
                        let first = parser.next_token();
                        let mut last = parser.next_token();

                        while !parser.is_end() {
                            last = parser.next_token();
                        }

                        let combined = first
                            .into_token()
                            .and_then(|first| {
                                first.span().join(&last.into_token().map_or_else(
                                    || first.span().to_owned(),
                                    |last| last.span().to_owned(),
                                ))
                            })
                            .expect("Invalid lua code span");

                        Some(combined.str().trim().to_owned())
                    },
                    handler,
                )?;

                Some(Primary::Lua(Box::new(LuaCode {
                    lua_keyword,
                    left_parenthesis: variables.open,
                    variables: variables.list,
                    right_parenthesis: variables.close,
                    left_brace: tree.open,
                    code: tree.tree?,
                    right_brace: tree.close,
                })))
            }

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
}
