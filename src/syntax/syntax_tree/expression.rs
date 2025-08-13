//! Syntax tree nodes for expressions.

use std::cmp::Ordering;

use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler, VoidHandler,
    },
    lexical::{
        token::{
            Boolean, Identifier, Integer, Keyword, KeywordKind, MacroStringLiteral, Punctuation,
            StringLiteral, Token,
        },
        token_stream::Delimiter,
    },
    syntax::{
        self,
        error::{Error, ParseResult, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use super::ConnectedList;

/// Represents a binary operator in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// BinaryOperator:
///     '+'
///     | '-'
///     | '*'
///     | '/'
///     | '%'
///     | '=='
///     | '!='
///     | '<'
///     | '<='
///     | '>'
///     | '>='
///     | '&&'
///     | '||'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[allow(missing_docs)]
pub enum BinaryOperator {
    Add(Punctuation),
    Subtract(Punctuation),
    Multiply(Punctuation),
    Divide(Punctuation),
    Modulo(Punctuation),
    Equal(Punctuation, Punctuation),
    NotEqual(Punctuation, Punctuation),
    LessThan(Punctuation),
    LessThanOrEqual(Punctuation, Punctuation),
    GreaterThan(Punctuation),
    GreaterThanOrEqual(Punctuation, Punctuation),
    LogicalAnd(Punctuation, Punctuation),
    LogicalOr(Punctuation, Punctuation),
}

impl BinaryOperator {
    /// Gets the precedence of the operator (the higher the number, the first it will be evaluated)
    ///
    /// The least operator has precedence 1.
    #[must_use]
    pub fn get_precedence(&self) -> u32 {
        match self {
            Self::LogicalOr(..) => 1,
            Self::LogicalAnd(..) => 2,
            Self::Equal(..) | Self::NotEqual(..) => 3,
            Self::LessThan(..)
            | Self::LessThanOrEqual(..)
            | Self::GreaterThan(..)
            | Self::GreaterThanOrEqual(..) => 4,
            Self::Add(..) | Self::Subtract(..) => 5,
            Self::Multiply(..) | Self::Divide(..) | Self::Modulo(..) => 6,
        }
    }
}

impl SourceElement for BinaryOperator {
    fn span(&self) -> Span {
        match self {
            Self::Add(token)
            | Self::Subtract(token)
            | Self::Multiply(token)
            | Self::Divide(token)
            | Self::Modulo(token)
            | Self::LessThan(token)
            | Self::GreaterThan(token) => token.span.clone(),
            Self::Equal(token, token1)
            | Self::NotEqual(token, token1)
            | Self::LessThanOrEqual(token, token1)
            | Self::GreaterThanOrEqual(token, token1)
            | Self::LogicalAnd(token, token1)
            | Self::LogicalOr(token, token1) => token.span().join(&token1.span).unwrap(),
        }
    }
}

/// Represents a binary expression in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// Binary:
///     Expression BinaryOperator Expression
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Binary {
    /// The left operand of the binary expression.
    #[get = "pub"]
    left_operand: Box<Expression>,
    /// The operator of the binary expression.
    #[get = "pub"]
    operator: BinaryOperator,
    /// The right operand of the binary expression.
    #[get = "pub"]
    right_operand: Box<Expression>,
}

impl SourceElement for Binary {
    fn span(&self) -> Span {
        self.left_operand
            .span()
            .join(&self.right_operand.span())
            .unwrap()
    }
}

/// Represents an expression in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// Expression:
///     Primary | Binary ;
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Expression {
    Primary(Primary),
    Binary(Binary),
}

impl SourceElement for Expression {
    fn span(&self) -> Span {
        match self {
            Self::Primary(primary) => primary.span(),
            Self::Binary(binary) => binary.span(),
        }
    }
}

/// Represents a primary expression in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// Primary:
///     Identifier
///     | Prefix
///     | Parenthesized
///     | Indexed
///     | Integer
///     | Boolean
///     | StringLiteral
///     | FunctionCall
///     | MemberAccess
///     | MacroStringLiteral
///     | LuaCode
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Primary {
    Identifier(Identifier),
    Prefix(Prefix),
    Parenthesized(Parenthesized),
    Indexed(Indexed),
    Integer(Integer),
    Boolean(Boolean),
    StringLiteral(StringLiteral),
    FunctionCall(FunctionCall),
    MemberAccess(MemberAccess),
    MacroStringLiteral(MacroStringLiteral),
    Lua(Box<LuaCode>),
}

impl SourceElement for Primary {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(identifier) => identifier.span(),
            Self::Prefix(prefix) => prefix.span(),
            Self::Parenthesized(parenthesized) => parenthesized.span(),
            Self::Indexed(indexed) => indexed.span(),
            Self::Integer(int) => int.span(),
            Self::Boolean(bool) => bool.span(),
            Self::StringLiteral(string_literal) => string_literal.span(),
            Self::FunctionCall(function_call) => function_call.span(),
            Self::MemberAccess(member_access) => member_access.span(),
            Self::MacroStringLiteral(macro_string_literal) => macro_string_literal.span(),
            Self::Lua(lua_code) => lua_code.span(),
        }
    }
}

/// Represents a parenthesized expression in the syntax tree.
///
/// Syntax Synopsis:    
/// ```ebnf
/// Parenthesized:
///     '(' Expression ')'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Parenthesized {
    /// The open parenthesis.
    #[get = "pub"]
    open: Punctuation,
    /// The expression inside the parenthesis.
    #[get = "pub"]
    expression: Box<Expression>,
    /// The close parenthesis.
    #[get = "pub"]
    close: Punctuation,
}

impl Parenthesized {
    /// Dissolves the parenthesized expression into its components
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Expression, Punctuation) {
        (self.open, *self.expression, self.close)
    }
}

impl SourceElement for Parenthesized {
    fn span(&self) -> Span {
        self.open.span().join(&self.close.span).unwrap()
    }
}

impl std::ops::Deref for Parenthesized {
    type Target = Expression;

    fn deref(&self) -> &Self::Target {
        &self.expression
    }
}

/// Represents a indexed expression in the syntax tree.
///
/// Syntax Synopsis:    
/// ```ebnf
/// Indexed:
///     PrimaryExpression '[' Expression ']'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Indexed {
    /// The object that is indexed.
    #[get = "pub"]
    object: Box<Primary>,
    /// The left bracket.
    #[get = "pub"]
    left_bracket: Punctuation,
    /// The index expression.
    #[get = "pub"]
    index: Box<Expression>,
    /// The right bracket.
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl Indexed {
    /// Dissolves the indexed expression into its components
    #[must_use]
    pub fn dissolve(self) -> (Primary, Punctuation, Expression, Punctuation) {
        (
            *self.object,
            self.left_bracket,
            *self.index,
            self.right_bracket,
        )
    }
}

impl SourceElement for Indexed {
    fn span(&self) -> Span {
        self.object.span().join(&self.right_bracket.span).unwrap()
    }
}

/// Represents a prefix operator in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// PrefixOperator:
///     '!' | '-' | 'run'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum PrefixOperator {
    /// The logical not operator '!'.
    LogicalNot(Punctuation),
    /// The negate operator '-'.
    Negate(Punctuation),
    /// The run keyword 'run'.
    Run(Keyword),
}

impl SourceElement for PrefixOperator {
    fn span(&self) -> Span {
        match self {
            Self::LogicalNot(token) | Self::Negate(token) => token.span.clone(),
            Self::Run(token) => token.span.clone(),
        }
    }
}

/// Represents a prefix expression in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// Prefix:
///     PrefixOperator Primary
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Prefix {
    /// The prefix operator.
    #[get = "pub"]
    operator: PrefixOperator,
    /// The operand.
    #[get = "pub"]
    operand: Box<Primary>,
}

impl SourceElement for Prefix {
    fn span(&self) -> Span {
        self.operator.span().join(&self.operand.span()).unwrap()
    }
}

/// Represents a function call in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
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

/// Represents a lua code block in the syntax tree.
///
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
    inputs: Option<ConnectedList<Identifier, Punctuation>>,
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

/// Represents a member access in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// MemberAccess:
///     Primary '.' Identifier
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct MemberAccess {
    /// The parent expression
    #[get = "pub"]
    parent: Box<Primary>,
    /// The dot in the middle
    #[get = "pub"]
    dot: Punctuation,
    /// The member being accessed
    #[get = "pub"]
    member: Identifier,
}

impl SourceElement for MemberAccess {
    fn span(&self) -> Span {
        self.parent
            .span()
            .join(&self.member.span)
            .expect("invalid span")
    }
}

impl MemberAccess {
    /// Dissolves the [`MemberAccess`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Box<Primary>, Punctuation, Identifier) {
        (self.parent, self.dot, self.member)
    }
}

impl Parser<'_> {
    /// Parses an [`Expression`]
    ///
    /// # Errors
    /// - If the parser is not at a primary expression.
    pub fn parse_expression(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Expression> {
        let mut first_primary = Expression::Primary(self.parse_primary(handler)?);
        let mut expressions = Vec::new();

        // loop until there are no more binary operators
        while let Ok(binary_operator) = self.try_parse(|p| p.parse_binary_operator(&VoidHandler)) {
            expressions.push((
                binary_operator,
                Some(Expression::Primary(self.parse_primary(handler)?)),
            ));
        }

        // fold based on precedence and associativity

        let mut candidate_index = 0;
        let mut current_precedence;

        while !expressions.is_empty() {
            current_precedence = 0;

            for (index, (binary_operator, _)) in expressions.iter().enumerate() {
                let new_precedence = binary_operator.get_precedence();
                if new_precedence.cmp(&current_precedence) == Ordering::Greater {
                    current_precedence = new_precedence;
                    candidate_index = index;
                }
            }

            assert!(current_precedence > 0, "Invalid precedence");

            if candidate_index == 0 {
                let (binary_operator, rhs) = expressions.remove(0);
                first_primary = Expression::Binary(Binary {
                    left_operand: Box::new(first_primary),
                    operator: binary_operator,
                    right_operand: Box::new(rhs.expect("checked above")),
                });
            } else {
                let (binary_operator, rhs) = expressions.remove(candidate_index);

                expressions[candidate_index - 1].1 = Some(Expression::Binary(Binary {
                    left_operand: Box::new(expressions[candidate_index - 1].1.take().unwrap()),
                    operator: binary_operator,
                    right_operand: Box::new(rhs.expect("checked above")),
                }));
            }
        }

        Ok(first_primary)
    }

    /// Parses an [`Primary`]
    ///
    /// # Errors
    /// - If the parser is not at a primary expression.
    /// - If the parser is not at a valid primary expression.
    #[expect(clippy::too_many_lines)]
    pub fn parse_primary(&mut self, handler: &impl Handler<base::Error>) -> ParseResult<Primary> {
        let prim = match self.stop_at_significant() {
            // prefixed expression with '!' or '-'
            Reading::Atomic(Token::Punctuation(punc)) if matches!(punc.punctuation, '!' | '-') => {
                // eat the prefix
                self.forward();

                let prefix_operator = match punc.punctuation {
                    '!' => PrefixOperator::LogicalNot(punc),
                    '-' => PrefixOperator::Negate(punc),
                    _ => unreachable!(),
                };

                let operand = Box::new(self.parse_primary(handler)?);

                Ok(Primary::Prefix(Prefix {
                    operator: prefix_operator,
                    operand,
                }))
            }

            // prefixed expression with 'run'
            Reading::Atomic(Token::Keyword(run_keyword))
                if run_keyword.keyword == KeywordKind::Run =>
            {
                // eat the run keyword
                self.forward();

                let expression = self.parse_primary(handler)?;

                tracing::trace!("Parsed run expression: {:?}", expression);

                Ok(Primary::Prefix(Prefix {
                    operator: PrefixOperator::Run(run_keyword),
                    operand: Box::new(expression),
                }))
            }

            // parenthesized expression
            Reading::IntoDelimited(left_parenthesis) if left_parenthesis.punctuation == '(' => self
                .parse_parenthesized(handler)
                .map(Primary::Parenthesized),

            // identifier expression
            Reading::Atomic(Token::Identifier(identifier)) => {
                // eat the identifier
                self.forward();

                // function call
                match self.stop_at_significant() {
                    Reading::IntoDelimited(punc) if punc.punctuation == '(' => {
                        let token_tree = self.parse_enclosed_list(
                            Delimiter::Parenthesis,
                            ',',
                            |parser| parser.parse_expression(handler).map(Box::new),
                            handler,
                        )?;

                        Ok(Primary::FunctionCall(FunctionCall {
                            identifier,
                            left_parenthesis: token_tree.open,
                            right_parenthesis: token_tree.close,
                            arguments: token_tree.list,
                        }))
                    }
                    Reading::IntoDelimited(punc) if punc.punctuation == '[' => {
                        let token_tree = self.step_into(
                            Delimiter::Bracket,
                            |p| p.parse_expression(handler),
                            handler,
                        )?;

                        Ok(Primary::Indexed(Indexed {
                            object: Box::new(Primary::Identifier(identifier)),
                            left_bracket: token_tree.open,
                            index: Box::new(token_tree.tree?),
                            right_bracket: token_tree.close,
                        }))
                    }
                    _ => {
                        // regular identifier
                        Ok(Primary::Identifier(identifier))
                    }
                }
            }

            // integer expression
            Reading::Atomic(Token::Integer(int)) => {
                // eat the int
                self.forward();

                Ok(Primary::Integer(int))
            }

            // boolean expression
            Reading::Atomic(Token::Boolean(bool)) => {
                // eat the bool
                self.forward();

                Ok(Primary::Boolean(bool))
            }

            // string literal expression
            Reading::Atomic(Token::StringLiteral(literal)) => {
                // eat the string literal
                self.forward();

                Ok(Primary::StringLiteral(literal))
            }

            // macro string literal expression
            Reading::Atomic(Token::MacroStringLiteral(macro_string_literal)) => {
                // eat the macro string literal
                self.forward();

                Ok(Primary::MacroStringLiteral(macro_string_literal))
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
                        Reading::Atomic(Token::Identifier(identifier)) => Ok(identifier),
                        unexpected => {
                            let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                                expected: syntax::error::SyntaxKind::Identifier,
                                found: unexpected.into_token(),
                            });
                            handler.receive(err.clone());
                            Err(err)
                        }
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
                                first.span().join(
                                    &last
                                        .into_token()
                                        .map_or_else(|| first.span(), |last| last.span()),
                                )
                            })
                            .expect("Invalid lua code span");

                        Ok(combined.str().trim().to_owned())
                    },
                    handler,
                )?;

                Ok(Primary::Lua(Box::new(LuaCode {
                    lua_keyword,
                    left_parenthesis: variables.open,
                    inputs: variables.list,
                    right_parenthesis: variables.close,
                    left_brace: tree.open,
                    code: tree.tree?,
                    right_brace: tree.close,
                })))
            }

            unexpected => {
                // make progress
                self.forward();

                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: syntax::error::SyntaxKind::Expression,
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());

                Err(err)
            }
        }?;

        match self.stop_at_significant() {
            Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '.' => {
                self.forward();

                let member = self.parse_identifier(handler)?;

                Ok(Primary::MemberAccess(MemberAccess {
                    parent: Box::new(prim),
                    dot: punc,
                    member,
                }))
            }
            _ => Ok(prim),
        }
    }

    pub(super) fn parse_parenthesized(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Parenthesized> {
        let token_tree = self.step_into(
            Delimiter::Parenthesis,
            |p| p.parse_expression(handler),
            handler,
        )?;

        Ok(Parenthesized {
            open: token_tree.open,
            expression: Box::new(token_tree.tree?),
            close: token_tree.close,
        })
    }

    fn parse_binary_operator(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<BinaryOperator> {
        match self.next_significant_token() {
            Reading::Atomic(Token::Punctuation(punc)) => match punc.punctuation {
                '+' => Ok(BinaryOperator::Add(punc)),
                '-' => Ok(BinaryOperator::Subtract(punc)),
                '*' => Ok(BinaryOperator::Multiply(punc)),
                '/' => Ok(BinaryOperator::Divide(punc)),
                '%' => Ok(BinaryOperator::Modulo(punc)),
                '!' => {
                    let equal = self.parse_punctuation('=', false, handler)?;
                    Ok(BinaryOperator::NotEqual(punc, equal))
                }
                '=' => {
                    let equal = self.parse_punctuation('=', false, handler)?;
                    Ok(BinaryOperator::Equal(punc, equal))
                }
                '<' => {
                    let equal = self.try_parse(|p| p.parse_punctuation('=', false, &VoidHandler));
                    if let Ok(equal) = equal {
                        Ok(BinaryOperator::LessThanOrEqual(punc, equal))
                    } else {
                        Ok(BinaryOperator::LessThan(punc))
                    }
                }
                '>' => {
                    let equal = self.try_parse(|p| p.parse_punctuation('=', false, &VoidHandler));
                    if let Ok(equal) = equal {
                        Ok(BinaryOperator::GreaterThanOrEqual(punc, equal))
                    } else {
                        Ok(BinaryOperator::GreaterThan(punc))
                    }
                }
                '&' => {
                    let second = self.parse_punctuation('&', false, handler)?;
                    Ok(BinaryOperator::LogicalAnd(punc, second))
                }
                '|' => {
                    let second = self.parse_punctuation('|', false, handler)?;
                    Ok(BinaryOperator::LogicalOr(punc, second))
                }

                _ => {
                    let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                        expected: syntax::error::SyntaxKind::Operator,
                        found: Some(Token::Punctuation(punc)),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            },
            unexpected => {
                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: syntax::error::SyntaxKind::Operator,
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }
    }
}
