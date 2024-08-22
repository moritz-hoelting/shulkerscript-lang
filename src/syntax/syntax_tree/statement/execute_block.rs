//! Execute block statement syntax tree.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        VoidHandler, Handler,
    },
    lexical::{
        token::{Keyword, KeywordKind, Punctuation, StringLiteral, Token},
        token_stream::Delimiter,
    },
    syntax::{
        self,
        error::{SyntaxKind, UnexpectedSyntax},
        parser::{DelimitedTree, Parser, Reading},
        syntax_tree::condition::ParenthesizedCondition,
    },
};

use super::Block;

/// Syntax Synopsis:
/// ```ebnf
/// ExecuteBlock:
///    (ExecuteBlockHead ExecuteBlockTail)
///   | (Conditional Block Else)
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

/// Syntax Synopsis:
/// ```ebnf
/// ExecuteBlockHead:
///   Conditional
///  | As
/// ;
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum ExecuteBlockHead {
    Conditional(Conditional),
    Align(Align),
    Anchored(Anchored),
    As(As),
    AsAt(AsAt),
    At(At),
    Facing(Facing),
    In(In),
    On(On),
    Positioned(Positioned),
    Rotated(Rotated),
    Store(Store),
    Summon(Summon),
}

impl SourceElement for ExecuteBlockHead {
    fn span(&self) -> Span {
        match self {
            Self::Conditional(conditional) => conditional.span(),
            Self::Align(align) => align.span(),
            Self::Anchored(anchored) => anchored.span(),
            Self::As(as_) => as_.span(),
            Self::AsAt(as_at) => as_at.span(),
            Self::At(at) => at.span(),
            Self::Facing(facing) => facing.span(),
            Self::In(in_) => in_.span(),
            Self::On(on) => on.span(),
            Self::Positioned(positioned) => positioned.span(),
            Self::Rotated(rotated) => rotated.span(),
            Self::Store(store) => store.span(),
            Self::Summon(summon) => summon.span(),
        }
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// ExecuteBlockTail:
///  ExecuteBlock
/// | Block
/// ;
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
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.as_keyword,
            self.open_paren,
            self.as_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Align:
///     'align' '(' StringLiteral ')'
/// ;
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Align {
    /// The `align` keyword.
    #[get = "pub"]
    align_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the align statement.
    #[get = "pub"]
    align_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}

impl SourceElement for Align {
    fn span(&self) -> Span {
        self.align_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the align statement is invalid.")
    }
}

impl Align {
    /// Dissolves the [`Align`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.align_keyword,
            self.open_paren,
            self.align_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Anchored:
///    'anchored' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Anchored {
    /// The `anchored` keyword.
    #[get = "pub"]
    anchored_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the anchored statement.
    #[get = "pub"]
    anchored_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Anchored {
    fn span(&self) -> Span {
        self.anchored_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the anchored statement is invalid.")
    }
}
impl Anchored {
    /// Dissolves the [`Anchored`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.anchored_keyword,
            self.open_paren,
            self.anchored_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// AsAt:
///   'asat' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct AsAt {
    /// The `asat` keyword.
    #[get = "pub"]
    asat_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the asat statement.
    #[get = "pub"]
    asat_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for AsAt {
    fn span(&self) -> Span {
        self.asat_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the asat statement is invalid.")
    }
}
impl AsAt {
    /// Dissolves the [`AsAt`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.asat_keyword,
            self.open_paren,
            self.asat_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// At:
///  'at' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct At {
    /// The `at` keyword.
    #[get = "pub"]
    at_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the at statement.
    #[get = "pub"]
    at_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for At {
    fn span(&self) -> Span {
        self.at_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the at statement is invalid.")
    }
}
impl At {
    /// Dissolves the [`At`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.at_keyword,
            self.open_paren,
            self.at_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Facing:
///  'facing' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Facing {
    /// The `facing` keyword.
    #[get = "pub"]
    facing_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the facing statement.
    #[get = "pub"]
    facing_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Facing {
    fn span(&self) -> Span {
        self.facing_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the facing statement is invalid.")
    }
}
impl Facing {
    /// Dissolves the [`Facing`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.facing_keyword,
            self.open_paren,
            self.facing_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// In:
/// 'in' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct In {
    /// The `in` keyword.
    #[get = "pub"]
    in_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the in statement.
    #[get = "pub"]
    in_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for In {
    fn span(&self) -> Span {
        self.in_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the in statement is invalid.")
    }
}
impl In {
    /// Dissolves the [`In`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.in_keyword,
            self.open_paren,
            self.in_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// On:
/// 'on' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct On {
    /// The `on` keyword.
    #[get = "pub"]
    on_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the on statement.
    #[get = "pub"]
    on_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for On {
    fn span(&self) -> Span {
        self.on_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the on statement is invalid.")
    }
}
impl On {
    /// Dissolves the [`On`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.on_keyword,
            self.open_paren,
            self.on_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Positioned:
/// 'positioned' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Positioned {
    /// The `positioned` keyword.
    #[get = "pub"]
    positioned_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the positioned statement.
    #[get = "pub"]
    positioned_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Positioned {
    fn span(&self) -> Span {
        self.positioned_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the positioned statement is invalid.")
    }
}
impl Positioned {
    /// Dissolves the [`Positioned`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.positioned_keyword,
            self.open_paren,
            self.positioned_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Rotated:
/// 'rotated' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Rotated {
    /// The `rotated` keyword.
    #[get = "pub"]
    rotated_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the rotated statement.
    #[get = "pub"]
    rotated_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Rotated {
    fn span(&self) -> Span {
        self.rotated_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the rotated statement is invalid.")
    }
}
impl Rotated {
    /// Dissolves the [`Rotated`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.rotated_keyword,
            self.open_paren,
            self.rotated_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Store:
/// 'store' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Store {
    /// The `store` keyword.
    #[get = "pub"]
    store_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the store statement.
    #[get = "pub"]
    store_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Store {
    fn span(&self) -> Span {
        self.store_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the store statement is invalid.")
    }
}
impl Store {
    /// Dissolves the [`Store`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.store_keyword,
            self.open_paren,
            self.store_selector,
            self.close_paren,
        )
    }
}

/// Syntax Synopsis:
/// ```ebnf
/// Summon:
/// 'summon' '(' StringLiteral ')'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Summon {
    /// The `summon` keyword.
    #[get = "pub"]
    summon_keyword: Keyword,
    /// The open parenthesis.
    #[get = "pub"]
    open_paren: Punctuation,
    /// The selector of the summon statement.
    #[get = "pub"]
    summon_selector: StringLiteral,
    /// The close parenthesis.
    #[get = "pub"]
    close_paren: Punctuation,
}
impl SourceElement for Summon {
    fn span(&self) -> Span {
        self.summon_keyword
            .span()
            .join(&self.close_paren.span())
            .expect("The span of the summon statement is invalid.")
    }
}
impl Summon {
    /// Dissolves the [`Summon`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Punctuation, StringLiteral, Punctuation) {
        (
            self.summon_keyword,
            self.open_paren,
            self.summon_selector,
            self.close_paren,
        )
    }
}

impl<'a> Parser<'a> {
    /// Parses an [`ExecuteBlock`].
    pub fn parse_execute_block_statement(
        &mut self,
        handler: &impl Handler<base::Error>,
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
                    let block = parser.parse_block(&VoidHandler)?;
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

            Reading::Atomic(Token::Keyword(keyword)) if keyword.keyword.starts_execute_block() => {
                // eat the as keyword
                self.forward();

                let argument = match self.stop_at_significant() {
                    Reading::IntoDelimited(punc) if punc.punctuation == '(' => self.step_into(
                        Delimiter::Parenthesis,
                        |parser| parser.parse_string_literal(handler),
                        handler,
                    ),
                    unexpected => {
                        handler.receive(syntax::error::Error::from(UnexpectedSyntax {
                            expected: SyntaxKind::Punctuation('('),
                            found: unexpected.into_token(),
                        }));
                        None
                    }
                }?;

                let tail = self.parse_execute_block_tail(handler)?;

                let head = head_from_keyword(keyword, argument)?;

                Some(ExecuteBlock::HeadTail(head, tail))
            }

            // unexpected
            unexpected => {
                handler.receive(syntax::error::Error::from(UnexpectedSyntax {
                    expected: SyntaxKind::ExecuteBlock,
                    found: unexpected.into_token(),
                }));
                None
            }
        }
    }

    fn parse_execute_block_tail(
        &mut self,
        handler: &impl Handler<base::Error>,
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
                handler.receive(syntax::error::Error::from(UnexpectedSyntax {
                    expected: SyntaxKind::ExecuteBlockTail,
                    found: unexpected.into_token(),
                }));
                None
            }
        }
    }
}

fn head_from_keyword(
    keyword: Keyword,
    argument: DelimitedTree<StringLiteral>,
) -> Option<ExecuteBlockHead> {
    Some(match keyword.keyword {
        KeywordKind::Align => Align {
            align_keyword: keyword,
            open_paren: argument.open,
            align_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Anchored => Anchored {
            anchored_keyword: keyword,
            open_paren: argument.open,
            anchored_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::As => As {
            as_keyword: keyword,
            open_paren: argument.open,
            as_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::AsAt => AsAt {
            asat_keyword: keyword,
            open_paren: argument.open,
            asat_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::At => At {
            at_keyword: keyword,
            open_paren: argument.open,
            at_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Facing => Facing {
            facing_keyword: keyword,
            open_paren: argument.open,
            facing_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::In => In {
            in_keyword: keyword,
            open_paren: argument.open,
            in_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::On => On {
            on_keyword: keyword,
            open_paren: argument.open,
            on_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Positioned => Positioned {
            positioned_keyword: keyword,
            open_paren: argument.open,
            positioned_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Rotated => Rotated {
            rotated_keyword: keyword,
            open_paren: argument.open,
            rotated_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Store => Store {
            store_keyword: keyword,
            open_paren: argument.open,
            store_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        KeywordKind::Summon => Summon {
            summon_keyword: keyword,
            open_paren: argument.open,
            summon_selector: argument.tree?,
            close_paren: argument.close,
        }
        .into(),
        _ => unreachable!("The keyword is not a valid execute block head."),
    })
}
