//! Contains the [`Token`] struct and its related types.

use std::{collections::HashMap, str::FromStr, sync::OnceLock};

use crate::base::{
    source_file::{SourceElement, SourceIterator, Span},
    Handler,
};
use derive_more::From;
use enum_as_inner::EnumAsInner;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use super::{error::UnterminatedDelimitedComment, Error};

/// Is an enumeration representing keywords in shulkerscript.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
#[allow(missing_docs)]
pub enum KeywordKind {
    Function,
    Pub,
    If,
    Else,
    Align,
    Anchored,
    As,
    AsAt,
    At,
    Facing,
    In,
    On,
    Positioned,
    Rotated,
    Store,
    Summon,
    Group,
    Run,
    Lua,
    Namespace,
}

impl ToString for KeywordKind {
    fn to_string(&self) -> String {
        self.as_str().to_string()
    }
}

/// Is an error that is returned when a string cannot be parsed into a [`Keyword`] in [`FromStr`]
/// trait implementation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, thiserror::Error)]
#[error("invalid string representation of keyword.")]
pub struct KeywordParseError;

impl FromStr for KeywordKind {
    type Err = KeywordParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static STRING_KEYWORD_MAP: OnceLock<HashMap<&'static str, KeywordKind>> = OnceLock::new();
        let map = STRING_KEYWORD_MAP.get_or_init(|| {
            let mut map = HashMap::new();

            for keyword in Self::iter() {
                map.insert(keyword.as_str(), keyword);
            }

            map
        });

        map.get(s).copied().ok_or(KeywordParseError)
    }
}

impl KeywordKind {
    /// Gets the string representation of the keyword as a `&str`.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Function => "fn",
            Self::Pub => "pub",
            Self::If => "if",
            Self::Else => "else",
            Self::Align => "align",
            Self::Anchored => "anchored",
            Self::As => "as",
            Self::AsAt => "asat",
            Self::At => "at",
            Self::Facing => "facing",
            Self::In => "in",
            Self::On => "on",
            Self::Positioned => "positioned",
            Self::Rotated => "rotated",
            Self::Store => "store",
            Self::Summon => "summon",
            Self::Group => "group",
            Self::Run => "run",
            Self::Lua => "lua",
            Self::Namespace => "namespace",
        }
    }

    /// Whether the keyword starts an execute block.
    #[must_use]
    pub fn starts_execute_block(&self) -> bool {
        matches!(
            self,
            Self::If
                | Self::Align
                | Self::Anchored
                | Self::As
                | Self::AsAt
                | Self::At
                | Self::Facing
                | Self::In
                | Self::On
                | Self::Positioned
                | Self::Rotated
                | Self::Store
                | Self::Summon
        )
    }
}

/// Is an enumeration containing all kinds of tokens in the Shulkerscript programming language.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From, EnumAsInner)]
#[allow(missing_docs)]
pub enum Token {
    WhiteSpaces(WhiteSpaces),
    Identifier(Identifier),
    Keyword(Keyword),
    Punctuation(Punctuation),
    Numeric(Numeric),
    Comment(Comment),
    DocComment(DocComment),
    CommandLiteral(CommandLiteral),
    StringLiteral(StringLiteral),
}

impl Token {
    /// Returns the span of the token.
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Self::WhiteSpaces(token) => &token.span,
            Self::Identifier(token) => &token.span,
            Self::Keyword(token) => &token.span,
            Self::Punctuation(token) => &token.span,
            Self::Numeric(token) => &token.span,
            Self::Comment(token) => &token.span,
            Self::DocComment(token) => &token.span,
            Self::CommandLiteral(token) => &token.span,
            Self::StringLiteral(token) => &token.span,
        }
    }
}

impl SourceElement for Token {
    fn span(&self) -> Span {
        match self {
            Self::WhiteSpaces(token) => token.span(),
            Self::Identifier(token) => token.span(),
            Self::Keyword(token) => token.span(),
            Self::Punctuation(token) => token.span(),
            Self::Numeric(token) => token.span(),
            Self::Comment(token) => token.span(),
            Self::DocComment(token) => token.span(),
            Self::CommandLiteral(token) => token.span(),
            Self::StringLiteral(token) => token.span(),
        }
    }
}

/// Represents a contiguous sequence of whitespace characters.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhiteSpaces {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for WhiteSpaces {
    fn span(&self) -> Span {
        self.span.clone()
    }
}
/// Represents a contiguous sequence of characters that are valid in an identifier.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Identifier {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Represents a contiguous sequence of characters that are reserved for a keyword.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Keyword {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the [`KeywordKind`] that the token represents.
    pub keyword: KeywordKind,
}

impl SourceElement for Keyword {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Represents a single ASCII punctuation character.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Punctuation {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the ASCII punctuation character that the token represents.
    pub punctuation: char,
}

impl SourceElement for Punctuation {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Represents a hardcoded numeric literal value in the source code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for Numeric {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Represents a hardcoded string literal value in the source code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StringLiteral {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl StringLiteral {
    /// Returns the string without the leading and trailing double quotes.
    #[must_use]
    pub fn str_content(&self) -> &str {
        let string = self.span.str();
        &string[1..string.len() - 1]
    }
}

impl SourceElement for StringLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Is an enumeration representing the two kinds of comments in the Shulkerscript programming language.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CommentKind {
    /// A comment that starts with `//` and ends at the end of the line.
    Line,

    /// A comment that starts with `/*` and ends with `*/`.
    Delimited,
}

/// Represents a portion of the source code that is ignored by the interpreter.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Comment {
    /// Is the span that makes up the token.
    pub span: Span,

    /// Is the kind of comment that the token represents.
    pub kind: CommentKind,
}

impl SourceElement for Comment {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

/// Represents a documentation comment in the source code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DocComment {
    /// Is the span that makes up the token.
    pub span: Span,
}

impl SourceElement for DocComment {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl DocComment {
    /// Returns the content of the doc comment without the leading `///`.
    #[must_use]
    pub fn content(&self) -> &str {
        &self.span.str().trim()[3..]
    }
}

/// Represents a hardcoded literal command in the source code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CommandLiteral {
    /// Span that makes up the token.
    pub span: Span,
}

impl SourceElement for CommandLiteral {
    fn span(&self) -> Span {
        self.span.clone()
    }
}
impl CommandLiteral {
    /// Returns the command without the leading slash.
    #[must_use]
    pub fn clean_command(&self) -> &str {
        &self.span.str().trim()[1..]
    }
}

/// Is an error that can occur when invoking the [`Token::tokenize`] method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error, From)]
#[allow(missing_docs)]
pub enum TokenizeError {
    #[error("encountered a fatal lexical error that causes the process to stop.")]
    FatalLexicalError,

    #[error("the iterator argument is at the end of the source code.")]
    EndOfSourceCodeIteratorArgument,
}

impl Token {
    /// Increments the iterator while the predicate returns true.
    pub fn walk_iter(iter: &mut SourceIterator, predicate: impl Fn(char) -> bool) {
        while let Some((_, character)) = iter.peek() {
            if !predicate(character) {
                break;
            }

            iter.next();
        }
    }

    /// Creates a span from the given start location to the current location of the iterator.
    fn create_span(start: usize, iter: &mut SourceIterator) -> Span {
        iter.peek().map_or_else(
            || Span::to_end(iter.source_file().clone(), start).unwrap(),
            |(index, _)| Span::new(iter.source_file().clone(), start, index).unwrap(),
        )
    }

    /// Checks if the given character is a valid first character of an identifier.
    fn is_first_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation()
                && !character.is_ascii_digit())
    }

    /// Checks if the given character is a valid character of an identifier.
    fn is_identifier_character(character: char) -> bool {
        character == '_'
            || (!character.is_control()
                && !character.is_whitespace()
                && !character.is_ascii_punctuation())
    }

    /// Handles a contiguous sequence of whitespace characters.
    fn handle_whitespace(iter: &mut SourceIterator, start: usize) -> Self {
        Self::walk_iter(iter, char::is_whitespace);

        WhiteSpaces {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    /// Handles a contiguous sequence of characters that are valid in an identifier.
    fn handle_identifier_and_keyword(iter: &mut SourceIterator, start: usize) -> Self {
        Self::walk_iter(iter, Self::is_identifier_character);

        let span = Self::create_span(start, iter);
        let word = span.str();

        // Checks if the word is a keyword
        KeywordKind::from_str(word).ok().map_or_else(
            || Identifier { span: span.clone() }.into(),
            |kw| {
                Keyword {
                    span: span.clone(),
                    keyword: kw,
                }
                .into()
            },
        )
    }

    /// Handles a sequence starting with a slash
    fn handle_comment(
        iter: &mut SourceIterator,
        start: usize,
        character: char,
        prev_token: Option<&Self>,
        handler: &impl Handler<Error>,
    ) -> Result<Self, TokenizeError> {
        // Single line comment
        if let Some((_, '/')) = iter.peek() {
            iter.next();

            let is_doccomment = if let Some((_, '/')) = iter.peek() {
                iter.next();
                true
            } else {
                false
            };

            Self::walk_iter(iter, |character| !(character == '\n' || character == '\r'));

            let is_cr = iter
                .peek()
                .map_or(false, |(_, character)| character == '\r');

            let span = Self::create_span(start, iter);

            if let (true, Some((_, '\n'))) = (is_cr, iter.next()) {
                // skips the crlf
                iter.next();
            }

            let comment = if is_doccomment {
                DocComment { span }.into()
            } else {
                Comment {
                    span,
                    kind: CommentKind::Line,
                }
                .into()
            };

            Ok(comment)
        }
        // Delimited comment
        else if let Some((_, '*')) = iter.peek() {
            iter.next();

            let mut is_terminated = false;

            while let Some((_, character)) = iter.next() {
                if character == '*' {
                    if let Some((_, '/')) = iter.peek() {
                        iter.next();

                        is_terminated = true;

                        break;
                    }
                }
            }

            // Checks if the comment is terminated
            if is_terminated {
                Ok(Comment {
                    span: Self::create_span(start, iter),
                    kind: CommentKind::Delimited,
                }
                .into())
            } else {
                handler.receive(
                    UnterminatedDelimitedComment {
                        span: Span::new(iter.source_file().clone(), start, start + 2).unwrap(),
                    }
                    .into(),
                );
                return Err(TokenizeError::FatalLexicalError);
            }
        }
        // When there is no second slash and at the start of a line
        else if prev_token.map_or(true, |token| token.span().str().contains('\n')) {
            Ok(Self::handle_command_literal(iter, start))
        }
        // Just a single slash punctuation
        else {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        }
    }

    /// Handles a sequence of digits
    fn handle_numeric_literal(iter: &mut SourceIterator, start: usize) -> Self {
        // Tokenizes the whole number part
        Self::walk_iter(iter, |character| character.is_ascii_digit());

        Numeric {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    /// Handles a sequence of characters that are enclosed in double quotes
    fn handle_string_literal(iter: &mut SourceIterator, start: usize) -> Self {
        let mut is_escaped = false;

        for (_, character) in iter.by_ref() {
            if character == '\\' {
                is_escaped = !is_escaped;
            } else if character == '"' && !is_escaped {
                break;
            } else {
                is_escaped = false;
            }
        }

        StringLiteral {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    /// Handles a command that is preceeded by a slash
    fn handle_command_literal(iter: &mut SourceIterator, start: usize) -> Self {
        Self::walk_iter(iter, |c| !(c.is_whitespace() && c.is_ascii_control()));

        CommandLiteral {
            span: Self::create_span(start, iter),
        }
        .into()
    }

    /// Lexes the source code from the given iterator.
    ///
    /// The tokenization starts at the current location of the iterator. The function moves the
    /// iterator at least once and forwards it until it makes a token. After the token is made, the
    /// iterator is left at the next character that is not part of the token.
    ///
    /// # Errors
    /// - [`TokenizeError::EndOfSourceCodeIteratorArgument`] - The iterator argument is at the end of the
    ///   source code.
    /// - [`TokenizeError::FatalLexicalError`] - A fatal lexical error occurred.
    pub fn tokenize(
        iter: &mut SourceIterator,
        handler: &impl Handler<Error>,
        prev_token: Option<&Self>,
    ) -> Result<Self, TokenizeError> {
        // Gets the first character
        let (start, character) = iter
            .next()
            .ok_or(TokenizeError::EndOfSourceCodeIteratorArgument)?;

        // Found white spaces
        if character.is_whitespace() {
            Ok(Self::handle_whitespace(iter, start))
        }
        // Found identifier/keyword
        else if Self::is_first_identifier_character(character) {
            Ok(Self::handle_identifier_and_keyword(iter, start))
        }
        // Found comment/single slash punctuation
        else if character == '/' {
            Self::handle_comment(iter, start, character, prev_token, handler)
        } else if character == '"' {
            Ok(Self::handle_string_literal(iter, start))
        }
        // Found numeric literal
        else if character.is_ascii_digit() {
            Ok(Self::handle_numeric_literal(iter, start))
        }
        // Found a punctuation
        else if character.is_ascii_punctuation() {
            Ok(Punctuation {
                span: Self::create_span(start, iter),
                punctuation: character,
            }
            .into())
        } else {
            unreachable!("all cases covered before")
        }
    }
}
