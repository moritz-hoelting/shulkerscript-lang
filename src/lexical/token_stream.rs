//! Contains the [`TokenStream`] struct and its related types.

use std::{fmt::Debug, sync::Arc};

use derive_more::{Deref, From};

use crate::base::{source_file::SourceFile, Handler};

use super::{
    error::{self, UndelimitedDelimiter},
    token::{Punctuation, Token, TokenizeError},
};

/// Is a list of well structured [`TokenTree`]s.
///
/// This struct is the final output of the lexical analysis phase and is meant to be used by the
/// next stage of the compilation process.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Deref)]
pub struct TokenStream {
    #[deref]
    token_trees: Vec<TokenTree>,
}

impl Debug for TokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.token_trees.iter()).finish()
    }
}

impl TokenStream {
    /// Tokenizes the given source code.
    ///
    /// This function tokenizes the given iterator of source code by calling the
    /// [`Token::tokenize()`] repeatedly until the iterator is exhausted.
    ///
    /// # Parameters
    /// - `source_file_iterator`: The iterator that iterates over the source code.
    ///
    /// # Returns
    /// A tuple containing the stream of successfully tokenized tokens and a list of lexical errors
    /// encountered during tokenization.
    #[must_use]
    pub fn tokenize(source_file: &Arc<SourceFile>, handler: &impl Handler<error::Error>) -> Self {
        // The list of token trees that will be returned.
        let mut tokens = Vec::new();
        let mut source_file_iterator = source_file.iter();

        // Tokenize the source code.
        loop {
            match Token::tokenize(&mut source_file_iterator, handler) {
                Ok(token) => tokens.push(token),
                Err(TokenizeError::EndOfSourceCodeIteratorArgument) => {
                    break;
                }
                Err(TokenizeError::FatalLexicalError) => (),
            }
        }

        // reverse to use pop() instead of remove(0)
        tokens.reverse();

        // stucture the tokens into a token stream
        let mut token_trees = Vec::new();
        while let Some(token_tree) = Self::handle_token(&mut tokens, handler) {
            token_trees.push(token_tree);
        }

        Self { token_trees }
    }

    /// Handles a token.
    fn handle_token(
        tokens: &mut Vec<Token>,
        handler: &impl Handler<error::Error>,
    ) -> Option<TokenTree> {
        tokens
            .pop()
            .and_then(|token| Self::handle_popped_token(tokens, token, handler))
    }

    /// Handles a token after it has been popped.
    fn handle_popped_token(
        tokens: &mut Vec<Token>,
        popped_token: Token,
        handler: &dyn Handler<error::Error>,
    ) -> Option<TokenTree> {
        match popped_token {
            Token::Punctuation(punc) if punc.punctuation == '{' => {
                Self::handle_delimited(tokens, punc, Delimiter::Brace, handler)
                    .map(TokenTree::Delimited)
            }
            Token::Punctuation(punc) if punc.punctuation == '[' => {
                Self::handle_delimited(tokens, punc, Delimiter::Bracket, handler)
                    .map(TokenTree::Delimited)
            }
            Token::Punctuation(punc) if punc.punctuation == '(' => {
                Self::handle_delimited(tokens, punc, Delimiter::Parenthesis, handler)
                    .map(TokenTree::Delimited)
            }
            token => Some(TokenTree::Token(token)),
        }
    }

    /// Handles a delimited token.
    fn handle_delimited(
        tokens: &mut Vec<Token>,
        open: Punctuation,
        delimiter: Delimiter,
        handler: &dyn Handler<error::Error>,
    ) -> Option<Delimited> {
        let mut token_trees = Vec::new();

        while let Some(token) = tokens.pop() {
            match (token, delimiter) {
                (Token::Punctuation(p), Delimiter::Brace) if p.punctuation == '}' => {
                    return Some(Delimited {
                        open,
                        token_stream: Self { token_trees },
                        close: p,
                        delimiter,
                    });
                }
                (Token::Punctuation(punc), Delimiter::Bracket) if punc.punctuation == ']' => {
                    return Some(Delimited {
                        open,
                        token_stream: Self { token_trees },
                        close: punc,
                        delimiter,
                    })
                }
                (Token::Punctuation(punc), Delimiter::Parenthesis) if punc.punctuation == ')' => {
                    return Some(Delimited {
                        open,
                        token_stream: Self { token_trees },
                        close: punc,
                        delimiter,
                    })
                }
                (token, _) => {
                    let Some(token_tree) = Self::handle_popped_token(tokens, token, handler) else {
                        break;
                    };

                    token_trees.push(token_tree);
                }
            }
        }

        handler.receive(error::Error::UndelimitedDelimiter(UndelimitedDelimiter {
            opening_span: open.span,
            delimiter,
        }));

        None
    }

    /// Dissolves this struct into a tuple of its components.
    #[must_use]
    pub fn dissolve(self) -> Vec<TokenTree> {
        self.token_trees
    }
}

/// Is an enumeration of either a [`Token`] or a [`Delimited`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
#[allow(missing_docs)]
pub enum TokenTree {
    Token(Token),
    Delimited(Delimited),
}

/// Is an enumeration of the different types of delimiters in the [`Delimited`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
}

/// Represents a list of tokens enclosed by a pair of delimiters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Delimited {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The stream of tokens inside the delimiter.
    pub token_stream: TokenStream,

    /// The closing delimiter.
    pub close: Punctuation,

    /// The type of delimiter.
    pub delimiter: Delimiter,
}
