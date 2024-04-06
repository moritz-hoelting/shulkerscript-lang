//! The program node of the syntax tree.

use getset::Getters;

use crate::{
    base::Handler,
    syntax::{
        error::Error,
        parser::{Parser, Reading},
    },
};

use super::declaration::Declaration;

/// Program is a collection of declarations.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Program {
    /// The declarations within the program.
    #[get = "pub"]
    declarations: Vec<Declaration>,
}

impl<'a> Parser<'a> {
    /// Parses a [`Program`].
    pub fn parse_program(&mut self, handler: &impl Handler<Error>) -> Option<Program> {
        let mut declarations = Vec::new();

        while !self.is_exhausted() {
            let result = self.parse_declaration(handler);

            #[allow(clippy::option_if_let_else)]
            if let Some(x) = result {
                declarations.push(x);
            } else {
                self.stop_at(|reading| {
                    matches!(
                        reading,
                        Reading::IntoDelimited(x) if x.punctuation == '{'
                    )
                });

                self.next_token();
            }
        }

        Some(Program { declarations })
    }
}
