//! The `ShulkerScript` language.
//!
//! `ShulkerScript` is a simple, imperative scripting language for creating Minecraft data packs.

#![deny(
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![warn(missing_docs, clippy::all, clippy::pedantic)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub use shulkerbox;

pub mod base;
pub mod lexical;
pub mod syntax;
pub mod transpile;

mod public_helpers;

use std::{cell::Cell, fmt::Display, path::Path};

use base::{Handler, Result};
use syntax::syntax_tree::program::ProgramFile;

#[cfg(feature = "shulkerbox")]
use shulkerbox::{datapack::Datapack, virtual_fs::VFolder};

use crate::lexical::token_stream::TokenStream;

/// Converts the given source code to tokens.
///
/// # Errors
/// - If an error occurs while reading the file.
pub fn tokenize(path: &Path) -> Result<TokenStream> {
    let printer = Printer::new();

    public_helpers::tokenize(&printer, path)
}

/// Parses the given source code.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
pub fn parse(path: &Path) -> Result<ProgramFile> {
    let printer = Printer::new();

    public_helpers::parse(&printer, path)
}

/// Transpiles the given source code into a shulkerbox [`Datapack`].
///
/// # Parameters:
/// - `script_paths`: A list of tuples containing the identifier of the program and the path to the script.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
/// - If an error occurs while transpiling the source code.
#[cfg(feature = "shulkerbox")]
pub fn transpile<P>(script_paths: &[(String, P)]) -> Result<Datapack>
where
    P: AsRef<Path>,
{
    let printer = Printer::new();

    public_helpers::transpile(&printer, script_paths)
}

/// Compiles the given source code.
///
/// # Parameters:
/// - `script_paths`: A list of tuples containing the identifier of the program and the path to the script.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
/// - If an error occurs while transpiling the source code.
#[cfg(feature = "shulkerbox")]
pub fn compile<P>(script_paths: &[(String, P)]) -> Result<VFolder>
where
    P: AsRef<Path>,
{
    let printer = Printer::new();

    public_helpers::compile(&printer, script_paths)
}

struct Printer {
    printed: Cell<bool>,
}

impl Printer {
    /// Creates a new [`Printer`].
    fn new() -> Self {
        Self {
            printed: Cell::new(false),
        }
    }

    fn has_printed(&self) -> bool {
        self.printed.get()
    }
}

impl<E: Display> Handler<E> for Printer {
    fn receive(&self, error: E) {
        eprintln!("{error}");
        self.printed.set(true);
    }
}
