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

use base::{FileProvider, Handler, Result};
use syntax::syntax_tree::program::ProgramFile;

#[cfg(feature = "shulkerbox")]
use shulkerbox::{datapack::Datapack, virtual_fs::VFolder};

use crate::lexical::token_stream::TokenStream;

const DEFAULT_PACK_FORMAT: u8 = 48;

/// Converts the given source code to tokens.
///
/// # Errors
/// - If an error occurs while reading the file.
pub fn tokenize<F>(file_provider: &F, path: &Path) -> Result<TokenStream>
where
    F: FileProvider,
{
    let printer = Printer::new();

    public_helpers::tokenize(&printer, file_provider, path)
}

/// Parses the given source code.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
pub fn parse<F>(file_provider: &F, path: &Path) -> Result<ProgramFile>
where
    F: FileProvider,
{
    let printer = Printer::new();

    public_helpers::parse(&printer, file_provider, path)
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
pub fn transpile<F, P>(file_provider: &F, script_paths: &[(String, P)]) -> Result<Datapack>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    let printer = Printer::new();

    public_helpers::transpile(&printer, file_provider, script_paths)
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
pub fn compile<F, P>(file_provider: &F, script_paths: &[(String, P)]) -> Result<VFolder>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    let printer = Printer::new();

    public_helpers::compile(&printer, file_provider, script_paths)
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

impl<T: Display> Handler<T> for Printer {
    fn receive<E: Into<T>>(&self, error: E) {
        eprintln!("{}", error.into());
        self.printed.set(true);
    }

    fn has_received(&self) -> bool {
        self.printed.get()
    }
}
