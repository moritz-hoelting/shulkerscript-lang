//! The `ShulkerScript` language.
//!
//! `ShulkerScript` is a simple, imperative scripting language for creating Minecraft data packs.

#![deny(
    missing_docs,
    missing_debug_implementations,
    missing_copy_implementations,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    rustdoc::broken_intra_doc_links,
    clippy::missing_errors_doc
)]
#![allow(clippy::missing_panics_doc, clippy::missing_const_for_fn)]

pub mod base;
pub mod lexical;

use std::{cell::Cell, fmt::Display, path::PathBuf};

use base::{source_file::SourceFile, Handler, Result};

use crate::{base::Error, lexical::token_stream::TokenStream};

/// Compiles the given source code.
///
/// # Errors
/// - If an error occurs while reading the file.
pub fn compile(path: PathBuf) -> Result<()> {
    let source_file = SourceFile::load(path)?;

    let printer = Printer::new();

    let tokens = TokenStream::tokenize(&source_file, &printer);

    println!("{tokens:#?}");

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    Ok(())
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
