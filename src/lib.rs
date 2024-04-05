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

pub mod base;
pub mod lexical;
pub mod syntax;
pub mod transpile;

use std::{cell::Cell, fmt::Display, path::Path};

use base::{source_file::SourceFile, Handler, Result};
use syntax::syntax_tree::program::Program;

#[cfg(feature = "shulkerbox")]
use transpile::transpiler::Transpiler;

#[cfg(feature = "shulkerbox")]
use shulkerbox::{datapack::Datapack, util::compile::CompileOptions, virtual_fs::VFolder};

use crate::{base::Error, lexical::token_stream::TokenStream, syntax::parser::Parser};

/// Converts the given source code to tokens.
///
/// # Errors
/// - If an error occurs while reading the file.
pub fn tokenize(path: &Path) -> Result<TokenStream> {
    let source_file = SourceFile::load(path)?;

    let printer = Printer::new();

    Ok(TokenStream::tokenize(&source_file, &printer))
}

/// Parses the given source code.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
pub fn parse(path: &Path) -> Result<Program> {
    let source_file = SourceFile::load(path)?;

    let printer = Printer::new();

    let tokens = TokenStream::tokenize(&source_file, &printer);

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program(&printer).ok_or(Error::Other(
        "An error occured while parsing the source code.",
    ))?;

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while parsing the source code.",
        ));
    }

    Ok(program)
}

/// Transpiles the given source code into a shulkerbox [`Datapack`].
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
/// - If an error occurs while transpiling the source code.
#[cfg(feature = "shulkerbox")]
pub fn transpile(path: &Path) -> Result<Datapack> {
    let source_file = SourceFile::load(path)?;

    let printer = Printer::new();

    let tokens = TokenStream::tokenize(&source_file, &printer);

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program(&printer).ok_or(Error::Other(
        "An error occured while parsing the source code.",
    ))?;

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while parsing the source code.",
        ));
    }

    let mut transpiler = Transpiler::new("shulkerscript-pack", 27);
    transpiler.transpile(&program, &printer)?;
    let datapack = transpiler.into_datapack();

    Ok(datapack)
}

/// Compiles the given source code.
///
/// # Errors
/// - If an error occurs while reading the file.
/// - If an error occurs while parsing the source code.
/// - If an error occurs while transpiling the source code.
#[cfg(feature = "shulkerbox")]
pub fn compile(path: &Path) -> Result<VFolder> {
    let source_file = SourceFile::load(path)?;

    let printer = Printer::new();

    let tokens = TokenStream::tokenize(&source_file, &printer);

    // println!("tokens: {tokens:#?}");

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program(&printer).ok_or(Error::Other(
        "An error occured while parsing the source code.",
    ))?;

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while parsing the source code.",
        ));
    }

    // println!("program: {program:#?}");

    let mut transpiler = Transpiler::new("shulkerscript-pack", 27);
    transpiler.transpile(&program, &printer)?;
    let datapack = transpiler.into_datapack();

    // println!("datapack: {datapack:#?}");

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while transpiling the source code.",
        ));
    }

    Ok(datapack.compile(&CompileOptions::default()))
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
