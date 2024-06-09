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

use std::{cell::Cell, fmt::Display, path::Path};

use base::{source_file::SourceFile, Handler, Result};
use syntax::syntax_tree::program::ProgramFile;

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
pub fn parse(path: &Path) -> Result<ProgramFile> {
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
pub fn transpile<P>(script_paths: &[(String, P)]) -> Result<Datapack>
where
    P: AsRef<Path>,
{
    let printer = Printer::new();

    let programs = script_paths
        .iter()
        .map(|(program_identifier, path)| {
            let source_file = SourceFile::load(path.as_ref())?;

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

            Ok((program_identifier, program))
        })
        .collect::<Vec<_>>();

    if programs.iter().any(Result::is_err) {
        return Err(programs.into_iter().find_map(Result::err).unwrap());
    }
    let programs = programs
        .into_iter()
        .filter_map(Result::ok)
        .collect::<Vec<_>>();

    let mut transpiler = Transpiler::new(27);
    transpiler.transpile(&programs, &printer)?;
    let datapack = transpiler.into_datapack();

    // println!("datapack: {datapack:#?}");

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while transpiling the source code.",
        ));
    }

    Ok(datapack)
}

/// Compiles the given source code.
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

    let programs = script_paths
        .iter()
        .map(|(program_identifier, path)| {
            let source_file = SourceFile::load(path.as_ref())?;

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

            Ok((program_identifier, program))
        })
        .collect::<Vec<_>>();

    if programs.iter().any(Result::is_err) {
        return Err(programs.into_iter().find_map(Result::err).unwrap());
    }
    let programs = programs
        .into_iter()
        .filter_map(Result::ok)
        .collect::<Vec<_>>();

    let mut transpiler = Transpiler::new(27);
    transpiler.transpile(&programs, &printer)?;
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
