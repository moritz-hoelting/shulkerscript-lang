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

use std::path::Path;

use base::{source_file::SourceFile, Error, FileProvider, Handler, Result};
use syntax::{parser::Parser, syntax_tree::program::ProgramFile};

#[cfg(feature = "shulkerbox")]
use shulkerbox::{datapack::Datapack, virtual_fs::VFolder};

use crate::lexical::token_stream::TokenStream;

/// Converts the given source code to tokens and returns a token stream.
///
/// # Errors
/// - If an error occurs while loading the [`SourceFile`].
pub fn tokenize(
    handler: &impl Handler<base::Error>,
    file_provider: &impl FileProvider,
    path: &Path,
) -> Result<TokenStream> {
    tracing::info!("Tokenizing the source code at path: {}", path.display());

    let source_file = SourceFile::load(path, file_provider)?;

    Ok(TokenStream::tokenize(&source_file, handler))
}

/// Parses the given source code and returns the AST of the program.
///
/// # Errors
/// - If an error occurs during [`tokenize()`].
/// - If an error occurs while parsing the source code.
pub fn parse(
    handler: &impl Handler<base::Error>,
    file_provider: &impl FileProvider,
    path: &Path,
) -> Result<ProgramFile> {
    let tokens = tokenize(handler, file_provider, path)?;

    if handler.has_received() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    tracing::info!("Parsing the source code at path: {}", path.display());

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program(handler).ok_or(Error::Other(
        "An error occured while parsing the source code.",
    ))?;

    if handler.has_received() {
        return Err(Error::Other(
            "An error occurred while parsing the source code.",
        ));
    }

    Ok(program)
}

/// Transpiles the given source code into a shulkerbox [`Datapack`].
///
/// # Parameters:
/// - `script_paths`: A list of tuples containing the identifier and the path of each script file.
///
/// # Errors
/// - If an error occurs during [`parse()`]
/// - If an error occurs while transpiling the source code.
#[cfg(feature = "shulkerbox")]
pub fn transpile<F, P>(
    handler: &impl Handler<base::Error>,
    file_provider: &F,
    pack_format: u8,
    script_paths: &[(String, P)],
) -> Result<Datapack>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    use transpile::Transpiler;

    let programs = script_paths
        .iter()
        .map(|(program_identifier, path)| {
            let program = parse(handler, file_provider, path.as_ref())?;

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

    tracing::info!("Transpiling the source code.");

    let mut transpiler = Transpiler::new(pack_format);
    transpiler.transpile(&programs, handler)?;
    let datapack = transpiler.into_datapack();

    if handler.has_received() {
        return Err(Error::Other(
            "An error occurred while transpiling the source code.",
        ));
    }

    Ok(datapack)
}

/// Compiles the given source code.
///
/// # Parameters:
/// - `script_paths`: A list of tuples containing the identifier and the path of each script file.
///
/// # Errors
/// - If an error occurs during [`transpile()`]
#[cfg(feature = "shulkerbox")]
pub fn compile<F, P>(
    handler: &impl Handler<base::Error>,
    file_provider: &F,
    pack_format: u8,
    script_paths: &[(String, P)],
) -> Result<VFolder>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    use shulkerbox::prelude::CompileOptions;

    let datapack = transpile(handler, file_provider, pack_format, script_paths)?;

    tracing::info!("Compiling the source code.");

    Ok(datapack.compile(&CompileOptions::default()))
}
