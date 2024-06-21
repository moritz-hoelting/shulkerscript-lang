use std::path::Path;

use crate::{
    base::{source_file::SourceFile, Error, FileProvider, Result},
    lexical::token_stream::TokenStream,
    syntax::{parser::Parser, syntax_tree::program::ProgramFile},
    Printer,
};

#[cfg(feature = "shulkerbox")]
use crate::transpile::transpiler::Transpiler;

#[cfg(feature = "shulkerbox")]
use shulkerbox::{datapack::Datapack, util::compile::CompileOptions, virtual_fs::VFolder};

/// Tokenizes the source code at the given path.
pub fn tokenize(
    printer: &Printer,
    file_provider: &impl FileProvider,
    path: &Path,
) -> Result<TokenStream> {
    tracing::info!("Tokenizing the source code at path: {}", path.display());

    let source_file = SourceFile::load(path, file_provider)?;

    Ok(TokenStream::tokenize(&source_file, printer))
}

/// Parses the source code at the given path.
pub fn parse(
    printer: &Printer,
    file_provider: &impl FileProvider,
    path: &Path,
) -> Result<ProgramFile> {
    let tokens = tokenize(printer, file_provider, path)?;

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while tokenizing the source code.",
        ));
    }

    tracing::info!("Parsing the source code at path: {}", path.display());

    let mut parser = Parser::new(&tokens);
    let program = parser.parse_program(printer).ok_or(Error::Other(
        "An error occured while parsing the source code.",
    ))?;

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while parsing the source code.",
        ));
    }

    Ok(program)
}

/// Transpiles the source code at the given paths into a shulkerbox [`Datapack`].
#[cfg(feature = "shulkerbox")]
pub fn transpile<F, P>(
    printer: &Printer,
    file_provider: &F,
    script_paths: &[(String, P)],
) -> Result<Datapack>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    let programs = script_paths
        .iter()
        .map(|(program_identifier, path)| {
            let program = parse(printer, file_provider, path.as_ref())?;

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

    let mut transpiler = Transpiler::new(crate::DEFAULT_PACK_FORMAT);
    transpiler.transpile(&programs, printer)?;
    let datapack = transpiler.into_datapack();

    if printer.has_printed() {
        return Err(Error::Other(
            "An error occurred while transpiling the source code.",
        ));
    }

    Ok(datapack)
}

/// Compiles the source code at the given paths.
#[cfg(feature = "shulkerbox")]
pub fn compile<F, P>(
    printer: &Printer,
    file_provider: &F,
    script_paths: &[(String, P)],
) -> Result<VFolder>
where
    F: FileProvider,
    P: AsRef<Path>,
{
    let datapack = transpile(printer, file_provider, script_paths)?;

    tracing::info!("Compiling the source code.");

    Ok(datapack.compile(&CompileOptions::default()))
}
