//! Compiler for `ShulkerScript`

use std::collections::HashMap;

use shulkerbox::datapack::{Command, Datapack};

use crate::{
    base::{source_file::SourceElement, Handler},
    syntax::syntax_tree::{declaration::Declaration, program::Program, statement::Statement},
};

use super::error::{self, CompileError};

/// A compiler for `ShulkerScript`.
#[derive(Debug, Clone, Default)]
pub struct Compiler {
    functions: HashMap<String, Vec<Statement>>,
}

impl Compiler {
    /// Creates a new compiler.
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Compiles the given program.
    ///
    /// # Errors
    /// - [`CompileError::MissingMainFunction`] If the main function is missing.
    pub fn compile(
        &mut self,
        program: &Program,
        handler: &impl Handler<error::CompileError>,
    ) -> Result<Datapack, CompileError> {
        for declaration in program.declarations() {
            match declaration {
                Declaration::Function(function) => self.functions.insert(
                    function.identifier().span().str().to_string(),
                    function.block().statements().clone(),
                ),
            };
        }

        let Some(main_function) = self.functions.get("main") else {
            handler.receive(CompileError::MissingMainFunction);
            return Err(CompileError::MissingMainFunction);
        };

        let main_commands = compile_function(main_function);
        // TODO: change this
        let mut datapack = shulkerbox::datapack::Datapack::new("shulkerscript-pack", 27);
        let namespace = datapack.namespace_mut("shulkerscript");
        let main_function = namespace.function_mut("main");
        main_function.get_commands_mut().extend(main_commands);

        Ok(datapack)
    }
}

fn compile_function(statements: &[Statement]) -> Vec<Command> {
    let mut commands = Vec::new();
    for statement in statements {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                commands.push(literal_command.clean_command().into());
            }
            Statement::Block(_) => {
                unreachable!("Only literal commands are allowed in functions at this time.")
            }
        }
    }
    commands
}
