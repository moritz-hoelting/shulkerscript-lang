//! Compiler for `ShulkerScript`

use std::collections::HashMap;

use shulkerbox::datapack::{Command, Datapack, Execute};

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
        commands.extend(compile_statement(statement));
    }
    commands
}

fn compile_statement(statement: &Statement) -> Option<Command> {
    match statement {
        Statement::LiteralCommand(literal_command) => Some(literal_command.clean_command().into()),
        Statement::Block(_) => {
            unreachable!("Only literal commands are allowed in functions at this time.")
        }
        Statement::Conditional(cond) => {
            let (_, cond, block, el) = cond.clone().dissolve();
            let (_, cond, _) = cond.dissolve();
            let statements = block.statements();

            let el = el
                .and_then(|el| {
                    let (_, block) = el.dissolve();
                    let statements = block.statements();
                    if statements.is_empty() {
                        None
                    } else if statements.len() == 1 {
                        compile_statement(&statements[0]).map(|cmd| Execute::Run(Box::new(cmd)))
                    } else {
                        let commands = statements.iter().filter_map(compile_statement).collect();
                        Some(Execute::Runs(commands))
                    }
                })
                .map(Box::new);

            if statements.is_empty() {
                if el.is_none() {
                    None
                } else {
                    Some(Command::Execute(Execute::If(
                        cond.value().string_content().into(),
                        Box::new(Execute::Runs(Vec::new())),
                        el,
                    )))
                }
            } else {
                let run = if statements.len() > 1 {
                    let commands = statements.iter().filter_map(compile_statement).collect();
                    Execute::Runs(commands)
                } else {
                    Execute::Run(Box::new(compile_statement(&statements[0])?))
                };

                Some(Command::Execute(Execute::If(
                    cond.value().string_content().into(),
                    Box::new(run),
                    el,
                )))
            }
        }
    }
}
