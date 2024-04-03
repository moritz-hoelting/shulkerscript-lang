//! Compiler for `ShulkerScript`

use std::collections::HashMap;

use shulkerbox::datapack::{self, Command, Datapack, Execute};

use crate::{
    base::{source_file::SourceElement, Handler},
    syntax::syntax_tree::{declaration::Declaration, program::Program, statement::Statement},
};

use super::error::{self, TranspileError};

/// A transpiler for `ShulkerScript`.
#[derive(Debug, Clone, Default)]
pub struct Transpiler {
    functions: HashMap<String, (Vec<Statement>, AnnotationMap)>,
}
type AnnotationMap = HashMap<String, Option<String>>;

impl Transpiler {
    /// Creates a new transpiler.
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    /// Transpiles the given program.
    ///
    /// # Errors
    /// - [`TranspileError::MissingMainFunction`] If the main function is missing.
    pub fn transpile(
        &mut self,
        program: &Program,
        handler: &impl Handler<error::TranspileError>,
    ) -> Result<Datapack, TranspileError> {
        for declaration in program.declarations() {
            match declaration {
                Declaration::Function(function) => {
                    let name = function.identifier().span().str().to_string();
                    let statements = function.block().statements().clone();
                    let annotations = function
                        .annotations()
                        .iter()
                        .map(|annotation| {
                            let key = annotation.identifier();
                            let value = annotation.value();
                            (
                                key.span().str().to_string(),
                                value.as_ref().map(|(_, ref v)| v.str_content().to_string()),
                            )
                        })
                        .collect();
                    self.functions.insert(name, (statements, annotations))
                }
            };
        }

        let Some((main_function, main_annotations)) = self.functions.get("main") else {
            handler.receive(TranspileError::MissingMainFunction);
            return Err(TranspileError::MissingMainFunction);
        };

        let main_commands = compile_function(main_function);
        // TODO: change this
        let mut datapack = shulkerbox::datapack::Datapack::new("shulkerscript-pack", 27);
        let namespace = datapack.namespace_mut("shulkerscript");
        let main_function = namespace.function_mut("main");
        main_function.get_commands_mut().extend(main_commands);

        if main_annotations.contains_key("tick") {
            datapack.add_tick("shulkerscript:main");
        }
        if main_annotations.contains_key("load") {
            datapack.add_load("shulkerscript:main");
        }

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
                        datapack::Condition::from(cond),
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
                    datapack::Condition::from(cond),
                    Box::new(run),
                    el,
                )))
            }
        }
        Statement::DocComment(doccomment) => {
            let content = doccomment.content();
            Some(Command::Comment(content.to_string()))
        }
        Statement::Grouping(group) => {
            let statements = group.block().statements();
            let commands = statements
                .iter()
                .filter_map(compile_statement)
                .collect::<Vec<_>>();
            if commands.is_empty() {
                None
            } else {
                Some(Command::Group(commands))
            }
        }
    }
}
