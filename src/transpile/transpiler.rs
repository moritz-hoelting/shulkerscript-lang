//! Compiler for `ShulkerScript`

use std::collections::HashMap;

use shulkerbox::datapack::{self, Command, Datapack, Execute};

use crate::{
    base::{source_file::SourceElement, Handler},
    syntax::syntax_tree::{declaration::Declaration, program::Program, statement::Statement},
};

use super::error::{self, TranspileError};

/// A transpiler for `ShulkerScript`.
#[derive(Debug, Clone)]
pub struct Transpiler {
    datapack: shulkerbox::datapack::Datapack,
    functions: HashMap<String, FunctionData>,
    function_locations: HashMap<String, String>,
}

#[derive(Debug, Clone)]
struct FunctionData {
    namespace: String,
    statements: Vec<Statement>,
    annotations: HashMap<String, Option<String>>,
}

impl Transpiler {
    /// Creates a new transpiler.
    #[must_use]
    pub fn new(pack_name: &str, pack_format: u8) -> Self {
        Self {
            datapack: shulkerbox::datapack::Datapack::new(pack_name, pack_format),
            functions: HashMap::new(),
            function_locations: HashMap::new(),
        }
    }

    /// Consumes the transpiler and returns the resulting datapack.
    #[must_use]
    pub fn into_datapack(self) -> Datapack {
        self.datapack
    }

    /// Transpiles the given program.
    ///
    /// # Errors
    /// - [`TranspileError::MissingMainFunction`] If the main function is missing.
    pub fn transpile(
        &mut self,
        program: &Program,
        handler: &impl Handler<error::TranspileError>,
    ) -> Result<(), TranspileError> {
        for declaration in program.declarations() {
            self.transpile_declaration(declaration);
        }

        self.get_or_transpile_function("main").ok_or_else(|| {
            handler.receive(TranspileError::MissingMainFunction);
            TranspileError::MissingMainFunction
        })?;

        Ok(())
    }

    /// Transpiles the given declaration.
    fn transpile_declaration(&mut self, declaration: &Declaration) {
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
                self.functions.insert(
                    name,
                    FunctionData {
                        namespace: "shulkerscript".to_string(),
                        statements,
                        annotations,
                    },
                );
            }
        };
    }

    /// Gets the function at the given path, or transpiles it if it hasn't been transpiled yet.
    fn get_or_transpile_function(&mut self, path: &str) -> Option<&str> {
        let already_transpiled = self.function_locations.get(path);
        if already_transpiled.is_none() {
            let function_data = self.functions.get(path)?;
            let commands = compile_function(&function_data.statements);

            let function = self
                .datapack
                .namespace_mut(&function_data.namespace)
                .function_mut(path);
            function.get_commands_mut().extend(commands);

            let function_location =
                format!("{namespace}:{path}", namespace = function_data.namespace);

            if function_data.annotations.contains_key("tick") {
                self.datapack.add_tick(&function_location);
            }
            if function_data.annotations.contains_key("load") {
                self.datapack.add_load(&function_location);
            }

            self.function_locations
                .insert(path.to_string(), function_location);
        }

        self.function_locations.get(path).map(String::as_str)
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
