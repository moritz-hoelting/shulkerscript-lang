//! Compiler for `ShulkerScript`

use std::{collections::HashMap, sync::RwLock};

use shulkerbox::datapack::{self, Command, Datapack, Execute};

use crate::{
    base::{source_file::SourceElement, Handler},
    syntax::syntax_tree::{
        declaration::Declaration,
        expression::{Expression, Primary},
        program::Program,
        statement::{Conditional, Statement},
    },
};

use super::error::{self, TranspileError};

/// A transpiler for `ShulkerScript`.
#[derive(Debug)]
pub struct Transpiler {
    datapack: shulkerbox::datapack::Datapack,
    functions: RwLock<HashMap<String, FunctionData>>,
    function_locations: RwLock<HashMap<String, String>>,
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
            functions: RwLock::new(HashMap::new()),
            function_locations: RwLock::new(HashMap::new()),
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
                self.functions.write().unwrap().insert(
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
    /// Returns the location of the function or None if the function does not exist.
    #[allow(clippy::significant_drop_tightening)]
    fn get_or_transpile_function(&mut self, path: &str) -> Option<String> {
        let already_transpiled = {
            let locations = self.function_locations.read().unwrap();
            locations.get(path).is_some()
        };
        if !already_transpiled {
            let statements = {
                let functions = self.functions.read().unwrap();
                let function_data = functions.get(path)?;
                function_data.statements.clone()
            };
            let commands = self.transpile_function(&statements);

            let functions = self.functions.read().unwrap();
            let function_data = functions.get(path)?;

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
                .write()
                .unwrap()
                .insert(path.to_string(), function_location);
        }

        let locations = self.function_locations.read().unwrap();
        locations.get(path).map(String::to_owned)
    }

    fn transpile_function(&mut self, statements: &[Statement]) -> Vec<Command> {
        let mut commands = Vec::new();
        for statement in statements {
            commands.extend(self.transpile_statement(statement));
        }
        commands
    }

    fn transpile_statement(&mut self, statement: &Statement) -> Option<Command> {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                Some(literal_command.clean_command().into())
            }
            Statement::Block(_) => {
                unreachable!("Only literal commands are allowed in functions at this time.")
            }
            Statement::Conditional(cond) => self.transpile_conditional(cond),
            Statement::DocComment(doccomment) => {
                let content = doccomment.content();
                Some(Command::Comment(content.to_string()))
            }
            Statement::Grouping(group) => {
                let statements = group.block().statements();
                let commands = statements
                    .iter()
                    .filter_map(|statement| self.transpile_statement(statement))
                    .collect::<Vec<_>>();
                if commands.is_empty() {
                    None
                } else {
                    Some(Command::Group(commands))
                }
            }
            Statement::Semicolon(semi) => match semi.expression() {
                Expression::Primary(primary) => self.transpile_primary_expression(primary),
            },
        }
    }

    fn transpile_conditional(&mut self, cond: &Conditional) -> Option<Command> {
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
                    self.transpile_statement(&statements[0])
                        .map(|cmd| Execute::Run(Box::new(cmd)))
                } else {
                    let commands = statements
                        .iter()
                        .filter_map(|statement| self.transpile_statement(statement))
                        .collect();
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
                let commands = statements
                    .iter()
                    .filter_map(|statement| self.transpile_statement(statement))
                    .collect();
                Execute::Runs(commands)
            } else {
                Execute::Run(Box::new(self.transpile_statement(&statements[0])?))
            };

            Some(Command::Execute(Execute::If(
                datapack::Condition::from(cond),
                Box::new(run),
                el,
            )))
        }
    }

    fn transpile_primary_expression(&mut self, primary: &Primary) -> Option<Command> {
        match primary {
            Primary::FunctionCall(func) => {
                let identifier = func.identifier().span();
                let identifier = identifier.str();
                let location = self.get_or_transpile_function(identifier)?;
                Some(Command::Raw(format!("function {location}")))
            }
        }
    }
}
