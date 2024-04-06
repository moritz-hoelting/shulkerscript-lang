//! Transpiler for `ShulkerScript`

use chksum_md5 as md5;
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

use super::error::{TranspileError, TranspileResult};

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
        handler: &impl Handler<TranspileError>,
    ) -> Result<(), TranspileError> {
        for declaration in program.declarations() {
            self.transpile_declaration(declaration, handler);
        }

        let mut always_transpile_functions = Vec::new();

        #[allow(clippy::significant_drop_in_scrutinee)]
        {
            let functions = self.functions.read().unwrap();
            for (name, data) in functions.iter() {
                let always_transpile_function = data.annotations.contains_key("tick")
                    || data.annotations.contains_key("load")
                    || data.annotations.contains_key("deobfuscate");
                if always_transpile_function {
                    always_transpile_functions.push(name.clone());
                };
            }
        }

        for name in always_transpile_functions {
            self.get_or_transpile_function(&name, handler)?;
        }

        Ok(())
    }

    /// Transpiles the given declaration.
    fn transpile_declaration(
        &mut self,
        declaration: &Declaration,
        _handler: &impl Handler<TranspileError>,
    ) {
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
    fn get_or_transpile_function(
        &mut self,
        name: &str,
        handler: &impl Handler<TranspileError>,
    ) -> TranspileResult<String> {
        let already_transpiled = {
            let locations = self.function_locations.read().unwrap();
            locations.get(name).is_some()
        };
        if !already_transpiled {
            let statements = {
                let functions = self.functions.read().unwrap();
                let function_data = functions.get(name).ok_or_else(|| {
                    let error = TranspileError::MissingFunctionDeclaration(name.to_string());
                    handler.receive(error.clone());
                    error
                })?;
                function_data.statements.clone()
            };
            let commands = self.transpile_function(&statements, handler)?;

            let functions = self.functions.read().unwrap();
            let function_data = functions.get(name).ok_or_else(|| {
                let error = TranspileError::MissingFunctionDeclaration(name.to_string());
                handler.receive(error.clone());
                error
            })?;

            let modified_name = function_data
                .annotations
                .get("deobfuscate")
                .map_or_else(
                    || Some("shu/".to_string() + &md5::hash(name).to_hex_lowercase()[..16]),
                    Clone::clone,
                )
                .unwrap_or_else(|| name.to_string());

            let function = self
                .datapack
                .namespace_mut(&function_data.namespace)
                .function_mut(&modified_name);
            function.get_commands_mut().extend(commands);

            let function_location = format!(
                "{namespace}:{modified_name}",
                namespace = function_data.namespace
            );

            if function_data.annotations.contains_key("tick") {
                self.datapack.add_tick(&function_location);
            }
            if function_data.annotations.contains_key("load") {
                self.datapack.add_load(&function_location);
            }

            self.function_locations
                .write()
                .unwrap()
                .insert(name.to_string(), function_location);
        }

        let locations = self.function_locations.read().unwrap();
        locations
            .get(name)
            .ok_or_else(|| {
                let error = TranspileError::MissingFunctionDeclaration(name.to_string());
                handler.receive(error.clone());
                error
            })
            .map(String::to_owned)
    }

    fn transpile_function(
        &mut self,
        statements: &[Statement],
        handler: &impl Handler<TranspileError>,
    ) -> TranspileResult<Vec<Command>> {
        let mut errors = Vec::new();
        let commands = statements
            .iter()
            .filter_map(|statement| {
                self.transpile_statement(statement, handler)
                    .unwrap_or_else(|err| {
                        errors.push(err);
                        None
                    })
            })
            .collect();

        if !errors.is_empty() {
            return Err(errors.remove(0));
        }

        Ok(commands)
    }

    fn transpile_statement(
        &mut self,
        statement: &Statement,
        handler: &impl Handler<TranspileError>,
    ) -> TranspileResult<Option<Command>> {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                Ok(Some(literal_command.clean_command().into()))
            }
            Statement::Block(_) => {
                unreachable!("Only literal commands are allowed in functions at this time.")
            }
            Statement::Conditional(cond) => self.transpile_conditional(cond, handler),
            Statement::DocComment(doccomment) => {
                let content = doccomment.content();
                Ok(Some(Command::Comment(content.to_string())))
            }
            Statement::Grouping(group) => {
                let statements = group.block().statements();
                let mut errors = Vec::new();
                let commands = statements
                    .iter()
                    .filter_map(|statement| {
                        self.transpile_statement(statement, handler)
                            .unwrap_or_else(|err| {
                                errors.push(err);
                                None
                            })
                    })
                    .collect::<Vec<_>>();
                if !errors.is_empty() {
                    return Err(errors.remove(0));
                }
                if commands.is_empty() {
                    Ok(None)
                } else {
                    Ok(Some(Command::Group(commands)))
                }
            }
            Statement::Semicolon(semi) => match semi.expression() {
                Expression::Primary(primary) => self
                    .transpile_primary_expression(primary, handler)
                    .map(Some),
            },
        }
    }

    fn transpile_conditional(
        &mut self,
        cond: &Conditional,
        handler: &impl Handler<TranspileError>,
    ) -> TranspileResult<Option<Command>> {
        let (_, cond, block, el) = cond.clone().dissolve();
        let (_, cond, _) = cond.dissolve();
        let statements = block.statements();
        let mut errors = Vec::new();

        let el = el
            .and_then(|el| {
                let (_, block) = el.dissolve();
                let statements = block.statements();
                if statements.is_empty() {
                    None
                } else if statements.len() == 1 {
                    self.transpile_statement(&statements[0], handler)
                        .unwrap_or_else(|err| {
                            errors.push(err);
                            None
                        })
                        .map(|cmd| Execute::Run(Box::new(cmd)))
                } else {
                    let commands = statements
                        .iter()
                        .filter_map(|statement| {
                            self.transpile_statement(statement, handler)
                                .unwrap_or_else(|err| {
                                    errors.push(err);
                                    None
                                })
                        })
                        .collect();
                    Some(Execute::Runs(commands))
                }
            })
            .map(Box::new);

        if !errors.is_empty() {
            return Err(errors.remove(0));
        }

        if statements.is_empty() {
            if el.is_none() {
                Ok(None)
            } else {
                Ok(Some(Command::Execute(Execute::If(
                    datapack::Condition::from(cond),
                    Box::new(Execute::Runs(Vec::new())),
                    el,
                ))))
            }
        } else {
            let run = if statements.len() > 1 {
                let commands = statements
                    .iter()
                    .filter_map(|statement| {
                        self.transpile_statement(statement, handler)
                            .unwrap_or_else(|err| {
                                errors.push(err);
                                None
                            })
                    })
                    .collect();
                Some(Execute::Runs(commands))
            } else {
                self.transpile_statement(&statements[0], handler)?
                    .map(|cmd| Execute::Run(Box::new(cmd)))
            };

            Ok(run.map(|run| {
                Command::Execute(Execute::If(
                    datapack::Condition::from(cond),
                    Box::new(run),
                    el,
                ))
            }))
        }
    }

    fn transpile_primary_expression(
        &mut self,
        primary: &Primary,
        handler: &impl Handler<TranspileError>,
    ) -> TranspileResult<Command> {
        match primary {
            Primary::FunctionCall(func) => {
                let identifier = func.identifier().span();
                let identifier_name = identifier.str();
                let location = self.get_or_transpile_function(identifier_name, handler)?;
                Ok(Command::Raw(format!("function {location}")))
            }
        }
    }
}
