//! Transpiler for `Shulkerscript`

use chksum_md5 as md5;
use std::{
    collections::{BTreeMap, HashMap},
    ops::Deref,
    sync::{Arc, OnceLock},
};

use shulkerbox::datapack::{self, Command, Datapack, Execute};

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler,
    },
    lexical::token::KeywordKind,
    semantic::error::{ConflictingFunctionNames, InvalidFunctionArguments, UnexpectedExpression},
    syntax::syntax_tree::{
        declaration::{Declaration, ImportItems},
        expression::{Expression, FunctionCall, Primary},
        program::{Namespace, ProgramFile},
        statement::{
            execute_block::{Conditional, Else, ExecuteBlock, ExecuteBlockHead, ExecuteBlockTail},
            SemicolonStatement, SingleVariableDeclaration, Statement, VariableDeclaration,
        },
        AnnotationAssignment,
    },
    transpile::error::{IllegalAnnotationContent, MissingFunctionDeclaration},
};

use super::{
    error::{TranspileError, TranspileResult},
    variables::{Scope, VariableType},
    FunctionData, TranspileAnnotationValue,
};

/// A transpiler for `Shulkerscript`.
#[derive(Debug)]
pub struct Transpiler {
    datapack: shulkerbox::datapack::Datapack,
    /// Top-level [`Scope`] for each program identifier
    scopes: BTreeMap<String, Arc<Scope<'static>>>,
    /// Key: (program identifier, function name)
    functions: BTreeMap<(String, String), FunctionData>,
    /// Key: alias, Value: target
    aliases: HashMap<(String, String), (String, String)>,
}

impl Transpiler {
    /// Creates a new transpiler.
    #[must_use]
    pub fn new(main_namespace_name: impl Into<String>, pack_format: u8) -> Self {
        Self {
            datapack: shulkerbox::datapack::Datapack::new(main_namespace_name, pack_format),
            scopes: BTreeMap::new(),
            functions: BTreeMap::new(),
            aliases: HashMap::new(),
        }
    }

    /// Consumes the transpiler and returns the resulting datapack.
    #[must_use]
    pub fn into_datapack(self) -> Datapack {
        self.datapack
    }

    /// Transpiles the given programs.
    ///
    /// # Errors
    /// - [`TranspileError::MissingFunctionDeclaration`] If a called function is missing
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn transpile(
        &mut self,
        programs: &[ProgramFile],
        handler: &impl Handler<base::Error>,
    ) -> Result<(), TranspileError> {
        tracing::trace!("Transpiling program declarations");
        for program in programs {
            let program_identifier = program
                .namespace()
                .span()
                .source_file()
                .identifier()
                .clone();
            let scope = self
                .scopes
                .entry(program_identifier)
                .or_default()
                .to_owned();
            self.transpile_program_declarations(program, &scope, handler);
        }

        let mut always_transpile_functions = Vec::new();

        {
            let functions = &mut self.functions;
            for (_, data) in functions.iter() {
                let always_transpile_function = data.annotations.contains_key("tick")
                    || data.annotations.contains_key("load")
                    || data.annotations.contains_key("deobfuscate");
                if always_transpile_function {
                    always_transpile_functions.push(data.identifier_span.clone());
                };
            }
        }

        tracing::trace!(
            "Transpiling functions requested by user: {:?}",
            always_transpile_functions
        );

        for identifier_span in always_transpile_functions {
            let scope = self
                .scopes
                .entry(identifier_span.source_file().identifier().to_owned())
                .or_default()
                .to_owned();
            self.get_or_transpile_function(&identifier_span, None, &scope, handler)?;
        }

        Ok(())
    }

    /// Transpiles the given program.
    fn transpile_program_declarations(
        &mut self,
        program: &ProgramFile,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) {
        let namespace = program.namespace();

        for declaration in program.declarations() {
            self.transpile_declaration(declaration, namespace, scope, handler);
        }
    }

    /// Transpiles the given declaration.
    #[allow(clippy::needless_pass_by_ref_mut)]
    fn transpile_declaration(
        &mut self,
        declaration: &Declaration,
        namespace: &Namespace,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) {
        let program_identifier = declaration.span().source_file().identifier().clone();
        match declaration {
            Declaration::Function(function) => {
                let identifier_span = &function.identifier().span;
                let name = identifier_span.str().to_string();
                let statements = function.block().statements().clone();
                let annotations = function
                    .annotations()
                    .iter()
                    .map(|annotation| {
                        let AnnotationAssignment {
                            identifier: key,
                            value,
                        } = annotation.assignment();
                        (
                            key.span().str().to_string(),
                            TranspileAnnotationValue::from(value.clone()),
                        )
                    })
                    .collect();
                let function_data = FunctionData {
                    namespace: namespace.namespace_name().str_content().to_string(),
                    identifier_span: identifier_span.clone(),
                    parameters: function
                        .parameters()
                        .as_ref()
                        .map(|l| {
                            l.elements()
                                .map(|i| i.span.str().to_string())
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default(),
                    statements,
                    public: function.is_public(),
                    annotations,
                };
                scope.set_variable(
                    &name,
                    VariableType::Function {
                        function_data: function_data.clone(),
                        path: OnceLock::new(),
                    },
                );
                self.functions
                    .insert((program_identifier, name), function_data);
            }
            Declaration::Import(import) => {
                let path = import.module().str_content();
                let import_identifier =
                    super::util::calculate_import_identifier(&program_identifier, path);

                let aliases = &mut self.aliases;

                match import.items() {
                    ImportItems::All(_) => {
                        handler.receive(base::Error::Other(
                            "Importing all items is not yet supported.".to_string(),
                        ));
                    }
                    ImportItems::Named(list) => {
                        for item in list.elements() {
                            let name = item.span.str();
                            aliases.insert(
                                (program_identifier.clone(), name.to_string()),
                                (import_identifier.clone(), name.to_string()),
                            );
                        }
                    }
                }
            }
            Declaration::Tag(tag) => {
                let namespace = self
                    .datapack
                    .namespace_mut(&namespace.namespace_name().str_content());
                let sb_tag = namespace.tag_mut(&tag.name().str_content(), tag.tag_type());

                if let Some(list) = &tag.entries().list {
                    for value in list.elements() {
                        sb_tag.add_value(value.str_content().as_ref().into());
                    }
                }

                if tag.replace().is_some() {
                    sb_tag.set_replace(true);
                }
                // TODO: handle global variables
            }
        };
    }

    /// Gets the function at the given path, or transpiles it if it hasn't been transpiled yet.
    /// Returns the location of the function or None if the function does not exist.
    #[allow(clippy::significant_drop_tightening)]
    #[tracing::instrument(level = "trace", skip(self, handler))]
    fn get_or_transpile_function(
        &mut self,
        identifier_span: &Span,
        arguments: Option<&[&Expression]>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(String, Option<BTreeMap<String, String>>)> {
        let program_identifier = identifier_span.source_file().identifier();
        let program_query = (
            program_identifier.to_string(),
            identifier_span.str().to_string(),
        );
        let alias_query = self.aliases.get(&program_query).cloned();
        let already_transpiled = match scope
            .get_variable(identifier_span.str())
            .expect("called function should be in scope")
            .as_ref()
        {
            VariableType::Function { path, .. } => Some(path.get().is_some()),
            _ => None,
        }
        .expect("called variable should be of type function");

        let function_data = scope
            .get_variable(identifier_span.str())
            .or_else(|| {
                alias_query
                    .clone()
                    .and_then(|(alias_program_identifier, alias_function_name)| {
                        self.scopes
                            .get(&alias_program_identifier)
                            .and_then(|s| s.get_variable(&alias_function_name))
                    })
            })
            .ok_or_else(|| {
                let error = TranspileError::MissingFunctionDeclaration(
                    MissingFunctionDeclaration::from_scope(identifier_span.clone(), scope),
                );
                handler.receive(error.clone());
                error
            })?;

        let VariableType::Function {
            function_data,
            path: function_path,
        } = function_data.as_ref()
        else {
            unreachable!("must be of correct type, otherwise errored out before");
        };

        if !already_transpiled {
            tracing::trace!("Function not transpiled yet, transpiling.");

            let function_scope = Scope::with_parent(scope);

            for (i, param) in function_data.parameters.iter().enumerate() {
                function_scope.set_variable(param, VariableType::FunctionArgument { index: i });
            }

            let statements = function_data.statements.clone();

            let commands =
                self.transpile_function(&statements, program_identifier, &function_scope, handler)?;

            let modified_name = function_data.annotations.get("deobfuscate").map_or_else(
                || {
                    let hash_data = program_identifier.to_string() + "\0" + identifier_span.str();
                    Ok("shu/".to_string() + &md5::hash(hash_data).to_hex_lowercase())
                },
                |val| match val {
                    TranspileAnnotationValue::None => Ok(identifier_span.str().to_string()),
                    TranspileAnnotationValue::Expression(expr) => {
                        expr.comptime_eval().ok_or_else(|| {
                            let err = TranspileError::IllegalAnnotationContent(
                                IllegalAnnotationContent {
                                    annotation: identifier_span.clone(),
                                    message: "Cannot evaluate annotation at compile time"
                                        .to_string(),
                                },
                            );
                            handler.receive(err.clone());
                            err
                        })
                    }
                    TranspileAnnotationValue::Map(_) => {
                        let err =
                            TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                annotation: identifier_span.clone(),
                                message: "Deobfuscate annotation cannot be a map.".to_string(),
                            });
                        handler.receive(err.clone());
                        Err(err)
                    }
                },
            )?;

            let namespace = self.datapack.namespace_mut(&function_data.namespace);

            if namespace.function(&modified_name).is_some() {
                let err = TranspileError::ConflictingFunctionNames(ConflictingFunctionNames {
                    name: modified_name,
                    definition: identifier_span.clone(),
                });
                handler.receive(err.clone());
                return Err(err);
            }

            let function = namespace.function_mut(&modified_name);
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

            function_path.set(function_location.clone()).unwrap();
        }

        let parameters = function_data.parameters.clone();

        let function_location = function_path
            .get()
            .ok_or_else(|| {
                let error = TranspileError::MissingFunctionDeclaration(
                    MissingFunctionDeclaration::from_scope(identifier_span.clone(), scope),
                );
                handler.receive(error.clone());
                error
            })
            .map(String::to_owned)?;

        let arg_count = arguments.map(<[&Expression]>::len);
        if arg_count.is_some_and(|arg_count| arg_count != parameters.len()) {
            let err = TranspileError::InvalidFunctionArguments(InvalidFunctionArguments {
                expected: parameters.len(),
                actual: arg_count.expect("checked in if condition"),
                span: identifier_span.clone(),
            });
            handler.receive(err.clone());
            Err(err)
        } else if arg_count.is_some_and(|arg_count| arg_count > 0) {
            let mut compiled_args = Vec::new();
            let mut errs = Vec::new();
            for expression in arguments.iter().flat_map(|x| x.iter()) {
                let value = match expression {
                    Expression::Primary(Primary::FunctionCall(func)) => self
                        .transpile_function_call(func, scope, handler)
                        .map(|cmd| match cmd {
                            Command::Raw(s) => s,
                            _ => unreachable!("Function call should always return a raw command"),
                        }),
                    Expression::Primary(Primary::Lua(lua)) => {
                        lua.eval_string(handler).map(Option::unwrap_or_default)
                    }
                    Expression::Primary(Primary::Integer(num)) => Ok(num.span.str().to_string()),
                    Expression::Primary(Primary::Boolean(bool)) => Ok(bool.span.str().to_string()),
                    Expression::Primary(Primary::StringLiteral(string)) => {
                        Ok(string.str_content().to_string())
                    }
                    Expression::Primary(Primary::MacroStringLiteral(literal)) => {
                        Ok(literal.str_content())
                    }
                };

                match value {
                    Ok(value) => {
                        compiled_args.push(value);
                    }
                    Err(err) => {
                        compiled_args.push(String::new());
                        errs.push(err.clone());
                    }
                }
            }
            if let Some(err) = errs.first() {
                return Err(err.clone());
            }
            let function_args = parameters.into_iter().zip(compiled_args).collect();
            Ok((function_location, Some(function_args)))
        } else {
            Ok((function_location, None))
        }
    }

    fn transpile_function(
        &mut self,
        statements: &[Statement],
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let mut errors = Vec::new();
        let commands = statements
            .iter()
            .filter_map(|statement| {
                self.transpile_statement(statement, program_identifier, scope, handler)
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
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                Ok(Some(literal_command.clean_command().into()))
            }
            Statement::Run(run) => match run.expression() {
                Expression::Primary(Primary::FunctionCall(func)) => {
                    self.transpile_function_call(func, scope, handler).map(Some)
                }
                Expression::Primary(Primary::Integer(num)) => {
                    let error = TranspileError::UnexpectedExpression(UnexpectedExpression(
                        Expression::Primary(Primary::Integer(num.clone())),
                    ));
                    handler.receive(error.clone());
                    Err(error)
                }
                Expression::Primary(Primary::Boolean(bool)) => {
                    let error = TranspileError::UnexpectedExpression(UnexpectedExpression(
                        Expression::Primary(Primary::Boolean(bool.clone())),
                    ));
                    handler.receive(error.clone());
                    Err(error)
                }
                Expression::Primary(Primary::StringLiteral(string)) => {
                    Ok(Some(Command::Raw(string.str_content().to_string())))
                }
                Expression::Primary(Primary::MacroStringLiteral(string)) => {
                    Ok(Some(Command::UsesMacro(string.into())))
                }
                Expression::Primary(Primary::Lua(code)) => {
                    Ok(code.eval_string(handler)?.map(Command::Raw))
                }
            },
            Statement::Block(_) => {
                unreachable!("Only literal commands are allowed in functions at this time.")
            }
            Statement::ExecuteBlock(execute) => {
                let child_scope = Scope::with_parent(scope);
                self.transpile_execute_block(execute, program_identifier, &child_scope, handler)
            }
            Statement::DocComment(doccomment) => {
                let content = doccomment.content();
                Ok(Some(Command::Comment(content.to_string())))
            }
            Statement::Grouping(group) => {
                let child_scope = Scope::with_parent(scope);
                let statements = group.block().statements();
                let mut errors = Vec::new();
                let commands = statements
                    .iter()
                    .filter_map(|statement| {
                        self.transpile_statement(
                            statement,
                            program_identifier,
                            &child_scope,
                            handler,
                        )
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
            Statement::Semicolon(semi) => match semi.statement() {
                #[expect(clippy::match_wildcard_for_single_variants)]
                SemicolonStatement::Expression(expr) => match expr {
                    Expression::Primary(Primary::FunctionCall(func)) => {
                        self.transpile_function_call(func, scope, handler).map(Some)
                    }
                    unexpected => {
                        let error = TranspileError::UnexpectedExpression(UnexpectedExpression(
                            unexpected.clone(),
                        ));
                        handler.receive(error.clone());
                        Err(error)
                    }
                },
                SemicolonStatement::VariableDeclaration(decl) => {
                    self.transpile_variable_declaration(decl, program_identifier, scope, handler)
                }
            },
        }
    }

    fn transpile_function_call(
        &mut self,
        func: &FunctionCall,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Command> {
        let arguments = func
            .arguments()
            .as_ref()
            .map(|l| l.elements().map(Deref::deref).collect::<Vec<_>>());
        let (location, arguments) = self.get_or_transpile_function(
            &func.identifier().span,
            arguments.as_deref(),
            scope,
            handler,
        )?;
        let mut function_call = format!("function {location}");
        if let Some(arguments) = arguments {
            use std::fmt::Write;
            let arguments = arguments
                .iter()
                .map(|(ident, v)| {
                    format!(
                        r#"{macro_name}:"{escaped}""#,
                        macro_name = crate::util::identifier_to_macro(ident),
                        escaped = crate::util::escape_str(v)
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            write!(function_call, " {{{arguments}}}").unwrap();
        }
        Ok(Command::Raw(function_call))
    }

    fn transpile_variable_declaration(
        &mut self,
        declaration: &VariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        match declaration {
            VariableDeclaration::Single(single) => self.transpile_single_variable_declaration(
                single,
                program_identifier,
                scope,
                handler,
            ),
            _ => todo!("declarations not supported yet: {declaration:?}"),
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_single_variable_declaration(
        &mut self,
        single: &SingleVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        let mut deobfuscate_annotations = single
            .annotations()
            .iter()
            .filter(|a| a.has_identifier("deobfuscate"));

        let variable_type = single.variable_type().keyword;

        let deobfuscate_annotation = deobfuscate_annotations.next();

        if let Some(duplicate) = deobfuscate_annotations.next() {
            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                annotation: duplicate.span(),
                message: "Multiple deobfuscate annotations are not allowed.".to_string(),
            });
            handler.receive(error.clone());
            return Err(error);
        }
        let (name, target) = if let Some(deobfuscate_annotation) = deobfuscate_annotation {
            let deobfuscate_annotation_value =
                TranspileAnnotationValue::from(deobfuscate_annotation.assignment().value.clone());

            if let TranspileAnnotationValue::Map(map) = deobfuscate_annotation_value {
                if map.len() > 2 {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message: "Deobfuscate annotation must have at most 2 key-value pairs."
                                .to_string(),
                        });
                    handler.receive(error.clone());
                    return Err(error);
                }
                if let (Some(name), Some(target)) = (map.get("name"), map.get("target")) {
                    if let (
                        TranspileAnnotationValue::Expression(objective),
                        TranspileAnnotationValue::Expression(target),
                    ) = (name, target)
                    {
                        if let (Some(name_eval), Some(target_eval)) =
                            (objective.comptime_eval(), target.comptime_eval())
                        {
                            // TODO: change invalid criteria if boolean
                            if !crate::util::is_valid_scoreboard_name(&name_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'name' must be a valid scoreboard name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            if !crate::util::is_valid_player_name(&target_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'target' must be a valid player name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            (name_eval, target_eval)
                        } else {
                            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                        annotation: deobfuscate_annotation.span(),
                                        message: "Deobfuscate annotation 'name' or 'target' could not have been evaluated at compile time.".to_string()
                                    });
                            handler.receive(error.clone());
                            return Err(error);
                        }
                    } else {
                        let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                    annotation: deobfuscate_annotation.span(),
                                    message: "Deobfuscate annotation 'name' and 'target' must be compile time expressions.".to_string()
                                });
                        handler.receive(error.clone());
                        return Err(error);
                    }
                } else {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message:
                                "Deobfuscate annotation must have both 'name' and 'target' keys."
                                    .to_string(),
                        });
                    handler.receive(error.clone());
                    return Err(error);
                }
            } else {
                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                    annotation: deobfuscate_annotation.span(),
                    message: "Deobfuscate annotation must be a map.".to_string(),
                });
                handler.receive(error.clone());
                return Err(error);
            }
        } else {
            let name =
                "shu_values_".to_string() + &md5::hash(program_identifier).to_hex_lowercase();
            let target = md5::hash((Arc::as_ptr(scope) as usize).to_le_bytes())
                .to_hex_lowercase()
                .split_off(16);

            (name, target)
        };

        if variable_type == KeywordKind::Int {
            if !self.datapack.scoreboards().contains_key(&name) {
                self.datapack.register_scoreboard(&name, None, None);
            }

            scope.set_variable(
                single.identifier().span.str(),
                VariableType::ScoreboardValue {
                    objective: name.clone(),
                    target,
                },
            );
        } else {
            todo!("implement other variable types")
        }

        Ok(single
            .assignment()
            .is_some()
            .then(|| todo!("transpile assignment")))
    }

    fn transpile_execute_block(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        self.transpile_execute_block_internal(execute, program_identifier, scope, handler)
            .map(|ex| ex.map(Command::Execute))
    }

    fn transpile_execute_block_internal(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Execute>> {
        match execute {
            ExecuteBlock::HeadTail(head, tail) => {
                let tail = match tail {
                    ExecuteBlockTail::Block(block) => {
                        let mut errors = Vec::new();
                        let commands = block
                            .statements()
                            .iter()
                            .filter_map(|s| {
                                self.transpile_statement(s, program_identifier, scope, handler)
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
                            Ok(Some(Execute::Runs(commands)))
                        }
                    }
                    ExecuteBlockTail::ExecuteBlock(_, execute_block) => self
                        .transpile_execute_block_internal(
                            execute_block,
                            program_identifier,
                            scope,
                            handler,
                        ),
                }?;

                self.combine_execute_head_tail(head, tail, program_identifier, scope, handler)
            }
            ExecuteBlock::IfElse(cond, block, el) => {
                let statements = block.statements();
                let then = if statements.is_empty() {
                    Some(Execute::Runs(Vec::new()))
                } else if statements.len() > 1 {
                    let mut errors = Vec::new();
                    let commands = statements
                        .iter()
                        .filter_map(|statement| {
                            self.transpile_statement(statement, program_identifier, scope, handler)
                                .unwrap_or_else(|err| {
                                    errors.push(err);
                                    None
                                })
                        })
                        .collect();
                    if !errors.is_empty() {
                        return Err(errors.remove(0));
                    }
                    Some(Execute::Runs(commands))
                } else {
                    self.transpile_statement(&statements[0], program_identifier, scope, handler)?
                        .map(|cmd| Execute::Run(Box::new(cmd)))
                };

                then.map_or_else(
                    || Ok(None),
                    |then| {
                        self.transpile_conditional(
                            cond,
                            then,
                            Some(el),
                            program_identifier,
                            scope,
                            handler,
                        )
                    },
                )
            }
        }
    }

    fn transpile_conditional(
        &mut self,
        cond: &Conditional,
        then: Execute,
        el: Option<&Else>,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Execute>> {
        let (_, cond) = cond.clone().dissolve();
        let (_, cond, _) = cond.dissolve();
        let mut errors = Vec::new();

        let el = el
            .and_then(|el| {
                let (_, block) = el.clone().dissolve();
                let statements = block.statements();
                if statements.is_empty() {
                    None
                } else if statements.len() == 1 {
                    self.transpile_statement(&statements[0], program_identifier, scope, handler)
                        .unwrap_or_else(|err| {
                            errors.push(err);
                            None
                        })
                        .map(|cmd| Execute::Run(Box::new(cmd)))
                } else {
                    let commands = statements
                        .iter()
                        .filter_map(|statement| {
                            self.transpile_statement(statement, program_identifier, scope, handler)
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

        Ok(Some(Execute::If(
            datapack::Condition::from(cond),
            Box::new(then),
            el,
        )))
    }

    fn combine_execute_head_tail(
        &mut self,
        head: &ExecuteBlockHead,
        tail: Option<Execute>,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Execute>> {
        Ok(match head {
            ExecuteBlockHead::Conditional(cond) => {
                if let Some(tail) = tail {
                    self.transpile_conditional(
                        cond,
                        tail,
                        None,
                        program_identifier,
                        scope,
                        handler,
                    )?
                } else {
                    None
                }
            }
            ExecuteBlockHead::As(r#as) => {
                let selector = r#as.as_selector();
                tail.map(|tail| Execute::As(selector.into(), Box::new(tail)))
            }
            ExecuteBlockHead::At(at) => {
                let selector = at.at_selector();
                tail.map(|tail| Execute::At(selector.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Align(align) => {
                let align = align.align_selector();
                tail.map(|tail| Execute::Align(align.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Anchored(anchored) => {
                let anchor = anchored.anchored_selector();
                tail.map(|tail| Execute::Anchored(anchor.into(), Box::new(tail)))
            }
            ExecuteBlockHead::In(r#in) => {
                let dimension = r#in.in_selector();
                tail.map(|tail| Execute::In(dimension.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Positioned(positioned) => {
                let position = positioned.positioned_selector();
                tail.map(|tail| Execute::Positioned(position.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Rotated(rotated) => {
                let rotation = rotated.rotated_selector();
                tail.map(|tail| Execute::Rotated(rotation.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Facing(facing) => {
                let facing = facing.facing_selector();
                tail.map(|tail| Execute::Facing(facing.into(), Box::new(tail)))
            }
            ExecuteBlockHead::AsAt(as_at) => {
                let selector = as_at.asat_selector();
                tail.map(|tail| Execute::AsAt(selector.into(), Box::new(tail)))
            }
            ExecuteBlockHead::On(on) => {
                let dimension = on.on_selector();
                tail.map(|tail| Execute::On(dimension.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Store(store) => {
                let store = store.store_selector();
                tail.map(|tail| Execute::Store(store.into(), Box::new(tail)))
            }
            ExecuteBlockHead::Summon(summon) => {
                let entity = summon.summon_selector();
                tail.map(|tail| Execute::Summon(entity.into(), Box::new(tail)))
            }
        })
    }
}
