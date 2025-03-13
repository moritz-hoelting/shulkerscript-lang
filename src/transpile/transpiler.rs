//! Transpiler for `Shulkerscript`

use chksum_md5 as md5;
use enum_as_inner::EnumAsInner;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::Deref,
    sync::{Arc, OnceLock},
};

use shulkerbox::{
    datapack::{self, Command, Datapack, Execute},
    util::{MacroString, MacroStringPart},
};

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler,
    },
    semantic::error::{ConflictingFunctionNames, InvalidFunctionArguments, UnexpectedExpression},
    syntax::syntax_tree::{
        declaration::{Declaration, ImportItems},
        expression::{Expression, FunctionCall, Primary},
        program::{Namespace, ProgramFile},
        statement::{
            execute_block::{Conditional, Else, ExecuteBlock, ExecuteBlockHead, ExecuteBlockTail},
            SemicolonStatement, Statement,
        },
        AnnotationAssignment,
    },
    transpile::error::{IllegalAnnotationContent, MissingFunctionDeclaration},
};

use super::{
    error::{MismatchedTypes, TranspileError, TranspileResult, UnknownIdentifier},
    expression::{ComptimeValue, ExpectedType, ExtendedCondition, StorageType},
    variables::{Scope, VariableData},
    FunctionData, TranspileAnnotationValue,
};

/// A transpiler for `Shulkerscript`.
#[derive(Debug)]
pub struct Transpiler {
    pub(super) main_namespace_name: String,
    pub(super) datapack: shulkerbox::datapack::Datapack,
    pub(super) setup_cmds: Vec<Command>,
    pub(super) initialized_constant_scores: HashSet<i64>,
    pub(super) temp_counter: usize,
    /// Top-level [`Scope`] for each program identifier
    scopes: BTreeMap<String, Arc<Scope<'static>>>,
    /// Key: (program identifier, function name)
    functions: BTreeMap<(String, String), FunctionData>,
    /// Key: alias, Value: target
    aliases: HashMap<(String, String), (String, String)>,
}

#[derive(Debug, Clone)]
pub enum TranspiledFunctionArguments {
    None,
    Static(BTreeMap<String, MacroString>),
    Dynamic(Vec<Command>),
}

impl Transpiler {
    /// Creates a new transpiler.
    #[must_use]
    pub fn new(main_namespace_name: impl Into<String>, pack_format: u8) -> Self {
        let main_namespace_name = main_namespace_name.into();
        Self {
            main_namespace_name: main_namespace_name.clone(),
            datapack: shulkerbox::datapack::Datapack::new(main_namespace_name, pack_format),
            setup_cmds: Vec::new(),
            initialized_constant_scores: HashSet::new(),
            temp_counter: 0,
            scopes: BTreeMap::new(),
            functions: BTreeMap::new(),
            aliases: HashMap::new(),
        }
    }

    /// Transpiles the given programs and returns the resulting datapack.
    ///
    /// # Errors
    /// - [`TranspileError::MissingFunctionDeclaration`] If a called function is missing
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn transpile(
        mut self,
        programs: &[ProgramFile],
        handler: &impl Handler<base::Error>,
    ) -> Result<Datapack, TranspileError> {
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

        if !self.setup_cmds.is_empty() {
            let main_namespace = self.datapack.namespace_mut(&self.main_namespace_name);
            let setup_fn = main_namespace.function_mut("shu/setup");
            setup_fn.get_commands_mut().extend(self.setup_cmds.clone());
            // prepend setup function to load tag
            let load_values = self
                .datapack
                .namespace_mut("minecraft")
                .tag_mut("load", datapack::tag::TagType::Function)
                .values_mut();
            load_values.insert(
                0,
                datapack::tag::TagValue::Simple(format!("{}:shu/setup", self.main_namespace_name)),
            );
        }

        Ok(self.datapack)
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
                    VariableData::Function {
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
                    .namespace_mut(namespace.namespace_name().str_content());
                let sb_tag = namespace.tag_mut(tag.name().str_content(), tag.tag_type());

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
    #[tracing::instrument(level = "trace", skip(self, handler))]
    pub(super) fn get_or_transpile_function(
        &mut self,
        identifier_span: &Span,
        arguments: Option<&[&Expression]>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(String, TranspiledFunctionArguments)> {
        let program_identifier = identifier_span.source_file().identifier();
        let program_query = (
            program_identifier.to_string(),
            identifier_span.str().to_string(),
        );
        let alias_query = self.aliases.get(&program_query).cloned();
        let already_transpiled = scope
            .get_variable(identifier_span.str())
            .expect("called function should be in scope")
            .as_ref()
            .as_function()
            .map(|(_, path)| path.get().is_some())
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

        let VariableData::Function {
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
                function_scope.set_variable(
                    param,
                    VariableData::MacroParameter {
                        index: i,
                        macro_name: crate::util::identifier_to_macro(param).to_string(),
                    },
                );
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
                    TranspileAnnotationValue::Expression(expr) => expr
                        .comptime_eval(scope, handler)
                        .and_then(|val| val.to_string_no_macro())
                        .ok_or_else(|| {
                            let err = TranspileError::IllegalAnnotationContent(
                                IllegalAnnotationContent {
                                    annotation: identifier_span.clone(),
                                    message: "Cannot evaluate annotation at compile time"
                                        .to_string(),
                                },
                            );
                            handler.receive(err.clone());
                            err
                        }),
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

        let parameters = &function_data.parameters;

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
                actual: arg_count.unwrap_or_default(),
                span: identifier_span.clone(),
            });
            handler.receive(err.clone());
            Err(err)
        } else if arg_count.is_some_and(|arg_count| arg_count > 0) {
            {
                #[derive(Debug, Clone, EnumAsInner)]
                enum Parameter {
                    Static(MacroString),
                    Dynamic {
                        prepare_cmds: Vec<Command>,
                        storage_name: String,
                        path: String,
                    },
                }

                let mut compiled_args = Vec::new();
                let mut errs = Vec::new();

                for expression in arguments.iter().flat_map(|x| x.iter()) {
                    let value = match expression {
                        Expression::Primary(Primary::Lua(lua)) => {
                            lua.eval_comptime(handler).and_then(|val| match val {
                                Some(ComptimeValue::MacroString(s)) => Ok(Parameter::Static(s)),
                                Some(val) => Ok(Parameter::Static(val.to_string().into())),
                                None => {
                                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                        expression: expression.span(),
                                        expected_type: ExpectedType::String,
                                    });
                                    handler.receive(err.clone());
                                    Err(err)
                                }
                            })
                        }
                        Expression::Primary(Primary::Integer(num)) => {
                            Ok(Parameter::Static(num.span.str().to_string().into()))
                        }
                        Expression::Primary(Primary::Boolean(bool)) => {
                            Ok(Parameter::Static(bool.span.str().to_string().into()))
                        }
                        Expression::Primary(Primary::StringLiteral(string)) => {
                            Ok(Parameter::Static(string.str_content().to_string().into()))
                        }
                        Expression::Primary(Primary::MacroStringLiteral(literal)) => {
                            Ok(Parameter::Static(literal.into()))
                        }
                        Expression::Primary(primary @ Primary::Identifier(ident)) => {
                            let var = scope.get_variable(ident.span.str()).ok_or_else(|| {
                                let err = TranspileError::UnknownIdentifier(UnknownIdentifier {
                                    identifier: ident.span(),
                                });
                                handler.receive(err.clone());
                                err
                            })?;
                            match var.as_ref() {
                                VariableData::MacroParameter { macro_name, .. } => {
                                    Ok(Parameter::Static(MacroString::MacroString(vec![
                                        MacroStringPart::MacroUsage(macro_name.clone()),
                                    ])))
                                }

                                VariableData::BooleanStorage { .. }
                                | VariableData::ScoreboardValue { .. } => {
                                    let (temp_storage, mut temp_path) =
                                        self.get_temp_storage_locations(1);
                                    let prepare_cmds = self.transpile_primary_expression(
                                        primary,
                                        &super::expression::DataLocation::Storage {
                                            storage_name: temp_storage.clone(),
                                            path: temp_path[0].clone(),
                                            r#type: match var.as_ref() {
                                                VariableData::BooleanStorage { .. } => {
                                                    StorageType::Boolean
                                                }
                                                VariableData::ScoreboardValue { .. } => {
                                                    StorageType::Int
                                                }
                                                _ => unreachable!("checked in parent match"),
                                            },
                                        },
                                        scope,
                                        handler,
                                    )?;

                                    Ok(Parameter::Dynamic {
                                        prepare_cmds,
                                        storage_name: temp_storage,
                                        path: std::mem::take(&mut temp_path[0]),
                                    })
                                }
                                _ => todo!("other variable types"),
                            }
                        }
                        Expression::Primary(
                            Primary::Parenthesized(_)
                            | Primary::Prefix(_)
                            | Primary::FunctionCall(_),
                        )
                        | Expression::Binary(_) => {
                            let (temp_storage, mut temp_path) = self.get_temp_storage_locations(1);
                            let prepare_cmds = self.transpile_expression(
                                expression,
                                &super::expression::DataLocation::Storage {
                                    storage_name: temp_storage.clone(),
                                    path: temp_path[0].clone(),
                                    r#type: StorageType::Int,
                                },
                                scope,
                                handler,
                            )?;

                            Ok(Parameter::Dynamic {
                                prepare_cmds,
                                storage_name: temp_storage,
                                path: std::mem::take(&mut temp_path[0]),
                            })
                        }
                    };

                    match value {
                        Ok(value) => {
                            compiled_args.push(value);
                        }
                        Err(err) => {
                            compiled_args
                                .push(Parameter::Static(MacroString::String(String::new())));
                            errs.push(err.clone());
                        }
                    }
                }
                if let Some(err) = errs.first() {
                    return Err(err.clone());
                }
                if compiled_args.iter().any(|arg| !arg.is_static()) {
                    let (mut setup_cmds, move_cmds, static_params) = parameters.clone().into_iter().zip(compiled_args).fold(
                        (Vec::new(), Vec::new(), BTreeMap::new()),
                        |(mut acc_setup, mut acc_move, mut statics), (arg_name, data)| {
                            let arg_name = crate::util::identifier_to_macro(&arg_name);
                            match data {
                            Parameter::Static(s) => {
                                match s {
                                    MacroString::String(value) => statics.insert(arg_name.to_string(), MacroString::String(crate::util::escape_str(&value).to_string())),
                                    MacroString::MacroString(parts) => {
                                        let parts = parts.into_iter().map(|part| {
                                            match part {
                                                MacroStringPart::String(s) => MacroStringPart::String(crate::util::escape_str(&s).to_string()),
                                                MacroStringPart::MacroUsage(m) => MacroStringPart::MacroUsage(m),
                                            }
                                        }).collect();
                                        statics.insert(arg_name.to_string(), MacroString::MacroString(parts))
                                    }
                                };
                            }
                            Parameter::Dynamic { prepare_cmds, storage_name, path } => {
                                acc_setup.extend(prepare_cmds);
                                acc_move.push(Command::Raw(format!(r#"data modify storage shulkerscript:function_arguments {arg_name} set from storage {storage_name} {path}"#)));
                            }
                        }
                        (acc_setup, acc_move, statics)},
                    );
                    let statics_len = static_params.len();
                    let joined_statics =
                        super::util::join_macro_strings(static_params.into_iter().enumerate().map(
                            |(i, (k, v))| match v {
                                MacroString::String(s) => {
                                    let mut s = format!(r#"{k}:"{s}""#);
                                    if i < statics_len - 1 {
                                        s.push(',');
                                    }
                                    MacroString::String(s)
                                }
                                MacroString::MacroString(mut parts) => {
                                    parts.insert(0, MacroStringPart::String(format!(r#"{k}:""#)));
                                    let mut ending = '"'.to_string();
                                    if i < statics_len - 1 {
                                        ending.push(',');
                                    }
                                    parts.push(MacroStringPart::String(ending));
                                    MacroString::MacroString(parts)
                                }
                            },
                        ));
                    let statics_cmd = match joined_statics {
                        MacroString::String(s) => Command::Raw(format!(
                            r#"data merge storage shulkerscript:function_arguments {{{s}}}"#
                        )),
                        MacroString::MacroString(_) => {
                            Command::UsesMacro(super::util::join_macro_strings([
                                MacroString::String(
                                    "data merge storage shulkerscript:function_arguments {"
                                        .to_string(),
                                ),
                                joined_statics,
                                MacroString::String("}".to_string()),
                            ]))
                        }
                    };
                    setup_cmds.push(statics_cmd);
                    setup_cmds.extend(move_cmds);

                    Ok((
                        function_location,
                        TranspiledFunctionArguments::Dynamic(setup_cmds),
                    ))
                } else {
                    let function_args = parameters
                        .clone()
                        .into_iter()
                        .zip(
                            compiled_args
                                .into_iter()
                                .map(|arg| arg.into_static().expect("checked in if condition")),
                        )
                        .collect();
                    Ok((
                        function_location,
                        TranspiledFunctionArguments::Static(function_args),
                    ))
                }
            }
        } else {
            Ok((function_location, TranspiledFunctionArguments::None))
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
            .flat_map(|statement| {
                self.transpile_statement(statement, program_identifier, scope, handler)
                    .unwrap_or_else(|err| {
                        errors.push(err);
                        Vec::new()
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
    ) -> TranspileResult<Vec<Command>> {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                Ok(vec![literal_command.clean_command().into()])
            }
            Statement::Run(run) => {
                self.transpile_run_expression(run.expression(), program_identifier, scope, handler)
            }
            Statement::Block(_) => {
                unreachable!("Only literal commands are allowed in functions at this time.")
            }
            Statement::ExecuteBlock(execute) => {
                let child_scope = Scope::with_parent(scope);
                Ok(self.transpile_execute_block(
                    execute,
                    program_identifier,
                    &child_scope,
                    handler,
                )?)
            }
            Statement::DocComment(doccomment) => {
                let content = doccomment.content();
                Ok(vec![Command::Comment(content.to_string())])
            }
            Statement::Grouping(group) => {
                let child_scope = Scope::with_parent(scope);
                let statements = group.block().statements();
                let mut errors = Vec::new();
                let commands = statements
                    .iter()
                    .flat_map(|statement| {
                        self.transpile_statement(
                            statement,
                            program_identifier,
                            &child_scope,
                            handler,
                        )
                        .unwrap_or_else(|err| {
                            errors.push(err);
                            Vec::new()
                        })
                    })
                    .collect::<Vec<_>>();
                if !errors.is_empty() {
                    return Err(errors.remove(0));
                }
                if commands.is_empty() {
                    Ok(Vec::new())
                } else {
                    Ok(vec![Command::Group(commands)])
                }
            }
            Statement::Semicolon(semi) => match semi.statement() {
                SemicolonStatement::Expression(expr) => match expr {
                    Expression::Primary(Primary::FunctionCall(func)) => {
                        self.transpile_function_call(func, scope, handler)
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
                SemicolonStatement::Assignment(assignment) => self.transpile_assignment(
                    assignment.identifier(),
                    assignment.expression(),
                    scope,
                    handler,
                ),
            },
        }
    }

    #[expect(clippy::only_used_in_recursion)]
    fn transpile_run_expression(
        &mut self,
        expression: &Expression,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match expression {
            Expression::Primary(Primary::FunctionCall(func)) => {
                self.transpile_function_call(func, scope, handler)
            }
            expression @ Expression::Primary(
                Primary::Integer(_)
                | Primary::Boolean(_)
                | Primary::Prefix(_)
                | Primary::Identifier(_),
            ) => {
                let error =
                    TranspileError::UnexpectedExpression(UnexpectedExpression(expression.clone()));
                handler.receive(error.clone());
                Err(error)
            }
            Expression::Primary(Primary::StringLiteral(string)) => {
                Ok(vec![Command::Raw(string.str_content().to_string())])
            }
            Expression::Primary(Primary::MacroStringLiteral(string)) => {
                Ok(vec![Command::UsesMacro(string.into())])
            }
            Expression::Primary(Primary::Lua(code)) => match code.eval_comptime(handler)? {
                Some(ComptimeValue::String(cmd)) => Ok(vec![Command::Raw(cmd)]),
                Some(ComptimeValue::MacroString(cmd)) => Ok(vec![Command::UsesMacro(cmd)]),
                Some(ComptimeValue::Boolean(_) | ComptimeValue::Integer(_)) => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::String,
                        expression: code.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
                None => Ok(Vec::new()),
            },

            Expression::Primary(Primary::Parenthesized(parenthesized)) => self
                .transpile_run_expression(
                    parenthesized.expression(),
                    program_identifier,
                    scope,
                    handler,
                ),
            Expression::Binary(bin) => match bin.comptime_eval(scope, handler) {
                Some(ComptimeValue::String(cmd)) => Ok(vec![Command::Raw(cmd)]),
                Some(ComptimeValue::MacroString(cmd)) => Ok(vec![Command::UsesMacro(cmd)]),
                _ => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expression: bin.span(),
                        expected_type: ExpectedType::String,
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            },
        }
    }

    pub(super) fn transpile_function_call(
        &mut self,
        func: &FunctionCall,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
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
        match arguments {
            TranspiledFunctionArguments::Static(arguments) => {
                use std::fmt::Write;
                let arguments_iter = arguments.iter().map(|(ident, v)| match v {
                    MacroString::String(s) => MacroString::String(format!(
                        r#"{macro_name}:"{escaped}""#,
                        macro_name = crate::util::identifier_to_macro(ident),
                        escaped = crate::util::escape_str(s)
                    )),
                    MacroString::MacroString(parts) => MacroString::MacroString(
                        std::iter::once(MacroStringPart::String(format!(
                            r#"{macro_name}:""#,
                            macro_name = crate::util::identifier_to_macro(ident)
                        )))
                        .chain(parts.clone().into_iter().map(|part| match part {
                            MacroStringPart::String(s) => {
                                MacroStringPart::String(crate::util::escape_str(&s).to_string())
                            }
                            macro_usage @ MacroStringPart::MacroUsage(_) => macro_usage,
                        }))
                        .chain(std::iter::once(MacroStringPart::String('"'.to_string())))
                        .collect(),
                    ),
                });
                let arguments = super::util::join_macro_strings(arguments_iter);

                let cmd = match arguments {
                    MacroString::String(arguments) => {
                        write!(function_call, " {{{arguments}}}").unwrap();
                        Command::Raw(function_call)
                    }
                    MacroString::MacroString(mut parts) => {
                        function_call.push_str(" {");
                        parts.insert(0, MacroStringPart::String(function_call));
                        parts.push(MacroStringPart::String('}'.to_string()));
                        Command::UsesMacro(MacroString::MacroString(parts))
                    }
                };

                Ok(vec![cmd])
            }
            TranspiledFunctionArguments::Dynamic(mut cmds) => {
                function_call.push_str(" with storage shulkerscript:function_arguments");
                cmds.push(Command::Raw(function_call));
                Ok(cmds)
            }
            TranspiledFunctionArguments::None => Ok(vec![Command::Raw(function_call)]),
        }
    }

    fn transpile_execute_block(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        self.transpile_execute_block_internal(execute, program_identifier, scope, handler)
            .map(|ex| {
                ex.map(|(mut pre_cmds, exec)| {
                    pre_cmds.push(exec.into());
                    pre_cmds
                })
                .unwrap_or_default()
            })
    }

    fn transpile_execute_block_internal(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<(Vec<Command>, Execute)>> {
        match execute {
            ExecuteBlock::HeadTail(head, tail) => {
                let tail = match tail {
                    ExecuteBlockTail::Block(block) => {
                        let mut errors = Vec::new();
                        let commands = block
                            .statements()
                            .iter()
                            .flat_map(|s| {
                                self.transpile_statement(s, program_identifier, scope, handler)
                                    .unwrap_or_else(|err| {
                                        errors.push(err);
                                        Vec::new()
                                    })
                            })
                            .collect::<Vec<_>>();

                        if !errors.is_empty() {
                            return Err(errors.remove(0));
                        }
                        if commands.is_empty() {
                            Ok(None)
                        } else {
                            Ok(Some((Vec::new(), Execute::Runs(commands))))
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
                        .flat_map(|statement| {
                            self.transpile_statement(statement, program_identifier, scope, handler)
                                .unwrap_or_else(|err| {
                                    errors.push(err);
                                    Vec::new()
                                })
                        })
                        .collect();
                    if !errors.is_empty() {
                        return Err(errors.remove(0));
                    }
                    Some(Execute::Runs(commands))
                } else {
                    let cmds = self.transpile_statement(
                        &statements[0],
                        program_identifier,
                        scope,
                        handler,
                    )?;
                    if cmds.len() > 1 {
                        Some(Execute::Runs(cmds))
                    } else {
                        cmds.into_iter()
                            .next()
                            .map(|cmd| Execute::Run(Box::new(cmd)))
                    }
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
    ) -> TranspileResult<Option<(Vec<Command>, Execute)>> {
        let cond_expression = cond.condition().expression().as_ref();

        let mut errors = Vec::new();

        let el = el.and_then(|el| {
            let (_, block) = el.clone().dissolve();
            let statements = block.statements();
            let cmds = statements
                .iter()
                .flat_map(|statement| {
                    self.transpile_statement(statement, program_identifier, scope, handler)
                        .unwrap_or_else(|err| {
                            errors.push(err);
                            Vec::new()
                        })
                })
                .collect::<Vec<_>>();

            match cmds.len() {
                0 => None,
                1 => Some(Execute::Run(Box::new(
                    cmds.into_iter().next().expect("length is 1"),
                ))),
                _ => Some(Execute::Runs(cmds)),
            }
        });

        if let Some(ComptimeValue::Boolean(value)) = cond_expression.comptime_eval(scope, handler) {
            if value {
                Ok(Some((Vec::new(), then)))
            } else {
                Ok(el.map(|el| (Vec::new(), el)))
            }
        } else {
            if !errors.is_empty() {
                return Err(errors.remove(0));
            }

            let (pre_cond_cmds, cond) =
                self.transpile_expression_as_condition(cond_expression, scope, handler)?;

            match cond {
                ExtendedCondition::Runtime(cond) => Ok(Some((
                    pre_cond_cmds,
                    Execute::If(cond, Box::new(then), el.map(Box::new)),
                ))),
                ExtendedCondition::Comptime(cond) => {
                    if cond {
                        Ok(Some((Vec::new(), then)))
                    } else {
                        Ok(el.map(|el| (Vec::new(), el)))
                    }
                }
            }
        }
    }

    fn combine_execute_head_tail(
        &mut self,
        head: &ExecuteBlockHead,
        tail: Option<(Vec<Command>, Execute)>,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<(Vec<Command>, Execute)>> {
        Ok(match head {
            ExecuteBlockHead::Conditional(cond) => {
                if let Some((mut pre_cmds, tail)) = tail {
                    self.transpile_conditional(
                        cond,
                        tail,
                        None,
                        program_identifier,
                        scope,
                        handler,
                    )?
                    .map(|(pre_cond_cmds, cond)| {
                        pre_cmds.extend(pre_cond_cmds);
                        (pre_cmds, cond)
                    })
                } else {
                    None
                }
            }
            ExecuteBlockHead::As(r#as) => {
                let selector = r#as.as_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::As(selector.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::At(at) => {
                let selector = at.at_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::At(selector.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Align(align) => {
                let align = align.align_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Align(align.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Anchored(anchored) => {
                let anchor = anchored.anchored_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Anchored(anchor.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::In(r#in) => {
                let dimension = r#in.in_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::In(dimension.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Positioned(positioned) => {
                let position = positioned.positioned_selector();
                tail.map(|(pre_cmds, tail)| {
                    (
                        pre_cmds,
                        Execute::Positioned(position.into(), Box::new(tail)),
                    )
                })
            }
            ExecuteBlockHead::Rotated(rotated) => {
                let rotation = rotated.rotated_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Rotated(rotation.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Facing(facing) => {
                let facing = facing.facing_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Facing(facing.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::AsAt(as_at) => {
                let selector = as_at.asat_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::AsAt(selector.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::On(on) => {
                let dimension = on.on_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::On(dimension.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Store(store) => {
                let store = store.store_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Store(store.into(), Box::new(tail)))
                })
            }
            ExecuteBlockHead::Summon(summon) => {
                let entity = summon.summon_selector();
                tail.map(|(pre_cmds, tail)| {
                    (pre_cmds, Execute::Summon(entity.into(), Box::new(tail)))
                })
            }
        })
    }
}
