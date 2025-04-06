//! Transpiler for `Shulkerscript`

use std::{
    collections::{BTreeMap, HashSet},
    ops::Deref,
    sync::{Arc, OnceLock},
};

use shulkerbox::datapack::{self, Command, Datapack, Execute};

use crate::{
    base::{self, source_file::SourceElement, Handler},
    semantic::error::UnexpectedExpression,
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
    transpile::util::{MacroString, MacroStringPart},
};

use super::{
    error::{MismatchedTypes, MissingValue, TranspileError, TranspileResult, UnknownIdentifier},
    expression::{ComptimeValue, ExpectedType, ExtendedCondition},
    variables::{Scope, TranspileAssignmentTarget, VariableData},
    FunctionData, TranspileAnnotationValue, TranspiledFunctionArguments,
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
    pub(super) scopes: BTreeMap<String, Arc<Scope>>,
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

        let mut aliases = Vec::new();

        for program in programs {
            let mut program_aliases = BTreeMap::new();
            let program_identifier = program
                .namespace()
                .span()
                .source_file()
                .identifier()
                .clone();
            let scope = self
                .scopes
                .entry(program_identifier.clone())
                .or_insert_with(Scope::with_internal_functions)
                .to_owned();
            self.transpile_program_declarations(program, &mut program_aliases, &scope, handler)?;
            aliases.push((scope.clone(), program_aliases));
        }

        for (scope, program_aliases) in aliases {
            for (alias_name, (alias_program_identifier, actual_name)) in program_aliases {
                if let Some(alias_scope) = self.scopes.get(&alias_program_identifier) {
                    if let Some(var_data) = alias_scope.get_variable(&actual_name) {
                        scope.set_arc_variable(&alias_name, var_data);
                    } else {
                        tracing::error!(
                            "Importing a non-existent variable: {} from {}",
                            actual_name,
                            alias_program_identifier
                        );
                    }
                } else {
                    tracing::error!(
                        "Importing from a non-existent program: {}",
                        alias_program_identifier
                    );
                }
            }
        }

        let mut always_transpile_functions = Vec::new();

        {
            let functions = self.scopes.iter().flat_map(|(_, scope)| {
                scope
                    .get_local_variables()
                    .read()
                    .unwrap()
                    .values()
                    .filter_map(|data| data.as_function().map(|(data, _, _)| data))
                    .cloned()
                    .collect::<Vec<_>>()
            });
            for data in functions {
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
                .or_insert_with(Scope::with_internal_functions)
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
        program_aliases: &mut BTreeMap<String, (String, String)>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<()> {
        let namespace = program.namespace();

        for declaration in program.declarations() {
            self.transpile_declaration(declaration, namespace, program_aliases, scope, handler)?;
        }

        Ok(())
    }

    /// Transpiles the given declaration.
    fn transpile_declaration(
        &mut self,
        declaration: &Declaration,
        namespace: &Namespace,
        program_aliases: &mut BTreeMap<String, (String, String)>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<()> {
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
                        .map(|l| l.elements().cloned().collect::<Vec<_>>())
                        .unwrap_or_default(),
                    statements,
                    public: function.is_public(),
                    annotations,
                };
                scope.set_variable(
                    &name,
                    VariableData::Function {
                        function_data,
                        path: OnceLock::new(),
                        function_scope: scope.clone(),
                    },
                );
            }
            Declaration::Import(import) => {
                let path = import.module().str_content();
                let import_identifier =
                    super::util::calculate_import_identifier(&program_identifier, path);

                // let aliases = &mut self.aliases;

                match import.items() {
                    ImportItems::All(_) => {
                        handler.receive(base::Error::Other(
                            "Importing all items is not yet supported.".to_string(),
                        ));
                    }
                    ImportItems::Named(list) => {
                        for item in list.elements() {
                            let name = item.span.str();
                            program_aliases.insert(
                                name.to_string(),
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
            }
            Declaration::GlobalVariable((_, declaration, _)) => {
                let setup_variable_cmds = self.transpile_variable_declaration(
                    declaration,
                    true,
                    &program_identifier,
                    scope,
                    handler,
                )?;

                self.setup_cmds.extend(setup_variable_cmds);
            }
        };

        Ok(())
    }

    pub(super) fn transpile_statement(
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
                let child_scope = Scope::with_parent(scope.clone());
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
                let child_scope = Scope::with_parent(scope.clone());
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
                SemicolonStatement::VariableDeclaration(decl) => self
                    .transpile_variable_declaration(
                        decl,
                        false,
                        program_identifier,
                        scope,
                        handler,
                    ),
                SemicolonStatement::Assignment(assignment) => self.transpile_assignment(
                    TranspileAssignmentTarget::from(assignment.destination()),
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
            Expression::Primary(Primary::Identifier(ident)) => {
                match scope.get_variable(ident.span.str()).as_deref() {
                    Some(VariableData::ComptimeValue {
                        value,
                        read_only: _,
                    }) => value.read().unwrap().as_ref().map_or_else(
                        || {
                            let error = TranspileError::MissingValue(MissingValue {
                                expression: ident.span.clone(),
                            });
                            handler.receive(error.clone());
                            Err(error)
                        },
                        |val| {
                            let cmd = val.to_string_no_macro().map_or_else(
                                || Command::UsesMacro(val.to_macro_string().into()),
                                Command::Raw,
                            );
                            Ok(vec![cmd])
                        },
                    ),
                    Some(_) => {
                        let error = TranspileError::UnexpectedExpression(UnexpectedExpression(
                            expression.clone(),
                        ));
                        handler.receive(error.clone());
                        Err(error)
                    }
                    None => {
                        let error = TranspileError::UnknownIdentifier(UnknownIdentifier {
                            identifier: ident.span.clone(),
                        });
                        handler.receive(error.clone());
                        Err(error)
                    }
                }
            }
            expression @ Expression::Primary(
                Primary::Integer(_)
                | Primary::Boolean(_)
                | Primary::Prefix(_)
                | Primary::Indexed(_),
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
            Expression::Primary(Primary::Lua(code)) => match code.eval_comptime(scope, handler)? {
                Ok(ComptimeValue::String(cmd)) => Ok(vec![Command::Raw(cmd)]),
                Ok(ComptimeValue::MacroString(cmd)) => Ok(vec![Command::UsesMacro(cmd.into())]),
                Ok(ComptimeValue::Boolean(_) | ComptimeValue::Integer(_)) => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::String,
                        expression: code.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
                Err(_) => {
                    let err = TranspileError::MissingValue(MissingValue {
                        expression: code.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            },

            Expression::Primary(Primary::Parenthesized(parenthesized)) => self
                .transpile_run_expression(
                    parenthesized.expression(),
                    program_identifier,
                    scope,
                    handler,
                ),
            Expression::Binary(bin) => match bin.comptime_eval(scope, handler) {
                Ok(ComptimeValue::String(cmd)) => Ok(vec![Command::Raw(cmd)]),
                Ok(ComptimeValue::MacroString(cmd)) => Ok(vec![Command::UsesMacro(cmd.into())]),
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
        if let Some(VariableData::InternalFunction { implementation }) =
            scope.get_variable(func.identifier().span.str()).as_deref()
        {
            implementation(self, scope, func).inspect_err(|err| {
                handler.receive(err.clone());
            })
        } else {
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
                            Command::UsesMacro(MacroString::MacroString(parts).into())
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

        if let Ok(ComptimeValue::Boolean(value)) = cond_expression.comptime_eval(scope, handler) {
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
