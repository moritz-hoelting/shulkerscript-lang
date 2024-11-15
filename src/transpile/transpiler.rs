//! Transpiler for `Shulkerscript`

use chksum_md5 as md5;
use std::{
    collections::{BTreeMap, HashMap},
    sync::RwLock,
};

use shulkerbox::datapack::{self, Command, Datapack, Execute};

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
            Statement,
        },
    },
    transpile::error::TranspileMissingFunctionDeclaration,
};

use super::error::{TranspileError, TranspileResult};

/// A transpiler for `Shulkerscript`.
#[derive(Debug)]
pub struct Transpiler {
    datapack: shulkerbox::datapack::Datapack,
    /// Key: (program identifier, function name)
    functions: RwLock<BTreeMap<(String, String), FunctionData>>,
    function_locations: RwLock<HashMap<(String, String), (String, bool)>>,
    aliases: RwLock<HashMap<(String, String), (String, String)>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FunctionData {
    pub(super) namespace: String,
    pub(super) identifier_span: Span,
    pub(super) parameters: Vec<String>,
    pub(super) statements: Vec<Statement>,
    pub(super) public: bool,
    pub(super) annotations: HashMap<String, Option<String>>,
}

impl Transpiler {
    /// Creates a new transpiler.
    #[must_use]
    pub fn new(pack_format: u8) -> Self {
        Self {
            datapack: shulkerbox::datapack::Datapack::new(pack_format),
            functions: RwLock::new(BTreeMap::new()),
            function_locations: RwLock::new(HashMap::new()),
            aliases: RwLock::new(HashMap::new()),
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
            self.transpile_program_declarations(program, handler);
        }

        let mut always_transpile_functions = Vec::new();

        {
            let functions = self.functions.read().unwrap();
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
            self.get_or_transpile_function(&identifier_span, None, handler)?;
        }

        Ok(())
    }

    /// Transpiles the given program.
    fn transpile_program_declarations(
        &mut self,
        program: &ProgramFile,
        handler: &impl Handler<base::Error>,
    ) {
        let namespace = program.namespace();

        for declaration in program.declarations() {
            self.transpile_declaration(declaration, namespace, handler);
        }
    }

    /// Transpiles the given declaration.
    #[allow(clippy::needless_pass_by_ref_mut)]
    fn transpile_declaration(
        &mut self,
        declaration: &Declaration,
        namespace: &Namespace,
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
                        let key = annotation.identifier();
                        let value = annotation.value();
                        (
                            key.span().str().to_string(),
                            value.as_ref().map(|(_, ref v)| v.str_content().to_string()),
                        )
                    })
                    .collect();
                #[allow(clippy::significant_drop_tightening)]
                self.functions.write().unwrap().insert(
                    (program_identifier, name),
                    FunctionData {
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
                    },
                );
            }
            Declaration::Import(import) => {
                let path = import.module().str_content();
                let import_identifier =
                    super::util::calculate_import_identifier(&program_identifier, path);

                let mut aliases = self.aliases.write().unwrap();

                match import.items() {
                    ImportItems::All(_) => {
                        handler.receive(base::Error::Other(
                            "Importing all items is not yet supported.".to_string(),
                        ));
                    }
                    ImportItems::Named(list) => {
                        let items = list.elements();

                        for item in items {
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
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(String, Option<BTreeMap<String, String>>)> {
        let program_identifier = identifier_span.source_file().identifier();
        let program_query = (
            program_identifier.to_string(),
            identifier_span.str().to_string(),
        );
        let alias_query = {
            let aliases = self.aliases.read().unwrap();
            aliases.get(&program_query).cloned()
        };
        let already_transpiled = {
            let locations = self.function_locations.read().unwrap();
            locations
                .get(&program_query)
                .or_else(|| {
                    alias_query
                        .clone()
                        .and_then(|q| locations.get(&q).filter(|(_, p)| *p))
                })
                .is_some()
        };
        if !already_transpiled {
            tracing::trace!("Function not transpiled yet, transpiling.");

            let statements = {
                let functions = self.functions.read().unwrap();
                let function_data = functions
                    .get(&program_query)
                    .or_else(|| {
                        alias_query
                            .clone()
                            .and_then(|q| functions.get(&q).filter(|f| f.public))
                    })
                    .ok_or_else(|| {
                        let error = TranspileError::MissingFunctionDeclaration(
                            TranspileMissingFunctionDeclaration::from_context(
                                identifier_span.clone(),
                                &functions,
                            ),
                        );
                        handler.receive(error.clone());
                        error
                    })?;

                function_data.statements.clone()
            };
            let commands = self.transpile_function(&statements, program_identifier, handler)?;

            let functions = self.functions.read().unwrap();
            let function_data = functions
                .get(&program_query)
                .or_else(|| {
                    alias_query
                        .clone()
                        .and_then(|q| functions.get(&q).filter(|f| f.public))
                })
                .ok_or_else(|| {
                    let error = TranspileError::MissingFunctionDeclaration(
                        TranspileMissingFunctionDeclaration::from_context(
                            identifier_span.clone(),
                            &functions,
                        ),
                    );
                    handler.receive(error.clone());
                    error
                })?;

            let modified_name = function_data
                .annotations
                .get("deobfuscate")
                .map_or_else(
                    || {
                        let hash_data =
                            program_identifier.to_string() + "\0" + identifier_span.str();
                        Some("shu/".to_string() + &md5::hash(hash_data).to_hex_lowercase())
                    },
                    Clone::clone,
                )
                .unwrap_or_else(|| identifier_span.str().to_string());

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

            self.function_locations.write().unwrap().insert(
                (
                    program_identifier.to_string(),
                    identifier_span.str().to_string(),
                ),
                (function_location, function_data.public),
            );
        }

        let parameters = {
            let functions = self.functions.read().unwrap();
            let function_data = functions
                .get(&program_query)
                .or_else(|| {
                    alias_query
                        .clone()
                        .and_then(|q| functions.get(&q).filter(|f| f.public))
                })
                .ok_or_else(|| {
                    let error = TranspileError::MissingFunctionDeclaration(
                        TranspileMissingFunctionDeclaration::from_context(
                            identifier_span.clone(),
                            &functions,
                        ),
                    );
                    handler.receive(error.clone());
                    error
                })?;

            function_data.parameters.clone()
        };

        let function_location = {
            let locations = self.function_locations.read().unwrap();
            locations
                .get(&program_query)
                .or_else(|| alias_query.and_then(|q| locations.get(&q).filter(|(_, p)| *p)))
                .ok_or_else(|| {
                    let error = TranspileError::MissingFunctionDeclaration(
                        TranspileMissingFunctionDeclaration::from_context(
                            identifier_span.clone(),
                            &self.functions.read().unwrap(),
                        ),
                    );
                    handler.receive(error.clone());
                    error
                })
                .map(|(s, _)| s.to_owned())?
        };

        let arg_count = arguments.iter().flat_map(|x| x.iter()).count();
        if arg_count != parameters.len() {
            let err = TranspileError::InvalidFunctionArguments(InvalidFunctionArguments {
                expected: parameters.len(),
                actual: arg_count,
                span: identifier_span.clone(),
            });
            handler.receive(err.clone());
            Err(err)
        } else if arg_count > 0 {
            let mut compiled_args = Vec::new();
            let mut errs = Vec::new();
            for expression in arguments.iter().flat_map(|x| x.iter()) {
                let value = match expression {
                    Expression::Primary(Primary::FunctionCall(func)) => self
                        .transpile_function_call(func, handler)
                        .map(|cmd| match cmd {
                            Command::Raw(s) => s,
                            _ => unreachable!("Function call should always return a raw command"),
                        }),
                    Expression::Primary(Primary::Lua(lua)) => {
                        lua.eval_string(handler).map(Option::unwrap_or_default)
                    }
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
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let mut errors = Vec::new();
        let commands = statements
            .iter()
            .filter_map(|statement| {
                self.transpile_statement(statement, program_identifier, handler)
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
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        match statement {
            Statement::LiteralCommand(literal_command) => {
                Ok(Some(literal_command.clean_command().into()))
            }
            Statement::Run(run) => match run.expression() {
                Expression::Primary(Primary::FunctionCall(func)) => {
                    self.transpile_function_call(func, handler).map(Some)
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
                self.transpile_execute_block(execute, program_identifier, handler)
            }
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
                        self.transpile_statement(statement, program_identifier, handler)
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
            #[allow(clippy::match_wildcard_for_single_variants)]
            Statement::Semicolon(semi) => match semi.expression() {
                Expression::Primary(Primary::FunctionCall(func)) => {
                    self.transpile_function_call(func, handler).map(Some)
                }
                unexpected => {
                    let error = TranspileError::UnexpectedExpression(UnexpectedExpression(
                        unexpected.clone(),
                    ));
                    handler.receive(error.clone());
                    Err(error)
                }
            },
        }
    }

    fn transpile_function_call(
        &mut self,
        func: &FunctionCall,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Command> {
        let arguments = func.arguments().as_ref().map(|l| {
            l.elements()
                .map(derive_more::Deref::deref)
                .collect::<Vec<_>>()
        });
        let (location, arguments) =
            self.get_or_transpile_function(&func.identifier().span, arguments.as_deref(), handler)?;
        let mut function_call = format!("function {location}");
        if let Some(arguments) = arguments {
            use std::fmt::Write;
            let arguments = arguments
                .iter()
                .map(|(ident, v)| {
                    format!(
                        r#"{macro_name}:"{escaped}""#,
                        macro_name = super::util::identifier_to_macro(ident),
                        escaped = crate::util::escape_str(v)
                    )
                })
                .collect::<Vec<_>>()
                .join(",");
            write!(function_call, " {{{arguments}}}").unwrap();
        }
        Ok(Command::Raw(function_call))
    }

    fn transpile_execute_block(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Command>> {
        self.transpile_execute_block_internal(execute, program_identifier, handler)
            .map(|ex| ex.map(Command::Execute))
    }

    fn transpile_execute_block_internal(
        &mut self,
        execute: &ExecuteBlock,
        program_identifier: &str,
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
                                self.transpile_statement(s, program_identifier, handler)
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
                            handler,
                        ),
                }?;

                self.combine_execute_head_tail(head, tail, program_identifier, handler)
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
                            self.transpile_statement(statement, program_identifier, handler)
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
                    self.transpile_statement(&statements[0], program_identifier, handler)?
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
                    self.transpile_statement(&statements[0], program_identifier, handler)
                        .unwrap_or_else(|err| {
                            errors.push(err);
                            None
                        })
                        .map(|cmd| Execute::Run(Box::new(cmd)))
                } else {
                    let commands = statements
                        .iter()
                        .filter_map(|statement| {
                            self.transpile_statement(statement, program_identifier, handler)
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
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Option<Execute>> {
        Ok(match head {
            ExecuteBlockHead::Conditional(cond) => {
                if let Some(tail) = tail {
                    self.transpile_conditional(cond, tail, None, program_identifier, handler)?
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
