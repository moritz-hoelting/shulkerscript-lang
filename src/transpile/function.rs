use chksum_md5 as md5;
use enum_as_inner::EnumAsInner;
use itertools::Itertools;
use std::{
    borrow::{Cow, ToOwned},
    collections::BTreeMap,
    sync::{Arc, RwLock},
};

use shulkerbox::datapack::{Command, Execute};

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler,
    },
    semantic::error::{ConflictingFunctionNames, InvalidFunctionArguments},
    syntax::syntax_tree::{
        declaration::FunctionVariableType,
        expression::{Expression, Primary},
        statement::Statement,
    },
    transpile::{
        error::{IllegalAnnotationContent, MissingFunctionDeclaration},
        util::{MacroString, MacroStringPart},
        variables::FunctionVariableDataType,
    },
};

use super::{
    error::{MismatchedTypes, TranspileError, TranspileResult, UnknownIdentifier},
    expression::{ComptimeValue, ExpectedType, StorageType},
    variables::{Scope, VariableData},
    FunctionData, TranspileAnnotationValue, Transpiler,
};

#[derive(Debug, Clone)]
pub enum TranspiledFunctionArguments {
    None,
    Static(BTreeMap<String, MacroString>, Vec<Command>),
    Dynamic(Vec<Command>),
}

impl Transpiler {
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
        let function = scope.get_variable(identifier_span.str());

        let function_data = function.ok_or_else(|| {
            let err = TranspileError::MissingFunctionDeclaration(
                MissingFunctionDeclaration::from_scope(identifier_span.clone(), scope),
            );
            handler.receive(Box::new(err.clone()));
            err
        })?;

        let (function_data, function_scope, function_type) = function_data
            .as_ref()
            .as_function()
            .expect("called variable should be of type function");

        match function_type {
            FunctionVariableDataType::Simple {
                path: function_path,
            } => {
                let already_transpiled = function_path.get().is_some();

                if !already_transpiled {
                    self.prepare_transpile_function(
                        function_data,
                        program_identifier,
                        identifier_span,
                        |path| function_path.set(path).expect("not set before"),
                        std::iter::repeat(None),
                        None,
                        function_scope,
                        scope,
                        handler,
                    )?;
                }

                let function_location = function_path
                    .get()
                    .ok_or_else(|| {
                        let err = TranspileError::MissingFunctionDeclaration(
                            MissingFunctionDeclaration::from_scope(identifier_span.clone(), scope),
                        );
                        handler.receive(Box::new(err.clone()));
                        err
                    })
                    .map(String::to_owned)?;

                let args = self.transpile_function_arguments(
                    function_data,
                    &function_location,
                    arguments,
                    scope,
                    handler,
                )?;

                Ok((function_location, args))
            }
            FunctionVariableDataType::ComptimeArguments { function_paths } => {
                let comptime_args = self.transpile_comptime_function_arguments(
                    function_data,
                    arguments,
                    scope,
                    handler,
                )?;
                let hash = comptime_args_hash(&comptime_args);

                let read_guard = function_paths.read().unwrap();
                let function_location = if let Some(data) = read_guard.get(&hash) {
                    data.to_owned()
                } else {
                    drop(read_guard);
                    let function_scope = Scope::with_parent(function_scope.to_owned());
                    let mut path = String::new();

                    self.prepare_transpile_function(
                        function_data,
                        program_identifier,
                        identifier_span,
                        |p| path = p,
                        comptime_args.into_iter(),
                        Some(&hash),
                        &function_scope,
                        scope,
                        handler,
                    )?;

                    function_paths
                        .write()
                        .unwrap()
                        .insert(hash.clone(), path.clone());

                    path
                };

                let args = self.transpile_function_arguments(
                    function_data,
                    &function_location,
                    arguments,
                    scope,
                    handler,
                )?;

                Ok((function_location, args))
            }
        }
    }

    #[expect(clippy::too_many_arguments, clippy::too_many_lines)]
    fn prepare_transpile_function(
        &mut self,
        function_data: &FunctionData,
        program_identifier: &str,
        identifier_span: &Span,
        set_function_path: impl FnOnce(String),
        comptime_args: impl Iterator<Item = Option<ComptimeValue>>,
        function_suffix: Option<&str>,
        function_scope: &Arc<Scope>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<()> {
        tracing::trace!("Function not transpiled yet, transpiling.");

        let statements = function_data.statements.clone();

        let mut modified_name = function_data.annotations.get("deobfuscate").map_or_else(
            || {
                let hash_data = program_identifier.to_string() + "\0" + identifier_span.str();
                Ok("shu/".to_string() + &md5::hash(hash_data).to_hex_lowercase())
            },
            |val| match val {
                TranspileAnnotationValue::None(_) => Ok(identifier_span.str().to_string()),
                TranspileAnnotationValue::Expression(expr, _) => expr
                    .comptime_eval(scope, handler)
                    .ok()
                    .and_then(|val| val.to_string_no_macro())
                    .ok_or_else(|| {
                        let err =
                            TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                annotation: identifier_span.clone(),
                                message: "Cannot evaluate annotation at compile time".to_string(),
                            });
                        handler.receive(Box::new(err.clone()));
                        err
                    }),
                TranspileAnnotationValue::Map(_, span) => {
                    let err = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                        annotation: span.clone(),
                        message: "Deobfuscate annotation cannot be a map.".to_string(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            },
        )?;

        if let Some(suffix) = function_suffix {
            use std::fmt::Write as _;

            let _ = write!(modified_name, "/{suffix}");
        }

        let function_location = format!(
            "{namespace}:{modified_name}",
            namespace = function_data.namespace
        );

        set_function_path(function_location.clone());

        for (i, (param, comptime_arg)) in function_data
            .parameters
            .iter()
            .zip(comptime_args)
            .enumerate()
        {
            let param_str = param.identifier().span.str();
            match param.variable_type() {
                FunctionVariableType::Macro(_) => {
                    function_scope.set_variable(
                        param_str,
                        VariableData::MacroParameter {
                            index: i,
                            macro_name: crate::util::identifier_to_macro(param_str).to_string(),
                        },
                    );
                }
                FunctionVariableType::Integer(_) => {
                    let objective = format!(
                        "shu_arguments_{}",
                        function_location.replace(['/', ':'], "_")
                    );
                    function_scope.set_variable(
                        param_str,
                        VariableData::ScoreboardValue {
                            objective: objective.clone(),
                            target: crate::util::identifier_to_scoreboard_target(param_str)
                                .into_owned(),
                        },
                    );
                }
                FunctionVariableType::Boolean(_) => {
                    let storage_name = format!(
                        "shulkerscript:arguments_{}",
                        function_location.replace(['/', ':'], "_")
                    );
                    // TODO: replace with proper path
                    function_scope.set_variable(
                        param_str,
                        VariableData::BooleanStorage {
                            storage_name,
                            path: crate::util::identifier_to_scoreboard_target(param_str)
                                .into_owned(),
                        },
                    );
                }
                FunctionVariableType::Value(_) => {
                    function_scope.set_variable(
                        param_str,
                        VariableData::ComptimeValue {
                            value: Arc::new(RwLock::new(comptime_arg)),
                            read_only: false,
                        },
                    );
                }
            }
        }

        let commands =
            self.transpile_function(&statements, program_identifier, function_scope, handler)?;

        let namespace = self.datapack.namespace_mut(&function_data.namespace);

        if namespace.function(&modified_name).is_some() {
            let err = TranspileError::ConflictingFunctionNames(ConflictingFunctionNames {
                name: modified_name,
                definition: identifier_span.clone(),
            });
            handler.receive(Box::new(err.clone()));
            return Err(err);
        }

        let function = namespace.function_mut(&modified_name);
        function.get_commands_mut().extend(commands);

        if function_data.annotations.contains_key("tick") {
            self.datapack.add_tick(&function_location);
        }
        if function_data.annotations.contains_key("load") {
            self.datapack.add_load(&function_location);
        }

        Ok(())
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

    #[expect(clippy::unused_self, clippy::needless_pass_by_ref_mut)]
    fn transpile_comptime_function_arguments(
        &mut self,
        function_data: &FunctionData,
        arguments: Option<&[&Expression]>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Option<ComptimeValue>>> {
        let parameters = &function_data.parameters;
        let arg_count = arguments.map(<[&Expression]>::len).unwrap_or_default();

        let mut has_comptime = parameters
            .iter()
            .any(|param| matches!(param.variable_type(), FunctionVariableType::Value(_)));

        if has_comptime {
            if parameters.len() == arg_count {
                let vals = parameters
                    .iter()
                    .zip(arguments.iter().flat_map(|args| args.iter()))
                    .map(|(param, arg)| match param.variable_type() {
                        FunctionVariableType::Value(_) => {
                            has_comptime = true;
                            let val = arg.comptime_eval(scope, handler)?;
                            Ok(Some(val))
                        }
                        _ => Ok(None),
                    })
                    .collect::<TranspileResult<Vec<Option<ComptimeValue>>>>()?;

                Ok(vals)
            } else {
                let err = TranspileError::InvalidFunctionArguments(InvalidFunctionArguments {
                    expected: parameters.len(),
                    actual: arg_count,
                    span: function_data.identifier_span.clone(),
                });
                handler.receive(Box::new(err.clone()));
                Err(err)
            }
        } else {
            Ok(vec![None; parameters.len()])
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_function_arguments(
        &mut self,
        function_data: &FunctionData,
        function_location: &str,
        arguments: Option<&[&Expression]>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<TranspiledFunctionArguments> {
        let parameters = &function_data.parameters;
        let identifier_span = &function_data.identifier_span;
        let arg_count = arguments.map(<[&Expression]>::len);

        match arg_count {
            Some(arg_count) if arg_count != parameters.len() => {
                let err = TranspileError::InvalidFunctionArguments(InvalidFunctionArguments {
                    expected: parameters.len(),
                    actual: arg_count,
                    span: identifier_span.clone(),
                });
                handler.receive(Box::new(err.clone()));
                Err(err)
            }
            Some(arg_count) if arg_count > 0 => {
                #[derive(Debug, Clone, EnumAsInner)]
                enum Parameter {
                    Comptime,
                    Static(MacroString),
                    Storage {
                        prepare_cmds: Vec<Command>,
                        storage_name: String,
                        path: String,
                    },
                }

                let mut compiled_args = Vec::<Parameter>::new();
                let mut errs = Vec::new();

                for (expression, is_comptime) in arguments
                    .iter()
                    .flat_map(|expressions| expressions.iter())
                    .zip(
                        parameters
                            .iter()
                            .map(|p| matches!(p.variable_type(), FunctionVariableType::Value(_))),
                    )
                {
                    let value = if is_comptime {
                        Ok(Parameter::Comptime)
                    } else {
                        match expression {
                            Expression::Primary(Primary::Lua(lua)) => {
                                lua.eval_comptime(scope, handler).and_then(|val| match val {
                                    Ok(ComptimeValue::MacroString(s)) => Ok(Parameter::Static(s)),
                                    Ok(val) => Ok(Parameter::Static(val.to_macro_string())),
                                    Err(err) => {
                                        let err = TranspileError::NotComptime(err);
                                        handler.receive(Box::new(err.clone()));
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
                            Expression::Primary(Primary::TemplateStringLiteral(literal)) => {
                                Ok(Parameter::Static(literal.to_macro_string(scope, handler)?))
                            }
                            Expression::Primary(primary @ Primary::Identifier(ident)) => {
                                let var =
                                    scope.get_variable(ident.span.str()).ok_or_else(|| {
                                        let err =
                                            TranspileError::UnknownIdentifier(UnknownIdentifier {
                                                identifier: ident.span(),
                                            });
                                        handler.receive(Box::new(err.clone()));
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
                                        let (temp_storage, [temp_path]) =
                                            self.get_temp_storage_locations_array();
                                        let prepare_cmds = self.transpile_primary_expression(
                                            primary,
                                            &super::expression::DataLocation::Storage {
                                                storage_name: temp_storage.clone(),
                                                path: temp_path.clone(),
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

                                        Ok(Parameter::Storage {
                                            prepare_cmds,
                                            storage_name: temp_storage,
                                            path: temp_path,
                                        })
                                    }
                                    _ => {
                                        let err =
                                            TranspileError::MismatchedTypes(MismatchedTypes {
                                                expression: expression.span(),
                                                expected_type: ExpectedType::AnyOf(vec![
                                                    ExpectedType::Integer,
                                                    ExpectedType::Boolean,
                                                    ExpectedType::String,
                                                ]),
                                            });
                                        handler.receive(Box::new(err.clone()));
                                        Err(err)
                                    }
                                }
                            }
                            Expression::Primary(Primary::MemberAccess(member_access)) => {
                                if let Ok(value) = member_access.parent().comptime_member_access(
                                    member_access,
                                    scope,
                                    handler,
                                ) {
                                    Ok(Parameter::Static(value.to_macro_string()))
                                } else {
                                    let (storage_name, [path]) =
                                        self.get_temp_storage_locations_array();
                                    let prepare_cmds = self.transpile_expression(
                                        expression,
                                        &super::expression::DataLocation::Storage {
                                            storage_name: storage_name.clone(),
                                            path: path.clone(),
                                            r#type: StorageType::Int,
                                        },
                                        scope,
                                        handler,
                                    )?;

                                    Ok(Parameter::Storage {
                                        prepare_cmds,
                                        storage_name,
                                        path,
                                    })
                                }
                            }
                            Expression::Primary(
                                Primary::Parenthesized(_)
                                | Primary::Prefix(_)
                                | Primary::Indexed(_)
                                | Primary::FunctionCall(_),
                            )
                            | Expression::Binary(_) => {
                                let (temp_storage, [temp_path]) =
                                    self.get_temp_storage_locations_array();
                                let prepare_cmds = self.transpile_expression(
                                    expression,
                                    &super::expression::DataLocation::Storage {
                                        storage_name: temp_storage.clone(),
                                        path: temp_path.clone(),
                                        r#type: StorageType::Int,
                                    },
                                    scope,
                                    handler,
                                )?;

                                Ok(Parameter::Storage {
                                    prepare_cmds,
                                    storage_name: temp_storage,
                                    path: temp_path,
                                })
                            }
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
                    let (require_dyn_params, mut setup_cmds, move_cmds, static_params) = parameters.clone().into_iter().zip(compiled_args).fold(
                        (false, Vec::new(), Vec::new(), BTreeMap::new()),
                        |(mut require_dyn_params, mut acc_setup, mut acc_move, mut statics), (param, data)| {
                            match param.variable_type() {
                                FunctionVariableType::Macro(_) => {
                                    let arg_name = crate::util::identifier_to_macro(param.identifier().span.str());
                                    match data {
                                        Parameter::Comptime => {}
                                        Parameter::Static(s) => {
                                            match s {
                                                MacroString::String(value) => statics.insert(
                                                    arg_name.to_string(),
                                                    MacroString::String(crate::util::escape_str(&value).to_string())
                                                ),
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
                                        Parameter::Storage { prepare_cmds, storage_name, path } => {
                                            require_dyn_params = true;
                                            acc_setup.extend(prepare_cmds);
                                            acc_move.push(Command::Raw(
                                                format!(r"data modify storage shulkerscript:function_arguments {arg_name} set from storage {storage_name} {path}")
                                            ));
                                        }
                                    }
                                }
                                FunctionVariableType::Integer(_) => {
                                    let objective = format!("shu_arguments_{}", function_location.replace(['/', ':'], "_"));
                                    let param_str = param.identifier().span.str();
                                    let target = crate::util::identifier_to_scoreboard_target(param_str);

                                    match data {
                                        Parameter::Comptime => {}
                                        Parameter::Static(s) => {
                                            match s.as_str() {
                                                Ok(s) => {
                                                    if s.parse::<i32>().is_ok() {
                                                        acc_move.push(Command::Raw(format!(r"scoreboard players set {target} {objective} {s}")));
                                                    } else {
                                                        panic!("non-integer static argument")
                                                    }
                                                }
                                                Err(parts) => {
                                                    acc_move.push(Command::UsesMacro(MacroString::MacroString(
                                                        std::iter::once(MacroStringPart::String(format!("scoreboard players set {target} {objective} ")))
                                                        .chain(parts.iter().cloned()).collect()
                                                    ).into()));
                                                }
                                            }
                                        }
                                        Parameter::Storage { prepare_cmds, storage_name, path } => {
                                            acc_setup.extend(prepare_cmds);
                                            acc_move.push(Command::Execute(Execute::Store(
                                                format!("result score {target} {objective}").into(), 
                                                Box::new(Execute::Run(Box::new(Command::Raw(format!("data get storage {storage_name} {path}")))))
                                            )));
                                        }
                                    }
                                },
                                FunctionVariableType::Boolean(_) => {
                                    require_dyn_params = true;
                                    let target_storage_name = format!("shulkerscript:arguments_{}", function_location.replace(['/', ':'], "_"));
                                    let param_str = param.identifier().span.str();
                                    let target_path = crate::util::identifier_to_scoreboard_target(param_str);

                                    match data {
                                        Parameter::Comptime => {}
                                        Parameter::Static(s) => {
                                            match s.as_str() {
                                                Ok(s) => {
                                                    if let Ok(b) = s.parse::<bool>() {
                                                        acc_move.push(Command::Raw(format!("data modify storage {target_storage_name} {target_path} set value {}", if b { "1b" } else { "0b" })));
                                                    } else {
                                                        panic!("non-integer static argument")
                                                    }
                                                }
                                                Err(parts) => {
                                                    acc_move.push(Command::UsesMacro(MacroString::MacroString(
                                                        std::iter::once(MacroStringPart::String(format!("data modify storage {target_storage_name} {target_path} set value ")))
                                                        .chain(parts.iter().cloned()).collect()
                                                    ).into()));
                                                }
                                            }
                                        }
                                        Parameter::Storage { prepare_cmds, storage_name, path } => {
                                            acc_setup.extend(prepare_cmds);
                                            acc_move.push(Command::Raw(format!("data modify storage {target_storage_name} {target_path} set from storage {storage_name} {path}")));
                                        }
                                    }
                                },
                                FunctionVariableType::Value(_) => {
                                    // handled before in `transpile_comptime_function_arguments`
                                }
                            }
                        (require_dyn_params, acc_setup, acc_move, statics)},
                    );
                    if require_dyn_params {
                        let statics_len = static_params.len();
                        let joined_statics = super::util::join_macro_strings(
                            static_params
                                .into_iter()
                                .enumerate()
                                .map(|(i, (k, v))| match v {
                                    MacroString::String(s) => {
                                        let mut s = format!(r#"{k}:"{s}""#);
                                        if i < statics_len - 1 {
                                            s.push(',');
                                        }
                                        MacroString::String(s)
                                    }
                                    MacroString::MacroString(mut parts) => {
                                        parts.insert(
                                            0,
                                            MacroStringPart::String(format!(r#"{k}:""#)),
                                        );
                                        let mut ending = '"'.to_string();
                                        if i < statics_len - 1 {
                                            ending.push(',');
                                        }
                                        parts.push(MacroStringPart::String(ending));
                                        MacroString::MacroString(parts)
                                    }
                                }),
                        );
                        let storage_suffix = function_location.replace(['/', ':'], "_");
                        let statics_cmd = match joined_statics {
                            MacroString::String(s) => Command::Raw(format!(
                                r"data merge storage shulkerscript:function_arguments_{storage_suffix} {{{s}}}"
                            )),
                            MacroString::MacroString(_) => {
                                let prefix = MacroString::String(
                                    format!("data merge storage shulkerscript:function_arguments_{storage_suffix} {{"),
                                );
                                Command::UsesMacro(
                                    super::util::join_macro_strings([
                                        prefix,
                                        joined_statics,
                                        MacroString::String("}".to_string()),
                                    ])
                                    .into(),
                                )
                            }
                        };
                        setup_cmds.push(statics_cmd);
                        setup_cmds.extend(move_cmds);

                        Ok(TranspiledFunctionArguments::Dynamic(setup_cmds))
                    } else {
                        setup_cmds.extend(move_cmds);

                        Ok(TranspiledFunctionArguments::Static(
                            static_params,
                            setup_cmds,
                        ))
                    }
                } else {
                    let function_args = parameters
                        .clone()
                        .into_iter()
                        .zip(
                            compiled_args
                                .into_iter()
                                .map(|arg| arg.into_static().expect("checked in if condition")),
                        )
                        .map(|(k, v)| (k.identifier().span.str().to_string(), v))
                        .collect();
                    Ok(TranspiledFunctionArguments::Static(
                        function_args,
                        Vec::new(),
                    ))
                }
            }
            _ => Ok(TranspiledFunctionArguments::None),
        }
    }
}

fn comptime_args_hash(args: &[Option<ComptimeValue>]) -> String {
    let combined = args
        .iter()
        .filter_map(Option::as_ref)
        .map(|arg| match arg {
            ComptimeValue::Boolean(b) => Cow::Owned(b.to_string()),
            ComptimeValue::Integer(i) => Cow::Owned(i.to_string()),
            ComptimeValue::String(s) => Cow::Borrowed(s.as_str()),
            ComptimeValue::MacroString(s) => match s.as_str() {
                Ok(s) => s,
                Err(parts) => {
                    let s = parts
                        .iter()
                        .map(|p| match p {
                            MacroStringPart::String(s) => Cow::Borrowed(s.as_str()),
                            MacroStringPart::MacroUsage(u) => Cow::Owned(format!("`{u}`")),
                        })
                        .join("\0\0");

                    Cow::Owned(s)
                }
            },
        })
        .join("\0");

    md5::hash(combined).to_hex_lowercase()
}
