use chksum_md5 as md5;
use enum_as_inner::EnumAsInner;
use std::{borrow::ToOwned, collections::BTreeMap, sync::Arc};

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
    Static(BTreeMap<String, MacroString>),
    Dynamic(Vec<Command>),
}

impl Transpiler {
    /// Gets the function at the given path, or transpiles it if it hasn't been transpiled yet.
    /// Returns the location of the function or None if the function does not exist.
    #[expect(clippy::too_many_lines)]
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
        let already_transpiled = function
            .as_ref()
            .expect("called function should be in scope")
            .as_ref()
            .as_function()
            .map(|(_, path, _)| path.get().is_some())
            .expect("called variable should be of type function");

        let function_data = function.ok_or_else(|| {
            let err = TranspileError::MissingFunctionDeclaration(
                MissingFunctionDeclaration::from_scope(identifier_span.clone(), scope),
            );
            handler.receive(Box::new(err.clone()));
            err
        })?;

        let VariableData::Function {
            function_data,
            path: function_path,
            function_scope,
        } = function_data.as_ref()
        else {
            unreachable!("must be of correct type, otherwise errored out before");
        };

        if !already_transpiled {
            tracing::trace!("Function not transpiled yet, transpiling.");

            let statements = function_data.statements.clone();

            let modified_name = function_data.annotations.get("deobfuscate").map_or_else(
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
                            let err = TranspileError::IllegalAnnotationContent(
                                IllegalAnnotationContent {
                                    annotation: identifier_span.clone(),
                                    message: "Cannot evaluate annotation at compile time"
                                        .to_string(),
                                },
                            );
                            handler.receive(Box::new(err.clone()));
                            err
                        }),
                    TranspileAnnotationValue::Map(_, span) => {
                        let err =
                            TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                annotation: span.clone(),
                                message: "Deobfuscate annotation cannot be a map.".to_string(),
                            });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    }
                },
            )?;

            let function_location = format!(
                "{namespace}:{modified_name}",
                namespace = function_data.namespace
            );

            function_path.set(function_location.clone()).unwrap();

            for (i, param) in function_data.parameters.iter().enumerate() {
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
                    Static(MacroString),
                    Storage {
                        prepare_cmds: Vec<Command>,
                        storage_name: String,
                        path: String,
                    },
                }

                let mut compiled_args = Vec::<Parameter>::new();
                let mut errs = Vec::new();

                for expression in arguments.iter().flat_map(|expressions| expressions.iter()) {
                    let value = match expression {
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
                        Expression::Primary(Primary::MacroStringLiteral(literal)) => {
                            Ok(Parameter::Static(literal.into()))
                        }
                        Expression::Primary(primary @ Primary::Identifier(ident)) => {
                            let var = scope.get_variable(ident.span.str()).ok_or_else(|| {
                                let err = TranspileError::UnknownIdentifier(UnknownIdentifier {
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
                                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
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
                        |(mut acc_setup, mut acc_move, mut statics), (param, data)| {
                            match param.variable_type() {
                                FunctionVariableType::Macro(_) => {
                                    let arg_name = crate::util::identifier_to_macro(param.identifier().span.str());
                                    match data {
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
                                    let target_storage_name = format!("shulkerscript:arguments_{}", function_location.replace(['/', ':'], "_"));
                                    let param_str = param.identifier().span.str();
                                    let target_path = crate::util::identifier_to_scoreboard_target(param_str);

                                    match data {
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
                            r"data merge storage shulkerscript:function_arguments {{{s}}}"
                        )),
                        MacroString::MacroString(_) => Command::UsesMacro(
                            super::util::join_macro_strings([
                                MacroString::String(
                                    "data merge storage shulkerscript:function_arguments {"
                                        .to_string(),
                                ),
                                joined_statics,
                                MacroString::String("}".to_string()),
                            ])
                            .into(),
                        ),
                    };
                    setup_cmds.push(statics_cmd);
                    setup_cmds.extend(move_cmds);

                    Ok(TranspiledFunctionArguments::Dynamic(setup_cmds))
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
                    Ok(TranspiledFunctionArguments::Static(function_args))
                }
            }
            _ => Ok(TranspiledFunctionArguments::None),
        }
    }
}
