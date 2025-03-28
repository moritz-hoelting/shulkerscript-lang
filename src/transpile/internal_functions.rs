//! Functions provided by the language itself.

use std::{
    ops::{Bound, Deref, RangeBounds},
    sync::Arc,
};

use shulkerbox::prelude::{Command, Execute};

use serde_json::{json, Value as JsonValue};

use crate::{
    base::{source_file::SourceElement as _, VoidHandler},
    lexical::token::{Identifier, MacroStringLiteralPart},
    semantic::error::{InvalidFunctionArguments, UnexpectedExpression},
    syntax::syntax_tree::expression::{Expression, FunctionCall, Primary},
    transpile::{
        error::{IllegalIndexing, IllegalIndexingReason, LuaRuntimeError, UnknownIdentifier},
        expression::{ComptimeValue, DataLocation, StorageType},
        util::MacroString,
        TranspileError,
    },
};

use super::{Scope, TranspileResult, Transpiler, VariableData};

/// A function that can be called from the language.
pub type InternalFunction =
    fn(&mut Transpiler, &Arc<Scope>, &FunctionCall) -> TranspileResult<Vec<Command>>;

/// Adds all internal functions to the scope.
pub fn add_all_to_scope(scope: &Arc<Scope>) {
    scope.set_variable(
        "print",
        VariableData::InternalFunction {
            implementation: print_function,
        },
    );
}

fn get_args_assert_in_range(
    call: &FunctionCall,
    range: impl RangeBounds<usize>,
) -> TranspileResult<Vec<&Expression>> {
    let args = call
        .arguments()
        .as_ref()
        .map(|args| args.elements().map(Deref::deref).collect::<Vec<_>>())
        .unwrap_or_default();
    if range.contains(&args.len()) {
        Ok(args)
    } else {
        let span = args
            .first()
            .and_then(|first| {
                args.last()
                    .map(|last| first.span().join(&last.span()).expect("invalid span"))
            })
            .unwrap_or_else(|| {
                call.left_parenthesis()
                    .span()
                    .join(&call.right_parenthesis().span())
                    .expect("invalid span")
            });

        let actual = args.len();
        let expected = match range.start_bound() {
            Bound::Excluded(excluded) => (excluded + 1 > actual).then_some(excluded + 1),
            Bound::Included(&included) => (included > actual).then_some(included),
            Bound::Unbounded => None,
        }
        .or_else(|| match range.end_bound() {
            Bound::Excluded(&excluded) => (excluded <= actual).then_some(excluded.wrapping_sub(1)),
            Bound::Included(&included) => (included < actual).then_some(included),
            Bound::Unbounded => None,
        })
        .unwrap_or_default();

        Err(TranspileError::InvalidFunctionArguments(
            InvalidFunctionArguments {
                expected,
                actual: args.len(),
                span,
            },
        ))
    }
}

#[expect(clippy::too_many_lines)]
fn print_function(
    transpiler: &mut Transpiler,
    scope: &Arc<Scope>,
    call: &FunctionCall,
) -> TranspileResult<Vec<Command>> {
    const PARAM_COLOR: &str = "gray";

    #[expect(clippy::option_if_let_else)]
    fn get_identifier_part(
        ident: &Identifier,
        transpiler: &mut Transpiler,
        scope: &Arc<Scope>,
    ) -> TranspileResult<(bool, Option<Command>, JsonValue)> {
        if let Some(var) = scope.get_variable(ident.span.str()).as_deref() {
            match var {
                VariableData::MacroParameter { macro_name, .. } => Ok((
                    true,
                    None,
                    json!({"text": format!("$({macro_name})"), "color": PARAM_COLOR}),
                )),
                VariableData::ScoreboardValue { objective, target } => {
                    let (cmd, value) = get_data_location(
                        &DataLocation::ScoreboardValue {
                            objective: objective.to_string(),
                            target: target.to_string(),
                        },
                        transpiler,
                    );

                    Ok((false, cmd, value))
                }
                VariableData::BooleanStorage { storage_name, path } => {
                    let (cmd, value) = get_data_location(
                        &DataLocation::Storage {
                            storage_name: storage_name.to_string(),
                            path: path.to_string(),
                            r#type: StorageType::Boolean,
                        },
                        transpiler,
                    );

                    Ok((false, cmd, value))
                }
                _ => Err(TranspileError::UnexpectedExpression(UnexpectedExpression(
                    Expression::Primary(Primary::Identifier(ident.to_owned())),
                ))),
            }
        } else {
            Err(TranspileError::UnknownIdentifier(UnknownIdentifier {
                identifier: ident.span(),
            }))
        }
    }

    fn get_data_location(
        location: &DataLocation,
        transpiler: &mut Transpiler,
    ) -> (Option<Command>, JsonValue) {
        match location {
            DataLocation::ScoreboardValue { objective, target } => (
                None,
                json!({"score": {"name": target, "objective": objective}, "color": PARAM_COLOR}),
            ),
            DataLocation::Storage {
                storage_name, path, ..
            } => (
                None,
                json!({"nbt": path, "storage": storage_name, "color": PARAM_COLOR}),
            ),
            DataLocation::Tag { tag_name, entity } => {
                let (temp_storage_name, temp_storage_paths) =
                    transpiler.get_temp_storage_locations(1);
                let selector =
                    super::util::add_to_entity_selector(entity, &format!("tag={tag_name}"));
                let cmd = Command::Execute(Execute::Store(
                    format!(
                        "success storage {temp_storage_name} {path} byte 1.0",
                        path = temp_storage_paths[0]
                    )
                    .into(),
                    Box::new(Execute::Run(Box::new(Command::Raw(format!(
                        "execute if entity {selector}"
                    ))))),
                ));

                (
                    Some(cmd),
                    json!({"nbt": temp_storage_paths[0], "storage": temp_storage_name, "color": PARAM_COLOR}),
                )
            }
        }
    }

    let args = get_args_assert_in_range(call, 1..=2)?;
    let first = args.first().expect("checked range");
    let (target, message_expression) = args.get(1).map_or_else(
        || ("@a".into(), first),
        |second| {
            (
                first
                    .comptime_eval(scope, &VoidHandler)
                    .map_or_else(|| "@a".into(), |val| val.to_macro_string()),
                second,
            )
        },
    );

    let mut contains_macro = matches!(target, MacroString::MacroString(_));

    let (mut cmds, parts) = match message_expression {
        Expression::Primary(primary) => match primary {
            Primary::Boolean(boolean) => Ok((
                Vec::new(),
                vec![JsonValue::String(boolean.value().to_string())],
            )),
            Primary::Integer(integer) => Ok((
                Vec::new(),
                vec![JsonValue::String(integer.as_i64().to_string())],
            )),
            Primary::StringLiteral(string) => Ok((
                Vec::new(),
                vec![JsonValue::String(string.str_content().to_string())],
            )),
            Primary::Lua(lua) => {
                let (ret, _lua) = lua.eval(scope, &VoidHandler)?;
                Ok((
                    Vec::new(),
                    vec![JsonValue::String(ret.to_string().map_err(|err| {
                        TranspileError::LuaRuntimeError(LuaRuntimeError::from_lua_err(
                            &err,
                            lua.span(),
                        ))
                    })?)],
                ))
            }
            Primary::Identifier(ident) => {
                let (cur_contains_macro, cmd, part) =
                    get_identifier_part(ident, transpiler, scope)?;
                contains_macro |= cur_contains_macro;
                Ok((cmd.into_iter().collect(), vec![part]))
            }
            Primary::Indexed(indexed) => match indexed.object().as_ref() {
                Primary::Identifier(ident) => {
                    match scope.get_variable(ident.span.str()).as_deref() {
                        Some(VariableData::Scoreboard { objective }) => {
                            if let Some(ComptimeValue::String(index)) =
                                indexed.index().comptime_eval(scope, &VoidHandler)
                            {
                                let (cmd, value) = get_data_location(
                                    &DataLocation::ScoreboardValue {
                                        objective: objective.to_string(),
                                        target: index,
                                    },
                                    transpiler,
                                );
                                Ok((cmd.into_iter().collect(), vec![value]))
                            } else {
                                todo!("allow macro string, but throw error when index is not constant string")
                            }
                        }
                        Some(VariableData::ScoreboardArray { objective, targets }) => {
                            if let Some(ComptimeValue::Integer(index)) =
                                indexed.index().comptime_eval(scope, &VoidHandler)
                            {
                                #[expect(clippy::option_if_let_else)]
                                if let Some(target) = usize::try_from(index)
                                    .ok()
                                    .and_then(|index| targets.get(index))
                                {
                                    let (cmd, value) = get_data_location(
                                        &DataLocation::ScoreboardValue {
                                            objective: objective.to_string(),
                                            target: target.to_string(),
                                        },
                                        transpiler,
                                    );
                                    Ok((cmd.into_iter().collect(), vec![value]))
                                } else {
                                    Err(TranspileError::IllegalIndexing(IllegalIndexing {
                                        reason: IllegalIndexingReason::IndexOutOfBounds {
                                            index: usize::try_from(index).unwrap_or(usize::MAX),
                                            length: targets.len(),
                                        },
                                        expression: indexed.index().span(),
                                    }))
                                }
                            } else {
                                todo!("throw error when index is not constant integer")
                            }
                        }
                        Some(VariableData::BooleanStorageArray {
                            storage_name,
                            paths,
                        }) => {
                            if let Some(ComptimeValue::Integer(index)) =
                                indexed.index().comptime_eval(scope, &VoidHandler)
                            {
                                #[expect(clippy::option_if_let_else)]
                                if let Some(path) = usize::try_from(index)
                                    .ok()
                                    .and_then(|index| paths.get(index))
                                {
                                    let (cmd, value) = get_data_location(
                                        &DataLocation::Storage {
                                            storage_name: storage_name.to_string(),
                                            path: path.to_string(),
                                            r#type: StorageType::Boolean,
                                        },
                                        transpiler,
                                    );
                                    Ok((cmd.into_iter().collect(), vec![value]))
                                } else {
                                    Err(TranspileError::IllegalIndexing(IllegalIndexing {
                                        reason: IllegalIndexingReason::IndexOutOfBounds {
                                            index: usize::try_from(index).unwrap_or(usize::MAX),
                                            length: paths.len(),
                                        },
                                        expression: indexed.index().span(),
                                    }))
                                }
                            } else {
                                todo!("throw error when index is not constant integer")
                            }
                        }
                        _ => todo!(),
                    }
                }
                _ => Err(TranspileError::IllegalIndexing(IllegalIndexing {
                    expression: indexed.object().span(),
                    reason: IllegalIndexingReason::NotIdentifier,
                })),
            },
            Primary::MacroStringLiteral(macro_string) => {
                let mut cmds = Vec::new();
                let mut parts = Vec::new();
                for part in macro_string.parts() {
                    match part {
                        MacroStringLiteralPart::Text(text) => {
                            parts.push(JsonValue::String(text.str().to_string()));
                        }
                        MacroStringLiteralPart::MacroUsage { identifier, .. } => {
                            let (cur_contains_macro, cur_cmds, part) =
                                get_identifier_part(identifier, transpiler, scope)?;
                            contains_macro |= cur_contains_macro;
                            cmds.extend(cur_cmds);
                            parts.push(part);
                        }
                    }
                }
                Ok((cmds, parts))
            }

            _ => todo!("print_function Primary"),
        },
        Expression::Binary(_) => todo!("print_function Binary"),
    }?;

    // TODO: prepend prefix with datapack name to parts and remove following
    let print_args = if parts.len() == 1 {
        serde_json::to_string(&parts[0]).expect("json serialization failed")
    } else {
        serde_json::to_string(&parts).expect("json serialization failed")
    };

    // TODO: throw correct error
    let cmd = format!("tellraw {target} {print_args}");

    let cmd = if contains_macro {
        Command::UsesMacro(cmd.parse::<MacroString>().expect("cannot fail").into())
    } else {
        Command::Raw(cmd)
    };

    cmds.push(cmd);

    Ok(cmds)
}
