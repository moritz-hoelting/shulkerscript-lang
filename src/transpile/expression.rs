//! The expression transpiler.

use std::{fmt::Display, sync::Arc};

use super::{Scope, VariableData};
use crate::{
    base::VoidHandler,
    lexical::token::MacroStringLiteralPart,
    syntax::syntax_tree::expression::{
        Binary, BinaryOperator, Expression, PrefixOperator, Primary,
    },
};

#[cfg(feature = "shulkerbox")]
use shulkerbox::prelude::{Command, Condition, Execute};

#[cfg(feature = "shulkerbox")]
use super::{
    error::{MismatchedTypes, UnknownIdentifier},
    TranspileResult, Transpiler,
};
#[cfg(feature = "shulkerbox")]
use crate::{
    base::{self, source_file::SourceElement, Handler},
    semantic::error::UnexpectedExpression,
    transpile::{error::FunctionArgumentsNotAllowed, TranspileError},
};

/// Compile-time evaluated value
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComptimeValue {
    Boolean(bool),
    Integer(i64),
    String(String),
    MacroString(String),
}

impl Display for ComptimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(boolean) => write!(f, "{boolean}"),
            Self::Integer(int) => write!(f, "{int}"),
            Self::String(string) => write!(f, "{string}"),
            Self::MacroString(macro_string) => write!(f, "{macro_string}"),
        }
    }
}

impl ComptimeValue {
    /// Returns the value as a string not containing a macro.
    #[must_use]
    pub fn to_string_no_macro(&self) -> Option<String> {
        match self {
            Self::Boolean(boolean) => Some(boolean.to_string()),
            Self::Integer(int) => Some(int.to_string()),
            Self::String(string) => Some(string.clone()),
            Self::MacroString(_) => None,
        }
    }
}

/// The type of an expression.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ValueType {
    Boolean,
    Integer,
    String,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::String => write!(f, "string"),
        }
    }
}

/// Location of data
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataLocation {
    ScoreboardValue {
        objective: String,
        target: String,
    },
    Tag {
        tag_name: String,
        entity: String,
    },
    Storage {
        storage_name: String,
        path: String,
        r#type: StorageType,
    },
}

impl DataLocation {
    /// Returns the type of the data location.
    #[must_use]
    pub fn value_type(&self) -> ValueType {
        match self {
            Self::ScoreboardValue { .. } => ValueType::Integer,
            Self::Tag { .. } => ValueType::Boolean,
            Self::Storage { r#type, .. } => match r#type {
                StorageType::Boolean => ValueType::Boolean,
                StorageType::Byte | StorageType::Int | StorageType::Long => ValueType::Integer,
                StorageType::Double => todo!("Double storage type"),
            },
        }
    }
}

/// The type of a storage.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum StorageType {
    Boolean,
    Byte,
    Int,
    Long,
    Double,
}

impl StorageType {
    /// Returns the suffix of the storage type.
    #[must_use]
    pub fn suffix(&self) -> &'static str {
        match self {
            Self::Boolean | Self::Byte => "b",
            Self::Int => "",
            Self::Long => "l",
            Self::Double => "d",
        }
    }

    /// Returns the string representation of the storage type.
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Boolean | Self::Byte => "byte",
            Self::Int => "int",
            Self::Long => "long",
            Self::Double => "double",
        }
    }
}

impl Expression {
    /// Returns whether the expression can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType, scope: &Arc<Scope>) -> bool {
        match self {
            Self::Primary(primary) => primary.can_yield_type(r#type, scope),
            Self::Binary(_binary) => todo!(),
        }
    }

    /// Evaluate at compile-time.
    #[must_use]
    pub fn comptime_eval(&self, scope: &Arc<Scope>) -> Option<ComptimeValue> {
        match self {
            Self::Primary(primary) => primary.comptime_eval(scope),
            Self::Binary(binary) => binary.comptime_eval(scope),
        }
    }
}

impl Primary {
    /// Returns whether the primary can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType, scope: &Arc<Scope>) -> bool {
        match self {
            Self::Boolean(_) => matches!(r#type, ValueType::Boolean),
            Self::Integer(_) => matches!(r#type, ValueType::Integer),
            Self::FunctionCall(_) => matches!(r#type, ValueType::Integer | ValueType::Boolean),
            Self::Identifier(ident) => {
                scope
                    .get_variable(ident.span.str())
                    .map_or(false, |variable| match r#type {
                        ValueType::Boolean => {
                            matches!(
                                variable.as_ref(),
                                VariableData::Tag { .. } | VariableData::BooleanStorage { .. }
                            )
                        }
                        ValueType::Integer => {
                            matches!(variable.as_ref(), VariableData::ScoreboardValue { .. })
                        }
                        ValueType::String => false,
                    })
            }
            Self::Parenthesized(parenthesized) => {
                parenthesized.expression().can_yield_type(r#type, scope)
            }
            Self::Prefix(prefix) => match prefix.operator() {
                PrefixOperator::LogicalNot(_) => {
                    matches!(r#type, ValueType::Boolean)
                        && prefix.operand().can_yield_type(r#type, scope)
                }
                PrefixOperator::Negate(_) => {
                    matches!(r#type, ValueType::Integer)
                        && prefix.operand().can_yield_type(r#type, scope)
                }
            },
            // TODO: Add support for Lua.
            #[expect(clippy::match_same_arms)]
            Self::Lua(_) => false,
            Self::StringLiteral(_) | Self::MacroStringLiteral(_) => false,
        }
    }

    /// Evaluate at compile-time.
    #[must_use]
    pub fn comptime_eval(&self, scope: &Arc<Scope>) -> Option<ComptimeValue> {
        #[expect(clippy::match_same_arms)]
        match self {
            Self::Boolean(boolean) => Some(ComptimeValue::Boolean(boolean.value())),
            Self::Integer(int) => Some(ComptimeValue::Integer(int.as_i64())),
            Self::StringLiteral(string_literal) => Some(ComptimeValue::String(
                string_literal.str_content().to_string(),
            )),
            Self::Identifier(_) => None,
            Self::Parenthesized(parenthesized) => parenthesized.expression().comptime_eval(scope),
            Self::Prefix(prefix) => prefix.operand().comptime_eval(scope).and_then(|val| {
                match (prefix.operator(), val) {
                    (PrefixOperator::LogicalNot(_), ComptimeValue::Boolean(boolean)) => {
                        Some(ComptimeValue::Boolean(!boolean))
                    }
                    (PrefixOperator::Negate(_), ComptimeValue::Integer(int)) => {
                        Some(ComptimeValue::Integer(-int))
                    }
                    _ => None,
                }
            }),
            // TODO: correctly evaluate lua code
            Self::Lua(lua) => lua
                .eval_string(&VoidHandler)
                .ok()
                .flatten()
                .map(ComptimeValue::String),
            Self::MacroStringLiteral(macro_string_literal) => {
                if macro_string_literal
                    .parts()
                    .iter()
                    .any(|part| matches!(part, MacroStringLiteralPart::MacroUsage { .. }))
                {
                    Some(ComptimeValue::MacroString(
                        macro_string_literal.str_content(),
                    ))
                } else {
                    Some(ComptimeValue::String(macro_string_literal.str_content()))
                }
            }
            // TODO: correctly evaluate function calls
            Self::FunctionCall(_) => None,
        }
    }
}

impl Binary {
    /// Evaluate at compile-time.
    #[must_use]
    pub fn comptime_eval(&self, scope: &Arc<Scope>) -> Option<ComptimeValue> {
        let left = self.left_operand().comptime_eval(scope)?;
        let right = self.right_operand().comptime_eval(scope)?;

        match (left, right) {
            (ComptimeValue::Boolean(left), ComptimeValue::Boolean(right)) => {
                match self.operator() {
                    BinaryOperator::Equal(..) => Some(ComptimeValue::Boolean(left == right)),
                    BinaryOperator::NotEqual(..) => Some(ComptimeValue::Boolean(left != right)),
                    BinaryOperator::LogicalAnd(..) => Some(ComptimeValue::Boolean(left && right)),
                    BinaryOperator::LogicalOr(..) => Some(ComptimeValue::Boolean(left || right)),
                    _ => None,
                }
            }
            (ComptimeValue::Boolean(true), _) | (_, ComptimeValue::Boolean(true)) => {
                if matches!(self.operator(), BinaryOperator::LogicalOr(..))
                    && self
                        .left_operand()
                        .can_yield_type(ValueType::Boolean, scope)
                    && self
                        .right_operand()
                        .can_yield_type(ValueType::Boolean, scope)
                {
                    Some(ComptimeValue::Boolean(true))
                } else {
                    None
                }
            }
            (ComptimeValue::Boolean(false), _) | (_, ComptimeValue::Boolean(false)) => {
                if matches!(self.operator(), BinaryOperator::LogicalAnd(..)) {
                    Some(ComptimeValue::Boolean(false))
                } else {
                    None
                }
            }
            (ComptimeValue::Integer(left), ComptimeValue::Integer(right)) => {
                match self.operator() {
                    BinaryOperator::Add(..) => left.checked_add(right).map(ComptimeValue::Integer),
                    BinaryOperator::Subtract(..) => {
                        left.checked_sub(right).map(ComptimeValue::Integer)
                    }
                    BinaryOperator::Multiply(..) => {
                        left.checked_mul(right).map(ComptimeValue::Integer)
                    }
                    BinaryOperator::Divide(..) => {
                        left.checked_div(right).map(ComptimeValue::Integer)
                    }
                    BinaryOperator::Modulo(..) => {
                        left.checked_rem(right).map(ComptimeValue::Integer)
                    }
                    BinaryOperator::Equal(..) => Some(ComptimeValue::Boolean(left == right)),
                    BinaryOperator::NotEqual(..) => Some(ComptimeValue::Boolean(left != right)),
                    BinaryOperator::LessThan(..) => Some(ComptimeValue::Boolean(left < right)),
                    BinaryOperator::LessThanOrEqual(..) => {
                        Some(ComptimeValue::Boolean(left <= right))
                    }
                    BinaryOperator::GreaterThan(..) => Some(ComptimeValue::Boolean(left > right)),
                    BinaryOperator::GreaterThanOrEqual(..) => {
                        Some(ComptimeValue::Boolean(left >= right))
                    }
                    _ => None,
                }
            }
            (ComptimeValue::String(left), ComptimeValue::String(right)) => match self.operator() {
                BinaryOperator::Add(..) => Some(ComptimeValue::String(left + &right)),
                BinaryOperator::Equal(..) => Some(ComptimeValue::Boolean(left == right)),
                BinaryOperator::NotEqual(..) => Some(ComptimeValue::Boolean(left != right)),
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl Transpiler {
    /// Initializes a constant score.
    pub(crate) fn initialize_constant_score(&mut self, constant: i64) {
        if self.initialized_constant_scores.is_empty() {
            self.datapack
                .register_scoreboard("shu_constants", None::<&str>, None::<&str>);
        }

        if self.initialized_constant_scores.insert(constant) {
            self.setup_cmds.push(Command::Raw(format!(
                "scoreboard players set {constant} shu_constants {constant}"
            )));
        }
    }

    /// Transpiles an expression.
    pub(super) fn transpile_expression(
        &mut self,
        expression: &Expression,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match expression {
            Expression::Primary(primary) => {
                self.transpile_primary_expression(primary, target, scope, handler)
            }
            Expression::Binary(binary) => {
                self.transpile_binary_expression(binary, target, scope, handler)
            }
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_primary_expression(
        &mut self,
        primary: &Primary,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match primary {
            Primary::Boolean(boolean) => match target {
                DataLocation::Tag { tag_name, entity } => {
                    let cmd = format!(
                        "tag {target} {op} {tag}",
                        target = entity,
                        op = if boolean.value() { "add" } else { "remove" },
                        tag = tag_name
                    );
                    Ok(vec![shulkerbox::prelude::Command::Raw(cmd)])
                }
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    let cmd = format!(
                        "data modify storage {storage} {path} set value {value}{suffix}",
                        storage = storage_name,
                        path = path,
                        value = if boolean.value() { "1" } else { "0" },
                        suffix = r#type.suffix()
                    );
                    Ok(vec![shulkerbox::prelude::Command::Raw(cmd)])
                }
                DataLocation::ScoreboardValue { objective, target } => {
                    let cmd = format!(
                        "scoreboard players set {target} {objective} {value}",
                        target = target,
                        objective = objective,
                        value = if boolean.value() { "1" } else { "0" }
                    );
                    Ok(vec![shulkerbox::prelude::Command::Raw(cmd)])
                }
            },
            Primary::FunctionCall(func) => match target {
                DataLocation::ScoreboardValue { objective, target } => {
                    let call_cmd = self.transpile_function_call(func, scope, handler)?;
                    Ok(vec![Command::Execute(Execute::Store(
                        format!("result score {target} {objective}").into(),
                        Box::new(Execute::Run(Box::new(call_cmd))),
                    ))])
                }
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    let call_cmd = self.transpile_function_call(func, scope, handler)?;
                    let result_success = if matches!(r#type, StorageType::Boolean) {
                        "success"
                    } else {
                        "result"
                    };
                    Ok(vec![Command::Execute(Execute::Store(
                        format!(
                            "{result_success} storage {storage_name} {path} {type} 1.0d",
                            r#type = r#type.as_str()
                        )
                        .into(),
                        Box::new(Execute::Run(Box::new(call_cmd))),
                    ))])
                }
                DataLocation::Tag { tag_name, entity } => {
                    if func
                        .arguments()
                        .as_ref()
                        .is_some_and(|args| !args.is_empty())
                    {
                        Err(TranspileError::FunctionArgumentsNotAllowed(
                            FunctionArgumentsNotAllowed {
                                arguments: func.arguments().as_ref().unwrap().span(),
                                message: "Assigning results to a tag does not support arguments."
                                    .into(),
                            },
                        ))
                    } else {
                        let prepare_cmd = Command::Raw(format!("tag {entity} remove {tag_name}"));
                        let success_cmd = Command::Raw(format!("tag {entity} add {tag_name}"));
                        let (function_location, _) = self.get_or_transpile_function(
                            &func.identifier().span,
                            None,
                            scope,
                            handler,
                        )?;
                        let if_cmd = Command::Execute(Execute::If(
                            Condition::Atom(format!("function {function_location}").into()),
                            Box::new(Execute::Run(Box::new(success_cmd))),
                            None,
                        ));

                        Ok(vec![prepare_cmd, if_cmd])
                    }
                }
            },
            Primary::Integer(int) => match target {
                DataLocation::ScoreboardValue { objective, target } => {
                    Ok(vec![Command::Raw(format!(
                        "scoreboard players set {target} {objective} {value}",
                        target = target,
                        objective = objective,
                        value = int.as_i64()
                    ))])
                }
                DataLocation::Tag { .. } => Err(TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: ValueType::Boolean,
                    expression: primary.span(),
                })),
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    if matches!(
                        r#type,
                        StorageType::Byte
                            | StorageType::Double
                            | StorageType::Int
                            | StorageType::Long
                    ) {
                        Ok(vec![Command::Raw(format!(
                            "data modify storage {storage} {path} set value {value}{suffix}",
                            storage = storage_name,
                            path = path,
                            value = int.as_i64(),
                            suffix = r#type.suffix()
                        ))])
                    } else {
                        Err(TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: primary.span(),
                            expected_type: ValueType::Integer,
                        }))
                    }
                }
            },
            Primary::Parenthesized(parenthesized) => {
                self.transpile_expression(parenthesized.expression(), target, scope, handler)
            }
            Primary::Lua(_) => {
                // TODO: Add support for Lua.
                Err(TranspileError::UnexpectedExpression(UnexpectedExpression(
                    Expression::Primary(primary.clone()),
                )))
            }
            Primary::StringLiteral(_) | Primary::MacroStringLiteral(_) => {
                Err(TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: target.value_type(),
                    expression: primary.span(),
                }))
            }
            Primary::Prefix(prefix) => match prefix.operator() {
                PrefixOperator::Negate(_) => match target {
                    DataLocation::ScoreboardValue {
                        objective,
                        target: score_target,
                    } => {
                        let mut expr_cmds = self.transpile_primary_expression(
                            prefix.operand(),
                            target,
                            scope,
                            handler,
                        )?;
                        self.initialize_constant_score(-1);
                        let negate_cmd = Command::Raw(format!("scoreboard players operation {score_target} {objective} *= -1 shu_constants"));
                        expr_cmds.push(negate_cmd);

                        Ok(expr_cmds)
                    }
                    _ => todo!("Negate operator for other types"),
                },
                PrefixOperator::LogicalNot(_) => todo!("Logical not operator"),
            },
            Primary::Identifier(ident) => {
                let variable = scope.get_variable(ident.span.str());
                #[expect(clippy::option_if_let_else)]
                if let Some(variable) = variable.as_deref() {
                    match variable {
                        VariableData::BooleanStorage { storage_name, path } => match target {
                            DataLocation::ScoreboardValue { objective, target } => {
                                let cmd = Command::Execute(Execute::Store(
                                    format!("store result score {target} {objective}").into(),
                                    Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                        "data get storage {storage_name} {path}"
                                    ))))),
                                ));
                                Ok(vec![cmd])
                            }
                            DataLocation::Tag { tag_name, entity } => {
                                let cmd = Command::Execute(Execute::If(
                                    Condition::Atom(
                                        format!("data storage {storage_name} {{{path}: 1b}}")
                                            .into(),
                                    ),
                                    Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                        "tag {entity} add {tag_name}"
                                    ))))),
                                    Some(Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                        "tag {entity} remove {tag_name}"
                                    )))))),
                                ));

                                Ok(vec![cmd])
                            }
                            DataLocation::Storage {
                                storage_name: target_storage_name,
                                path: target_path,
                                r#type,
                            } => {
                                if matches!(r#type, StorageType::Boolean) {
                                    let cmd = Command::Raw(format!(
                                        "data modify storage {target_storage_name} {target_path} set from storage {storage_name} {path}"
                                    ));
                                    Ok(vec![cmd])
                                } else {
                                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                        expression: primary.span(),
                                        expected_type: target.value_type(),
                                    });
                                    handler.receive(err.clone());
                                    Err(err)
                                }
                            }
                        },
                        VariableData::ScoreboardValue {
                            objective,
                            target: score_target,
                        } => match target {
                            DataLocation::ScoreboardValue {
                                objective: target_objective,
                                target: target_target,
                            } => {
                                let cmd = Command::Raw(format!(
                                    "scoreboard players operation {target_target} {target_objective} = {score_target} {objective}"
                                ));
                                Ok(vec![cmd])
                            }
                            DataLocation::Storage {
                                storage_name,
                                path,
                                r#type,
                            } => {
                                if matches!(
                                    r#type,
                                    StorageType::Byte
                                        | StorageType::Double
                                        | StorageType::Int
                                        | StorageType::Long
                                ) {
                                    let cmd = Command::Raw(format!(
                                        "data modify storage {storage_name} {path} set value {value}{suffix}",
                                        value = score_target,
                                        suffix = r#type.suffix()
                                    ));
                                    Ok(vec![cmd])
                                } else {
                                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                        expression: primary.span(),
                                        expected_type: target.value_type(),
                                    });
                                    handler.receive(err.clone());
                                    Err(err)
                                }
                            }
                            DataLocation::Tag { .. } => {
                                let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                    expected_type: ValueType::Boolean,
                                    expression: primary.span(),
                                });
                                handler.receive(err.clone());
                                Err(err)
                            }
                        },
                        _ => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: target.value_type(),
                                expression: primary.span(),
                            });
                            handler.receive(err.clone());
                            Err(err)
                        }
                    }
                } else {
                    let err = TranspileError::UnknownIdentifier(UnknownIdentifier {
                        identifier: ident.span.clone(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_binary_expression(
        &mut self,
        binary: &Binary,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match binary.comptime_eval(scope) {
            Some(ComptimeValue::Integer(value)) => match target {
                DataLocation::ScoreboardValue { objective, target } => Ok(vec![Command::Raw(
                    format!("scoreboard players set {target} {objective} {value}"),
                )]),
                DataLocation::Tag { .. } => Err(TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: ValueType::Boolean,
                    expression: binary.span(),
                })),
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    if matches!(
                        r#type,
                        StorageType::Byte
                            | StorageType::Double
                            | StorageType::Int
                            | StorageType::Long
                    ) {
                        Ok(vec![Command::Raw(format!(
                            "data modify storage {storage_name} {path} set value {value}{suffix}",
                            suffix = r#type.suffix(),
                        ))])
                    } else {
                        Err(TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: binary.span(),
                            expected_type: target.value_type(),
                        }))
                    }
                }
            },
            Some(ComptimeValue::Boolean(value)) => match target {
                DataLocation::ScoreboardValue { objective, target } => {
                    Ok(vec![Command::Raw(format!(
                        "scoreboard players set {target} {objective} {value}",
                        value = u8::from(value)
                    ))])
                }
                DataLocation::Tag { tag_name, entity } => Ok(vec![Command::Raw(format!(
                    "tag {entity} {op} {tag_name}",
                    op = if value { "add" } else { "remove" }
                ))]),
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    if matches!(r#type, StorageType::Boolean) {
                        Ok(vec![Command::Raw(format!(
                            "data modify storage {storage_name} {path} set value {value}{suffix}",
                            value = u8::from(value),
                            suffix = r#type.suffix(),
                        ))])
                    } else {
                        Err(TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: binary.span(),
                            expected_type: target.value_type(),
                        }))
                    }
                }
            },
            Some(ComptimeValue::String(_) | ComptimeValue::MacroString(_)) => {
                Err(TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: target.value_type(),
                    expression: binary.span(),
                }))
            }
            None => {
                let left = binary.left_operand();
                let right = binary.right_operand();
                let operator = binary.operator();

                let (temp_objective, temp_locations) = self.get_temp_scoreboard_locations(2);

                let score_target_location = match target {
                    DataLocation::ScoreboardValue { objective, target } => (objective, target),
                    _ => (&temp_objective, &temp_locations[0]),
                };

                let left_cmds = self.transpile_expression(
                    left,
                    &DataLocation::ScoreboardValue {
                        objective: score_target_location.0.clone(),
                        target: score_target_location.1.clone(),
                    },
                    scope,
                    handler,
                )?;
                let right_cmds = self.transpile_expression(
                    right,
                    &DataLocation::ScoreboardValue {
                        objective: temp_objective.clone(),
                        target: temp_locations[1].clone(),
                    },
                    scope,
                    handler,
                )?;

                let calc_cmds = match operator {
                    BinaryOperator::Add(_) => {
                        vec![Command::Raw(format!(
                            "scoreboard players operation {target} {target_objective} += {source} {source_objective}",
                            target = score_target_location.1,
                            target_objective = score_target_location.0,
                            source = temp_locations[1],
                            source_objective = temp_objective
                        ))]
                    }
                    BinaryOperator::Subtract(_) => {
                        vec![Command::Raw(format!(
                            "scoreboard players operation {target} {target_objective} -= {source} {source_objective}",
                            target = score_target_location.1,
                            target_objective = score_target_location.0,
                            source = temp_locations[1],
                            source_objective = temp_objective
                        ))]
                    }
                    BinaryOperator::Multiply(_) => {
                        vec![Command::Raw(format!(
                            "scoreboard players operation {target} {target_objective} *= {source} {source_objective}",
                            target = score_target_location.1,
                            target_objective = score_target_location.0,
                            source = temp_locations[1],
                            source_objective = temp_objective
                        ))]
                    }
                    BinaryOperator::Divide(_) => {
                        vec![Command::Raw(format!(
                            "scoreboard players operation {target} {target_objective} /= {source} {source_objective}",
                            target = score_target_location.1,
                            target_objective = score_target_location.0,
                            source = temp_locations[1],
                            source_objective = temp_objective
                        ))]
                    }
                    BinaryOperator::Modulo(_) => {
                        vec![Command::Raw(format!(
                            "scoreboard players operation {target} {target_objective} %= {source} {source_objective}",
                            target = score_target_location.1,
                            target_objective = score_target_location.0,
                            source = temp_locations[1],
                            source_objective = temp_objective
                        ))]
                    }
                    _ => todo!("Transpile binary expression"),
                };

                let transfer_cmd = match target {
                    DataLocation::ScoreboardValue { .. } => None,
                    DataLocation::Tag { .. } => {
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expected_type: ValueType::Boolean,
                            expression: binary.span(),
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    DataLocation::Storage {
                        storage_name,
                        path,
                        r#type,
                    } => match r#type {
                        StorageType::Byte
                        | StorageType::Double
                        | StorageType::Int
                        | StorageType::Long => Some(Command::Execute(Execute::Store(
                            format!(
                                "result storage {storage_name} {path} {t} 1",
                                t = r#type.as_str()
                            )
                            .into(),
                            Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "scoreboard players get {target} {objective}",
                                objective = score_target_location.0,
                                target = score_target_location.1
                            ))))),
                        ))),
                        StorageType::Boolean => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: ValueType::Boolean,
                                expression: binary.span(),
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                    },
                };

                Ok(left_cmds
                    .into_iter()
                    .chain(right_cmds)
                    .chain(calc_cmds)
                    .chain(transfer_cmd)
                    .collect())
            }
        }
    }

    /// Get temporary scoreboard locations.
    fn get_temp_scoreboard_locations(&mut self, amount: usize) -> (String, Vec<String>) {
        let objective = "shu_temp_".to_string()
            + &chksum_md5::hash(&self.main_namespace_name).to_hex_lowercase();

        self.datapack
            .register_scoreboard(&objective, None::<&str>, None::<&str>);

        let targets = (0..amount)
            .map(|i| {
                chksum_md5::hash(format!("{}\0{i}", self.main_namespace_name))
                    .to_hex_lowercase()
                    .split_off(16)
            })
            .collect();

        (objective, targets)
    }
}
