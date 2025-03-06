//! The expression transpiler.

use std::fmt::Display;

use crate::syntax::syntax_tree::expression::{Expression, Primary};

#[cfg(feature = "shulkerbox")]
use std::sync::Arc;

#[cfg(feature = "shulkerbox")]
use shulkerbox::prelude::{Command, Condition, Execute};

#[cfg(feature = "shulkerbox")]
use super::{error::MismatchedTypes, TranspileResult, Transpiler};
#[cfg(feature = "shulkerbox")]
use crate::{
    base::{self, source_file::SourceElement, Handler},
    semantic::error::UnexpectedExpression,
    transpile::{error::FunctionArgumentsNotAllowed, TranspileError},
};

/// The type of an expression.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum ValueType {
    ScoreboardValue,
    Tag,
    NumberStorage,
    BooleanStorage,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ScoreboardValue => write!(f, "scoreboard value"),
            Self::Tag => write!(f, "tag"),
            Self::BooleanStorage => write!(f, "boolean storage"),
            Self::NumberStorage => write!(f, "number storage"),
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
    pub fn can_yield_type(&self, r#type: ValueType) -> bool {
        match self {
            Self::Primary(primary) => primary.can_yield_type(r#type),
        }
    }
}

impl Primary {
    /// Returns whether the primary can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType) -> bool {
        match self {
            Self::Boolean(_) => matches!(r#type, ValueType::Tag | ValueType::BooleanStorage),
            Self::Integer(_) => matches!(r#type, ValueType::ScoreboardValue),
            Self::FunctionCall(_) => matches!(
                r#type,
                ValueType::ScoreboardValue | ValueType::Tag | ValueType::BooleanStorage
            ),
            // TODO: Add support for Lua.
            #[expect(clippy::match_same_arms)]
            Self::Lua(_) => false,
            Self::StringLiteral(_) | Self::MacroStringLiteral(_) => false,
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl Transpiler {
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
                    expected_type: ValueType::Tag,
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
                            expected_type: ValueType::NumberStorage,
                        }))
                    }
                }
            },
            Primary::Lua(_) => {
                // TODO: Add support for Lua.
                Err(TranspileError::UnexpectedExpression(UnexpectedExpression(
                    Expression::Primary(primary.clone()),
                )))
            }
            Primary::StringLiteral(_) | Primary::MacroStringLiteral(_) => {
                Err(TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: match target {
                        DataLocation::ScoreboardValue { .. } => ValueType::ScoreboardValue,
                        DataLocation::Tag { .. } => ValueType::Tag,
                        DataLocation::Storage { .. } => ValueType::NumberStorage,
                    },
                    expression: primary.span(),
                }))
            }
        }
    }
}
