//! The expression transpiler.

use std::fmt::Display;

#[cfg(feature = "shulkerbox")]
use std::collections::BTreeMap;

#[cfg(feature = "shulkerbox")]
use enum_as_inner::EnumAsInner;

#[cfg(feature = "shulkerbox")]
use shulkerbox::prelude::{Command, Condition, Execute};

#[cfg(feature = "shulkerbox")]
use super::{
    error::{
        IllegalIndexing, IllegalIndexingReason, MismatchedTypes, NotComptime, UnknownIdentifier,
    },
    util::MacroString,
    Scope, TranspileResult, Transpiler, VariableData,
};
#[cfg(feature = "shulkerbox")]
use crate::{
    base::{self, source_file::SourceElement, Handler, VoidHandler},
    lexical::token::{Identifier, StringLiteral},
    syntax::syntax_tree::expression::{
        Binary, BinaryOperator, Expression, Indexed, MemberAccess, Parenthesized, PrefixOperator,
        Primary,
    },
    transpile::{
        conversions::ShulkerboxMacroStringMap,
        error::{FunctionArgumentsNotAllowed, MissingValue},
        variables::FunctionVariableDataType,
        TranspileError,
    },
};
#[cfg(feature = "shulkerbox")]
use std::sync::Arc;

/// Compile-time evaluated value
#[cfg(feature = "shulkerbox")]
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComptimeValue {
    Boolean(bool),
    Integer(i64),
    String(String),
    MacroString(MacroString),
}

#[cfg(feature = "shulkerbox")]
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

    /// Returns the value as a [`MacroString`].
    #[must_use]
    pub fn to_macro_string(&self) -> MacroString {
        match self {
            Self::Boolean(boolean) => MacroString::String(boolean.to_string()),
            Self::Integer(int) => MacroString::String(int.to_string()),
            Self::String(string) => MacroString::String(string.clone()),
            Self::MacroString(macro_string) => macro_string.clone(),
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

#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExpectedType {
    Boolean,
    Integer,
    String,
    Any,
    AnyOf(Vec<ExpectedType>),
}

impl ExpectedType {
    /// Add another expected type to the list of expected types.
    #[must_use]
    pub fn or(self, or: Self) -> Self {
        match self {
            Self::Boolean | Self::Integer | Self::String => match or {
                Self::Boolean | Self::Integer | Self::String => Self::AnyOf(vec![self, or]),
                Self::Any => Self::Any,
                Self::AnyOf(mut types) => {
                    types.push(self);
                    Self::AnyOf(types)
                }
            },
            Self::Any => Self::Any,
            Self::AnyOf(mut types) => {
                types.push(or);
                Self::AnyOf(types)
            }
        }
    }
}

impl Display for ExpectedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::String => write!(f, "string"),
            Self::Any => write!(f, "any"),
            Self::AnyOf(types) => {
                write!(f, "any of [")?;
                for (i, r#type) in types.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{type}")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl From<ValueType> for ExpectedType {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::Boolean => Self::Boolean,
            ValueType::Integer => Self::Integer,
            ValueType::String => Self::String,
        }
    }
}

/// Location of data
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
                StorageType::String => ValueType::String,
                StorageType::Double => todo!("Double storage type"),
            },
        }
    }

    /// Returns the storage type of the data location.
    #[must_use]
    pub fn storage_type(&self) -> StorageType {
        match self {
            Self::ScoreboardValue { .. } => StorageType::Int,
            Self::Tag { .. } => StorageType::Boolean,
            Self::Storage { r#type, .. } => *r#type,
        }
    }
}

/// The type of a storage.
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StorageType {
    Boolean,
    Byte,
    Int,
    Long,
    Double,
    String,
}

impl StorageType {
    /// Returns the suffix of the storage type.
    #[must_use]
    pub fn suffix(&self) -> &'static str {
        match self {
            Self::Boolean | Self::Byte => "b",
            Self::Int | Self::String => "",
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
            Self::String => "string",
        }
    }
}

/// Condition
#[cfg(feature = "shulkerbox")]
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum ExtendedCondition {
    /// Runtime condition
    Runtime(Condition),
    /// Compile-time condition
    Comptime(bool),
}

#[cfg(feature = "shulkerbox")]
impl Expression {
    /// Returns whether the expression can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType, scope: &Arc<Scope>) -> bool {
        match self {
            Self::Primary(primary) => primary.can_yield_type(r#type, scope),
            Self::Binary(binary) => binary.can_yield_type(r#type, scope),
        }
    }

    /// Evaluate at compile-time.
    ///
    /// # Errors
    /// - If the expression is not compile-time evaluatable.
    pub fn comptime_eval(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self {
            Self::Primary(primary) => primary.comptime_eval(scope, handler),
            Self::Binary(binary) => binary.comptime_eval(scope, handler),
        }
    }
}

#[cfg(feature = "shulkerbox")]
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
                    .is_some_and(|variable| match r#type {
                        ValueType::Boolean => {
                            matches!(variable.as_ref(), VariableData::BooleanStorage { .. })
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
                PrefixOperator::Run(_) => {
                    matches!(r#type, ValueType::Integer | ValueType::Boolean)
                        && prefix.operand().can_yield_type(ValueType::String, scope)
                }
            },
            Self::Indexed(indexed) => {
                if let Self::Identifier(ident) = indexed.object().as_ref() {
                    scope
                        .get_variable(ident.span.str())
                        .is_some_and(|variable| match r#type {
                            ValueType::Boolean => {
                                matches!(
                                    variable.as_ref(),
                                    VariableData::Tag { .. }
                                        | VariableData::BooleanStorageArray { .. }
                                )
                            }
                            ValueType::Integer => {
                                matches!(
                                    variable.as_ref(),
                                    VariableData::Scoreboard { .. }
                                        | VariableData::ScoreboardArray { .. }
                                )
                            }
                            ValueType::String => false,
                        })
                } else {
                    false
                }
            }
            Self::MemberAccess(member_access) => member_access.can_yield_type(r#type, scope),
            #[cfg_attr(not(feature = "lua"), expect(unused_variables))]
            Self::Lua(lua) => {
                cfg_if::cfg_if! {
                    if #[cfg(feature = "lua")] {
                        lua.eval(scope, &VoidHandler).is_ok_and(|(value, _)| match value {
                            mlua::Value::Boolean(_) => matches!(r#type, ValueType::Boolean),
                            mlua::Value::Integer(_) => matches!(r#type, ValueType::Integer),
                            mlua::Value::String(_) => matches!(r#type, ValueType::String),
                            _ => false,
                        })
                    } else {
                        false
                    }
                }
            }
            Self::StringLiteral(_) | Self::TemplateStringLiteral(_) => {
                matches!(r#type, ValueType::String | ValueType::Boolean)
            }
        }
    }

    /// Evaluate at compile-time.
    ///
    /// # Errors
    /// - If the expression is not compile-time evaluatable.
    pub fn comptime_eval(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self {
            Self::Boolean(boolean) => Ok(ComptimeValue::Boolean(boolean.value())),
            Self::Integer(int) => Ok(ComptimeValue::Integer(int.as_i64())),
            Self::StringLiteral(string_literal) => Ok(ComptimeValue::String(
                string_literal.str_content().to_string(),
            )),
            Self::Identifier(ident) => scope.get_variable(ident.span.str()).map_or_else(
                || {
                    Err(NotComptime {
                        expression: self.span(),
                    })
                },
                |var| match var.as_ref() {
                    VariableData::ComptimeValue {
                        value,
                        read_only: _,
                    } => value.read().unwrap().clone().ok_or_else(|| NotComptime {
                        expression: ident.span.clone(),
                    }),
                    _ => Err(NotComptime {
                        expression: self.span(),
                    }),
                },
            ),
            Self::FunctionCall(_) | Self::Indexed(_) => Err(NotComptime {
                expression: self.span(),
            }),
            Self::Parenthesized(parenthesized) => {
                parenthesized.expression().comptime_eval(scope, handler)
            }
            Self::MemberAccess(member_access) => {
                member_access
                    .parent()
                    .comptime_member_access(member_access, scope, handler)
            }
            Self::Prefix(prefix) => {
                prefix
                    .operand()
                    .comptime_eval(scope, handler)
                    .and_then(|val| match (prefix.operator(), val) {
                        (PrefixOperator::LogicalNot(_), ComptimeValue::Boolean(boolean)) => {
                            Ok(ComptimeValue::Boolean(!boolean))
                        }
                        (PrefixOperator::Negate(_), ComptimeValue::Integer(int)) => {
                            Ok(ComptimeValue::Integer(-int))
                        }
                        _ => Err(NotComptime {
                            expression: prefix.span(),
                        }),
                    })
            }
            Self::Lua(lua) => lua
                .eval_comptime(scope, &VoidHandler)
                .inspect_err(|err| {
                    handler.receive(Box::new(err.clone()));
                })
                .map_err(|_| NotComptime {
                    expression: lua.span(),
                })
                .and_then(|val| val),
            Self::TemplateStringLiteral(template_string_literal) => {
                if template_string_literal.contains_expression() {
                    template_string_literal
                        .to_macro_string(None, scope, handler)
                        .map(ComptimeValue::MacroString)
                        .map_err(|_| NotComptime {
                            expression: template_string_literal.span(),
                        })
                } else {
                    Ok(ComptimeValue::String(
                        template_string_literal
                            .as_str(scope, handler)
                            .map_err(|_| NotComptime {
                                expression: template_string_literal.span(),
                            })?
                            .into_owned(),
                    ))
                }
            }
        }
    }

    pub(super) fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self {
            Self::StringLiteral(s) => s.comptime_member_access(member_access, scope, handler),
            Self::Identifier(ident) => ident.comptime_member_access(member_access, scope, handler),
            Self::Indexed(idx) => idx.comptime_member_access(member_access, scope, handler),
            Self::Parenthesized(paren) => {
                paren.comptime_member_access(member_access, scope, handler)
            }

            Self::Boolean(_)
            | Self::FunctionCall(_)
            | Self::Integer(_)
            | Self::Lua(_)
            | Self::TemplateStringLiteral(_)
            | Self::MemberAccess(_)
            | Self::Prefix(_) => Err(NotComptime {
                expression: self.span(),
            }),
        }
    }

    fn member_access_can_yield_type(
        &self,
        member_access: &MemberAccess,
        r#type: ValueType,
        scope: &Arc<Scope>,
    ) -> bool {
        match self {
            Self::StringLiteral(_) => {
                StringLiteral::member_access_can_yield_type(member_access, r#type, scope)
            }
            Self::Identifier(ident) => {
                ident.member_access_can_yield_type(member_access, r#type, scope)
            }
            Self::Indexed(idx) => idx.member_access_can_yield_type(member_access, r#type, scope),
            Self::Parenthesized(paren) => {
                paren.member_access_can_yield_type(member_access, r#type, scope)
            }

            Self::Boolean(_)
            | Self::FunctionCall(_)
            | Self::Integer(_)
            | Self::Lua(_)
            | Self::TemplateStringLiteral(_)
            | Self::MemberAccess(_)
            | Self::Prefix(_) => false,
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl StringLiteral {
    fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        _scope: &Arc<Scope>,
        _handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match member_access.member().span.str() {
            "length" => Ok(ComptimeValue::Integer(
                i64::try_from(self.str_content().len())
                    .expect("string literal length should fit in i64"),
            )),
            _ => Err(NotComptime {
                expression: member_access.member().span(),
            }),
        }
    }

    fn member_access_can_yield_type(
        member_access: &MemberAccess,
        r#type: ValueType,
        _scope: &Arc<Scope>,
    ) -> bool {
        match member_access.member().span.str() {
            "length" => r#type == ValueType::Integer,
            _ => false,
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl Identifier {
    #[expect(clippy::too_many_lines)]
    fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        scope: &Arc<Scope>,
        _handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        scope.get_variable(self.span.str()).map_or_else(
            || {
                Err(NotComptime {
                    expression: self.span(),
                })
            },
            |data| match data.as_ref() {
                VariableData::ComptimeValue { value, .. } => {
                    let value = value.read().unwrap();
                    value.as_ref().map_or_else(
                        || {
                            Err(NotComptime {
                                expression: self.span(),
                            })
                        },
                        |value| match value {
                            ComptimeValue::String(s) => match member_access.member().span.str() {
                                "length" => Ok(ComptimeValue::Integer(
                                    i64::try_from(s.len())
                                        .expect("comptime string length should fit in i64"),
                                )),
                                _ => Err(NotComptime {
                                    expression: member_access.member().span(),
                                }),
                            },
                            _ => Err(NotComptime {
                                expression: self.span(),
                            }),
                        },
                    )
                }
                VariableData::BooleanStorage { storage_name, path } => {
                    match member_access.member().span.str() {
                        "storage" => Ok(ComptimeValue::String(storage_name.to_owned())),
                        "path" => Ok(ComptimeValue::String(path.to_owned())),
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::BooleanStorageArray {
                    storage_name,
                    paths: _,
                } => {
                    match member_access.member().span.str() {
                        "storage" => Ok(ComptimeValue::String(storage_name.to_owned())),
                        "paths" => {
                            // TODO: implement when comptime arrays are implemented
                            Err(NotComptime {
                                expression: member_access.member().span(),
                            })
                        }
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::Function {
                    variable_data: FunctionVariableDataType::Simple { path, .. },
                    ..
                } => {
                    match member_access.member().span.str() {
                        "path" => {
                            #[expect(clippy::option_if_let_else)]
                            if let Some(path) = path.get() {
                                Ok(ComptimeValue::String(path.to_owned()))
                            } else {
                                // TODO: add support for non already compiled functions
                                Err(NotComptime {
                                    expression: member_access.member().span(),
                                })
                            }
                        }
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::Function {
                    variable_data: FunctionVariableDataType::ComptimeArguments { .. },
                    ..
                }
                | VariableData::InternalFunction { .. } => Err(NotComptime {
                    expression: member_access.member().span(),
                }),
                VariableData::MacroParameter { macro_name, .. } => {
                    match member_access.member().span.str() {
                        "name" => Ok(ComptimeValue::String(macro_name.to_owned())),
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::Scoreboard { objective } => match member_access.member().span.str() {
                    "objective" => Ok(ComptimeValue::String(objective.to_owned())),
                    _ => Err(NotComptime {
                        expression: member_access.member().span(),
                    }),
                },
                VariableData::ScoreboardArray {
                    objective,
                    targets: _,
                } => {
                    match member_access.member().span.str() {
                        "objective" => Ok(ComptimeValue::String(objective.to_owned())),
                        "targets" => {
                            // TODO: implement when comptime arrays are implemented
                            Err(NotComptime {
                                expression: member_access.member().span(),
                            })
                        }
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::ScoreboardValue { objective, target } => {
                    match member_access.member().span.str() {
                        "objective" => Ok(ComptimeValue::String(objective.to_owned())),
                        "target" => Ok(ComptimeValue::String(target.to_owned())),
                        _ => Err(NotComptime {
                            expression: member_access.member().span(),
                        }),
                    }
                }
                VariableData::Tag { tag_name } => match member_access.member().span.str() {
                    "name" => Ok(ComptimeValue::String(tag_name.to_owned())),
                    _ => Err(NotComptime {
                        expression: member_access.member().span(),
                    }),
                },
            },
        )
    }

    fn member_access_can_yield_type(
        &self,
        member_access: &MemberAccess,
        r#type: ValueType,
        scope: &Arc<Scope>,
    ) -> bool {
        scope
            .get_variable(self.span.str())
            .is_some_and(|data| match data.as_ref() {
                VariableData::ComptimeValue { value, .. } => {
                    let value = value.read().unwrap();
                    value.as_ref().is_some_and(|value| match value {
                        ComptimeValue::String(_) => match member_access.member().span.str() {
                            "length" => r#type == ValueType::Integer,
                            _ => false,
                        },
                        _ => false,
                    })
                }
                VariableData::BooleanStorage { .. } => match member_access.member().span.str() {
                    "path" | "storage" => r#type == ValueType::String,
                    _ => false,
                },
                VariableData::BooleanStorageArray { .. } => {
                    match member_access.member().span.str() {
                        "storage" => r#type == ValueType::String,
                        #[expect(clippy::match_same_arms)]
                        "paths" => {
                            // TODO: implement when comptime arrays are implemented
                            false
                        }
                        _ => false,
                    }
                }
                VariableData::Function { .. } => match member_access.member().span.str() {
                    "path" => r#type == ValueType::String,
                    _ => false,
                },
                VariableData::InternalFunction { .. } => false,
                VariableData::MacroParameter { .. } | VariableData::Tag { .. } => {
                    match member_access.member().span.str() {
                        "name" => r#type == ValueType::String,
                        _ => false,
                    }
                }
                VariableData::Scoreboard { .. } => match member_access.member().span.str() {
                    "objective" => r#type == ValueType::String,
                    _ => false,
                },
                VariableData::ScoreboardArray { .. } => {
                    match member_access.member().span.str() {
                        "objective" => r#type == ValueType::String,
                        #[expect(clippy::match_same_arms)]
                        "targets" => {
                            // TODO: implement when comptime arrays are implemented
                            false
                        }
                        _ => false,
                    }
                }
                VariableData::ScoreboardValue { .. } => match member_access.member().span.str() {
                    "target" | "objective" => r#type == ValueType::String,
                    _ => false,
                },
            })
    }
}

#[cfg(feature = "shulkerbox")]
impl Indexed {
    fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self.object().as_ref() {
            Primary::Identifier(ident) => scope.get_variable(ident.span.str()).map_or_else(
                || {
                    Err(NotComptime {
                        expression: self.span(),
                    })
                },
                |data| match data.as_ref() {
                    VariableData::BooleanStorageArray {
                        storage_name,
                        paths,
                    } => {
                        if let Ok(ComptimeValue::Integer(idx)) =
                            self.index().comptime_eval(scope, handler)
                        {
                            usize::try_from(idx).map_or_else(
                                |_| {
                                    Err(NotComptime {
                                        expression: self.index().span(),
                                    })
                                },
                                |idx| {
                                    paths.get(idx).map_or_else(
                                        || {
                                            Err(NotComptime {
                                                expression: self.span(),
                                            })
                                        },
                                        |path| match member_access.member().span.str() {
                                            "storage" => {
                                                Ok(ComptimeValue::String(storage_name.to_owned()))
                                            }
                                            "path" => Ok(ComptimeValue::String(path.to_owned())),
                                            _ => Err(NotComptime {
                                                expression: member_access.member().span(),
                                            }),
                                        },
                                    )
                                },
                            )
                        } else {
                            Err(NotComptime {
                                expression: self.index().span(),
                            })
                        }
                    }
                    VariableData::ScoreboardArray { objective, targets } => {
                        if let Ok(ComptimeValue::Integer(idx)) =
                            self.index().comptime_eval(scope, handler)
                        {
                            usize::try_from(idx).map_or_else(
                                |_| {
                                    Err(NotComptime {
                                        expression: self.index().span(),
                                    })
                                },
                                |idx| {
                                    targets.get(idx).map_or_else(
                                        || {
                                            Err(NotComptime {
                                                expression: self.span(),
                                            })
                                        },
                                        |target| match member_access.member().span.str() {
                                            "objective" => {
                                                Ok(ComptimeValue::String(objective.to_owned()))
                                            }
                                            "target" => {
                                                Ok(ComptimeValue::String(target.to_owned()))
                                            }
                                            _ => Err(NotComptime {
                                                expression: member_access.member().span(),
                                            }),
                                        },
                                    )
                                },
                            )
                        } else {
                            Err(NotComptime {
                                expression: self.index().span(),
                            })
                        }
                    }
                    _ => Err(NotComptime {
                        expression: self.span(),
                    }),
                },
            ),
            _ => Err(NotComptime {
                expression: self.span(),
            }),
        }
    }

    fn member_access_can_yield_type(
        &self,
        member_access: &MemberAccess,
        r#type: ValueType,
        scope: &Arc<Scope>,
    ) -> bool {
        match self.object().as_ref() {
            Primary::Identifier(ident) => {
                scope
                    .get_variable(ident.span.str())
                    .is_some_and(|data| match data.as_ref() {
                        VariableData::BooleanStorageArray { .. } => {
                            matches!(r#type, ValueType::String)
                                && matches!(member_access.member().span.str(), "storage" | "path")
                                && self.index().can_yield_type(ValueType::Integer, scope)
                        }
                        VariableData::ScoreboardArray { .. } => {
                            matches!(r#type, ValueType::String)
                                && matches!(
                                    member_access.member().span.str(),
                                    "objective" | "target"
                                )
                                && self.index().can_yield_type(ValueType::Integer, scope)
                        }
                        _ => false,
                    })
            }
            _ => false,
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl Parenthesized {
    fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self.expression().as_ref() {
            Expression::Primary(prim) => prim.comptime_member_access(member_access, scope, handler),
            Expression::Binary(bin) => bin.comptime_member_access(member_access, scope, handler),
        }
    }

    fn member_access_can_yield_type(
        &self,
        member_access: &MemberAccess,
        r#type: ValueType,
        scope: &Arc<Scope>,
    ) -> bool {
        match self.expression().as_ref() {
            Expression::Primary(prim) => {
                prim.member_access_can_yield_type(member_access, r#type, scope)
            }
            Expression::Binary(bin) => {
                bin.member_access_can_yield_type(member_access, r#type, scope)
            }
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl MemberAccess {
    /// Returns whether the member access can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType, scope: &Arc<Scope>) -> bool {
        self.parent()
            .member_access_can_yield_type(self, r#type, scope)
    }
}

#[cfg(feature = "shulkerbox")]
impl Binary {
    /// Returns whether the binary can yield a certain type.
    #[must_use]
    pub fn can_yield_type(&self, r#type: ValueType, scope: &Arc<Scope>) -> bool {
        match self.operator() {
            BinaryOperator::Add(_) => {
                matches!(r#type, ValueType::Integer | ValueType::String)
                    && self.left_operand().can_yield_type(r#type, scope)
                    && self.right_operand().can_yield_type(r#type, scope)
            }
            BinaryOperator::Subtract(_)
            | BinaryOperator::Multiply(_)
            | BinaryOperator::Divide(_)
            | BinaryOperator::Modulo(_) => {
                matches!(r#type, ValueType::Integer)
                    && self.left_operand().can_yield_type(r#type, scope)
                    && self.right_operand().can_yield_type(r#type, scope)
            }
            BinaryOperator::Equal(..) | BinaryOperator::NotEqual(..) => {
                matches!(r#type, ValueType::Boolean)
            }
            BinaryOperator::GreaterThan(_)
            | BinaryOperator::GreaterThanOrEqual(..)
            | BinaryOperator::LessThan(_)
            | BinaryOperator::LessThanOrEqual(..) => {
                matches!(r#type, ValueType::Boolean)
                    && self
                        .left_operand()
                        .can_yield_type(ValueType::Integer, scope)
                    && self
                        .right_operand()
                        .can_yield_type(ValueType::Integer, scope)
            }
            BinaryOperator::LogicalAnd(..) | BinaryOperator::LogicalOr(..) => {
                matches!(r#type, ValueType::Boolean)
                    && self.left_operand().can_yield_type(r#type, scope)
                    && self.right_operand().can_yield_type(r#type, scope)
            }
        }
    }

    /// Evaluate at compile-time.
    ///
    /// # Errors
    /// - If the expression is not compile-time evaluatable.
    pub fn comptime_eval(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        let left = self.left_operand().comptime_eval(scope, handler)?;
        let right = self.right_operand().comptime_eval(scope, handler)?;

        match (left, right) {
            (ComptimeValue::Boolean(true), _) | (_, ComptimeValue::Boolean(true))
                if matches!(self.operator(), BinaryOperator::LogicalOr(..))
                    && self
                        .left_operand()
                        .can_yield_type(ValueType::Boolean, scope)
                    && self
                        .right_operand()
                        .can_yield_type(ValueType::Boolean, scope) =>
            {
                Ok(ComptimeValue::Boolean(true))
            }
            (ComptimeValue::Boolean(false), _) | (_, ComptimeValue::Boolean(false))
                if matches!(self.operator(), BinaryOperator::LogicalAnd(..))
                    && self
                        .left_operand()
                        .can_yield_type(ValueType::Boolean, scope)
                    && self
                        .right_operand()
                        .can_yield_type(ValueType::Boolean, scope) =>
            {
                Ok(ComptimeValue::Boolean(false))
            }
            (ComptimeValue::Boolean(left), ComptimeValue::Boolean(right)) => {
                match self.operator() {
                    BinaryOperator::Equal(..) => Ok(ComptimeValue::Boolean(left == right)),
                    BinaryOperator::NotEqual(..) => Ok(ComptimeValue::Boolean(left != right)),
                    BinaryOperator::LogicalAnd(..) => Ok(ComptimeValue::Boolean(left && right)),
                    BinaryOperator::LogicalOr(..) => Ok(ComptimeValue::Boolean(left || right)),
                    _ => Err(NotComptime {
                        expression: self.span(),
                    }),
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
                .ok_or_else(|| NotComptime {
                    expression: self.span(),
                })
            }
            (ComptimeValue::String(left), ComptimeValue::String(right)) => match self.operator() {
                BinaryOperator::Add(..) => Ok(ComptimeValue::String(left + &right)),
                BinaryOperator::Equal(..) => Ok(ComptimeValue::Boolean(left == right)),
                BinaryOperator::NotEqual(..) => Ok(ComptimeValue::Boolean(left != right)),
                _ => Err(NotComptime {
                    expression: self.span(),
                }),
            },
            // TODO: also allow macro strings
            (
                left @ ComptimeValue::String(_),
                right @ (ComptimeValue::Boolean(_) | ComptimeValue::Integer(_)),
            )
            | (
                left @ (ComptimeValue::Boolean(_) | ComptimeValue::Integer(_)),
                right @ ComptimeValue::String(_),
            ) => match self.operator() {
                BinaryOperator::Add(_) => Ok(ComptimeValue::String(
                    left.to_string_no_macro().unwrap() + &right.to_string_no_macro().unwrap(),
                )),
                _ => Err(NotComptime {
                    expression: self.span(),
                }),
            },
            _ => Err(NotComptime {
                expression: self.span(),
            }),
        }
    }

    fn comptime_member_access(
        &self,
        member_access: &MemberAccess,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> Result<ComptimeValue, NotComptime> {
        match self.operator() {
            BinaryOperator::Add(_) => match self.comptime_eval(scope, handler)? {
                ComptimeValue::String(s) => match member_access.member().span.str() {
                    "length" => i64::try_from(s.len()).map_or_else(
                        |_| {
                            Err(NotComptime {
                                expression: self.span(),
                            })
                        },
                        |len| Ok(ComptimeValue::Integer(len)),
                    ),
                    _ => Err(NotComptime {
                        expression: self.span(),
                    }),
                },
                _ => Err(NotComptime {
                    expression: self.span(),
                }),
            },
            _ => Err(NotComptime {
                expression: self.span(),
            }),
        }
    }

    fn member_access_can_yield_type(
        &self,
        member_access: &MemberAccess,
        r#type: ValueType,
        scope: &Arc<Scope>,
    ) -> bool {
        match self.operator() {
            BinaryOperator::Add(_) => {
                r#type == ValueType::Integer
                    && member_access.member().span.str() == "length"
                    && self.left_operand().can_yield_type(ValueType::String, scope)
                    && self
                        .right_operand()
                        .can_yield_type(ValueType::String, scope)
            }
            _ => false,
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

    #[expect(clippy::too_many_lines, clippy::cognitive_complexity)]
    pub(super) fn transpile_primary_expression(
        &mut self,
        primary: &Primary,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match primary {
            Primary::Boolean(boolean) => self.store_comptime_value(
                &ComptimeValue::Boolean(boolean.value()),
                target,
                boolean,
                handler,
            ),
            Primary::FunctionCall(func) => match target {
                DataLocation::ScoreboardValue { objective, target } => {
                    let mut cmds = self.transpile_function_call(func, scope, handler)?;
                    if let Some(call_cmd) = cmds.pop() {
                        let modified = Command::Execute(Execute::Store(
                            format!("result score {target} {objective}").into(),
                            Box::new(Execute::Run(Box::new(call_cmd))),
                        ));
                        cmds.push(modified);
                    }
                    Ok(cmds)
                }
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    let mut cmds = self.transpile_function_call(func, scope, handler)?;
                    if let Some(call_cmd) = cmds.pop() {
                        let result_success = if matches!(r#type, StorageType::Boolean) {
                            "success"
                        } else {
                            "result"
                        };
                        let modified = Command::Execute(Execute::Store(
                            format!(
                                "{result_success} storage {storage_name} {path} {type} 1.0d",
                                r#type = r#type.as_str()
                            )
                            .into(),
                            Box::new(Execute::Run(Box::new(call_cmd))),
                        ));
                        cmds.push(modified);
                    }
                    Ok(cmds)
                }
                DataLocation::Tag { tag_name, entity } => {
                    if func
                        .arguments()
                        .as_ref()
                        .is_some_and(|args| !args.is_empty())
                    {
                        let err = TranspileError::FunctionArgumentsNotAllowed(
                            FunctionArgumentsNotAllowed {
                                arguments: func.arguments().as_ref().unwrap().span(),
                                message: "Assigning results to a tag does not support arguments."
                                    .into(),
                            },
                        );
                        handler.receive(Box::new(err.clone()));
                        Err(err)
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
            Primary::Integer(int) => self.store_comptime_value(
                &ComptimeValue::Integer(int.as_i64()),
                target,
                int,
                handler,
            ),
            Primary::Parenthesized(parenthesized) => {
                self.transpile_expression(parenthesized.expression(), target, scope, handler)
            }
            Primary::MemberAccess(member_access) => member_access
                .parent()
                .comptime_member_access(member_access, scope, handler)
                .map_or_else(
                    |_| {
                        // TODO: implement non-comptime access

                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            expression: member_access.member().span(),
                            reason: IllegalIndexingReason::InvalidIndex {
                                index: member_access.member().span(),
                            },
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    },
                    |value| self.store_comptime_value(&value, target, member_access, handler),
                ),
            Primary::Lua(lua) =>
            {
                #[expect(clippy::option_if_let_else)]
                if let Ok(value) = lua.eval_comptime(scope, handler)? {
                    self.store_comptime_value(&value, target, lua, handler)
                } else {
                    let err = TranspileError::MissingValue(MissingValue {
                        expression: lua.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            }
            Primary::StringLiteral(_) | Primary::TemplateStringLiteral(_) => {
                if matches!(
                    target,
                    DataLocation::Storage {
                        r#type: StorageType::Boolean,
                        ..
                    } | DataLocation::Tag { .. }
                ) {
                    let (mut cmds, prepare_variables, cond) =
                        self.transpile_primary_expression_as_condition(primary, scope, handler)?;

                    let store_cmds =
                        self.store_condition_success(cond, target, primary, handler)?;
                    cmds.extend(store_cmds);

                    self.transpile_commands_with_variable_macros(cmds, prepare_variables, handler)
                } else {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: target.value_type().into(),
                        expression: primary.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
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
                        let negate_cmd = Command::Raw(format!(
                            "scoreboard players operation {score_target} {objective} *= -1 shu_constants"
                        ));
                        expr_cmds.push(negate_cmd);

                        Ok(expr_cmds)
                    }
                    DataLocation::Storage {
                        storage_name,
                        path,
                        r#type,
                    } if matches!(
                        r#type,
                        StorageType::Byte | StorageType::Int | StorageType::Long
                    ) =>
                    {
                        let (target_objective, [target_ident]) =
                            self.get_temp_scoreboard_locations_array();

                        let score_to_storage_cmd = Command::Execute(Execute::Store(
                            format!(
                                "result storage {storage_name} {path} {t} 1.0",
                                t = r#type.as_str()
                            )
                            .into(),
                            Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "scoreboard players get {target_ident} {target_objective} "
                            ))))),
                        ));

                        let mut scoreboard_cmds = self.transpile_primary_expression(
                            primary,
                            &DataLocation::ScoreboardValue {
                                objective: target_objective,
                                target: target_ident,
                            },
                            scope,
                            handler,
                        )?;

                        scoreboard_cmds.push(score_to_storage_cmd);

                        Ok(scoreboard_cmds)
                    }
                    _ => {
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: prefix.span(),
                            expected_type: target.value_type().into(),
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    }
                },
                PrefixOperator::LogicalNot(_) => {
                    let (mut cmds, prepare_variables, cond) =
                        self.transpile_primary_expression_as_condition(primary, scope, handler)?;

                    let store_cmds =
                        self.store_condition_success(cond, target, primary, handler)?;
                    cmds.extend(store_cmds);

                    self.transpile_commands_with_variable_macros(cmds, prepare_variables, handler)
                }
                PrefixOperator::Run(_) => {
                    let run_cmds =
                        self.transpile_run_expression(prefix.operand(), scope, handler)?;
                    let run_cmd = if run_cmds.len() == 1 {
                        run_cmds.into_iter().next().expect("length is 1")
                    } else {
                        use shulkerbox::datapack::Group;

                        Command::Group(Group::new(run_cmds))
                    };
                    match target {
                        DataLocation::ScoreboardValue { objective, target } => {
                            let store = format!("result score {target} {objective}");
                            let exec = Command::Execute(Execute::Store(
                                store.into(),
                                Box::new(Execute::Run(Box::new(run_cmd))),
                            ));
                            Ok(vec![exec])
                        }
                        DataLocation::Storage {
                            storage_name,
                            path,
                            r#type,
                        } => {
                            let store = format!(
                                "{result_success} storage {storage_name} {path} {t} 1.0",
                                t = r#type.as_str(),
                                result_success = if matches!(r#type, StorageType::Boolean) {
                                    "success"
                                } else {
                                    "result"
                                }
                            );
                            let exec = Command::Execute(Execute::Store(
                                store.into(),
                                Box::new(Execute::Run(Box::new(run_cmd))),
                            ));
                            Ok(vec![exec])
                        }
                        DataLocation::Tag { tag_name, entity } => {
                            let prepare_cmd =
                                Command::Raw(format!("tag {entity} remove {tag_name}"));
                            let success_cmd = Command::Raw(format!("tag {entity} add {tag_name}"));
                            let (temp_storage_name, [temp_storage_path]) =
                                self.get_temp_storage_locations_array();

                            let store_cmd = Command::Execute(Execute::Store(
                                format!(
                                    "success storage {temp_storage_name} {temp_storage_path} boolean 1.0"
                                )
                                .into(),
                                Box::new(Execute::Run(Box::new(run_cmd))),
                            ));

                            let if_cmd = Command::Execute(Execute::If(
                                Condition::Atom(
                                    format!("data storage {temp_storage_name} {{{temp_storage_name}:1b}}")
                                        .into(),
                                ),
                                Box::new(Execute::Run(Box::new(success_cmd))),
                                None,
                            ));

                            Ok(vec![store_cmd, prepare_cmd, if_cmd])
                        }
                    }
                }
            },
            Primary::Identifier(ident) => {
                let variable = scope.get_variable(ident.span.str());
                if let Some(variable) = variable.as_deref() {
                    let from = match variable {
                        VariableData::BooleanStorage { storage_name, path } => {
                            Ok(DataLocation::Storage {
                                storage_name: storage_name.to_string(),
                                path: path.to_string(),
                                r#type: StorageType::Boolean,
                            })
                        }
                        VariableData::ScoreboardValue { objective, target } => {
                            Ok(DataLocation::ScoreboardValue {
                                objective: objective.to_string(),
                                target: target.to_string(),
                            })
                        }
                        _ => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: target.value_type().into(),
                                expression: primary.span(),
                            });
                            handler.receive(Box::new(err.clone()));
                            Err(err)
                        }
                    }?;

                    self.move_data(&from, target, primary, handler)
                } else {
                    let err = TranspileError::UnknownIdentifier(UnknownIdentifier::from_scope(
                        ident.span.clone(),
                        scope,
                    ));
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            }
            Primary::Indexed(indexed) => {
                let ident = if let Primary::Identifier(ident) = indexed.object().as_ref() {
                    Ok(ident)
                } else {
                    let err = TranspileError::IllegalIndexing(IllegalIndexing {
                        reason: IllegalIndexingReason::NotIdentifier,
                        expression: indexed.object().span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }?;
                let variable = scope.get_variable(ident.span.str());
                #[expect(clippy::option_if_let_else)]
                if let Some(variable) = variable.as_deref() {
                    let from = match variable {
                        VariableData::Scoreboard { objective } => {
                            if let Ok(ComptimeValue::String(target)) =
                                indexed.index().comptime_eval(scope, handler)
                            {
                                Ok(DataLocation::ScoreboardValue {
                                    objective: objective.to_string(),
                                    target,
                                })
                            } else {
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::String,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(Box::new(err.clone()));
                                Err(err)
                            }
                        }
                        VariableData::ScoreboardArray { objective, targets } => {
                            if let Ok(ComptimeValue::Integer(index)) =
                                indexed.index().comptime_eval(scope, handler)
                            {
                                if let Some(target) = usize::try_from(index)
                                    .ok()
                                    .and_then(|index| targets.get(index))
                                    .map(ToString::to_string)
                                {
                                    Ok(DataLocation::ScoreboardValue {
                                        objective: objective.to_string(),
                                        target,
                                    })
                                } else {
                                    let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                        reason: IllegalIndexingReason::IndexOutOfBounds {
                                            length: targets.len(),
                                            index: usize::try_from(index).unwrap_or(usize::MAX),
                                        },
                                        expression: indexed.index().span(),
                                    });
                                    handler.receive(Box::new(err.clone()));
                                    Err(err)
                                }
                            } else {
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::Integer,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(Box::new(err.clone()));
                                Err(err)
                            }
                        }
                        VariableData::BooleanStorageArray {
                            storage_name,
                            paths,
                        } => {
                            if let Ok(ComptimeValue::Integer(index)) =
                                indexed.index().comptime_eval(scope, handler)
                            {
                                if let Some(path) = usize::try_from(index)
                                    .ok()
                                    .and_then(|index| paths.get(index))
                                    .map(ToString::to_string)
                                {
                                    Ok(DataLocation::Storage {
                                        storage_name: storage_name.to_string(),
                                        path,
                                        r#type: StorageType::Boolean,
                                    })
                                } else {
                                    let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                        reason: IllegalIndexingReason::IndexOutOfBounds {
                                            length: paths.len(),
                                            index: usize::try_from(index).unwrap_or(usize::MAX),
                                        },
                                        expression: indexed.index().span(),
                                    });
                                    handler.receive(Box::new(err.clone()));
                                    Err(err)
                                }
                            } else {
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::Integer,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(Box::new(err.clone()));
                                Err(err)
                            }
                        }
                        _ => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: target.value_type().into(),
                                expression: primary.span(),
                            });
                            handler.receive(Box::new(err.clone()));
                            Err(err)
                        }
                    }?;

                    self.move_data(&from, target, primary, handler)
                } else {
                    let err = TranspileError::UnknownIdentifier(UnknownIdentifier::from_scope(
                        ident.span.clone(),
                        scope,
                    ));
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            }
        }
    }

    pub(super) fn transpile_binary_expression(
        &mut self,
        binary: &Binary,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        if let Ok(value) = binary.comptime_eval(scope, handler) {
            self.store_comptime_value(&value, target, binary, handler)
        } else {
            match binary.operator() {
                BinaryOperator::Add(_)
                | BinaryOperator::Subtract(_)
                | BinaryOperator::Multiply(_)
                | BinaryOperator::Divide(_)
                | BinaryOperator::Modulo(_) => {
                    self.transpile_scoreboard_operation(binary, target, scope, handler)
                }
                BinaryOperator::Equal(..)
                | BinaryOperator::GreaterThan(_)
                | BinaryOperator::GreaterThanOrEqual(..)
                | BinaryOperator::LessThan(_)
                | BinaryOperator::LessThanOrEqual(..)
                | BinaryOperator::NotEqual(..)
                | BinaryOperator::LogicalAnd(..)
                | BinaryOperator::LogicalOr(..) => {
                    let (mut cmds, prepare_variables, cond) =
                        self.transpile_binary_expression_as_condition(binary, scope, handler)?;

                    let store_cmds = self.store_condition_success(cond, target, binary, handler)?;
                    cmds.extend(store_cmds);

                    self.transpile_commands_with_variable_macros(cmds, prepare_variables, handler)
                }
            }
        }
    }

    pub(super) fn transpile_expression_as_condition(
        &mut self,
        expression: &Expression,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(Vec<Command>, ShulkerboxMacroStringMap, ExtendedCondition)> {
        match expression {
            Expression::Primary(primary) => {
                self.transpile_primary_expression_as_condition(primary, scope, handler)
            }
            Expression::Binary(binary) => {
                self.transpile_binary_expression_as_condition(binary, scope, handler)
            }
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_primary_expression_as_condition(
        &mut self,
        primary: &Primary,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(Vec<Command>, ShulkerboxMacroStringMap, ExtendedCondition)> {
        use std::collections::BTreeMap;

        match primary {
            Primary::Boolean(boolean) => Ok((
                Vec::new(),
                BTreeMap::new(),
                ExtendedCondition::Comptime(boolean.value()),
            )),
            Primary::Integer(_) => {
                let err = TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: ExpectedType::Boolean,
                    expression: primary.span(),
                });
                handler.receive(Box::new(err.clone()));
                Err(err)
            }
            Primary::StringLiteral(s) => Ok((
                Vec::new(),
                BTreeMap::new(),
                ExtendedCondition::Runtime(Condition::Atom(s.str_content().to_string().into())),
            )),
            Primary::TemplateStringLiteral(template_string) => {
                let (macro_string, prepare_variables) = template_string
                    .to_macro_string(Some(self), scope, handler)?
                    .into_sb();
                Ok((
                    Vec::new(),
                    prepare_variables,
                    ExtendedCondition::Runtime(Condition::Atom(macro_string)),
                ))
            }
            Primary::FunctionCall(func) => {
                if func
                    .arguments()
                    .as_ref()
                    .is_some_and(|args| !args.is_empty())
                {
                    let err =
                        TranspileError::FunctionArgumentsNotAllowed(FunctionArgumentsNotAllowed {
                            arguments: func.arguments().as_ref().unwrap().span(),
                            message: "Function calls as conditions do not support arguments."
                                .into(),
                        });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                } else {
                    let (func_location, _) = self.get_or_transpile_function(
                        &func.identifier().span,
                        None,
                        scope,
                        handler,
                    )?;

                    Ok((
                        Vec::new(),
                        BTreeMap::new(),
                        ExtendedCondition::Runtime(Condition::Atom(
                            format!("function {func_location}").into(),
                        )),
                    ))
                }
            }
            Primary::Identifier(ident) => {
                #[expect(clippy::option_if_let_else)]
                if let Some(variable) = scope.get_variable(ident.span.str()).as_deref() {
                    match variable {
                        VariableData::BooleanStorage { storage_name, path } => Ok((
                            Vec::new(),
                            BTreeMap::new(),
                            ExtendedCondition::Runtime(Condition::Atom(
                                format!("data storage {storage_name} {{{path}: 1b}}").into(),
                            )),
                        )),
                        VariableData::MacroParameter { macro_name, .. } => Ok((
                            Vec::new(),
                            BTreeMap::new(),
                            ExtendedCondition::Runtime(Condition::Atom(
                                shulkerbox::util::MacroString::MacroString(vec![
                                    shulkerbox::util::MacroStringPart::MacroUsage(
                                        macro_name.clone(),
                                    ),
                                ]),
                            )),
                        )),
                        _ => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: ExpectedType::Boolean,
                                expression: primary.span(),
                            });
                            handler.receive(Box::new(err.clone()));
                            Err(err)
                        }
                    }
                } else {
                    let err = TranspileError::UnknownIdentifier(UnknownIdentifier::from_scope(
                        ident.span.clone(),
                        scope,
                    ));
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            }
            Primary::Parenthesized(parenthesized) => {
                self.transpile_expression_as_condition(parenthesized.expression(), scope, handler)
            }
            Primary::Indexed(indexed) => {
                let ident = if let Primary::Identifier(ident) = indexed.object().as_ref() {
                    Ok(ident)
                } else {
                    let err = TranspileError::IllegalIndexing(IllegalIndexing {
                        reason: IllegalIndexingReason::NotIdentifier,
                        expression: indexed.object().span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }?;
                #[expect(clippy::option_if_let_else)]
                if let Some(variable) = scope.get_variable(ident.span.str()).as_deref() {
                    #[expect(clippy::single_match_else)]
                    match variable {
                        VariableData::BooleanStorageArray {
                            storage_name,
                            paths,
                        } => {
                            if let Ok(ComptimeValue::Integer(index)) =
                                indexed.index().comptime_eval(scope, handler)
                            {
                                if let Some(path) = usize::try_from(index)
                                    .ok()
                                    .and_then(|index| paths.get(index))
                                    .map(ToString::to_string)
                                {
                                    Ok((
                                        Vec::new(),
                                        BTreeMap::new(),
                                        ExtendedCondition::Runtime(Condition::Atom(
                                            format!("data storage {storage_name} {{{path}: 1b}}")
                                                .into(),
                                        )),
                                    ))
                                } else {
                                    let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                        reason: IllegalIndexingReason::IndexOutOfBounds {
                                            length: paths.len(),
                                            index: usize::try_from(index).unwrap_or(usize::MAX),
                                        },
                                        expression: indexed.index().span(),
                                    });
                                    handler.receive(Box::new(err.clone()));
                                    Err(err)
                                }
                            } else {
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::Integer,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(Box::new(err.clone()));
                                Err(err)
                            }
                        }
                        _ => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: ExpectedType::Boolean,
                                expression: primary.span(),
                            });
                            handler.receive(Box::new(err.clone()));
                            Err(err)
                        }
                    }
                } else {
                    let err = TranspileError::UnknownIdentifier(UnknownIdentifier::from_scope(
                        ident.span.clone(),
                        scope,
                    ));
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            }
            Primary::MemberAccess(member_access) => member_access
                .parent()
                .comptime_member_access(member_access, scope, handler)
                .map_or_else(
                    |_| {
                        // TODO: implement non-comptime access

                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            expression: member_access.member().span(),
                            reason: IllegalIndexingReason::InvalidIndex {
                                index: member_access.member().span(),
                            },
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    },
                    |value| match value {
                        ComptimeValue::Boolean(b) => {
                            Ok((Vec::new(), BTreeMap::new(), ExtendedCondition::Comptime(b)))
                        }
                        ComptimeValue::Integer(_) => {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expected_type: ExpectedType::Boolean,
                                expression: primary.span(),
                            });
                            handler.receive(Box::new(err.clone()));
                            Err(err)
                        }
                        ComptimeValue::String(s) => Ok((
                            Vec::new(),
                            BTreeMap::new(),
                            ExtendedCondition::Runtime(Condition::Atom(
                                shulkerbox::util::MacroString::String(s),
                            )),
                        )),
                        ComptimeValue::MacroString(s) => {
                            let (macro_string, prepare_variables) = s.into_sb();
                            Ok((
                                Vec::new(),
                                prepare_variables,
                                ExtendedCondition::Runtime(Condition::Atom(macro_string)),
                            ))
                        }
                    },
                ),
            Primary::Prefix(prefix) => match prefix.operator() {
                PrefixOperator::LogicalNot(_) => {
                    let (cmds, prepare_variables, cond) = self
                        .transpile_primary_expression_as_condition(
                            prefix.operand(),
                            scope,
                            handler,
                        )?;
                    Ok((
                        cmds,
                        prepare_variables,
                        match cond {
                            ExtendedCondition::Runtime(cond) => {
                                ExtendedCondition::Runtime(Condition::Not(Box::new(cond)))
                            }
                            ExtendedCondition::Comptime(cond) => ExtendedCondition::Comptime(!cond),
                        },
                    ))
                }
                PrefixOperator::Run(_) => {
                    let (temp_storage_name, [temp_storage_path]) =
                        self.get_temp_storage_locations_array();
                    let cond = ExtendedCondition::Runtime(Condition::Atom(
                        format!("data storage {temp_storage_name} {{{temp_storage_path}:1b}}")
                            .into(),
                    ));
                    let store_cmds = self.transpile_primary_expression(
                        primary,
                        &DataLocation::Storage {
                            storage_name: temp_storage_name,
                            path: temp_storage_path,
                            r#type: StorageType::Boolean,
                        },
                        scope,
                        handler,
                    )?;
                    Ok((store_cmds, BTreeMap::new(), cond))
                }
                PrefixOperator::Negate(_) => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: primary.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            },
            Primary::Lua(lua) => match lua.eval_comptime(scope, handler)? {
                Ok(ComptimeValue::String(value)) => Ok((
                    Vec::new(),
                    BTreeMap::new(),
                    ExtendedCondition::Runtime(Condition::Atom(value.into())),
                )),
                Ok(ComptimeValue::MacroString(value)) => {
                    let (macro_string, prepare_variables) = value.into_sb();
                    Ok((
                        Vec::new(),
                        prepare_variables,
                        ExtendedCondition::Runtime(Condition::Atom(macro_string)),
                    ))
                }
                Ok(ComptimeValue::Boolean(boolean)) => Ok((
                    Vec::new(),
                    BTreeMap::new(),
                    ExtendedCondition::Comptime(boolean),
                )),
                _ => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: primary.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
                }
            },
        }
    }

    fn transpile_binary_expression_as_condition(
        &mut self,
        binary: &Binary,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(Vec<Command>, ShulkerboxMacroStringMap, ExtendedCondition)> {
        match binary.operator() {
            BinaryOperator::Equal(..)
            | BinaryOperator::NotEqual(..)
            | BinaryOperator::GreaterThan(_)
            | BinaryOperator::GreaterThanOrEqual(..)
            | BinaryOperator::LessThan(_)
            | BinaryOperator::LessThanOrEqual(..) => self
                .transpile_comparison_operator(binary, scope, handler)
                .map(|(cmds, prepare_variables, cond)| {
                    (cmds, prepare_variables, ExtendedCondition::Runtime(cond))
                }),
            BinaryOperator::LogicalAnd(..) | BinaryOperator::LogicalOr(..) => {
                self.transpile_logic_operator(binary, scope, handler)
            }
            _ => {
                let err = TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: ExpectedType::Boolean,
                    expression: binary.span(),
                });
                handler.receive(Box::new(err.clone()));
                Err(err)
            }
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_scoreboard_operation(
        &mut self,
        binary: &Binary,
        target: &DataLocation,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let left = binary.left_operand();
        let right = binary.right_operand();
        let operator = binary.operator();

        let (temp_objective, [temp_location_a, temp_location_b]) =
            self.get_temp_scoreboard_locations_array();

        let score_target_location = match target {
            DataLocation::ScoreboardValue { objective, target } => (objective, target),
            _ => (&temp_objective, &temp_location_a),
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

        let (right_cmds, rhs_score) =
            if let Ok(ComptimeValue::Integer(int)) = right.comptime_eval(scope, handler) {
                self.initialize_constant_score(int);
                (
                    Vec::new(),
                    ("shu_constants", std::borrow::Cow::Owned(int.to_string())),
                )
            } else {
                let right_cmds = self.transpile_expression(
                    right,
                    &DataLocation::ScoreboardValue {
                        objective: temp_objective.clone(),
                        target: temp_location_b.clone(),
                    },
                    scope,
                    handler,
                )?;

                (
                    right_cmds,
                    (
                        temp_objective.as_str(),
                        std::borrow::Cow::Borrowed(&temp_location_b),
                    ),
                )
            };

        let calc_cmds = {
            let (target_objective, target) = score_target_location;
            let source = rhs_score.1.as_ref();
            let source_objective = rhs_score.0;

            let operation = match operator {
                BinaryOperator::Add(_) => "+=",
                BinaryOperator::Subtract(_) => "-=",
                BinaryOperator::Multiply(_) => "*=",
                BinaryOperator::Divide(_) => "/=",
                BinaryOperator::Modulo(_) => "%=",
                _ => unreachable!("This operator should not be handled here."),
            };

            vec![Command::Raw(format!(
                "scoreboard players operation {target} {target_objective} {operation} {source} {source_objective}"
            ))]
        };

        let transfer_cmd = match target {
            DataLocation::ScoreboardValue { .. } => None,
            DataLocation::Tag { .. } => {
                let err = TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: ExpectedType::Boolean,
                    expression: binary.span(),
                });
                handler.receive(Box::new(err.clone()));
                return Err(err);
            }
            DataLocation::Storage {
                storage_name,
                path,
                r#type,
            } => match r#type {
                StorageType::Byte | StorageType::Double | StorageType::Int | StorageType::Long => {
                    Some(Command::Execute(Execute::Store(
                        format!(
                            "result storage {storage_name} {path} {t} 1.0",
                            t = r#type.as_str()
                        )
                        .into(),
                        Box::new(Execute::Run(Box::new(Command::Raw(format!(
                            "scoreboard players get {target} {objective}",
                            objective = score_target_location.0,
                            target = score_target_location.1
                        ))))),
                    )))
                }
                StorageType::Boolean => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: binary.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    return Err(err);
                }
                StorageType::String => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::String,
                        expression: binary.span(),
                    });
                    handler.receive(Box::new(err.clone()));
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

    fn transpile_comparison_operator(
        &mut self,
        binary: &Binary,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(Vec<Command>, ShulkerboxMacroStringMap, Condition)> {
        let invert = matches!(binary.operator(), BinaryOperator::NotEqual(..));

        // TODO: evaluate comptime values and compare using `matches` and integer ranges

        let operator = match binary.operator() {
            BinaryOperator::Equal(..) | BinaryOperator::NotEqual(..) => "=",
            BinaryOperator::GreaterThan(_) => ">",
            BinaryOperator::GreaterThanOrEqual(..) => ">=",
            BinaryOperator::LessThan(_) => "<",
            BinaryOperator::LessThanOrEqual(..) => "<=",
            _ => unreachable!("This function should only be called for comparison operators."),
        };

        let (temp_objective, [temp_location_a, temp_location_b]) =
            self.get_temp_scoreboard_locations_array();

        let condition = Condition::Atom(
            format!(
                "score {temp_location_a} {temp_objective} {operator} {temp_location_b} {temp_objective}"
            )
            .into(),
        );

        let left_cmds = self.transpile_expression(
            binary.left_operand(),
            &DataLocation::ScoreboardValue {
                objective: temp_objective.clone(),
                target: temp_location_a,
            },
            scope,
            handler,
        )?;
        let right_cmds = self.transpile_expression(
            binary.right_operand(),
            &DataLocation::ScoreboardValue {
                objective: temp_objective,
                target: temp_location_b,
            },
            scope,
            handler,
        )?;

        Ok((
            left_cmds.into_iter().chain(right_cmds).collect(),
            BTreeMap::new(),
            if invert {
                Condition::Not(Box::new(condition))
            } else {
                condition
            },
        ))
    }

    fn transpile_logic_operator(
        &mut self,
        binary: &Binary,
        scope: &Arc<super::Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(Vec<Command>, ShulkerboxMacroStringMap, ExtendedCondition)> {
        let left = binary.left_operand().as_ref();
        let right = binary.right_operand().as_ref();

        let (left_cmds, mut left_prep_variables, left_cond) =
            self.transpile_expression_as_condition(left, scope, handler)?;
        let (right_cmds, right_prep_variables, right_cond) =
            self.transpile_expression_as_condition(right, scope, handler)?;
        left_prep_variables.extend(right_prep_variables);
        let prep_variables = left_prep_variables;

        match (binary.operator(), left_cond, right_cond) {
            (BinaryOperator::LogicalAnd(..), ExtendedCondition::Comptime(true), other)
            | (BinaryOperator::LogicalOr(..), ExtendedCondition::Comptime(false), other) => {
                Ok((right_cmds, prep_variables, other))
            }
            (BinaryOperator::LogicalAnd(..), other, ExtendedCondition::Comptime(true))
            | (BinaryOperator::LogicalOr(..), other, ExtendedCondition::Comptime(false)) => {
                Ok((left_cmds, prep_variables, other))
            }
            (BinaryOperator::LogicalAnd(..), ExtendedCondition::Comptime(false), _)
            | (BinaryOperator::LogicalAnd(..), _, ExtendedCondition::Comptime(false)) => Ok((
                Vec::new(),
                BTreeMap::new(),
                ExtendedCondition::Comptime(false),
            )),
            (BinaryOperator::LogicalOr(..), ExtendedCondition::Comptime(true), _)
            | (BinaryOperator::LogicalOr(..), _, ExtendedCondition::Comptime(true)) => Ok((
                Vec::new(),
                BTreeMap::new(),
                ExtendedCondition::Comptime(true),
            )),
            (
                BinaryOperator::LogicalAnd(..),
                ExtendedCondition::Runtime(left_cond),
                ExtendedCondition::Runtime(right_cond),
            ) => Ok((
                left_cmds.into_iter().chain(right_cmds).collect(),
                prep_variables,
                ExtendedCondition::Runtime(Condition::And(
                    Box::new(left_cond),
                    Box::new(right_cond),
                )),
            )),
            (
                BinaryOperator::LogicalOr(..),
                ExtendedCondition::Runtime(left_cond),
                ExtendedCondition::Runtime(right_cond),
            ) => Ok((
                left_cmds.into_iter().chain(right_cmds).collect(),
                prep_variables,
                ExtendedCondition::Runtime(Condition::Or(
                    Box::new(left_cond),
                    Box::new(right_cond),
                )),
            )),
            _ => unreachable!("This function should only be called for logical operators."),
        }
    }

    #[expect(clippy::too_many_lines)]
    fn store_comptime_value(
        &mut self,
        value: &ComptimeValue,
        target: &DataLocation,
        source: &impl SourceElement,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match value {
            ComptimeValue::Integer(value) => match target {
                DataLocation::ScoreboardValue { objective, target } => Ok(vec![Command::Raw(
                    format!("scoreboard players set {target} {objective} {value}"),
                )]),
                DataLocation::Tag { .. } => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: source.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    Err(err)
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
                        Ok(vec![Command::Raw(format!(
                            "data modify storage {storage_name} {path} set value {value}{suffix}",
                            suffix = r#type.suffix(),
                        ))])
                    } else {
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: source.span(),
                            expected_type: target.value_type().into(),
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    }
                }
            },
            &ComptimeValue::Boolean(value) => match target {
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
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: source.span(),
                            expected_type: target.value_type().into(),
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    }
                }
                DataLocation::ScoreboardValue { objective, target } => {
                    Ok(vec![Command::Raw(format!(
                        "scoreboard players set {target} {objective} {value}",
                        value = u8::from(value)
                    ))])
                }
            },
            ComptimeValue::String(value) => self.store_comptime_value(
                &ComptimeValue::MacroString(value.clone().into()),
                target,
                source,
                handler,
            ),
            ComptimeValue::MacroString(value) => {
                match target {
                    DataLocation::Storage {
                        r#type: StorageType::Boolean,
                        ..
                    }
                    | DataLocation::Tag { .. } => {
                        let (macro_string, prepare_variables) = value.clone().into_sb();
                        let cmds = self.store_condition_success(
                            ExtendedCondition::Runtime(Condition::Atom(macro_string)),
                            target,
                            source,
                            handler,
                        )?;

                        self.transpile_commands_with_variable_macros(
                            cmds,
                            prepare_variables,
                            handler,
                        )
                    }
                    // DataLocation::Storage { storage_name, path, r#type: StorageType::String } => todo!("implement storage string")
                    _ => {
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expected_type: target.value_type().into(),
                            expression: source.span(),
                        });
                        handler.receive(Box::new(err.clone()));
                        Err(err)
                    }
                }
            }
        }
    }

    #[expect(clippy::unused_self, clippy::needless_pass_by_ref_mut)]
    fn store_condition_success(
        &mut self,
        cond: ExtendedCondition,
        target: &DataLocation,
        source: &impl SourceElement,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let (success_cmd, else_cmd) = match target {
            DataLocation::ScoreboardValue { objective, target } => (
                format!("scoreboard players set {target} {objective} 1"),
                format!("scoreboard players set {target} {objective} 0"),
            ),
            DataLocation::Storage {
                storage_name,
                path,
                r#type,
            } => {
                if matches!(r#type, StorageType::Boolean) {
                    (
                        format!("data modify storage {storage_name} {path} set value 1b"),
                        format!("data modify storage {storage_name} {path} set value 0b"),
                    )
                } else {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: source.span(),
                    });
                    handler.receive(Box::new(err.clone()));
                    return Err(err);
                }
            }
            DataLocation::Tag { tag_name, entity } => (
                format!("tag {entity} add {tag_name}"),
                format!("tag {entity} remove {tag_name}"),
            ),
        };

        let cmd = match cond {
            ExtendedCondition::Runtime(cond) => Command::Execute(Execute::If(
                cond,
                Box::new(Execute::Run(Box::new(Command::Raw(success_cmd)))),
                Some(Box::new(Execute::Run(Box::new(Command::Raw(else_cmd))))),
            )),
            ExtendedCondition::Comptime(cond) => {
                if cond {
                    Command::Raw(success_cmd)
                } else {
                    Command::Raw(else_cmd)
                }
            }
        };

        Ok(vec![cmd])
    }

    pub(super) fn get_temp_count(&mut self, amount: usize) -> usize {
        let current = self.temp_counter;
        self.temp_counter = self.temp_counter.wrapping_add(amount);
        current
    }

    /// Get temporary scoreboard locations.
    pub(super) fn get_temp_scoreboard_locations(&mut self, amount: usize) -> (String, Vec<String>) {
        let objective = "shu_temp_".to_string()
            + &chksum_md5::hash(&self.main_namespace_name).to_hex_lowercase();

        self.datapack
            .register_scoreboard(&objective, None::<&str>, None::<&str>);

        let temp_count = self.get_temp_count(amount);

        let targets = (0..amount)
            .map(|i| {
                chksum_md5::hash(format!(
                    "{namespace}\0{j}",
                    namespace = self.main_namespace_name,
                    j = i + temp_count
                ))
                .to_hex_lowercase()
                .split_off(16)
            })
            .collect();

        (objective, targets)
    }

    /// Get temporary scoreboard locations.
    pub(super) fn get_temp_scoreboard_locations_array<const N: usize>(
        &mut self,
    ) -> (String, [String; N]) {
        let (objective, targets) = self.get_temp_scoreboard_locations(N);

        let targets = targets.try_into().expect("build from range of type");

        (objective, targets)
    }

    /// Get temporary storage locations.
    pub(super) fn get_temp_storage_locations(&mut self, amount: usize) -> (String, Vec<String>) {
        let storage_name = "shulkerscript:temp_".to_string()
            + &chksum_md5::hash(&self.main_namespace_name).to_hex_lowercase();

        let temp_count = self.get_temp_count(amount);

        let paths = (0..amount)
            .map(|i| {
                chksum_md5::hash(format!(
                    "{namespace}\0{j}",
                    namespace = self.main_namespace_name,
                    j = i + temp_count
                ))
                .to_hex_lowercase()
                .split_off(16)
            })
            .collect::<Vec<_>>();

        self.temp_data_storage_locations.extend(
            paths
                .iter()
                .map(|path| (storage_name.clone(), path.clone())),
        );

        (storage_name, paths)
    }

    /// Get temporary storage locations.
    pub(super) fn get_temp_storage_locations_array<const N: usize>(
        &mut self,
    ) -> (String, [String; N]) {
        let (storage_name, paths) = self.get_temp_storage_locations(N);

        let paths = paths.try_into().expect("build from range of type");

        (storage_name, paths)
    }
}
