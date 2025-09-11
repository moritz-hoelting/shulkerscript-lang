//! Utility methods for transpiling

#[cfg(feature = "shulkerbox")]
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

#[cfg(feature = "shulkerbox")]
use shulkerbox::prelude::Command;

#[cfg(feature = "shulkerbox")]
use crate::{
    base::{
        self,
        source_file::{SourceElement as _, Span},
        Handler,
    },
    syntax::syntax_tree::{
        expression::{Expression, Primary, TemplateStringLiteral, TemplateStringLiteralPart},
        AnyStringLiteral,
    },
    transpile::{
        error::{TranspileError, UnknownIdentifier},
        expression::{ComptimeValue, DataLocation},
        Scope, TranspileResult, Transpiler, VariableData,
    },
    util::identifier_to_macro,
};

/// String that can contain macros
#[cfg(feature = "shulkerbox")]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroString {
    /// A normal string
    String(String),
    /// A string containing expressions
    MacroString {
        /// Parts that make up the macro string
        parts: Vec<MacroStringPart>,
        /// Variables that need special preparation before using the macro string
        prepare_variables: BTreeMap<String, (DataLocation, Vec<Command>, Span)>,
    },
}

/// Part of a [`MacroString`]
#[cfg(feature = "shulkerbox")]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroStringPart {
    /// A normal string
    String(String),
    /// A macro usage
    MacroUsage(String),
}

#[cfg(feature = "shulkerbox")]
impl std::fmt::Display for MacroString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => s.fmt(f),
            Self::MacroString { parts, .. } => {
                for part in parts {
                    match part {
                        MacroStringPart::String(s) => s.fmt(f)?,
                        MacroStringPart::MacroUsage(m) => write!(f, "$({m})")?,
                    }
                }
                Ok(())
            }
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl MacroString {
    /// Check if the macro string contains any macros
    #[must_use]
    pub fn contains_macros(&self) -> bool {
        match self {
            Self::String(_) => false,
            Self::MacroString { parts, .. } => parts
                .iter()
                .any(|p| matches!(p, MacroStringPart::MacroUsage(_))),
        }
    }

    /// Get the string representation of the macro string or the parts if it contains macros
    ///
    /// # Errors
    /// - If the macro string contains macros
    #[expect(clippy::type_complexity)]
    pub fn as_str(
        &self,
    ) -> Result<
        std::borrow::Cow<'_, str>,
        (
            &[MacroStringPart],
            &BTreeMap<String, (DataLocation, Vec<Command>, Span)>,
        ),
    > {
        match self {
            Self::String(s) => Ok(std::borrow::Cow::Borrowed(s)),
            Self::MacroString {
                parts,
                prepare_variables,
            } if self.contains_macros() => Err((parts, prepare_variables)),
            Self::MacroString { parts, .. } => Ok(std::borrow::Cow::Owned(
                parts
                    .iter()
                    .map(|p| match p {
                        MacroStringPart::String(s) => s.clone(),
                        MacroStringPart::MacroUsage(m) => format!("$({m})"),
                    })
                    .collect::<String>(),
            )),
        }
    }
}

fn normalize_program_identifier<S>(identifier: S) -> String
where
    S: AsRef<str>,
{
    identifier
        .as_ref()
        .split('/')
        .fold(Vec::new(), |mut acc, el| match el {
            "." | "" => acc,
            ".." => {
                acc.pop();
                acc
            }
            _ => {
                acc.push(el);
                acc
            }
        })
        .join("/")
}

/// Calculate the identifier to import the function based on the current identifier and the import path
#[must_use]
pub fn calculate_import_identifier<S, T>(current_identifier: S, import_path: T) -> String
where
    S: AsRef<str>,
    T: AsRef<str>,
{
    if import_path.as_ref().starts_with('/') {
        normalize_program_identifier(&import_path.as_ref()[1..])
    } else {
        let mut identifier_elements = current_identifier.as_ref().split('/').collect::<Vec<_>>();
        identifier_elements.pop();
        normalize_program_identifier(identifier_elements.join("/") + "/" + import_path.as_ref())
    }
}

/// Join multiple macro strings into one
#[cfg(feature = "shulkerbox")]
#[must_use]
pub fn join_macro_strings<I>(strings: I) -> MacroString
where
    I: IntoIterator<Item = MacroString>,
{
    strings
        .into_iter()
        .fold(MacroString::String(String::new()), |acc, cur| match acc {
            MacroString::String(mut s) => match cur {
                MacroString::String(cur) => {
                    s.push_str(&cur);
                    MacroString::String(s)
                }
                MacroString::MacroString {
                    parts: cur,
                    prepare_variables: preparation_cmds,
                } => {
                    let mut parts = vec![MacroStringPart::String(s)];
                    parts.extend(cur);
                    MacroString::MacroString {
                        parts,
                        prepare_variables: preparation_cmds,
                    }
                }
            },
            MacroString::MacroString {
                mut parts,
                prepare_variables: mut preparation_cmds,
            } => match cur {
                MacroString::String(cur) => {
                    parts.push(MacroStringPart::String(cur));
                    MacroString::MacroString {
                        parts,
                        prepare_variables: preparation_cmds,
                    }
                }
                MacroString::MacroString {
                    parts: cur,
                    prepare_variables: cur_preparation_cmds,
                } => {
                    parts.extend(cur);
                    preparation_cmds.extend(cur_preparation_cmds);
                    MacroString::MacroString {
                        parts,
                        prepare_variables: preparation_cmds,
                    }
                }
            },
        })
}

/// Add additional information to an entity selector
#[must_use]
pub fn add_to_entity_selector(selector: impl Into<String>, additional: &str) -> String {
    let selector: String = selector.into();
    if selector.starts_with('@') {
        if selector.ends_with(']') {
            selector[..selector.len() - 1].to_string() + "," + additional + "]"
        } else {
            selector + "[" + additional + "]"
        }
    } else {
        format!("@a[name={selector},{additional}]")
    }
}

#[cfg(feature = "shulkerbox")]
impl std::str::FromStr for MacroString {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pos = s.find("$(");
        if pos.is_some_and(|pos| s[pos..].contains(')')) {
            let mut parts = Vec::new();
            let mut rem = s;
            while let Some(pos) = rem.find("$(") {
                let (before, after) = rem.split_at(pos);

                let last_macro_index = after
                    .char_indices()
                    .skip(2)
                    .take_while(|&(_, c)| c.is_ascii_alphanumeric() || c == '_')
                    .map(|(i, _)| i)
                    .last();

                match last_macro_index {
                    Some(last_macro_index) if after[last_macro_index + 1..].starts_with(')') => {
                        if !before.is_empty() {
                            match parts.last_mut() {
                                Some(MacroStringPart::String(last)) => {
                                    *last += before;
                                }
                                _ => {
                                    parts.push(MacroStringPart::String(before.to_string()));
                                }
                            }
                        }
                        parts.push(MacroStringPart::MacroUsage(
                            after[2..=last_macro_index].to_string(),
                        ));
                        rem = &after[last_macro_index + 2..];
                        if rem.is_empty() {
                            break;
                        }
                    }
                    _ => {
                        let part = &rem[..=pos + 1];
                        match parts.last_mut() {
                            Some(MacroStringPart::String(last)) => {
                                *last += part;
                            }
                            _ => {
                                parts.push(MacroStringPart::String(part.to_string()));
                            }
                        }
                        rem = &rem[pos + 2..];
                    }
                }
            }
            if !rem.is_empty() {
                match parts.last_mut() {
                    Some(MacroStringPart::String(last)) => {
                        *last += rem;
                    }
                    _ => {
                        parts.push(MacroStringPart::String(rem.to_string()));
                    }
                }
            }
            if parts
                .iter()
                .any(|p| matches!(p, MacroStringPart::MacroUsage(_)))
            {
                Ok(Self::MacroString {
                    parts,
                    prepare_variables: BTreeMap::new(),
                })
            } else {
                Ok(Self::String(s.to_string()))
            }
        } else {
            Ok(Self::String(s.to_string()))
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl<S> From<S> for MacroString
where
    S: Into<String>,
{
    fn from(value: S) -> Self {
        Self::String(value.into())
    }
}

#[cfg(feature = "shulkerbox")]
impl AnyStringLiteral {
    /// Convert the any string literal to a macro string, using the provided scope to resolve variables
    ///
    /// # Errors
    /// - If an identifier in a template string is not found in the scope
    pub fn to_macro_string(
        &self,
        transpiler: Option<&mut Transpiler>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<MacroString> {
        match self {
            Self::StringLiteral(literal) => Ok(MacroString::from(literal.str_content().as_ref())),
            Self::TemplateStringLiteral(literal) => {
                literal.to_macro_string(transpiler, scope, handler)
            }
        }
    }
}

#[cfg(feature = "shulkerbox")]
impl TemplateStringLiteral {
    /// Convert the template string literal to a macro string, using the provided scope to resolve variables
    ///
    /// # Errors
    /// - If an identifier in a template string is not found in the scope
    #[expect(clippy::too_many_lines)]
    pub fn to_macro_string(
        &self,
        mut transpiler: Option<&mut Transpiler>,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<MacroString> {
        if self.contains_expression() {
            let mut prepare_variables = BTreeMap::new();
            let mut prepare_variables_reverse = HashMap::<DataLocation, String>::new();

            let parts = self
                .parts()
                .iter()
                .map(|part| match part {
                    TemplateStringLiteralPart::Text(text) => Ok(vec![MacroStringPart::String(
                        crate::util::unescape_template_string(text.span.str()).into_owned(),
                    )]),
                    TemplateStringLiteralPart::Expression { expression, .. } => match expression
                        .as_ref()
                    {
                        Expression::Primary(Primary::Identifier(identifier)) => {
                            #[expect(clippy::option_if_let_else)]
                            if let Some(var_data) = scope.get_variable(identifier.span.str()) {
                                match var_data.as_ref() {
                                    VariableData::MacroParameter { macro_name, .. } => {
                                        Ok(vec![MacroStringPart::MacroUsage(macro_name.to_owned())])
                                    }
                                    VariableData::ComptimeValue { value, .. } => {
                                        let value = value.read().unwrap().as_ref().map_or_else(
                                            || "null".into(),
                                            ComptimeValue::to_macro_string,
                                        );

                                        match value.as_str() {
                                            Ok(s) => {
                                                Ok(vec![MacroStringPart::String(s.into_owned())])
                                            }
                                            Err((inner_parts, inner_prepare_variables)) => {
                                                prepare_variables
                                                    .extend(inner_prepare_variables.to_owned());
                                                Ok(inner_parts.to_vec())
                                            }
                                        }
                                    }
                                    VariableData::BooleanStorage { storage_name, path } => {
                                        use crate::transpile::expression::StorageType;

                                        let macro_name = if let Some(transpiler) = &mut transpiler {
                                            let temp_count = transpiler.get_temp_count(1);
                                            format!(
                                                "shu_temp_{hash}",
                                                hash = chksum_md5::hash(temp_count.to_le_bytes())
                                            )
                                        } else {
                                            identifier_to_macro(identifier.span.str()).into_owned()
                                        };

                                        let data_location = DataLocation::Storage {
                                            storage_name: storage_name.to_owned(),
                                            path: path.to_owned(),
                                            r#type: StorageType::Boolean,
                                        };

                                        let macro_name = if let Some(prev_macro_name) =
                                            prepare_variables_reverse.get(&data_location)
                                        {
                                            prev_macro_name.to_string()
                                        } else {
                                            prepare_variables.insert(
                                                macro_name.clone(),
                                                (
                                                    data_location.clone(),
                                                    Vec::new(),
                                                    expression.span(),
                                                ),
                                            );
                                            prepare_variables_reverse
                                                .insert(data_location, macro_name.clone());

                                            macro_name
                                        };

                                        Ok(vec![MacroStringPart::MacroUsage(macro_name)])
                                    }
                                    VariableData::ScoreboardValue { objective, target } => {
                                        let macro_name = if let Some(transpiler) = &mut transpiler {
                                            let temp_count = transpiler.get_temp_count(1);
                                            format!(
                                                "shu_temp_{hash}",
                                                hash = chksum_md5::hash(temp_count.to_le_bytes())
                                            )
                                        } else {
                                            identifier_to_macro(identifier.span.str()).into_owned()
                                        };
                                        let data_location = DataLocation::ScoreboardValue {
                                            objective: objective.to_owned(),
                                            target: target.to_owned(),
                                        };
                                        let macro_name = if let Some(prev_macro_name) =
                                            prepare_variables_reverse.get(&data_location)
                                        {
                                            prev_macro_name.to_string()
                                        } else {
                                            prepare_variables.insert(
                                                macro_name.clone(),
                                                (
                                                    data_location.clone(),
                                                    Vec::new(),
                                                    expression.span(),
                                                ),
                                            );
                                            prepare_variables_reverse
                                                .insert(data_location, macro_name.clone());

                                            macro_name
                                        };

                                        Ok(vec![MacroStringPart::MacroUsage(macro_name)])
                                    }
                                    _ => {
                                        use crate::semantic::error::UnexpectedExpression;

                                        let err = TranspileError::UnexpectedExpression(
                                            UnexpectedExpression(expression.to_owned()),
                                        );
                                        handler.receive(Box::new(err.clone()));

                                        Err(err)
                                    }
                                }
                            } else {
                                let err = TranspileError::UnknownIdentifier(
                                    UnknownIdentifier::from_scope(identifier.span(), scope),
                                );
                                handler.receive(Box::new(err.clone()));
                                Err(err)
                            }
                        }
                        Expression::Primary(Primary::MemberAccess(member_access)) => {
                            let value = member_access
                                .parent()
                                .comptime_member_access(member_access, scope, handler)
                                .inspect_err(|err| {
                                    handler.receive(Box::new(TranspileError::NotComptime(
                                        err.clone(),
                                    )));
                                })?
                                .to_macro_string();

                            match value.as_str() {
                                Ok(s) => Ok(vec![MacroStringPart::String(s.into_owned())]),
                                Err((inner_parts, inner_prepare_variables)) => {
                                    prepare_variables.extend(inner_prepare_variables.to_owned());
                                    Ok(inner_parts.to_vec())
                                }
                            }
                        }
                        _ => {
                            if let Some(transpiler) = &mut transpiler {
                                use crate::transpile::expression::{StorageType, ValueType};

                                let temp_count = transpiler.get_temp_count(1);
                                let macro_name = format!(
                                    "shu_temp_{hash}",
                                    hash = chksum_md5::hash(temp_count.to_le_bytes())
                                );

                                let data_location =
                                    if expression.can_yield_type(ValueType::Integer, scope) {
                                        let (scoreboard_name, [scoreboard_target]) =
                                            transpiler.get_temp_scoreboard_locations_array();

                                        DataLocation::ScoreboardValue {
                                            objective: scoreboard_name,
                                            target: scoreboard_target,
                                        }
                                    } else if expression.can_yield_type(ValueType::Boolean, scope) {
                                        let (storage_name, [storage_path]) =
                                            transpiler.get_temp_storage_locations_array();
                                        DataLocation::Storage {
                                            storage_name,
                                            path: storage_path,
                                            r#type: StorageType::Boolean,
                                        }
                                    } else if expression.can_yield_type(ValueType::String, scope) {
                                        let (storage_name, [storage_path]) =
                                            transpiler.get_temp_storage_locations_array();
                                        DataLocation::Storage {
                                            storage_name,
                                            path: storage_path,
                                            r#type: StorageType::String,
                                        }
                                    } else {
                                        use crate::semantic::error::UnexpectedExpression;

                                        let err = TranspileError::UnexpectedExpression(
                                            UnexpectedExpression(expression.to_owned()),
                                        );
                                        handler.receive(Box::new(err.clone()));
                                        return Err(err);
                                    };

                                let commands = transpiler.transpile_expression(
                                    expression,
                                    &data_location,
                                    scope,
                                    handler,
                                )?;

                                prepare_variables.insert(
                                    macro_name.clone(),
                                    (data_location, commands, expression.span()),
                                );

                                Ok(vec![MacroStringPart::MacroUsage(macro_name)])
                            } else {
                                use crate::semantic::error::UnexpectedExpression;

                                let err = TranspileError::UnexpectedExpression(
                                    UnexpectedExpression(expression.to_owned()),
                                );
                                handler.receive(Box::new(err.clone()));

                                Err(err)
                            }
                        }
                    },
                })
                .flat_map(|res| match res {
                    Ok(parts) => parts.into_iter().map(Ok).collect(),
                    Err(err) => vec![Err(err)],
                })
                .collect::<TranspileResult<Vec<MacroStringPart>>>()?;

            Ok(MacroString::MacroString {
                parts,
                prepare_variables,
            })
        } else {
            Ok(MacroString::String(
                self.as_str(scope, handler)?.into_owned(),
            ))
        }
    }
}

#[cfg(all(test, feature = "shulkerbox"))]
mod tests {
    use std::str::FromStr as _;

    use assert_struct::assert_struct;

    use super::*;

    #[test]
    fn test_parse_macro_string() {
        assert_struct!(
            MacroString::from_str("Hello, $(world)!").unwrap(),
            MacroString::MacroString {
                parts: vec![
                    MacroStringPart::String("Hello, ".to_string()),
                    MacroStringPart::MacroUsage("world".to_string()),
                    MacroStringPart::String("!".to_string())
                ],
                prepare_variables.is_empty(): true,
            }
        );
        assert_struct!(
            MacroString::from_str("Hello, $(world)! $(world").unwrap(),
            MacroString::MacroString {
                parts: vec![
                    MacroStringPart::String("Hello, ".to_string()),
                    MacroStringPart::MacroUsage("world".to_string()),
                    MacroStringPart::String("! $(world".to_string()),
                ],
                prepare_variables.is_empty(): true,
            }
        );
        assert_struct!(
            MacroString::from_str("Hello $(a) from $(b) and $(c)").unwrap(),
            MacroString::MacroString {
                parts: vec![
                    MacroStringPart::String("Hello ".to_string()),
                    MacroStringPart::MacroUsage("a".to_string()),
                    MacroStringPart::String(" from ".to_string()),
                    MacroStringPart::MacroUsage("b".to_string()),
                    MacroStringPart::String(" and ".to_string()),
                    MacroStringPart::MacroUsage("c".to_string()),
                ],
                prepare_variables.is_empty(): true,
            }
        );
        assert_struct!(
            MacroString::from_str("Hello, $(world! $(world)!").unwrap(),
            MacroString::MacroString {
                parts: vec![
                    MacroStringPart::String("Hello, $(world! ".to_string()),
                    MacroStringPart::MacroUsage("world".to_string()),
                    MacroStringPart::String("!".to_string()),
                ],
                prepare_variables.is_empty(): true,
            }
        );
    }
}
