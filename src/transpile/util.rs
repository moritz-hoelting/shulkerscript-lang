//! Utility methods for transpiling

use std::fmt::Display;

use crate::{
    lexical::token::{MacroStringLiteral, MacroStringLiteralPart},
    syntax::syntax_tree::AnyStringLiteral,
};

/// String that can contain macros
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroString {
    /// A normal string
    String(String),
    /// A string containing macros
    MacroString(Vec<MacroStringPart>),
}

/// Part of a [`MacroString`]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroStringPart {
    /// A normal string
    String(String),
    /// A macro usage
    MacroUsage(String),
}

impl Display for MacroString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => s.fmt(f),
            Self::MacroString(parts) => {
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
                MacroString::MacroString(cur) => {
                    let mut parts = vec![MacroStringPart::String(s)];
                    parts.extend(cur);
                    MacroString::MacroString(parts)
                }
            },
            MacroString::MacroString(mut parts) => match cur {
                MacroString::String(cur) => {
                    parts.push(MacroStringPart::String(cur));
                    MacroString::MacroString(parts)
                }
                MacroString::MacroString(cur) => {
                    parts.extend(cur);
                    MacroString::MacroString(parts)
                }
            },
        })
}

impl<S> From<S> for MacroString
where
    S: Into<String>,
{
    fn from(value: S) -> Self {
        Self::String(value.into())
    }
}

impl From<&AnyStringLiteral> for MacroString {
    fn from(value: &AnyStringLiteral) -> Self {
        match value {
            AnyStringLiteral::StringLiteral(literal) => Self::from(literal.str_content().as_ref()),
            AnyStringLiteral::MacroStringLiteral(literal) => Self::from(literal),
        }
    }
}

impl From<AnyStringLiteral> for MacroString {
    fn from(value: AnyStringLiteral) -> Self {
        Self::from(&value)
    }
}

impl From<&MacroStringLiteral> for MacroString {
    fn from(value: &MacroStringLiteral) -> Self {
        if value
            .parts()
            .iter()
            .any(|p| matches!(p, MacroStringLiteralPart::MacroUsage { .. }))
        {
            Self::MacroString(
                value
                    .parts()
                    .iter()
                    .map(|part| match part {
                        MacroStringLiteralPart::Text(span) => MacroStringPart::String(
                            crate::util::unescape_macro_string(span.str()).to_string(),
                        ),
                        MacroStringLiteralPart::MacroUsage { identifier, .. } => {
                            MacroStringPart::MacroUsage(
                                crate::util::identifier_to_macro(identifier.span.str()).to_string(),
                            )
                        }
                    })
                    .collect(),
            )
        } else {
            Self::String(value.str_content())
        }
    }
}

impl From<MacroStringLiteral> for MacroString {
    fn from(value: MacroStringLiteral) -> Self {
        Self::from(&value)
    }
}
