//! Utility methods for transpiling

use std::{fmt::Display, str::FromStr};

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

impl FromStr for MacroString {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pos = s.find("$(");
        if pos.is_some_and(|pos| s[pos..].contains(')')) {
            let mut parts = Vec::new();
            let mut s = s;
            while let Some(pos) = s.find("$(") {
                let (before, after) = s.split_at(pos);

                let last_macro_index = after
                    .char_indices()
                    .skip(2)
                    .take_while(|&(_, c)| c.is_ascii_alphanumeric() || c == '_')
                    .map(|(i, _)| i)
                    .last();

                match last_macro_index {
                    Some(last_macro_index) if after[last_macro_index + 1..].starts_with(')') => {
                        if !before.is_empty() {
                            parts.push(MacroStringPart::String(before.to_string()));
                        }
                        parts.push(MacroStringPart::MacroUsage(
                            after[2..=last_macro_index].to_string(),
                        ));
                        s = &after[last_macro_index + 2..];
                        if s.is_empty() {
                            break;
                        }
                    }
                    _ => {
                        parts.push(MacroStringPart::String(s.to_string()));
                        s = "";
                        break;
                    }
                }
            }
            if !s.is_empty() {
                parts.push(MacroStringPart::String(s.to_string()));
            }
            if parts
                .iter()
                .any(|p| matches!(p, MacroStringPart::MacroUsage(_)))
            {
                Ok(Self::MacroString(parts))
            } else {
                Ok(Self::String(s.to_string()))
            }
        } else {
            Ok(Self::String(s.to_string()))
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_macro_string() {
        assert_eq!(
            MacroString::from_str("Hello, $(world)!").unwrap(),
            MacroString::MacroString(vec![
                MacroStringPart::String("Hello, ".to_string()),
                MacroStringPart::MacroUsage("world".to_string()),
                MacroStringPart::String("!".to_string())
            ])
        );
        assert_eq!(
            MacroString::from_str("Hello, $(world)! $(world").unwrap(),
            MacroString::MacroString(vec![
                MacroStringPart::String("Hello, ".to_string()),
                MacroStringPart::MacroUsage("world".to_string()),
                MacroStringPart::String("! $(world".to_string()),
            ])
        );
        assert_eq!(
            MacroString::from_str("Hello $(a) from $(b) and $(c)").unwrap(),
            MacroString::MacroString(vec![
                MacroStringPart::String("Hello ".to_string()),
                MacroStringPart::MacroUsage("a".to_string()),
                MacroStringPart::String(" from ".to_string()),
                MacroStringPart::MacroUsage("b".to_string()),
                MacroStringPart::String(" and ".to_string()),
                MacroStringPart::MacroUsage("c".to_string()),
            ])
        );
    }
}
