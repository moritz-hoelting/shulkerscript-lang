//! Conversion functions for converting between tokens/ast-nodes and [`shulkerbox`] types

use shulkerbox::util::{MacroString, MacroStringPart};

use crate::{
    lexical::token::{MacroStringLiteral, MacroStringLiteralPart},
    syntax::syntax_tree::AnyStringLiteral,
};

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
