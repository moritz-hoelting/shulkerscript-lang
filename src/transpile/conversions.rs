//! Conversion functions for converting between tokens/ast-nodes and [`shulkerbox`] types

use shulkerbox::util::{MacroString as ExtMacroString, MacroStringPart as ExtMacroStringPart};

use crate::{lexical::token::MacroStringLiteral, syntax::syntax_tree::AnyStringLiteral};

use super::util::{MacroString, MacroStringPart};

impl From<MacroString> for ExtMacroString {
    fn from(value: MacroString) -> Self {
        match value {
            MacroString::String(s) => Self::String(s),
            MacroString::MacroString(parts) => {
                Self::MacroString(parts.into_iter().map(ExtMacroStringPart::from).collect())
            }
        }
    }
}

impl From<MacroStringPart> for ExtMacroStringPart {
    fn from(value: MacroStringPart) -> Self {
        match value {
            MacroStringPart::String(s) => Self::String(s),
            MacroStringPart::MacroUsage(m) => Self::MacroUsage(m),
        }
    }
}

impl From<&AnyStringLiteral> for ExtMacroString {
    fn from(value: &AnyStringLiteral) -> Self {
        Self::from(MacroString::from(value))
    }
}

impl From<AnyStringLiteral> for ExtMacroString {
    fn from(value: AnyStringLiteral) -> Self {
        Self::from(&value)
    }
}

impl From<&MacroStringLiteral> for ExtMacroString {
    fn from(value: &MacroStringLiteral) -> Self {
        Self::from(MacroString::from(value))
    }
}

impl From<MacroStringLiteral> for ExtMacroString {
    fn from(value: MacroStringLiteral) -> Self {
        Self::from(&value)
    }
}
