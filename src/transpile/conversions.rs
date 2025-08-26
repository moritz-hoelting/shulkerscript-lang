//! Conversion functions for converting between tokens/ast-nodes and [`shulkerbox`] types

use std::{borrow::Cow, sync::Arc};

use shulkerbox::util::{MacroString as ExtMacroString, MacroStringPart as ExtMacroStringPart};

use crate::{
    base::{self, Handler},
    semantic::error::UnexpectedExpression,
    syntax::syntax_tree::expression::{TemplateStringLiteral, TemplateStringLiteralPart},
    transpile::{Scope, TranspileError, TranspileResult},
    util,
};

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

impl TemplateStringLiteral {
    pub fn as_str(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Cow<'_, str>> {
        let mut res = Cow::Borrowed("");

        for part in &self.parts {
            match part {
                TemplateStringLiteralPart::Text(s) => {
                    let s = util::unescape_template_string(s.span.str());
                    if res.is_empty() {
                        res = s;
                    } else {
                        res.to_mut().push_str(&s);
                    }
                }
                TemplateStringLiteralPart::Expression { expression, .. } => {
                    let compiled = expression.comptime_eval(scope, handler)?;
                    let s = compiled.to_string_no_macro().ok_or_else(|| {
                        let err = TranspileError::UnexpectedExpression(UnexpectedExpression(
                            expression.clone(),
                        ));
                        handler.receive(Box::new(err.clone()));
                        err
                    })?;
                    res.to_mut().push_str(&s);
                }
            }
        }

        Ok(res)
    }
}
