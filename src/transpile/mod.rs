//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

use std::collections::{BTreeMap, HashMap};

use crate::{
    base::source_file::Span,
    syntax::syntax_tree::{expression::Expression, statement::Statement, AnnotationValue},
};

#[doc(hidden)]
#[cfg(feature = "shulkerbox")]
pub mod conversions;
mod error;

#[doc(inline)]
#[allow(clippy::module_name_repetitions)]
pub use error::{TranspileError, TranspileResult};
#[doc(hidden)]
pub mod lua;
#[cfg(feature = "shulkerbox")]
mod transpiler;
use strum::EnumIs;
#[cfg(feature = "shulkerbox")]
#[cfg_attr(feature = "shulkerbox", doc(inline))]
pub use transpiler::Transpiler;
#[cfg(feature = "shulkerbox")]
mod variables;

pub mod util;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FunctionData {
    pub(super) namespace: String,
    pub(super) identifier_span: Span,
    pub(super) parameters: Vec<String>,
    pub(super) statements: Vec<Statement>,
    pub(super) public: bool,
    pub(super) annotations: HashMap<String, TranspileAnnotationValue>,
}

/// Possible values for an annotation.
#[expect(clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq, Eq, EnumIs)]
pub enum TranspileAnnotationValue {
    /// No value.
    None,
    /// A single expression.
    Expression(Expression),
    /// A map of key-value pairs.
    Map(BTreeMap<String, TranspileAnnotationValue>),
}

impl From<Option<AnnotationValue>> for TranspileAnnotationValue {
    fn from(value: Option<AnnotationValue>) -> Self {
        match value {
            None => Self::None,
            Some(AnnotationValue::Single { value, .. }) => Self::Expression(value),
            Some(AnnotationValue::Multiple { list, .. }) => {
                let map = list
                    .into_elements()
                    .map(|elem| {
                        let key = elem.identifier.span.str().to_string();
                        let value = Self::from(elem.value);
                        (key, value)
                    })
                    .collect();
                Self::Map(map)
            }
        }
    }
}
