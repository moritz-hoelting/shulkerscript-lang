//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
};

use crate::{
    base::source_file::{SourceElement, Span},
    syntax::syntax_tree::{expression::Expression, statement::Statement, AnnotationValue},
};

#[doc(hidden)]
#[cfg(feature = "shulkerbox")]
pub mod conversions;
mod error;

pub mod expression;

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
pub mod internal_functions;

mod variables;
pub use variables::{Scope, VariableData};

pub mod util;

/// Data of a function.
#[derive(Clone, PartialEq, Eq)]
pub struct FunctionData {
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

impl Debug for FunctionData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct HiddenList;
        impl Debug for HiddenList {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut list = f.debug_list();
                list.entry(&..);
                list.finish()
            }
        }

        struct AnnotationsWrapper<'a, I>(&'a I);
        impl<'a, I> Debug for AnnotationsWrapper<'a, I>
        where
            &'a I: IntoIterator<Item = (&'a String, &'a TranspileAnnotationValue)>,
        {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                struct AnnotationValueWrapper<'a>(&'a TranspileAnnotationValue);
                impl<'a> Debug for AnnotationValueWrapper<'a> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        match self.0 {
                            TranspileAnnotationValue::None => None::<u8>.fmt(f),
                            TranspileAnnotationValue::Expression(expr) => expr.span().str().fmt(f),
                            TranspileAnnotationValue::Map(map) => AnnotationsWrapper(map).fmt(f),
                        }
                    }
                }

                let mut m = f.debug_map();

                m.entries(
                    self.0
                        .into_iter()
                        .map(|(k, v)| (k, AnnotationValueWrapper(v))),
                );

                m.finish()
            }
        }

        let mut s = f.debug_struct("FunctionData");

        s.field("namespace", &self.namespace);
        s.field("identifier", &self.identifier_span.str());
        s.field("public", &self.public);
        s.field("parameters", &self.parameters);
        s.field("statements", &HiddenList);
        s.field("annotations", &AnnotationsWrapper(&self.annotations));

        s.finish()
    }
}
