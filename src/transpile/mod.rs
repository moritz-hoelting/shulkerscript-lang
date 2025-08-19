//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

use std::{
    collections::{BTreeMap, HashMap},
    fmt::Debug,
};

use crate::{
    base::source_file::{SourceElement, Span},
    syntax::syntax_tree::{
        declaration::FunctionParameter, expression::Expression, statement::Statement,
        AnnotationValue,
    },
};

#[doc(hidden)]
#[cfg(feature = "shulkerbox")]
pub mod conversions;

pub mod error;

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

#[doc(hidden)]
#[cfg(feature = "shulkerbox")]
pub mod function;
#[doc(inline)]
#[cfg(feature = "shulkerbox")]
pub use function::TranspiledFunctionArguments;

mod variables;
#[cfg(feature = "shulkerbox")]
pub use variables::{Scope, VariableData};

pub mod util;

/// Data of a function.
#[derive(Clone, PartialEq, Eq)]
pub struct FunctionData {
    pub(super) namespace: String,
    pub(super) identifier_span: Span,
    pub(super) parameters: Vec<FunctionParameter>,
    pub(super) statements: Vec<Statement>,
    pub(super) public: bool,
    pub(super) annotations: HashMap<String, TranspileAnnotationValue>,
}

/// Possible values for an annotation.
#[expect(clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumIs)]
pub enum TranspileAnnotationValue {
    /// No value.
    None(Span),
    /// A single expression.
    Expression(Expression, Span),
    /// A map of key-value pairs.
    Map(BTreeMap<String, TranspileAnnotationValue>, Span),
}

impl TranspileAnnotationValue {
    /// Creates a new `TranspileAnnotationValue` from an [`AnnotationValue`] and [`Span`] of the key.
    #[must_use]
    pub fn from_annotation_value(value: Option<AnnotationValue>, key_span: &Span) -> Self {
        match value {
            None => Self::None(key_span.clone()),
            Some(AnnotationValue::Single { value, .. }) => {
                let span = value.span();
                Self::Expression(value, span)
            }
            Some(AnnotationValue::Multiple { list, .. }) => {
                let span = list.span();
                let map = list
                    .into_elements()
                    .map(|elem| {
                        let key = elem.identifier.span.str().to_string();
                        let value = Self::from_annotation_value(elem.value, &elem.identifier.span);
                        (key, value)
                    })
                    .collect();
                Self::Map(map, span)
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
                impl Debug for AnnotationValueWrapper<'_> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        match self.0 {
                            TranspileAnnotationValue::None(_) => None::<u8>.fmt(f),
                            TranspileAnnotationValue::Expression(expr, _) => {
                                expr.span().str().fmt(f)
                            }
                            TranspileAnnotationValue::Map(map, _) => AnnotationsWrapper(map).fmt(f),
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
