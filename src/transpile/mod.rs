//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

use std::collections::HashMap;

use crate::{base::source_file::Span, syntax::syntax_tree::statement::Statement};

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
#[cfg(feature = "shulkerbox")]
#[cfg_attr(feature = "shulkerbox", doc(inline))]
pub use transpiler::Transpiler;

#[cfg(feature = "shulkerbox")]
mod util;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FunctionData {
    pub(super) namespace: String,
    pub(super) identifier_span: Span,
    pub(super) statements: Vec<Statement>,
    pub(super) public: bool,
    pub(super) annotations: HashMap<String, Option<String>>,
}
