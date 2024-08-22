//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

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
#[doc(inline)]
pub use transpiler::Transpiler;

mod util;
