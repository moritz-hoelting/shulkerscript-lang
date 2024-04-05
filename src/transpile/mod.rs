//! The transpile module is responsible for transpiling the abstract syntax tree into a data pack.

#[doc(hidden)]
#[cfg(feature = "shulkerbox")]
pub mod conversions;
pub mod error;
#[cfg(feature = "shulkerbox")]
pub mod transpiler;
