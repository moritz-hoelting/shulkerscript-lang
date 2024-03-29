//! The base module contains the core functionality of the `ShulkerScript` language.

pub mod source_file;

mod error;
#[doc(inline)]
pub use error::{Error, Result};

mod diagnostic;
pub use diagnostic::{Dummy, Handler};

pub mod log;
