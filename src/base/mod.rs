//! The base module contains the core functionality of the `Shulkerscript` language.

pub mod source_file;

mod error;
#[doc(inline)]
pub use error::{Error, Result};

mod diagnostic;
pub use diagnostic::{Handler, PrintHandler, SilentHandler, VoidHandler};

mod file_provider;
pub use file_provider::{Error as FileProviderError, FileProvider, FsProvider};

pub mod log;
