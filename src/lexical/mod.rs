//! The lexical module is responsible for converting raw text into a stream of tokens that the parser can understand.

pub mod token_stream;

pub mod token;

mod error;
pub use error::Error;
