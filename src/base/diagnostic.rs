use std::{cell::Cell, fmt::Display};

/// Represents a trait responsible for handling diagnostics in the interpreter.
pub trait Handler<T> {
    /// Receive an error and handles it.
    fn receive<E: Into<T>>(&self, error: E);
    /// Returns whether the handler has received any error.
    fn has_received(&self) -> bool;
}

/// Is a struct that implements [`Handler`] trait by doing nothing with the errors and
/// never signifying that it has received a message.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct VoidHandler;

impl<T> Handler<T> for VoidHandler {
    fn receive<E: Into<T>>(&self, _error: E) {}
    fn has_received(&self) -> bool {
        false
    }
}

/// A handler that does not print the error to the standard error stream.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SilentHandler {
    received: Cell<bool>,
}

impl SilentHandler {
    /// Creates a new [`SilentHandler`].
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> Handler<T> for SilentHandler {
    fn receive<E: Into<T>>(&self, _error: E) {
        self.received.set(true);
    }

    fn has_received(&self) -> bool {
        self.received.get()
    }
}

/// A simple error handler that prints the error to the standard error stream.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrintHandler {
    printed: Cell<bool>,
}

impl PrintHandler {
    /// Creates a new [`PrintHandler`].
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T: Display> Handler<T> for PrintHandler {
    fn receive<E: Into<T>>(&self, error: E) {
        eprintln!("{}", error.into());
        self.printed.set(true);
    }

    fn has_received(&self) -> bool {
        self.printed.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_void_handler() {
        let handler = VoidHandler;
        Handler::<&str>::receive(&handler, "error");
        assert!(!Handler::<&str>::has_received(&handler));
    }

    #[test]
    fn test_silent_handler() {
        let handler = SilentHandler::new();
        Handler::<&str>::receive(&handler, "error");
        assert!(Handler::<&str>::has_received(&handler));
    }

    #[test]
    fn test_print_handler() {
        let handler = PrintHandler::new();
        Handler::<&str>::receive(&handler, "error");
        assert!(Handler::<&str>::has_received(&handler));
    }
}
