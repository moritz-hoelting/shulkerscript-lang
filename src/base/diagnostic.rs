/// Represents a trait responsible for handling diagnostics in the interpreter.
pub trait Handler<T> {
    /// Receive an error and handles it.
    fn receive<E: Into<T>>(&self, error: E);
    /// Returns whether the handler has received any error.
    fn has_received(&self) -> bool;
}

/// Is a struct that implements [`Handler`] trait by doing nothing with the errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DummyHandler;

impl<T> Handler<T> for DummyHandler {
    fn receive<E: Into<T>>(&self, _error: E) {}
    fn has_received(&self) -> bool {
        false
    }
}
