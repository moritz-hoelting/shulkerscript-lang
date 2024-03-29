/// Represents a trait responsible for handling diagnostics in the interpreter.
pub trait Handler<T> {
    /// Receive an error and handles it.
    fn receive(&self, error: T);
}

/// Is a struct that implements [`Handler`] trait by doing nothing with the errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Dummy;

impl<T> Handler<T> for Dummy {
    fn receive(&self, _error: T) {}
}
