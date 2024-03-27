/// Represents a trait responsible for handling diagnostics in the interpreter.
pub trait Handler<T> {
    /// Receive an error and handles it.
    fn receive(&self, error: T);
}
