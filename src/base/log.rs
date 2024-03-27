//! Module containing structures and implementations for logging messages to the user.

use colored::Colorize;
use std::fmt::Display;

use super::source_file::Span;

/// Represent the severity of a log message to be printed to the console.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[allow(missing_docs)]
pub enum Severity {
    Error,
    Info,
    Warning,
}

/// Struct implementing [`Display`] that represents a log message to be displayed to the user.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Message<T> {
    /// The severity of the log message.
    pub severity: Severity,

    /// The message to be displayed.
    pub display: T,
}
impl<T> Message<T> {
    /// Create a new log message with the given severity and message to be displayed.
    pub fn new(severity: Severity, display: T) -> Self {
        Self { severity, display }
    }
}

impl<T: Display> Display for Message<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let log_header = (match self.severity {
            Severity::Error => "[error]:".red(),
            Severity::Info => "[info]:".green(),
            Severity::Warning => "[warning]:".yellow(),
        })
        .bold();

        let message_part = &self.display.to_string().bold();

        write!(f, "{log_header} {message_part}")
    }
}

/// Structure implementing [`Display`] that prints the particular span of the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceCodeDisplay<'a, T> {
    /// The span of the source code to be printed.
    pub span: &'a Span,

    /// The help message to be displayed.
    pub help_display: Option<T>,
}

impl<'a, T> SourceCodeDisplay<'a, T> {
    /// Create a new source code display with the given span and help message to be displayed.
    pub fn new(span: &'a Span, help_display: Option<T>) -> Self {
        Self { span, help_display }
    }
}

impl<'a, T: std::fmt::Display> Display for SourceCodeDisplay<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.span.str())?;

        if let Some(help_display) = &self.help_display {
            write!(f, "\n\n{help_display}")?;
        }

        Ok(())
    }
}
