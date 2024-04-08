//! Module containing structures and implementations for logging messages to the user.

use colored::Colorize;
use std::{fmt::Display, sync::Arc};

use super::source_file::{Location, SourceFile, Span};

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

fn get_digit_count(mut number: usize) -> usize {
    let mut digit = 0;

    while number > 0 {
        number /= 10;
        digit += 1;
    }

    digit
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
        let start_location = self.span.start_location();
        let end_location = self.span.end_location();

        let start_line = start_location.line;
        let end_line = end_location.map_or_else(
            || self.span.source_file().line_amount(),
            |end_location| end_location.line,
        );
        let is_multiline = start_line != end_line;

        // when printing the source code, show the line before the span and the line after the span
        let largest_line_number_digits = get_digit_count(end_line + 1);

        // prints the source location
        for _ in 0..largest_line_number_digits {
            write!(f, " ")?;
        }

        writeln!(
            f,
            "{} {}",
            "-->".cyan().bold(),
            format_args!(
                "{}:{}:{}",
                self.span.source_file().path().display(),
                start_location.line,
                start_location.column
            )
        )?;

        // prints the empty pipe
        write_empty_pipe(f, largest_line_number_digits)?;

        // prints previous line
        if let Some((line_number, line)) = start_line.checked_sub(1).and_then(|line_number| {
            self.span
                .source_file()
                .get_line(line_number)
                .map(|line| (line_number, line))
        }) {
            write_line(f, largest_line_number_digits, line_number, line)?;
        }

        // prints the line with the error
        write_error_line(
            f,
            start_location,
            end_location,
            largest_line_number_digits,
            is_multiline,
            self.span.source_file(),
        )?;

        if let Some(message) = &self.help_display {
            if !is_multiline {
                // prints the empty pipe
                {
                    for _ in 0..=largest_line_number_digits {
                        write!(f, " ")?;
                    }
                    write!(f, "{} ", "┃".cyan().bold())?;
                }

                // prints the whitespace until the start's column
                {
                    for (index, ch) in self
                        .span
                        .source_file()
                        .get_line(start_line)
                        .unwrap()
                        .chars()
                        .enumerate()
                    {
                        if index + 1 >= start_location.column {
                            break;
                        }

                        // if the char is tab, print 4 spaces
                        write!(f, "{}", if ch == '\t' { "    " } else { " " })?;
                    }
                }

                // prints the message
                writeln!(f, "{}: {message}", "help".bold())?;
            }
        }

        // prints the post line
        if let Some((line_number, line)) = end_line.checked_add(1).and_then(|line_number| {
            self.span
                .source_file()
                .get_line(line_number)
                .map(|line| (line_number, line))
        }) {
            write_line(f, largest_line_number_digits, line_number, line)?;
        }

        // prints the empty pipe
        write_empty_pipe(f, largest_line_number_digits)?;

        if is_multiline {
            if let Some(help_display) = &self.help_display {
                write_help_message_multiline(f, largest_line_number_digits, help_display)?;
            }
        }

        Ok(())
    }
}

fn write_empty_pipe(
    f: &mut std::fmt::Formatter<'_>,
    largest_line_number_digits: usize,
) -> std::fmt::Result {
    for _ in 0..=largest_line_number_digits {
        write!(f, " ")?;
    }
    writeln!(f, "{}", "┃".cyan().bold())?;

    Ok(())
}

fn write_line(
    f: &mut std::fmt::Formatter<'_>,
    largest_line_number_digits: usize,
    line_number: usize,
    line: &str,
) -> std::fmt::Result {
    // prints the line number
    write!(
        f,
        "{}{}{} ",
        line_number.to_string().cyan().bold(),
        format_args!(
            "{:width$}",
            "",
            width = largest_line_number_digits - get_digit_count(line_number) + 1
        ),
        "┃".cyan().bold(),
    )?;

    for ch in line.chars() {
        // if the char is tab, print 4 spaces
        if ch == '\t' {
            write!(f, "    ")?;
        } else if ch != '\n' {
            write!(f, "{ch}")?;
        }
    }

    writeln!(f)?;

    Ok(())
}

fn write_help_message_multiline<T: Display>(
    f: &mut std::fmt::Formatter<'_>,
    largest_line_number_digits: usize,
    help_display: T,
) -> std::fmt::Result {
    for _ in 0..=largest_line_number_digits {
        write!(f, " ")?;
    }
    write!(f, "{} ", "=".cyan().bold())?;

    // prints the message
    writeln!(f, "{}: {help_display}", "help".bold())?;

    Ok(())
}

fn write_error_line(
    f: &mut std::fmt::Formatter<'_>,
    start_location: Location,
    end_location: Option<Location>,
    largest_line_number_digits: usize,
    is_multiline: bool,
    source_file: &Arc<SourceFile>,
) -> std::fmt::Result {
    let start_line = start_location.line;
    let end_line = end_location.map_or_else(
        || source_file.line_amount(),
        |end_location| end_location.line,
    );

    for line_number in start_line..=end_line {
        // prints the line number
        write!(
            f,
            "{}{}{} ",
            line_number.to_string().cyan().bold(),
            format_args!(
                "{:width$}",
                "",
                width = largest_line_number_digits - get_digit_count(line_number) + 1
            ),
            "┃".cyan().bold(),
        )?;

        for (index, ch) in source_file
            .get_line(line_number)
            .unwrap()
            .chars()
            .enumerate()
        {
            // if the char is tab, print 4 spaces
            if ch == '\t' {
                write!(f, "    ")?;
            } else if ch != '\n' {
                // check if the character is in the span
                let is_in_span = {
                    let index = index + 1;
                    if is_multiline {
                        (line_number == start_line && index >= start_location.column)
                            || (line_number == end_line
                                && (index + 1)
                                    < end_location
                                        .map_or(usize::MAX, |end_location| end_location.column))
                            || (line_number > start_line && line_number < end_line)
                    } else {
                        line_number == start_line
                            && index >= start_location.column
                            && index
                                < end_location
                                    .map_or(usize::MAX, |end_location| end_location.column)
                    }
                };

                if is_in_span {
                    write!(f, "{}", ch.to_string().red().bold().underline())?;
                } else {
                    write!(f, "{ch}")?;
                }
            }
        }
        writeln!(f)?;
    }

    Ok(())
}
