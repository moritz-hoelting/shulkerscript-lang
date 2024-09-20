//! Module for handling source files and their elements.

use std::{
    cmp::Ordering,
    fmt::Debug,
    iter::{Iterator, Peekable},
    ops::Range,
    path::{Path, PathBuf},
    str::CharIndices,
    sync::Arc,
};

use getset::{CopyGetters, Getters};

use super::{file_provider::FileProvider, Error};

/// Represents a source file that contains the source code.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Getters)]
pub struct SourceFile {
    /// Get the path of the source file.
    #[get = "pub"]
    path: PathBuf,
    /// Get the identifier of the source file.
    #[get = "pub"]
    identifier: String,
    /// Get the content of the source file
    #[get = "pub"]
    content: String,
    lines: Vec<Range<usize>>,
}

#[allow(clippy::missing_fields_in_debug)]
impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("path", &self.path)
            .field("lines", &self.lines)
            .finish()
    }
}

impl SourceFile {
    fn new(path: PathBuf, identifier: String, content: String) -> Arc<Self> {
        let lines = get_line_byte_positions(&content);

        Arc::new(Self {
            path,
            identifier,
            content,
            lines,
        })
    }

    /// Get the line of the source file at the given line number.
    ///
    /// Numbering starts at 1.
    #[must_use]
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 {
            return None;
        }

        let line = line - 1;
        self.lines
            .get(line)
            .map(|range| &self.content()[range.clone()])
    }

    /// Get the [`SourceIterator`] for the source file.
    #[must_use]
    pub fn iter<'a>(self: &'a Arc<Self>) -> SourceIterator<'a> {
        SourceIterator {
            source_file: self,
            iterator: self.content().char_indices().peekable(),
            prev: None,
        }
    }

    /// Get the number of lines in the source file.
    #[must_use]
    pub fn line_amount(&self) -> usize {
        self.lines.len()
    }

    /// Load the source file from the given file path.
    ///
    /// # Errors
    /// - [`Error::IoError`]: Error occurred when reading the file contents.
    pub fn load(
        path: &Path,
        identifier: String,
        provider: &impl FileProvider,
    ) -> Result<Arc<Self>, Error> {
        let source = provider.read_str(path)?;
        Ok(Self::new(
            path.to_path_buf(),
            identifier,
            source.into_owned(),
        ))
    }

    /// Get the [`Location`] of a given byte index
    #[must_use]
    pub fn get_location(&self, byte_index: usize) -> Option<Location> {
        if self.content.is_char_boundary(byte_index) {
            // get the line number by binary searching the line ranges
            let line = self
                .lines
                .binary_search_by(|range| {
                    if range.contains(&byte_index) {
                        Ordering::Equal
                    } else if byte_index < range.start {
                        Ordering::Greater
                    } else {
                        Ordering::Less
                    }
                })
                .ok()?;

            let line_starting_byte_index = self.lines[line].start;
            let line_str = self.get_line(line + 1).unwrap();

            // get the column number by iterating through the utf-8 characters (starts at 1)
            let column = line_str
                .char_indices()
                .take_while(|(i, _)| *i + line_starting_byte_index < byte_index)
                .count()
                + 1;

            Some(Location {
                line: line + 1,
                column,
            })
        } else {
            None
        }
    }

    /// Get the relative path of the source file from the current working directory.
    #[must_use]
    pub fn path_relative(&self) -> Option<PathBuf> {
        pathdiff::diff_paths(&self.path, std::env::current_dir().ok()?)
    }
}

/// Represents a range of characters in a source file.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Clone, Getters, CopyGetters)]
pub struct Span {
    /// Get the start byte index of the span.
    #[get_copy = "pub"]
    start: usize,

    /// Get the end byte index of the span (exclusive).
    #[get_copy = "pub"]
    end: usize,

    /// Get the source file that the span is located in.
    #[get = "pub"]
    source_file: Arc<SourceFile>,
}

#[allow(clippy::missing_fields_in_debug)]
impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("content", &self.str())
            .finish()
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.source_file, &other.source_file)
            && self.start == other.start
            && self.end == other.end
    }
}

impl Eq for Span {}

#[allow(clippy::non_canonical_partial_ord_impl)]
impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let self_ptr_value = Arc::as_ptr(&self.source_file) as usize;
        let other_ptr_value = Arc::as_ptr(&other.source_file) as usize;

        Some(self_ptr_value.cmp(&other_ptr_value).then_with(|| {
            self.start
                .cmp(&other.start)
                .then_with(|| self.end.cmp(&other.end))
        }))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_ptr_value = Arc::as_ptr(&self.source_file) as usize;
        let other_ptr_value = Arc::as_ptr(&other.source_file) as usize;

        self_ptr_value
            .cmp(&other_ptr_value)
            .then_with(|| self.start.cmp(&other.start))
            .then_with(|| self.end.cmp(&other.end))
    }
}

impl std::hash::Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
        Arc::as_ptr(&self.source_file).hash(state);
    }
}

impl Span {
    /// Create a span from the given start and end byte indices in the source file.
    ///
    /// # Parameters
    /// - `start`: The start byte index of the span.
    /// - `end`: The end byte index of the span (exclusive).
    #[must_use]
    pub fn new(source_file: Arc<SourceFile>, start: usize, end: usize) -> Option<Self> {
        if start > end
            || !source_file.content().is_char_boundary(start)
            || source_file.content().len() < end
            || (source_file.content().len() + 1 != end
                && !source_file.content().is_char_boundary(end))
        {
            return None;
        }

        Some(Self {
            start,
            end,
            source_file,
        })
    }

    /// Create a span from the given start byte index to the end of the source file.
    #[must_use]
    pub fn to_end(source_file: Arc<SourceFile>, start: usize) -> Option<Self> {
        if !source_file.content().is_char_boundary(start) {
            return None;
        }
        Some(Self {
            start,
            end: source_file.content().len(),
            source_file,
        })
    }

    /// Get the string slice of the source code that the span represents.
    #[must_use]
    pub fn str(&self) -> &str {
        &self.source_file.content()[self.start..self.end]
    }

    /// Get the starting [`Location`] of the span.
    #[must_use]
    pub fn start_location(&self) -> Location {
        self.source_file.get_location(self.start).unwrap()
    }

    /// Get the ending [`Location`] of the span.
    ///
    /// Returns [`None`] if the end of the span is the end of the source file.
    #[must_use]
    pub fn end_location(&self) -> Option<Location> {
        self.source_file.get_location(self.end)
    }

    /// Join the starting position of this span with the end position of the given span.
    #[must_use]
    pub fn join(&self, end: &Self) -> Option<Self> {
        if !Arc::ptr_eq(&self.source_file, &end.source_file) || self.start > end.end {
            return None;
        }

        Some(Self {
            start: self.start,
            end: end.end,
            source_file: self.source_file.clone(),
        })
    }
}

/// Pointing to a particular location in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Location {
    /// Line number of the location (starts at 1).
    pub line: usize,

    /// Column number of the location (starts at 1).
    pub column: usize,
}

/// Represents an element that is located within a source file.
pub trait SourceElement {
    /// Get the span location of the element.
    fn span(&self) -> Span;
}

impl<T: SourceElement> SourceElement for Box<T> {
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

/// Iterator iterating over the characters in a source file that can be peeked at.
#[derive(Debug, Clone, CopyGetters)]
pub struct SourceIterator<'a> {
    /// Get the source file that the iterator is iterating over.
    #[get_copy = "pub"]
    source_file: &'a Arc<SourceFile>,
    iterator: Peekable<CharIndices<'a>>,
    /// Get the previous character that was iterated over.
    #[get_copy = "pub"]
    prev: Option<(usize, char)>,
}
impl<'a> SourceIterator<'a> {
    /// Peek at the next character in the source file.
    pub fn peek(&mut self) -> Option<(usize, char)> {
        self.iterator.peek().copied()
    }
}
impl<'a> Iterator for SourceIterator<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iterator.next();
        if item.is_some() {
            self.prev = item;
        }
        item
    }
}

/// Get the byte positions of the lines in the given text.
fn get_line_byte_positions(text: &str) -> Vec<Range<usize>> {
    let mut current_position = 0;
    let mut results = Vec::new();

    let mut skip = false;

    for (byte, char) in text.char_indices() {
        if skip {
            skip = false;
            continue;
        }

        // lf
        if char == '\n' {
            #[allow(clippy::range_plus_one)]
            results.push(current_position..byte + 1);

            current_position = byte + 1;
        }

        // crlf
        if char == '\r' {
            if text.as_bytes().get(byte + 1) == Some(&b'\n') {
                results.push(current_position..byte + 2);

                current_position = byte + 2;

                skip = true;
            } else {
                #[allow(clippy::range_plus_one)]
                results.push(current_position..byte + 1);

                current_position = byte + 1;
            }
        }
    }

    // add the last line
    results.push(current_position..text.len());

    results
}
