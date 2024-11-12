//! Utility methods for transpiling

use chksum_md5 as md5;
use std::borrow::Cow;

fn normalize_program_identifier<S>(identifier: S) -> String
where
    S: AsRef<str>,
{
    identifier
        .as_ref()
        .split('/')
        .fold(Vec::new(), |mut acc, el| match el {
            "." | "" => acc,
            ".." => {
                acc.pop();
                acc
            }
            _ => {
                acc.push(el);
                acc
            }
        })
        .join("/")
}

/// Calculate the identifier to import the function based on the current identifier and the import path
#[must_use]
pub fn calculate_import_identifier<S, T>(current_identifier: S, import_path: T) -> String
where
    S: AsRef<str>,
    T: AsRef<str>,
{
    if import_path.as_ref().starts_with('/') {
        normalize_program_identifier(&import_path.as_ref()[1..])
    } else {
        let mut identifier_elements = current_identifier.as_ref().split('/').collect::<Vec<_>>();
        identifier_elements.pop();
        normalize_program_identifier(identifier_elements.join("/") + "/" + import_path.as_ref())
    }
}

/// Escapes `"` and `\` in a string.
#[must_use]
pub fn escape_str(s: &str) -> Cow<str> {
    if s.contains('"') || s.contains('\\') {
        let mut escaped = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '"' => escaped.push_str("\\\""),
                '\\' => escaped.push_str("\\\\"),
                _ => escaped.push(c),
            }
        }
        Cow::Owned(escaped)
    } else {
        Cow::Borrowed(s)
    }
}

/// Transforms an identifier to a macro name that only contains `a-zA-Z0-9_`.
#[must_use]
pub fn identifier_to_macro(ident: &str) -> Cow<str> {
    if ident
        .chars()
        .any(|c| !(c == '_' && c.is_ascii_alphanumeric()))
    {
        let new_ident = ident
            .chars()
            .filter(|c| *c == '_' || c.is_ascii_alphanumeric())
            .collect::<String>();

        let chksum = md5::hash(ident).to_hex_lowercase();

        Cow::Owned(new_ident + "_" + &chksum[..8])
    } else {
        Cow::Borrowed(ident)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_str() {
        assert_eq!(escape_str("Hello, world!"), "Hello, world!");
        assert_eq!(escape_str(r#"Hello, "world"!"#), r#"Hello, \"world\"!"#);
        assert_eq!(escape_str(r"Hello, \world\!"), r"Hello, \\world\\!");
    }
}
