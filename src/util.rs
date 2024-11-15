//! Utility functions for the `Shulkerscript` language.

use std::borrow::Cow;

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

/// Unescapes '\`', `\`, `\n`, `\r` and `\t` in a string.
#[must_use]
pub fn unescape_macro_string(s: &str) -> Cow<str> {
    if s.contains('\\') || s.contains('`') {
        Cow::Owned(
            s.replace("\\n", "\n")
                .replace("\\r", "\r")
                .replace("\\t", "\t")
                .replace("\\`", "`")
                .replace("\\\\", "\\"),
        )
    } else {
        Cow::Borrowed(s)
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

    #[test]
    fn test_unescape_macro_string() {
        assert_eq!(unescape_macro_string("Hello, world!"), "Hello, world!");
        assert_eq!(
            unescape_macro_string(r#"Hello, "world"!"#),
            r#"Hello, "world"!"#
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \world\!"),
            r"Hello, \world\!"
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \nworld\!"),
            "Hello, \nworld\\!"
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \rworld\!"),
            "Hello, \rworld\\!"
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \tworld\!"),
            "Hello, \tworld\\!"
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \`world\!"),
            r"Hello, `world\!"
        );
        assert_eq!(
            unescape_macro_string(r"Hello, \\world\!"),
            r"Hello, \world\!"
        );
    }
}
