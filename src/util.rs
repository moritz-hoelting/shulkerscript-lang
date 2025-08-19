//! Utility functions for the `Shulkerscript` language.

use std::borrow::Cow;

/// Escapes `"` and `\` in a string.
#[must_use]
pub fn escape_str(s: &str) -> Cow<'_, str> {
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
pub fn unescape_macro_string(s: &str) -> Cow<'_, str> {
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

/// Transforms an identifier to a macro name that only contains `a-zA-Z0-9_`.
#[cfg(feature = "shulkerbox")]
#[must_use]
pub fn identifier_to_macro(ident: &str) -> std::borrow::Cow<'_, str> {
    if ident.contains("__")
        || ident
            .chars()
            .any(|c| c == '_' || !c.is_ascii_alphanumeric())
    {
        let new_ident = ident
            .chars()
            .filter(|c| *c != '_' && c.is_ascii_alphanumeric())
            .collect::<String>();

        let chksum = chksum_md5::hash(ident).to_hex_lowercase();

        std::borrow::Cow::Owned(new_ident + "__" + &chksum[..8])
    } else {
        std::borrow::Cow::Borrowed(ident)
    }
}

/// Transforms an identifier to a macro name that only contains `a-zA-Z0-9_`.
/// Does only strip invalid characters if the `shulkerbox` feature is not enabled.
#[cfg(not(feature = "shulkerbox"))]
#[must_use]
pub fn identifier_to_macro(ident: &str) -> std::borrow::Cow<'_, str> {
    if ident.contains("__")
        || ident
            .chars()
            .any(|c| c == '_' || !c.is_ascii_alphanumeric())
    {
        let new_ident = ident
            .chars()
            .filter(|c| *c != '_' && c.is_ascii_alphanumeric())
            .collect::<String>();

        std::borrow::Cow::Owned(new_ident)
    } else {
        std::borrow::Cow::Borrowed(ident)
    }
}

/// Transforms an identifier to a macro name that only contains `a-zA-Z0-9_`.
#[cfg(feature = "shulkerbox")]
#[must_use]
pub fn identifier_to_scoreboard_target(ident: &str) -> std::borrow::Cow<'_, str> {
    if !(..=16).contains(&ident.len())
        || ident
            .chars()
            .any(|c| c != '_' || !c.is_ascii_alphanumeric())
    {
        std::borrow::Cow::Owned(chksum_md5::hash(ident).to_hex_lowercase().split_off(16))
    } else {
        std::borrow::Cow::Borrowed(ident)
    }
}

/// Transforms an identifier to a name that only contains `a-zA-Z0-9_`.
/// Does only strip invalid characters if the `shulkerbox` feature is not enabled.
#[cfg(not(feature = "shulkerbox"))]
#[must_use]
pub fn identifier_to_scoreboard_target(ident: &str) -> std::borrow::Cow<'_, str> {
    if !(..=16).contains(&ident.len())
        || ident
            .chars()
            .any(|c| c != '_' || !c.is_ascii_alphanumeric())
    {
        let new_ident = ident
            .chars()
            .map(|c| {
                if c != '_' && !c.is_ascii_alphanumeric() {
                    '_'
                } else {
                    c
                }
            })
            .collect::<String>();

        std::borrow::Cow::Owned(new_ident)
    } else {
        std::borrow::Cow::Borrowed(ident)
    }
}

/// Returns whether a string is a valid scoreboard name.
#[must_use]
pub fn is_valid_scoreboard_objective_name(name: &str) -> bool {
    name.chars()
        .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '+' | '.'))
}

/// Returns whether a string is a valid scoreboard target.
#[must_use]
pub fn is_valid_scoreboard_target(name: &str) -> bool {
    (..=16).contains(&name.len()) && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
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
