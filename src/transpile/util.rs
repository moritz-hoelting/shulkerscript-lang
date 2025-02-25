//! Utility methods for transpiling

#[cfg(feature = "shulkerbox")]
use chksum_md5 as md5;

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

/// Transforms an identifier to a macro name that only contains `a-zA-Z0-9_`.
#[cfg(feature = "shulkerbox")]
#[must_use]
pub fn identifier_to_macro(ident: &str) -> std::borrow::Cow<str> {
    if ident.contains("__")
        || ident
            .chars()
            .any(|c| c == '_' || !c.is_ascii_alphanumeric())
    {
        let new_ident = ident
            .chars()
            .filter(|c| *c != '_' && c.is_ascii_alphanumeric())
            .collect::<String>();

        let chksum = md5::hash(ident).to_hex_lowercase();

        std::borrow::Cow::Owned(new_ident + "__" + &chksum[..8])
    } else {
        std::borrow::Cow::Borrowed(ident)
    }
}
