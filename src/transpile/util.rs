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
