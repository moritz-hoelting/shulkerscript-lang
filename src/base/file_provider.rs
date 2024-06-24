use std::path::{Path, PathBuf};

use super::Error;

/// A trait for providing file contents.
pub trait FileProvider {
    /// Reads the contents of the file at the given path.
    ///
    /// # Errors
    /// - If an error occurs while reading the file.
    /// - If the file does not exist.
    fn read_to_string<P: AsRef<Path>>(&self, path: P) -> Result<String, Error>;
}

/// Provides file contents from the file system.
#[derive(Debug, Clone)]
pub struct FsProvider {
    /// The root directory to base paths off of.
    root: PathBuf,
}

impl Default for FsProvider {
    fn default() -> Self {
        Self {
            root: PathBuf::from("."),
        }
    }
}

impl FileProvider for FsProvider {
    fn read_to_string<P: AsRef<Path>>(&self, path: P) -> Result<String, Error> {
        let full_path = self.root.join(path);
        std::fs::read_to_string(full_path).map_err(|err| Error::IoError(err.to_string()))
    }
}

#[cfg(feature = "shulkerbox")]
mod vfs {
    use super::{Error, FileProvider, Path};
    use shulkerbox::virtual_fs::{VFile, VFolder};

    impl FileProvider for VFolder {
        fn read_to_string<P: AsRef<Path>>(&self, path: P) -> Result<String, Error> {
            normalize_path_str(path).map_or_else(
                || Err(Error::IoError("Invalid path".to_string())),
                |path| {
                    self.get_file(&path)
                        .ok_or_else(|| Error::IoError("File not found".to_string()))
                        .and_then(|file| match file {
                            VFile::Text(text) => Ok(text.to_owned()),
                            VFile::Binary(bin) => String::from_utf8(bin.clone())
                                .map_err(|err| Error::IoError(err.to_string())),
                        })
                },
            )
        }
    }

    fn normalize_path_str<P: AsRef<Path>>(path: P) -> Option<String> {
        let mut err = false;
        let res = path
            .as_ref()
            .to_str()?
            .split('/')
            .fold(Vec::new(), |mut acc, el| match el {
                "." | "" => acc,
                ".." => {
                    let popped = acc.pop();
                    if popped.is_none() {
                        err = true;
                    }
                    acc
                }
                _ => {
                    acc.push(el);
                    acc
                }
            })
            .join("/");

        if err {
            None
        } else {
            Some(res)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_normalize_path() {
            assert_eq!(normalize_path_str("a/b/c"), Some("a/b/c".to_string()));
            assert_eq!(normalize_path_str("a/b/../c"), Some("a/c".to_string()));
            assert_eq!(normalize_path_str("./a/b/c"), Some("a/b/c".to_string()));
            assert_eq!(normalize_path_str("../a/b/c"), None);
        }

        #[test]
        fn test_vfolder_provider() {
            let mut dir = VFolder::new();
            dir.add_file("foo.txt", VFile::Text("foo".to_string()));
            dir.add_file("bar/baz.txt", VFile::Text("bar, baz".to_string()));

            assert_eq!(dir.read_to_string("foo.txt").unwrap(), "foo".to_string());
            assert_eq!(
                dir.read_to_string("bar/baz.txt").unwrap(),
                "bar, baz".to_string()
            );
            assert!(matches!(
                dir.read_to_string("nonexistent.txt"),
                Err(Error::IoError(_))
            ));
        }
    }
}
