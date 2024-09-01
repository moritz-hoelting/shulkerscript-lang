use std::{
    borrow::Cow,
    path::{Path, PathBuf},
};

use super::Error;

/// A trait for providing file contents.
pub trait FileProvider {
    /// Reads the contents of the file at the given path as bytes.
    ///
    /// # Errors
    /// - If an error occurs while reading the file.
    /// - If the file does not exist.
    fn read_bytes<P: AsRef<Path>>(&self, path: P) -> Result<Cow<[u8]>, Error>;

    /// Reads the contents of the file at the given path.
    ///
    /// # Errors
    /// - If an error occurs while reading the file.
    /// - If the file does not exist.
    /// - If the file is not valid UTF-8.
    fn read_str<P: AsRef<Path>>(&self, path: P) -> Result<Cow<str>, Error> {
        let bytes = self.read_bytes(path)?;
        let string = std::str::from_utf8(&bytes)?.to_string();
        Ok(Cow::Owned(string))
    }
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

impl<P> From<P> for FsProvider
where
    P: Into<PathBuf>,
{
    fn from(root: P) -> Self {
        Self { root: root.into() }
    }
}

impl FileProvider for FsProvider {
    fn read_bytes<P: AsRef<Path>>(&self, path: P) -> Result<Cow<[u8]>, Error> {
        let full_path = self.root.join(path);
        std::fs::read(full_path)
            .map(Cow::Owned)
            .map_err(|err| Error::IoError(err.to_string()))
    }

    fn read_str<P: AsRef<Path>>(&self, path: P) -> Result<Cow<str>, Error> {
        let full_path = self.root.join(path);
        std::fs::read_to_string(full_path)
            .map(Cow::Owned)
            .map_err(|err| Error::IoError(err.to_string()))
    }
}

#[cfg(feature = "shulkerbox")]
mod vfs {
    use std::borrow::Cow;

    use super::{Error, FileProvider, Path};
    use shulkerbox::virtual_fs::{VFile, VFolder};

    impl FileProvider for VFolder {
        fn read_bytes<P: AsRef<Path>>(&self, path: P) -> Result<Cow<[u8]>, Error> {
            normalize_path_str(path).map_or_else(
                || Err(Error::IoError("Invalid path".to_string())),
                |path| {
                    self.get_file(&path)
                        .ok_or_else(|| Error::IoError("File not found".to_string()))
                        .map(|file| Cow::Borrowed(file.as_bytes()))
                },
            )
        }

        fn read_str<P: AsRef<Path>>(&self, path: P) -> Result<Cow<str>, Error> {
            normalize_path_str(path).map_or_else(
                || Err(Error::IoError("Invalid path".to_string())),
                |path| {
                    self.get_file(&path)
                        .ok_or_else(|| Error::IoError("File not found".to_string()))
                        .and_then(|file| match file {
                            VFile::Text(text) => Ok(Cow::Borrowed(text.as_str())),
                            VFile::Binary(bin) => {
                                let string = std::str::from_utf8(bin)
                                    .map_err(|err| Error::IoError(err.to_string()))?;

                                Ok(Cow::Borrowed(string))
                            }
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

            assert_eq!(
                dir.read_str("foo.txt").unwrap().into_owned(),
                "foo".to_string()
            );
            assert_eq!(
                dir.read_str("bar/baz.txt").unwrap().into_owned(),
                "bar, baz".to_string()
            );
            assert!(matches!(
                dir.read_str("nonexistent.txt"),
                Err(Error::IoError(_))
            ));
        }
    }
}
