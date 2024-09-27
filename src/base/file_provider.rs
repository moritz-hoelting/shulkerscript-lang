use std::{
    borrow::Cow,
    fmt::Display,
    path::{Path, PathBuf},
    sync::Arc,
};

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
        let string = std::str::from_utf8(&bytes)
            .map_err(|err| {
                let arc: Arc<dyn std::error::Error + Send + Sync> = Arc::new(err);
                Error::other(arc)
            })?
            .to_string();
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
            .map_err(Error::from)
    }

    fn read_str<P: AsRef<Path>>(&self, path: P) -> Result<Cow<str>, Error> {
        let full_path = self.root.join(path);
        std::fs::read_to_string(full_path)
            .map(Cow::Owned)
            .map_err(Error::from)
    }
}

/// The error type for [`FileProvider`] operations.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, thiserror::Error)]
pub struct Error {
    kind: std::io::ErrorKind,
    #[source]
    error: Option<Arc<dyn std::error::Error + Send + Sync>>,
}

impl Error {
    /// Creates a new [`Error`] from a known kind of error as well as an
    /// arbitrary error payload.
    ///
    /// The `error` argument is an arbitrary
    /// payload which will be contained in this [`Error`].
    ///
    /// Note that this function allocates memory on the heap.
    /// If no extra payload is required, use the `From` conversion from
    /// `ErrorKind`.
    pub fn new<E>(kind: std::io::ErrorKind, error: E) -> Self
    where
        E: Into<Arc<dyn std::error::Error + Send + Sync>>,
    {
        Self {
            kind,
            error: Some(error.into()),
        }
    }

    /// Creates a new [`Error`] from an arbitrary error payload.
    ///
    /// It is a shortcut for [`Error::new`]
    /// with [`std::io::ErrorKind::Other`].
    pub fn other<E>(error: E) -> Self
    where
        E: Into<Arc<dyn std::error::Error + Send + Sync>>,
    {
        Self::new(std::io::ErrorKind::Other, error)
    }

    /// Returns a reference to the inner error wrapped by this error (if any).
    ///
    /// If this [`Error`] was constructed via [`Self::new`] then this function will
    /// return [`Some`], otherwise it will return [`None`].
    #[must_use]
    pub fn get_ref(&self) -> Option<&(dyn std::error::Error + Send + Sync + 'static)> {
        return self.error.as_deref();
    }

    /// Consumes the [`Error`], returning its inner error (if any).
    ///
    /// If this [`Error`] was constructed via [`Self::new`] then this function will
    /// return [`Some`], otherwise it will return [`None`].
    #[must_use]
    pub fn into_inner(self) -> Option<Arc<dyn std::error::Error + Send + Sync>> {
        self.error
    }

    /// Returns the corresponding [`std::io::ErrorKind`] for this error.
    #[must_use]
    pub fn kind(&self) -> std::io::ErrorKind {
        self.kind
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            Some(err) => write!(f, "{}: {}", self.kind, err),
            None => write!(f, "{}", self.kind),
        }
    }
}

impl PartialEq for Error {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl From<std::io::ErrorKind> for Error {
    fn from(value: std::io::ErrorKind) -> Self {
        Self {
            kind: value,
            error: None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        let kind = value.kind();
        let error = value.into_inner().map(Arc::from);

        Self { kind, error }
    }
}

#[cfg(feature = "shulkerbox")]
mod vfs {
    use std::{borrow::Cow, sync::Arc};

    use super::{Error, FileProvider, Path};
    use shulkerbox::virtual_fs::{VFile, VFolder};

    impl FileProvider for VFolder {
        fn read_bytes<P: AsRef<Path>>(&self, path: P) -> Result<Cow<[u8]>, Error> {
            normalize_path_str(path).map_or_else(
                || Err(Error::from(std::io::ErrorKind::InvalidData)),
                |path| {
                    self.get_file(&path)
                        .ok_or_else(|| Error::from(std::io::ErrorKind::NotFound))
                        .map(|file| Cow::Borrowed(file.as_bytes()))
                },
            )
        }

        fn read_str<P: AsRef<Path>>(&self, path: P) -> Result<Cow<str>, Error> {
            normalize_path_str(path).map_or_else(
                || Err(Error::from(std::io::ErrorKind::InvalidData)),
                |path| {
                    self.get_file(&path)
                        .ok_or_else(|| Error::from(std::io::ErrorKind::NotFound))
                        .and_then(|file| match file {
                            VFile::Text(text) => Ok(Cow::Borrowed(text.as_str())),
                            VFile::Binary(bin) => {
                                let string = std::str::from_utf8(bin).map_err(|err| {
                                    let arc: Arc<dyn std::error::Error + Send + Sync> =
                                        Arc::new(err);
                                    Error::new(std::io::ErrorKind::InvalidData, arc)
                                })?;

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
            assert!(dir.read_str("nonexistent.txt").is_err());
        }
    }
}
