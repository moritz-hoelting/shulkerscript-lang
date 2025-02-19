//! Utilities for (de-)serializing

use std::{
    collections::BTreeMap,
    marker::PhantomData,
    sync::{Arc, LazyLock, Mutex, RwLock},
};

use serde::{
    de::{self, Visitor},
    ser::SerializeStruct,
    Deserialize, Serialize,
};

use crate::base::source_file::SourceFile;

thread_local! {
    static DEDUPLICATE_SOURCE_FILES: LazyLock<RwLock<bool>> = LazyLock::new(|| RwLock::new(false));

    static SERIALIZE_DATA: LazyLock<Mutex<SerializeData>> =
        LazyLock::new(|| Mutex::new(SerializeData::default()));

    static DESERIALIZE_DATA: LazyLock<RwLock<Option<DeserializeData>>> =
        LazyLock::new(|| RwLock::new(None));
}

/// Wrapper to remove duplicate source file data during (de-)serialization
#[expect(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct SerdeWrapper<T>(pub T);

impl<T> Serialize for SerdeWrapper<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        DEDUPLICATE_SOURCE_FILES.with(|d| *d.write().unwrap() = true);
        SERIALIZE_DATA.with(|d| d.lock().unwrap().clear());
        // hold guard so no other can serialize at the same time in same thread
        let s = DEDUPLICATE_SOURCE_FILES.with(|d| {
            let guard = d.read().unwrap();
            let mut serialized_data = flexbuffers::FlexbufferSerializer::new();
            self.0
                .serialize(&mut serialized_data)
                .map_err(|_| serde::ser::Error::custom("could not buffer serialization"))?;
            drop(serialized_data);
            let mut s = serializer.serialize_struct("SerdeWrapper", 3)?;

            SERIALIZE_DATA.with(|d| {
                s.serialize_field("source_files", &d.lock().unwrap().id_to_source_file)
            })?;
            s.serialize_field("data", &self.0)?;
            drop(guard);
            Ok(s)
        })?;

        DEDUPLICATE_SOURCE_FILES.with(|d| *d.write().unwrap() = false);
        s.end()
    }
}

impl<'de, T> Deserialize<'de> for SerdeWrapper<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Field {
            Data,
            SourceFiles,
        }

        struct WrapperVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for WrapperVisitor<T>
        where
            T: Deserialize<'de>,
        {
            type Value = SerdeWrapper<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct SerdeWrapper")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: de::SeqAccess<'de>,
            {
                let source_files: BTreeMap<u64, SourceFile> = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                DESERIALIZE_DATA.with(|d| {
                    *d.write().unwrap() = Some(DeserializeData {
                        id_to_source_file: source_files
                            .into_iter()
                            .map(|(k, v)| (k, Arc::new(v)))
                            .collect(),
                    })
                });
                let data = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;

                Ok(SerdeWrapper(data))
            }

            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut source_files: Option<BTreeMap<u64, SourceFile>> = None;
                let mut data = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Data => {
                            if data.is_some() {
                                return Err(de::Error::duplicate_field("data"));
                            }
                            DESERIALIZE_DATA.with(|d| {
                                *d.write().unwrap() =
                                    source_files.as_ref().map(|source_files| DeserializeData {
                                        id_to_source_file: source_files
                                            .iter()
                                            .map(|(&k, v)| (k, Arc::new(v.clone())))
                                            .collect(),
                                    })
                            });
                            data = Some(map.next_value()?);
                        }
                        Field::SourceFiles => {
                            if source_files.is_some() {
                                return Err(de::Error::duplicate_field("source_files"));
                            }
                            source_files = Some(map.next_value()?);
                        }
                    }
                }

                let data = data.ok_or_else(|| de::Error::missing_field("data"))?;
                Ok(SerdeWrapper(data))
            }
        }

        DEDUPLICATE_SOURCE_FILES.with(|d| *d.write().unwrap() = true);
        DESERIALIZE_DATA.with(|d| *d.write().unwrap() = None);
        let res = deserializer.deserialize_struct(
            "SerdeWrapper",
            &["source_files", "data"],
            WrapperVisitor(PhantomData::<T>),
        );
        DEDUPLICATE_SOURCE_FILES.with(|d| *d.write().unwrap() = false);

        res
    }
}

/// Internally used for Serialization
#[derive(Debug, Default)]
struct SerializeData {
    id_counter: u64,
    ptr_to_id: BTreeMap<usize, u64>,
    id_to_source_file: BTreeMap<u64, SourceFile>,
}

impl SerializeData {
    fn clear(&mut self) {
        self.id_counter = 0;
        self.id_to_source_file.clear();
        self.ptr_to_id.clear();
    }

    /// Get id of already stored [`Arc`] or store it and return new id
    pub fn get_id_of(&mut self, source_file: &Arc<SourceFile>) -> u64 {
        let ptr = Arc::as_ptr(source_file);
        if let Some(&id) = self.ptr_to_id.get(&(ptr as usize)) {
            id
        } else {
            let id = self.id_counter;
            self.id_counter += 1;

            self.ptr_to_id.insert(ptr as usize, id);
            self.id_to_source_file
                .insert(id, Arc::unwrap_or_clone(source_file.to_owned()));

            id
        }
    }
}

#[derive(Debug, Default)]
struct DeserializeData {
    id_to_source_file: BTreeMap<u64, Arc<SourceFile>>,
}

pub mod source_file {
    use std::sync::Arc;

    use serde::{de, Deserialize, Serialize};

    use crate::{base::source_file::SourceFile, serde::DESERIALIZE_DATA};

    use super::{DEDUPLICATE_SOURCE_FILES, SERIALIZE_DATA};

    pub fn serialize<S>(this: &Arc<SourceFile>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        if DEDUPLICATE_SOURCE_FILES.with(|d| *d.read().unwrap()) {
            SERIALIZE_DATA.with(|d| {
                let mut data = d.lock().unwrap();
                serializer.serialize_u64(data.get_id_of(this))
            })
        } else {
            this.as_ref().serialize(serializer)
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Arc<SourceFile>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        if DEDUPLICATE_SOURCE_FILES.with(|d| *d.read().unwrap()) {
            let id = u64::deserialize(deserializer)?;
            Ok(DESERIALIZE_DATA.with(|d| {
                d.read()
                    .unwrap()
                    .as_ref()
                    .ok_or_else(|| de::Error::custom("SourceFiles do not have been loaded yet"))?
                    .id_to_source_file
                    .get(&id)
                    .map(Arc::clone)
                    .ok_or_else(|| serde::de::Error::custom("invalid source_file id"))
            }))?
        } else {
            Ok(Arc::new(SourceFile::deserialize(deserializer)?))
        }
    }
}

#[cfg(all(test, feature = "shulkerbox"))]
mod tests {
    use std::path::Path;

    use shulkerbox::virtual_fs::{VFile, VFolder};

    use crate::{base::SilentHandler, syntax::syntax_tree::program::ProgramFile};

    use super::*;

    #[test]
    fn test_serde_wrapper() {
        let mut vfolder = VFolder::new();
        let vfile = VFile::Text(r#"namespace "test";"#.to_string());
        vfolder.add_file("main.shu", vfile);

        let parsed = crate::parse(
            &SilentHandler::new(),
            &vfolder,
            Path::new("main.shu"),
            "main".to_string(),
        )
        .unwrap();

        let wrapper = SerdeWrapper(parsed);

        let serialized = serde_json::to_string_pretty(&wrapper).unwrap();
        let SerdeWrapper(deserialized) =
            serde_json::from_str::<SerdeWrapper<ProgramFile>>(&serialized).unwrap();

        assert_eq!(
            Arc::as_ptr(
                deserialized
                    .namespace()
                    .namespace_keyword()
                    .span
                    .source_file()
            ),
            Arc::as_ptr(deserialized.namespace().namespace_name().span.source_file())
        );
    }
}
