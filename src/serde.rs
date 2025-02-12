//! Utilities for (de-)serializing

use std::{
    collections::HashMap,
    marker::PhantomData,
    sync::{Arc, LazyLock, Mutex, RwLock},
};

use serde::{
    de::{self, Visitor},
    ser::SerializeStruct,
    Deserialize, Serialize,
};

use crate::base::source_file::{SourceFile, Span};

static DEDUPLICATE_SOURCE_FILES: LazyLock<RwLock<bool>> = LazyLock::new(|| RwLock::new(false));

static SERIALIZE_DATA: LazyLock<Mutex<SerializeData>> =
    LazyLock::new(|| Mutex::new(SerializeData::default()));

static DESERIALIZE_DATA: LazyLock<RwLock<Option<DeserializeData>>> =
    LazyLock::new(|| RwLock::new(None));

/// Wrapper to remove duplicate source file data during (de-)serialization
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
        *DEDUPLICATE_SOURCE_FILES.write().unwrap() = true;
        SERIALIZE_DATA.lock().unwrap().clear();
        let mut s = serializer.serialize_struct("SourceFileWrapper", 2)?;
        s.serialize_field("data", &self.0)?;
        *DEDUPLICATE_SOURCE_FILES.write().unwrap() = false;
        s.serialize_field(
            "source_files",
            &SERIALIZE_DATA.lock().unwrap().id_to_source_file,
        )?;
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
                let source_files: HashMap<usize, SourceFile> = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                *DESERIALIZE_DATA.write().unwrap() = Some(DeserializeData {
                    id_to_source_file: source_files
                        .into_iter()
                        .map(|(k, v)| (k, Arc::new(v)))
                        .collect(),
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
                let mut source_files: Option<HashMap<usize, SourceFile>> = None;
                let mut data = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Data => {
                            if data.is_some() {
                                return Err(de::Error::duplicate_field("data"));
                            }
                            *DESERIALIZE_DATA.write().unwrap() =
                                source_files.as_ref().map(|source_files| DeserializeData {
                                    id_to_source_file: source_files
                                        .iter()
                                        .map(|(&k, v)| (k, Arc::new(v.clone())))
                                        .collect(),
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

        *DEDUPLICATE_SOURCE_FILES.write().unwrap() = true;
        *DESERIALIZE_DATA.write().unwrap() = None;
        let res = deserializer.deserialize_struct(
            "SerdeWrapper",
            &["source_files", "data"],
            WrapperVisitor(PhantomData::<T>::default()),
        );
        *DEDUPLICATE_SOURCE_FILES.write().unwrap() = false;

        res
    }
}

/// Internally used for Serialization
#[derive(Debug, Default)]
struct SerializeData {
    id_counter: usize,
    ptr_to_id: HashMap<usize, usize>,
    id_to_source_file: HashMap<usize, SourceFile>,
}

impl SerializeData {
    fn clear(&mut self) {
        self.id_counter = 0;
        self.id_to_source_file.clear();
        self.ptr_to_id.clear();
    }

    /// Get id of already stored [`Arc`] or store it and return new id
    pub fn get_id_of(&mut self, source_file: &Arc<SourceFile>) -> usize {
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

impl Serialize for Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_struct("Span", 3)?;
        s.serialize_field("start", &self.start())?;
        s.serialize_field("end", &self.end())?;

        if *DEDUPLICATE_SOURCE_FILES.read().unwrap() {
            let mut data = SERIALIZE_DATA.lock().unwrap();
            s.serialize_field("source_file", &data.get_id_of(self.source_file()))?;
        } else {
            s.serialize_field("source_file", self.source_file())?;
        }

        s.end()
    }
}

#[derive(Debug, Default)]
struct DeserializeData {
    id_to_source_file: HashMap<usize, Arc<SourceFile>>,
}

impl<'de> Deserialize<'de> for Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Field {
            Start,
            End,
            SourceFile,
        }

        struct SpanVisitor;

        impl<'de> Visitor<'de> for SpanVisitor {
            type Value = Span;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                if *DEDUPLICATE_SOURCE_FILES.read().unwrap() {
                    formatter.write_str("struct Span with deduplicated SourceFiles")
                } else {
                    formatter.write_str("struct Span")
                }
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Self::Value, V::Error>
            where
                V: serde::de::SeqAccess<'de>,
            {
                let start = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let end = seq
                    .next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let source_file = if *DEDUPLICATE_SOURCE_FILES.read().unwrap() {
                    DESERIALIZE_DATA
                        .read()
                        .unwrap()
                        .as_ref()
                        .ok_or_else(|| {
                            de::Error::custom("SourceFiles do not have been loaded yet")
                        })?
                        .id_to_source_file
                        .get(
                            &seq.next_element()?
                                .ok_or_else(|| de::Error::invalid_length(2, &self))?,
                        )
                        .ok_or_else(|| de::Error::custom("invalid source_file id"))?
                        .clone()
                } else {
                    Arc::new(
                        seq.next_element()?
                            .ok_or_else(|| de::Error::invalid_length(2, &self))?,
                    )
                };

                Ok(Span::new(source_file, start, end)
                    .ok_or_else(|| de::Error::custom("Invalid data"))?)
            }

            fn visit_map<V>(self, mut map: V) -> Result<Self::Value, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut start = None;
                let mut end = None;
                let mut source_file = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Start => {
                            if start.is_some() {
                                return Err(de::Error::duplicate_field("start"));
                            }
                            start = Some(map.next_value()?);
                        }
                        Field::End => {
                            if end.is_some() {
                                return Err(de::Error::duplicate_field("end"));
                            }
                            end = Some(map.next_value()?);
                        }
                        Field::SourceFile => {
                            if source_file.is_some() {
                                return Err(de::Error::duplicate_field("source_file"));
                            }
                            source_file = if *DEDUPLICATE_SOURCE_FILES.read().unwrap() {
                                Some(
                                    DESERIALIZE_DATA
                                        .read()
                                        .unwrap()
                                        .as_ref()
                                        .ok_or_else(|| {
                                            de::Error::custom(
                                                "SourceFiles do not have been loaded yet",
                                            )
                                        })?
                                        .id_to_source_file
                                        .get(&map.next_value()?)
                                        .ok_or_else(|| de::Error::custom("invalid source_file id"))?
                                        .clone(),
                                )
                            } else {
                                Some(Arc::new(map.next_value()?))
                            };
                        }
                    }
                }
                let start = start.ok_or_else(|| de::Error::missing_field("start"))?;
                let end = end.ok_or_else(|| de::Error::missing_field("end"))?;
                let source_file = source_file.ok_or_else(|| de::Error::missing_field("source"))?;

                Ok(Span::new(source_file, start, end)
                    .ok_or_else(|| de::Error::custom("Invalid data"))?)
            }
        }

        deserializer.deserialize_struct("Span", &["start", "end", "source_file"], SpanVisitor)
    }
}
