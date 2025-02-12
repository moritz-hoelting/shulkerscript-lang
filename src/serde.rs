//! Utilities for (de-)serializing

use std::{
    collections::HashMap,
    sync::{Arc, LazyLock, Mutex, RwLock},
};

use serde::{de::{self, Visitor}, ser::SerializeStruct, Deserialize, Serialize};

use crate::base::source_file::{SourceFile, Span};

static SERIALIZE_DATA: LazyLock<Mutex<SerializeData>> =
    LazyLock::new(|| Mutex::new(SerializeData::default()));

static DEDUPLICATE_SOURCE_FILES: LazyLock<RwLock<bool>> = LazyLock::new(|| RwLock::new(false));

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
