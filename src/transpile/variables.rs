#![expect(unused)]

use std::{
    collections::HashMap,
    sync::{Arc, OnceLock, RwLock},
};

use strum::EnumIs;

use super::{FunctionData, Transpiler};

#[derive(Debug, Clone, EnumIs)]
pub enum VariableType {
    Function {
        function_data: FunctionData,
        path: OnceLock<String>,
    },
    FunctionArgument {
        index: usize,
    },
    Scoreboard {
        objective: String,
    },
    ScoreboardValue {
        objective: String,
        name: String,
    },
    ScoreboardArray {
        objective: String,
        names: Vec<String>,
    },
    Tag {
        tag_name: String,
    },
    BooleanStorage {
        storage_name: String,
        path: String,
    },
    BooleanStorageArray {
        storage_name: String,
        paths: Vec<String>,
    },
}

#[derive(Debug, Default)]
pub struct Scope<'a> {
    parent: Option<&'a Arc<Self>>,
    variables: RwLock<HashMap<String, Arc<VariableType>>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    pub fn with_parent(parent: &'a Arc<Self>) -> Arc<Self> {
        Arc::new(Self {
            parent: Some(parent),
            ..Default::default()
        })
    }

    pub fn get_variable(&self, name: &str) -> Option<Arc<VariableType>> {
        let var = self.variables.read().unwrap().get(name).cloned();
        if var.is_some() {
            var
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(name))
        }
    }

    pub fn set_variable(&self, name: &str, var: VariableType) {
        self.variables
            .write()
            .unwrap()
            .insert(name.to_string(), Arc::new(var));
    }

    pub fn get_variables(&self) -> &RwLock<HashMap<String, Arc<VariableType>>> {
        &self.variables
    }

    pub fn get_parent(&self) -> Option<Arc<Self>> {
        self.parent.map(|v| v.clone())
    }
}

impl Transpiler {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope() {
        let scope = Scope::new();
        scope.set_variable(
            "test",
            VariableType::Scoreboard {
                objective: "test".to_string(),
            },
        );
        if let Some(var) = scope.get_variable("test") {
            match var.as_ref() {
                VariableType::Scoreboard { objective } => assert_eq!(objective, "test"),
                _ => panic!("Incorrect Variable"),
            }
        } else {
            panic!("Variable missing")
        }
    }

    #[test]
    fn test_parent() {
        let scope = Scope::new();
        scope.set_variable(
            "test",
            VariableType::Scoreboard {
                objective: "test".to_string(),
            },
        );
        let child = Scope::with_parent(&scope);
        if let Some(var) = child.get_variable("test") {
            match var.as_ref() {
                VariableType::Scoreboard { objective } => assert_eq!(objective, "test"),
                _ => panic!("Incorrect Variable"),
            }
        } else {
            panic!("Variable missing")
        }
    }
}
