#![expect(unused)]

use std::{collections::HashMap, sync::RwLock};

use super::Transpiler;

#[derive(Debug, Clone)]
pub enum VariableType {
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
    parent: Option<&'a Scope<'a>>,
    variables: RwLock<HashMap<String, VariableType>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_child(&'a self) -> Self {
        Self {
            parent: Some(self),
            variables: RwLock::new(HashMap::new()),
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<VariableType> {
        let var = self.variables.read().unwrap().get(name).cloned();
        if var.is_some() {
            var
        } else {
            self.parent.and_then(|parent| parent.get_variable(name))
        }
    }

    pub fn set_variable(&self, name: &str, var: VariableType) {
        self.variables
            .write()
            .unwrap()
            .insert(name.to_string(), var);
    }

    pub fn get_parent(&self) -> Option<&'a Self> {
        self.parent
    }
}

impl Transpiler {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope() {
        let scope = Scope::new();
        {
            let mut variables = scope.variables.write().unwrap();
            variables.insert(
                "test".to_string(),
                VariableType::Scoreboard {
                    objective: "test".to_string(),
                },
            );
        }
        match scope.get_variable("test") {
            Some(VariableType::Scoreboard { objective }) => assert_eq!(objective, "test"),
            _ => panic!("Incorrect Variable"),
        }
    }

    #[test]
    fn test_parent() {
        let scope = Scope::new();
        {
            let mut variables = scope.variables.write().unwrap();
            variables.insert(
                "test".to_string(),
                VariableType::Scoreboard {
                    objective: "test".to_string(),
                },
            );
        }
        let child = scope.new_child();
        match child.get_variable("test") {
            Some(VariableType::Scoreboard { objective }) => assert_eq!(objective, "test"),
            _ => panic!("Incorrect Variable"),
        }
    }
}
