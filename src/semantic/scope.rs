use std::{collections::HashMap, sync::RwLock};

/// Type of variable
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum VariableType {
    /// A function.
    Function,
    /// A macro function parameter.
    MacroParameter,
    /// A scoreboard.
    Scoreboard,
    /// A scoreboard value.
    ScoreboardValue,
    /// Multiple values stored in scoreboard.
    ScoreboardArray,
    /// A tag applied to entities.
    Tag,
    /// A boolean stored in a data storage.
    BooleanStorage,
    /// Multiple booleans stored in a data storage array.
    BooleanStorageArray,
    /// Compiler internal function.
    InternalFunction,
    /// Compiler internal value.
    ComptimeValue,
}

/// A scope that stores variables.
#[derive(Debug, Default)]
pub struct SemanticScope<'a> {
    /// Parent scope where variables are inherited from.
    parent: Option<&'a Self>,
    /// Variables stored in the scope.
    variables: RwLock<HashMap<String, VariableType>>,
}

impl<'a> SemanticScope<'a> {
    /// Creates a new scope.
    #[must_use]
    pub fn new() -> Self {
        let scope = Self::default();

        scope.set_variable("print", VariableType::InternalFunction);

        scope
    }

    /// Creates a new scope with a parent.
    #[must_use]
    pub fn with_parent(parent: &'a Self) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    /// Gets a variable from the scope.
    pub fn get_variable(&self, name: &str) -> Option<VariableType> {
        let var = self.variables.read().unwrap().get(name).copied();
        if var.is_some() {
            var
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(name))
        }
    }

    /// Sets a variable in the scope.
    pub fn set_variable(&self, name: &str, var: VariableType) {
        self.variables
            .write()
            .unwrap()
            .insert(name.to_string(), var);
    }

    /// Gets the variables stored in the current scope.
    pub fn get_local_variables(&self) -> &RwLock<HashMap<String, VariableType>> {
        &self.variables
    }

    /// Gets all variables stored in the scope.
    ///
    /// This function does not return a reference to the variables, but clones them.
    pub fn get_all_variables(&self) -> HashMap<String, VariableType> {
        let mut variables = self.variables.read().unwrap().clone();
        if let Some(parent) = self.parent.as_ref() {
            variables.extend(parent.get_all_variables());
        }
        variables
    }

    /// Gets the parent scope.
    pub fn get_parent(&self) -> Option<&Self> {
        self.parent
    }
}
