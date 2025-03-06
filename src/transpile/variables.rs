#![expect(unused)]

use std::{
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    sync::{Arc, OnceLock, RwLock},
};

use chksum_md5 as md5;
use shulkerbox::prelude::Command;
use strum::EnumIs;

use crate::{
    base::{self, source_file::SourceElement as _, Handler},
    lexical::token::KeywordKind,
    syntax::syntax_tree::{
        expression::{Expression, Primary},
        statement::{SingleVariableDeclaration, VariableDeclaration},
    },
};

use super::{
    error::IllegalAnnotationContent, expression::DataLocation, FunctionData,
    TranspileAnnotationValue, TranspileError, TranspileResult, Transpiler,
};

/// Stores the data required to access a variable.
#[derive(Debug, Clone, EnumIs)]
pub enum VariableData {
    /// A function.
    Function {
        /// The function data.
        function_data: FunctionData,
        /// The path to the function once it is generated.
        path: OnceLock<String>,
    },
    /// A function argument/parameter.
    FunctionArgument {
        /// The index of the argument.
        index: usize,
    },
    /// A scoreboard.
    Scoreboard {
        /// The objective name.
        objective: String,
    },
    /// A scoreboard value.
    ScoreboardValue {
        /// The objective name.
        objective: String,
        /// The target.
        target: String,
    },
    /// Multiple values stored in scoreboard.
    ScoreboardArray {
        /// The objective name.
        objective: String,
        /// The targets.
        targets: Vec<String>,
    },
    /// A tag applied to entities.
    Tag {
        /// The tag name.
        tag_name: String,
    },
    /// A boolean stored in a data storage.
    BooleanStorage {
        /// The storage name.
        storage_name: String,
        /// The path to the boolean.
        path: String,
    },
    /// Multiple booleans stored in a data storage array.
    BooleanStorageArray {
        /// The storage name.
        storage_name: String,
        /// The paths to the booleans.
        paths: Vec<String>,
    },
}

/// A scope that stores variables.
#[derive(Default)]
pub struct Scope<'a> {
    /// Parent scope where variables are inherited from.
    parent: Option<&'a Arc<Self>>,
    /// Variables stored in the scope.
    variables: RwLock<HashMap<String, Arc<VariableData>>>,
}

impl<'a> Scope<'a> {
    /// Creates a new scope.
    #[must_use]
    pub fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    /// Creates a new scope with a parent.
    #[must_use]
    pub fn with_parent(parent: &'a Arc<Self>) -> Arc<Self> {
        Arc::new(Self {
            parent: Some(parent),
            ..Default::default()
        })
    }

    /// Gets a variable from the scope.
    pub fn get_variable(&self, name: &str) -> Option<Arc<VariableData>> {
        let var = self.variables.read().unwrap().get(name).cloned();
        if var.is_some() {
            var
        } else {
            self.parent
                .as_ref()
                .and_then(|parent| parent.get_variable(name))
        }
    }

    /// Sets a variable in the scope.
    pub fn set_variable(&self, name: &str, var: VariableData) {
        self.variables
            .write()
            .unwrap()
            .insert(name.to_string(), Arc::new(var));
    }

    /// Gets the variables stored in the current scope.
    pub fn get_local_variables(&self) -> &RwLock<HashMap<String, Arc<VariableData>>> {
        &self.variables
    }

    /// Gets all variables stored in the scope.
    ///
    /// This function does not return a reference to the variables, but clones them.
    pub fn get_all_variables(&self) -> HashMap<String, Arc<VariableData>> {
        let mut variables = self.variables.read().unwrap().clone();
        if let Some(parent) = self.parent.as_ref() {
            variables.extend(parent.get_all_variables());
        }
        variables
    }

    /// Gets the parent scope.
    pub fn get_parent(&self) -> Option<Arc<Self>> {
        self.parent.cloned()
    }
}

impl<'a> Debug for Scope<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct VariableWrapper<'a>(&'a RwLock<HashMap<String, Arc<VariableData>>>);
        impl<'a> Debug for VariableWrapper<'a> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let s = self.0.read().unwrap();
                s.deref().fmt(f)
            }
        }

        let mut s = f.debug_struct("Scope");
        s.field("parent", &self.parent);

        s.field("variables", &VariableWrapper(&self.variables));
        s.finish()
    }
}

impl Transpiler {
    pub(super) fn transpile_variable_declaration(
        &mut self,
        declaration: &VariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match declaration {
            VariableDeclaration::Single(single) => self.transpile_single_variable_declaration(
                single,
                program_identifier,
                scope,
                handler,
            ),
            _ => todo!("declarations not supported yet: {declaration:?}"),
        }
    }

    #[expect(clippy::too_many_lines)]
    fn transpile_single_variable_declaration(
        &mut self,
        single: &SingleVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let mut deobfuscate_annotations = single
            .annotations()
            .iter()
            .filter(|a| a.has_identifier("deobfuscate"));

        let variable_type = single.variable_type().keyword;

        let deobfuscate_annotation = deobfuscate_annotations.next();

        if let Some(duplicate) = deobfuscate_annotations.next() {
            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                annotation: duplicate.span(),
                message: "Multiple deobfuscate annotations are not allowed.".to_string(),
            });
            handler.receive(error.clone());
            return Err(error);
        }
        let (name, target) = if let Some(deobfuscate_annotation) = deobfuscate_annotation {
            let deobfuscate_annotation_value =
                TranspileAnnotationValue::from(deobfuscate_annotation.assignment().value.clone());

            if let TranspileAnnotationValue::Map(map) = deobfuscate_annotation_value {
                if map.len() > 2 {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message: "Deobfuscate annotation must have at most 2 key-value pairs."
                                .to_string(),
                        });
                    handler.receive(error.clone());
                    return Err(error);
                }
                if let (Some(name), Some(target)) = (map.get("name"), map.get("target")) {
                    if let (
                        TranspileAnnotationValue::Expression(objective),
                        TranspileAnnotationValue::Expression(target),
                    ) = (name, target)
                    {
                        if let (Some(name_eval), Some(target_eval)) =
                            (objective.comptime_eval(), target.comptime_eval())
                        {
                            // TODO: change invalid criteria if boolean
                            if !crate::util::is_valid_scoreboard_name(&name_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'name' must be a valid scoreboard name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            if !crate::util::is_valid_player_name(&target_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'target' must be a valid player name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            (name_eval, target_eval)
                        } else {
                            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                        annotation: deobfuscate_annotation.span(),
                                        message: "Deobfuscate annotation 'name' or 'target' could not have been evaluated at compile time.".to_string()
                                    });
                            handler.receive(error.clone());
                            return Err(error);
                        }
                    } else {
                        let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                    annotation: deobfuscate_annotation.span(),
                                    message: "Deobfuscate annotation 'name' and 'target' must be compile time expressions.".to_string()
                                });
                        handler.receive(error.clone());
                        return Err(error);
                    }
                } else {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message:
                                "Deobfuscate annotation must have both 'name' and 'target' keys."
                                    .to_string(),
                        });
                    handler.receive(error.clone());
                    return Err(error);
                }
            } else {
                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                    annotation: deobfuscate_annotation.span(),
                    message: "Deobfuscate annotation must be a map.".to_string(),
                });
                handler.receive(error.clone());
                return Err(error);
            }
        } else {
            let name =
                "shu_values_".to_string() + &md5::hash(program_identifier).to_hex_lowercase();
            let target = md5::hash((Arc::as_ptr(scope) as usize).to_le_bytes())
                .to_hex_lowercase()
                .split_off(16);

            (name, target)
        };

        match variable_type {
            KeywordKind::Int => {
                if !self.datapack.scoreboards().contains_key(&name) {
                    self.datapack.register_scoreboard(&name, None, None);
                }

                scope.set_variable(
                    single.identifier().span.str(),
                    VariableData::ScoreboardValue {
                        objective: name.clone(),
                        target,
                    },
                );
            }
            _ => todo!("implement other variable types"),
        }

        single.assignment().as_ref().map_or_else(
            || Ok(Vec::new()),
            |assignment| {
                self.transpile_assignment(
                    single.identifier().span.str(),
                    assignment.expression(),
                    scope,
                    handler,
                )
            },
        )
    }

    fn transpile_assignment(
        &mut self,
        name: &str,
        expression: &crate::syntax::syntax_tree::expression::Expression,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let target = scope.get_variable(name).unwrap();
        let data_location = match target.as_ref() {
            VariableData::BooleanStorage { storage_name, path } => DataLocation::Storage {
                storage_name: storage_name.to_owned(),
                path: path.to_owned(),
                r#type: super::expression::StorageType::Boolean,
            },
            VariableData::ScoreboardValue { objective, target } => DataLocation::ScoreboardValue {
                objective: objective.to_owned(),
                target: target.to_owned(),
            },
            _ => todo!("implement other variable types"),
        };
        self.transpile_expression(expression, &data_location, scope, handler)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope() {
        let scope = Scope::new();
        scope.set_variable(
            "test",
            VariableData::Scoreboard {
                objective: "test".to_string(),
            },
        );
        if let Some(var) = scope.get_variable("test") {
            match var.as_ref() {
                VariableData::Scoreboard { objective } => assert_eq!(objective, "test"),
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
            VariableData::Scoreboard {
                objective: "test".to_string(),
            },
        );
        let child = Scope::with_parent(&scope);
        if let Some(var) = child.get_variable("test") {
            match var.as_ref() {
                VariableData::Scoreboard { objective } => assert_eq!(objective, "test"),
                _ => panic!("Incorrect Variable"),
            }
        } else {
            panic!("Variable missing")
        }
    }
}
