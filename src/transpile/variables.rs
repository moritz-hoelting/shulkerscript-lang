#![expect(unused)]

use std::{
    collections::{HashMap, VecDeque},
    fmt::Debug,
    ops::Deref,
    sync::{Arc, OnceLock, RwLock},
};

#[cfg(feature = "shulkerbox")]
use chksum_md5 as md5;
#[cfg(feature = "shulkerbox")]
use shulkerbox::prelude::{Command, Condition, Execute};

use enum_as_inner::EnumAsInner;

use crate::{
    base::{self, source_file::SourceElement, Handler},
    lexical::token::{Identifier, KeywordKind},
    syntax::syntax_tree::{
        expression::{Expression, Primary},
        statement::{
            ArrayVariableDeclaration, AssignmentDestination, ScoreVariableDeclaration,
            SingleVariableDeclaration, TagVariableDeclaration, VariableDeclaration,
        },
        Annotation,
    },
};

use super::{
    error::{
        AssignmentError, IllegalAnnotationContent, IllegalIndexing, IllegalIndexingReason,
        MismatchedTypes, NotComptime,
    },
    expression::{ComptimeValue, DataLocation, ExpectedType, StorageType},
    FunctionData, TranspileAnnotationValue, TranspileError, TranspileResult,
};

#[cfg(feature = "shulkerbox")]
use super::{internal_functions::InternalFunction, Transpiler};

/// Stores the data required to access a variable.
#[cfg(feature = "shulkerbox")]
#[derive(Debug, Clone, EnumAsInner)]
pub enum VariableData {
    /// A function.
    Function {
        /// The function data.
        function_data: FunctionData,
        /// The path to the function once it is generated.
        path: OnceLock<String>,
        /// The scope of the function.
        function_scope: Arc<Scope>,
    },
    /// A macro function parameter.
    MacroParameter {
        /// The index of the parameter.
        index: usize,
        /// The macro name.
        macro_name: String,
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
    /// Compiler internal function.
    InternalFunction {
        /// The implementation
        implementation: InternalFunction,
    },
    /// Compiler internal variable.
    ComptimeValue {
        /// The value.
        value: Arc<RwLock<Option<ComptimeValue>>>,
        /// Whether the value is read-only.
        read_only: bool,
    },
}

#[derive(Debug, Clone, Copy, EnumAsInner)]
pub enum TranspileAssignmentTarget<'a> {
    Identifier(&'a Identifier),
    Indexed(&'a Identifier, &'a Expression),
}

impl<'a> From<&'a AssignmentDestination> for TranspileAssignmentTarget<'a> {
    fn from(destination: &'a AssignmentDestination) -> Self {
        match destination {
            AssignmentDestination::Identifier(ident) => {
                TranspileAssignmentTarget::Identifier(ident)
            }
            AssignmentDestination::Indexed(ident, _, expr, _) => {
                TranspileAssignmentTarget::Indexed(ident, expr)
            }
        }
    }
}

/// A scope that stores variables.
#[cfg(feature = "shulkerbox")]
#[derive(Default)]
pub struct Scope {
    /// Parent scope where variables are inherited from.
    parent: Option<Arc<Self>>,
    /// Variables stored in the scope.
    variables: RwLock<HashMap<String, Arc<VariableData>>>,
    /// How many times the variable has been shadowed in the current scope.
    shadowed: RwLock<HashMap<String, usize>>,
}

#[cfg(feature = "shulkerbox")]
impl Scope {
    /// Creates a new scope.
    #[must_use]
    pub fn new() -> Arc<Self> {
        Arc::new(Self::default())
    }

    /// Creates a new scope with internal functions.
    #[cfg(feature = "shulkerbox")]
    #[must_use]
    pub fn with_internal_functions() -> Arc<Self> {
        use super::internal_functions;

        let scope = Self::new();

        internal_functions::add_all_to_scope(&scope);

        scope
    }

    /// Creates a new scope with a parent.
    #[must_use]
    pub fn with_parent(parent: Arc<Self>) -> Arc<Self> {
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

    /// Gets the number of times a variable has been shadowed.
    ///
    ///
    pub fn get_variable_shadow_count(&self, name: &str) -> usize {
        let count = self
            .shadowed
            .read()
            .unwrap()
            .get(name)
            .copied()
            .unwrap_or(0);
        self.parent.as_ref().map_or(count, |parent| {
            count.saturating_sub(1) + parent.get_variable_shadow_count(name)
        })
    }

    /// Sets a variable in the scope.
    pub fn set_variable(&self, name: &str, var: VariableData) {
        self.set_arc_variable(name, Arc::new(var));
    }

    /// Sets a variable in the scope.
    /// This function is used to set a variable that is already wrapped in an `Arc`.
    pub fn set_arc_variable(&self, name: &str, var: Arc<VariableData>) {
        let prev = self
            .variables
            .write()
            .unwrap()
            .insert(name.to_string(), var);
        *self
            .shadowed
            .write()
            .unwrap()
            .entry(name.to_string())
            .or_default() += 1;
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
        self.parent.clone()
    }
}

#[cfg(feature = "shulkerbox")]
impl Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct VariableWrapper<'a>(&'a RwLock<HashMap<String, Arc<VariableData>>>);
        impl Debug for VariableWrapper<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let s = self.0.read().unwrap();
                s.deref().fmt(f)
            }
        }

        let mut s = f.debug_struct("Scope");
        s.field("parent", &self.parent);

        s.field("variables", &VariableWrapper(&self.variables));
        s.field("shadowed", &self.shadowed);
        s.finish()
    }
}

#[cfg(feature = "shulkerbox")]
impl Transpiler {
    pub(super) fn transpile_variable_declaration(
        &mut self,
        declaration: &VariableDeclaration,
        is_global: bool,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match declaration {
            VariableDeclaration::Single(declaration) => self.transpile_single_variable_declaration(
                declaration,
                is_global,
                program_identifier,
                scope,
                handler,
            ),
            VariableDeclaration::Score(declaration) => self.transpile_score_variable_declaration(
                declaration,
                program_identifier,
                scope,
                handler,
            ),
            VariableDeclaration::Array(declaration) => self.transpile_array_variable_declaration(
                declaration,
                is_global,
                program_identifier,
                scope,
                handler,
            ),
            VariableDeclaration::Tag(declaration) => self.transpile_tag_variable_declaration(
                declaration,
                program_identifier,
                scope,
                handler,
            ),
            VariableDeclaration::ComptimeValue(declaration) => {
                let value = if let Some(assignment) = declaration.assignment() {
                    Some(
                        assignment
                            .expression()
                            .comptime_eval(scope, handler)
                            .map_err(|err| {
                                let err = TranspileError::NotComptime(err);
                                handler.receive(err.clone());
                                err
                            })?,
                    )
                } else {
                    None
                };

                scope.set_variable(
                    declaration.identifier().span.str(),
                    VariableData::ComptimeValue {
                        value: Arc::new(RwLock::new(value)),
                        read_only: is_global,
                    },
                );

                Ok(Vec::new())
            }
        }
    }

    fn transpile_single_variable_declaration(
        &mut self,
        declaration: &SingleVariableDeclaration,
        is_global: bool,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let variable_type = declaration.variable_type().keyword;

        let (name, target) = self.get_data_location_identifier_pair(
            declaration,
            program_identifier,
            scope,
            handler,
        )?;

        match variable_type {
            KeywordKind::Int => {
                if !self.datapack.scoreboards().contains_key(&name) {
                    self.datapack
                        .register_scoreboard(&name, None::<&str>, None::<&str>);
                }

                scope.set_variable(
                    declaration.identifier().span.str(),
                    VariableData::ScoreboardValue {
                        objective: name.clone(),
                        target: target.clone(),
                    },
                );
            }
            KeywordKind::Bool => {
                scope.set_variable(
                    declaration.identifier().span.str(),
                    VariableData::BooleanStorage {
                        storage_name: name.clone(),
                        path: target.clone(),
                    },
                );
            }
            _ => unreachable!("no other variable types"),
        }

        if let Some(assignment) = declaration.assignment().as_ref() {
            let cmds = self.transpile_assignment(
                TranspileAssignmentTarget::Identifier(declaration.identifier()),
                assignment.expression(),
                scope,
                handler,
            )?;
            if is_global {
                let (temp_objective, [temp_target]) = self.get_temp_scoreboard_locations_array();
                let test_cmd = match declaration.variable_type().keyword {
                    KeywordKind::Int => {
                        Command::Raw(format!("scoreboard players get {name} {target}"))
                    }
                    KeywordKind::Bool => Command::Raw(format!("data get storage {name} {target}")),
                    _ => unreachable!("no other variable types"),
                };
                let test_exists_cmd = Command::Execute(Execute::Store(
                    format!("success score {temp_target} {temp_objective}").into(),
                    Box::new(Execute::Run(Box::new(test_cmd))),
                ));
                let cond_cmd = Command::Execute(Execute::If(
                    Condition::Atom(
                        format!("score {temp_target} {temp_objective} matches 0").into(),
                    ),
                    Box::new(Execute::Run(Box::new(Command::Group(cmds)))),
                    None,
                ));
                Ok(vec![test_exists_cmd, cond_cmd])
            } else {
                Ok(cmds)
            }
        } else {
            Ok(Vec::new())
        }
    }

    fn transpile_score_variable_declaration(
        &mut self,
        declaration: &ScoreVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let name = self.get_data_location_identifier(
            declaration.int_keyword().keyword,
            declaration.identifier(),
            declaration.annotations(),
            program_identifier,
            scope,
            handler,
        )?;

        let criteria = declaration
            .criteria()
            .as_ref()
            .map(|(_, c, _)| c.str_content());

        if !self.datapack.scoreboards().contains_key(&name) {
            self.datapack
                .register_scoreboard(&name, criteria, None::<&str>);
        }

        scope.set_variable(
            declaration.identifier().span.str(),
            VariableData::Scoreboard {
                objective: name.clone(),
            },
        );

        // TODO: implement assignment when map literal is implemented
        Ok(Vec::new())
    }

    fn transpile_array_variable_declaration(
        &mut self,
        declaration: &ArrayVariableDeclaration,
        _is_global: bool,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let variable_type = declaration.variable_type().keyword;

        let (name, targets) =
            self.get_array_data_location_pair(declaration, program_identifier, scope, handler)?;

        match variable_type {
            KeywordKind::Int => {
                if !self.datapack.scoreboards().contains_key(&name) {
                    self.datapack
                        .register_scoreboard(&name, None::<&str>, None::<&str>);
                }

                scope.set_variable(
                    declaration.identifier().span.str(),
                    VariableData::ScoreboardArray {
                        objective: name,
                        targets,
                    },
                );
            }
            KeywordKind::Bool => {
                scope.set_variable(
                    declaration.identifier().span.str(),
                    VariableData::BooleanStorageArray {
                        storage_name: name,
                        paths: targets,
                    },
                );
            }
            _ => unreachable!("no other variable types"),
        }

        declaration.assignment().as_ref().map_or_else(
            || Ok(Vec::new()),
            |assignment| {
                // TODO: implement global already exists check when array assignments are implemented
                self.transpile_assignment(
                    TranspileAssignmentTarget::Identifier(declaration.identifier()),
                    assignment.expression(),
                    scope,
                    handler,
                )
            },
        )
    }

    fn transpile_tag_variable_declaration(
        &mut self,
        declaration: &TagVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let name = self.get_data_location_identifier(
            declaration.bool_keyword().keyword,
            declaration.identifier(),
            declaration.annotations(),
            program_identifier,
            scope,
            handler,
        )?;

        scope.set_variable(
            declaration.identifier().span.str(),
            VariableData::Tag { tag_name: name },
        );

        // TODO: implement assignment when map literal is implemented
        Ok(Vec::new())
    }

    #[expect(clippy::too_many_lines)]
    pub(super) fn transpile_assignment(
        &mut self,
        destination: TranspileAssignmentTarget,
        expression: &crate::syntax::syntax_tree::expression::Expression,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        let (identifier, indexing_value) = match destination {
            TranspileAssignmentTarget::Identifier(ident) => (ident, None),
            TranspileAssignmentTarget::Indexed(ident, expression) => {
                (ident, expression.comptime_eval(scope, handler).ok())
            }
        };
        if let Some(target) = scope.get_variable(identifier.span.str()) {
            let data_location = match target.as_ref() {
                VariableData::BooleanStorage { storage_name, path } => {
                    // TODO: make sure that no indexing is done
                    Ok(DataLocation::Storage {
                        storage_name: storage_name.to_owned(),
                        path: path.to_owned(),
                        r#type: super::expression::StorageType::Boolean,
                    })
                }
                VariableData::ScoreboardValue { objective, target } => {
                    // TODO: make sure that no indexing is done
                    Ok(DataLocation::ScoreboardValue {
                        objective: objective.to_owned(),
                        target: target.to_owned(),
                    })
                }
                VariableData::Scoreboard { objective } => match indexing_value {
                    Some(ComptimeValue::String(s)) => Ok(DataLocation::ScoreboardValue {
                        objective: objective.clone(),
                        target: s,
                    }),
                    Some(ComptimeValue::MacroString(s)) => {
                        // TODO: allow indexing with macro string
                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            reason: IllegalIndexingReason::InvalidComptimeType {
                                expected: ExpectedType::String,
                            },
                            expression: expression.span(),
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    Some(_) => {
                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            expression: expression.span(),
                            reason: IllegalIndexingReason::InvalidComptimeType {
                                expected: ExpectedType::String,
                            },
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    None => {
                        // TODO: allow when map literals are implemented
                        let err = TranspileError::AssignmentError(AssignmentError {
                            identifier: identifier.span(),
                            message: "Cannot assign to a scoreboard without indexing".to_string(),
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                },
                VariableData::ScoreboardArray { objective, targets } => {
                    match indexing_value {
                        Some(ComptimeValue::Integer(index)) => {
                            if let Some(target) = usize::try_from(index)
                                .ok()
                                .and_then(|index| targets.get(index))
                            {
                                Ok(DataLocation::ScoreboardValue {
                                    objective: objective.to_string(),
                                    target: target.to_string(),
                                })
                            } else {
                                let index_span = match destination {
                                    TranspileAssignmentTarget::Indexed(_, expr) => expr.span(),
                                    TranspileAssignmentTarget::Identifier(_) => unreachable!(
                                        "indexing value must be present (checked before)"
                                    ),
                                };
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    expression: index_span,
                                    reason: IllegalIndexingReason::IndexOutOfBounds {
                                        index: usize::try_from(index).unwrap_or(usize::MAX),
                                        length: targets.len(),
                                    },
                                });
                                handler.receive(err.clone());
                                return Err(err);
                            }
                        }
                        Some(_) => {
                            let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                expression: expression.span(),
                                reason: IllegalIndexingReason::InvalidComptimeType {
                                    expected: ExpectedType::Integer,
                                },
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                        None => {
                            // TODO: implement when array literals are implemented
                            let err = TranspileError::AssignmentError(AssignmentError {
                                identifier: identifier.span(),
                                message: "Cannot assign to an array without indexing".to_string(),
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                    }
                }
                VariableData::BooleanStorageArray {
                    storage_name,
                    paths,
                } => {
                    match indexing_value {
                        Some(ComptimeValue::Integer(index)) => {
                            if let Some(path) = usize::try_from(index)
                                .ok()
                                .and_then(|index| paths.get(index))
                            {
                                Ok(DataLocation::Storage {
                                    storage_name: storage_name.to_string(),
                                    path: path.to_string(),
                                    r#type: StorageType::Boolean,
                                })
                            } else {
                                let index_span = match destination {
                                    TranspileAssignmentTarget::Indexed(_, expr) => expr.span(),
                                    TranspileAssignmentTarget::Identifier(_) => unreachable!(
                                        "indexing value must be present (checked before)"
                                    ),
                                };
                                let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                    expression: index_span,
                                    reason: IllegalIndexingReason::IndexOutOfBounds {
                                        index: usize::try_from(index).unwrap_or(usize::MAX),
                                        length: paths.len(),
                                    },
                                });
                                handler.receive(err.clone());
                                return Err(err);
                            }
                        }
                        Some(_) => {
                            let err = TranspileError::IllegalIndexing(IllegalIndexing {
                                expression: expression.span(),
                                reason: IllegalIndexingReason::InvalidComptimeType {
                                    expected: ExpectedType::Integer,
                                },
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                        None => {
                            // TODO: implement when array literals are implemented
                            let err = TranspileError::AssignmentError(AssignmentError {
                                identifier: identifier.span(),
                                message: "Cannot assign to an array without indexing".to_string(),
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                    }
                }
                VariableData::Tag { tag_name } => match indexing_value {
                    Some(ComptimeValue::String(s)) => Ok(DataLocation::Tag {
                        tag_name: tag_name.to_string(),
                        entity: s,
                    }),
                    Some(ComptimeValue::MacroString(s)) => {
                        // TODO: allow indexing tag with macro string
                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            expression: expression.span(),
                            reason: IllegalIndexingReason::InvalidComptimeType {
                                expected: ExpectedType::String,
                            },
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    Some(_) => {
                        let err = TranspileError::IllegalIndexing(IllegalIndexing {
                            expression: expression.span(),
                            reason: IllegalIndexingReason::InvalidComptimeType {
                                expected: ExpectedType::String,
                            },
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    None => {
                        // TODO: allow when map literals are implemented
                        let err = TranspileError::AssignmentError(AssignmentError {
                            identifier: identifier.span(),
                            message: "Cannot assign to a tag without indexing".to_string(),
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                },
                VariableData::ComptimeValue { value, read_only } => {
                    if *read_only {
                        let err = TranspileError::AssignmentError(AssignmentError {
                            identifier: identifier.span(),
                            message: "Cannot assign to a read-only value.".to_string(),
                        });
                        handler.receive(err.clone());
                        return Err(err);
                    }
                    let comptime_value =
                        expression.comptime_eval(scope, handler).map_err(|err| {
                            let err = TranspileError::NotComptime(err);
                            handler.receive(err.clone());
                            err
                        })?;
                    *value.write().unwrap() = Some(comptime_value);
                    return Ok(Vec::new());
                }
                VariableData::Function { .. }
                | VariableData::MacroParameter { .. }
                | VariableData::InternalFunction { .. } => {
                    let err = TranspileError::AssignmentError(AssignmentError {
                        identifier: identifier.span(),
                        message: format!(
                            "Cannot assign to a {}.",
                            match target.as_ref() {
                                VariableData::Function { .. } => "function",
                                VariableData::MacroParameter { .. } => "macro parameter",
                                VariableData::InternalFunction { .. } => "internal function",
                                _ => unreachable!(),
                            }
                        ),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }?;
            self.transpile_expression(expression, &data_location, scope, handler)
        } else {
            let err = TranspileError::AssignmentError(AssignmentError {
                identifier: identifier.span(),
                message: "Variable does not exist.".to_string(),
            });
            handler.receive(err.clone());
            Err(err)
        }
    }

    fn get_data_location_identifier(
        &mut self,
        variable_type: KeywordKind,
        identifier: &Identifier,
        annotations: &VecDeque<Annotation>,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<String> {
        let mut deobfuscate_annotations = annotations
            .iter()
            .filter(|a| a.has_identifier("deobfuscate"));

        let deobfuscate_annotation = deobfuscate_annotations.next();

        if let Some(duplicate) = deobfuscate_annotations.next() {
            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                annotation: duplicate.span(),
                message: "Multiple deobfuscate annotations are not allowed.".to_string(),
            });
            handler.receive(error.clone());
            return Err(error);
        }
        if let Some(deobfuscate_annotation) = deobfuscate_annotation {
            let deobfuscate_annotation_value = TranspileAnnotationValue::from_annotation_value(
                deobfuscate_annotation.assignment().value.clone(),
                &deobfuscate_annotation.assignment().identifier.span,
            );

            match deobfuscate_annotation_value {
                TranspileAnnotationValue::Expression(expr, _) => {
                    if let Some(name_eval) = expr
                        .comptime_eval(scope, handler)
                        .ok()
                        .and_then(|val| val.to_string_no_macro())
                    {
                        // TODO: change invalid criteria if boolean
                        if !crate::util::is_valid_scoreboard_objective_name(&name_eval) {
                            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                                    annotation: deobfuscate_annotation.span(),
                                                    message: "Deobfuscate annotation must be a valid scoreboard objective name.".to_string()
                                                });
                            handler.receive(error.clone());
                            return Err(error);
                        }
                        Ok(name_eval)
                    } else {
                        let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                                annotation: deobfuscate_annotation.span(),
                                                message: "Deobfuscate annotation could not have been evaluated at compile time.".to_string()
                                            });
                        handler.receive(error.clone());
                        Err(error)
                    }
                }
                TranspileAnnotationValue::None(_) => Ok(identifier.span.str().to_string()),
                TranspileAnnotationValue::Map(_, _) => {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message: "Deobfuscate annotation must have no value or must be string."
                                .to_string(),
                        });
                    handler.receive(error.clone());
                    Err(error)
                }
            }
        } else {
            let hashed = md5::hash(program_identifier).to_hex_lowercase();
            let name = "shu_values_".to_string() + &hashed;
            let identifier_name = identifier.span.str();
            let scope_ident = self.temp_counter;
            self.temp_counter = self.temp_counter.wrapping_add(1);
            let mut target = md5::hash(format!(
                "{scope_ident}\0{identifier_name}\0{shadowed}",
                shadowed = scope.get_variable_shadow_count(identifier_name)
            ))
            .to_hex_lowercase();

            Ok(name)
        }
    }

    #[expect(clippy::too_many_lines)]
    fn get_data_location_identifier_pair(
        &mut self,
        single: &SingleVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(String, String)> {
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
        if let Some(deobfuscate_annotation) = deobfuscate_annotation {
            let deobfuscate_annotation_value = TranspileAnnotationValue::from_annotation_value(
                deobfuscate_annotation.assignment().value.clone(),
                &deobfuscate_annotation.assignment().identifier.span,
            );

            if let TranspileAnnotationValue::Map(map, _) = deobfuscate_annotation_value {
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
                        TranspileAnnotationValue::Expression(objective, _),
                        TranspileAnnotationValue::Expression(target, _),
                    ) = (name, target)
                    {
                        if let (Some(name_eval), Some(target_eval)) = (
                            objective
                                .comptime_eval(scope, handler)
                                .ok()
                                .and_then(|val| val.to_string_no_macro()),
                            target
                                .comptime_eval(scope, handler)
                                .ok()
                                .and_then(|val| val.to_string_no_macro()),
                        ) {
                            // TODO: change invalid criteria if boolean
                            if !crate::util::is_valid_scoreboard_objective_name(&name_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'name' must be a valid scoreboard objective name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            if !crate::util::is_valid_scoreboard_target(&target_eval) {
                                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                            annotation: deobfuscate_annotation.span(),
                                            message: "Deobfuscate annotation 'target' must be a valid scoreboard player name.".to_string()
                                        });
                                handler.receive(error.clone());
                                return Err(error);
                            }
                            Ok((name_eval, target_eval))
                        } else {
                            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                        annotation: deobfuscate_annotation.span(),
                                        message: "Deobfuscate annotation 'name' or 'target' could not have been evaluated at compile time.".to_string()
                                    });
                            handler.receive(error.clone());
                            Err(error)
                        }
                    } else {
                        let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                                    annotation: deobfuscate_annotation.span(),
                                    message: "Deobfuscate annotation 'name' and 'target' must be compile time expressions.".to_string()
                                });
                        handler.receive(error.clone());
                        Err(error)
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
                    Err(error)
                }
            } else {
                let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                    annotation: deobfuscate_annotation.span(),
                    message: "Deobfuscate annotation must be a map.".to_string(),
                });
                handler.receive(error.clone());
                Err(error)
            }
        } else {
            let hashed = md5::hash(program_identifier).to_hex_lowercase();
            let name = if matches!(variable_type, KeywordKind::Int) {
                "shu_values_"
            } else {
                "shulkerbox:values_"
            }
            .to_string()
                + &hashed;
            let identifier_name = single.identifier().span.str();
            let scope_ident = self.temp_counter;
            self.temp_counter = self.temp_counter.wrapping_add(1);
            let mut target = md5::hash(format!(
                "{scope_ident}\0{identifier_name}\0{shadowed}",
                shadowed = scope.get_variable_shadow_count(identifier_name)
            ))
            .to_hex_lowercase();

            if matches!(variable_type, KeywordKind::Int) {
                target.split_off(16);
            }

            Ok((name, target))
        }
    }

    fn get_array_data_location_pair(
        &mut self,
        declaration: &ArrayVariableDeclaration,
        program_identifier: &str,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<(String, Vec<String>)> {
        let mut deobfuscate_annotations = declaration
            .annotations()
            .iter()
            .filter(|a| a.has_identifier("deobfuscate"));

        let variable_type = declaration.variable_type().keyword;

        let deobfuscate_annotation = deobfuscate_annotations.next();

        if let Some(duplicate) = deobfuscate_annotations.next() {
            let error = TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                annotation: duplicate.span(),
                message: "Multiple deobfuscate annotations are not allowed.".to_string(),
            });
            handler.receive(error.clone());
            return Err(error);
        }
        if let Some(deobfuscate_annotation) = deobfuscate_annotation {
            let deobfuscate_annotation_value = TranspileAnnotationValue::from_annotation_value(
                deobfuscate_annotation.assignment().value.clone(),
                &deobfuscate_annotation.assignment().identifier.span,
            );

            match deobfuscate_annotation_value {
                TranspileAnnotationValue::None(_) => {
                    let ident_str = declaration.identifier().span.str();
                    let name = if matches!(variable_type, KeywordKind::Int) {
                        ident_str.to_string()
                    } else {
                        format!(
                            "{namespace}:{ident_str}",
                            namespace = self.main_namespace_name
                        )
                    };

                    let len = declaration.size().as_i64();
                    let targets = (0..len).map(|i| i.to_string()).collect();
                    Ok((name, targets))
                }
                TranspileAnnotationValue::Map(map, _) => {
                    // TODO: implement when map deobfuscate annotation is implemented
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message: "Deobfuscate annotation value must be a string or none."
                                .to_string(),
                        });
                    handler.receive(error.clone());
                    Err(error)
                }
                TranspileAnnotationValue::Expression(_, _) => {
                    let error =
                        TranspileError::IllegalAnnotationContent(IllegalAnnotationContent {
                            annotation: deobfuscate_annotation.span(),
                            message: "Deobfuscate annotation value must be a map or none."
                                .to_string(),
                        });
                    handler.receive(error.clone());
                    Err(error)
                }
            }
        } else {
            let hashed = md5::hash(program_identifier).to_hex_lowercase();
            let name = if matches!(variable_type, KeywordKind::Int) {
                "shu_values_"
            } else {
                "shulkerbox:values_"
            }
            .to_string()
                + &hashed;
            let identifier_name = declaration.identifier().span.str();
            let scope_ident = self.temp_counter;
            self.temp_counter = self.temp_counter.wrapping_add(1);
            let len = declaration.size().as_i64();
            let targets = (0..len)
                .map(|i| {
                    let mut target = md5::hash(format!(
                        "{scope_ident}\0{identifier_name}\0{i}\0{shadowed}",
                        shadowed = scope.get_variable_shadow_count(identifier_name)
                    ))
                    .to_hex_lowercase();

                    if matches!(variable_type, KeywordKind::Int) {
                        target.split_off(16);
                    }

                    target
                })
                .collect();

            Ok((name, targets))
        }
    }

    /// Move data from location `from` to location `to`.
    ///
    /// # Errors
    /// - if the data type does not match
    #[expect(clippy::too_many_lines)]
    pub fn move_data(
        &mut self,
        from: &DataLocation,
        to: &DataLocation,
        expression: &impl SourceElement,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<Vec<Command>> {
        match from {
            DataLocation::Storage {
                storage_name,
                path,
                r#type,
            } => match r#type {
                StorageType::Boolean
                | StorageType::Byte
                | StorageType::Int
                | StorageType::Long
                | StorageType::Double => match to {
                    DataLocation::ScoreboardValue { objective, target } => {
                        let cmd = Command::Execute(Execute::Store(
                            format!("store result score {target} {objective}").into(),
                            Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "data get storage {storage_name} {path}"
                            ))))),
                        ));
                        Ok(vec![cmd])
                    }
                    DataLocation::Tag { tag_name, entity } => {
                        let cmd = Command::Execute(Execute::If(
                            Condition::Atom(
                                format!("data storage {storage_name} {{{path}: 1b}}").into(),
                            ),
                            Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "tag {entity} add {tag_name}"
                            ))))),
                            Some(Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "tag {entity} remove {tag_name}"
                            )))))),
                        ));

                        Ok(vec![cmd])
                    }
                    DataLocation::Storage {
                        storage_name: target_storage_name,
                        path: target_path,
                        r#type: target_type,
                    } => {
                        if storage_name == target_storage_name
                            && path == target_path
                            && r#type == target_type
                        {
                            Ok(Vec::new())
                        } else if matches!(target_type, StorageType::Boolean) {
                            let cmd = Command::Raw(format!(
                                        "data modify storage {target_storage_name} {target_path} set from storage {storage_name} {path}"
                                    ));
                            Ok(vec![cmd])
                        } else {
                            let err = TranspileError::MismatchedTypes(MismatchedTypes {
                                expression: expression.span(),
                                expected_type: to.value_type().into(),
                            });
                            handler.receive(err.clone());
                            Err(err)
                        }
                    }
                },
            },
            DataLocation::ScoreboardValue {
                objective,
                target: score_target,
            } => match to {
                DataLocation::ScoreboardValue {
                    objective: target_objective,
                    target: target_target,
                } => {
                    if objective == target_objective && score_target == target_target {
                        Ok(Vec::new())
                    } else {
                        let cmd = Command::Raw(format!(
                            "scoreboard players operation {target_target} {target_objective} = {score_target} {objective}"
                        ));
                        Ok(vec![cmd])
                    }
                }
                DataLocation::Storage {
                    storage_name,
                    path,
                    r#type,
                } => {
                    if matches!(
                        r#type,
                        StorageType::Byte
                            | StorageType::Double
                            | StorageType::Int
                            | StorageType::Long
                    ) {
                        let cmd = Command::Execute(Execute::Store(
                            format!(
                                "result storage {storage_name} {path} {t} 1.0",
                                t = r#type.as_str()
                            )
                            .into(),
                            Box::new(Execute::Run(Box::new(Command::Raw(format!(
                                "scoreboard players get {score_target} {objective}"
                            ))))),
                        ));
                        Ok(vec![cmd])
                    } else {
                        let err = TranspileError::MismatchedTypes(MismatchedTypes {
                            expression: expression.span(),
                            expected_type: to.value_type().into(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    }
                }
                DataLocation::Tag { .. } => {
                    let err = TranspileError::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: expression.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            },
            DataLocation::Tag { .. } => {
                let err = TranspileError::MismatchedTypes(MismatchedTypes {
                    expected_type: to.value_type().into(),
                    expression: expression.span(),
                });
                handler.receive(err.clone());
                Err(err)
            }
        }
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
        let child = Scope::with_parent(scope);
        if let Some(var) = child.get_variable("test") {
            match var.as_ref() {
                VariableData::Scoreboard { objective } => assert_eq!(objective, "test"),
                _ => panic!("Incorrect Variable"),
            }
        } else {
            panic!("Variable missing")
        }
    }

    #[test]
    fn test_shadowed_count() {
        let scope = Scope::new();
        scope.set_variable(
            "test",
            VariableData::Scoreboard {
                objective: "test1".to_string(),
            },
        );
        let child = Scope::with_parent(scope);
        child.set_variable(
            "test",
            VariableData::Scoreboard {
                objective: "test2".to_string(),
            },
        );
        assert_eq!(child.get_variable_shadow_count("test"), 1);
    }
}
