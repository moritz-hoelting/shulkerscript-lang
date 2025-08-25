//! This module contains the semantic analysis of the AST.

#![allow(clippy::missing_errors_doc)]

use crate::{
    base::{self, source_file::SourceElement as _, Handler},
    lexical::token::KeywordKind,
    syntax::syntax_tree::{
        declaration::{Declaration, Function, FunctionVariableType, ImportItems},
        expression::{
            Binary, BinaryOperator, Expression, LuaCode, PrefixOperator, Primary,
            TemplateStringLiteral, TemplateStringLiteralPart,
        },
        program::{Namespace, ProgramFile},
        statement::{
            execute_block::{
                Conditional, Else, ExecuteBlock, ExecuteBlockHead, ExecuteBlockHeadItem as _,
                ExecuteBlockTail,
            },
            Assignment, AssignmentDestination, Block, Grouping, Semicolon, SemicolonStatement,
            Statement, VariableDeclaration,
        },
        AnyStringLiteral,
    },
    transpile::{
        error::{
            AssignmentError, IllegalIndexing, IllegalIndexingReason, MismatchedTypes,
            UnknownIdentifier,
        },
        expression::{ExpectedType, ValueType},
    },
};

pub mod error;

mod scope;
use error::{IncompatibleFunctionAnnotation, InvalidNamespaceName, UnexpectedExpression};
pub use scope::{SemanticScope, VariableType};

use super::syntax::syntax_tree::ConnectedList;

impl ProgramFile {
    /// Analyzes the semantics of the program.
    pub fn analyze_semantics(
        &self,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.namespace().analyze_semantics(handler)?;

        let mut errs = Vec::new();
        let scope = get_global_scope(self.declarations());

        for declaration in self.declarations() {
            if let Err(err) = declaration.analyze_semantics(&scope, handler) {
                errs.push(err);
            }
        }

        errs.into_iter().next().map_or(Ok(()), Err)
    }
}

fn get_global_scope(declarations: &[Declaration]) -> SemanticScope<'static> {
    let scope = SemanticScope::new();

    for declaration in declarations {
        declaration.add_to_semantic_scope(&scope);
    }

    scope
}

impl Namespace {
    /// Analyzes the semantics of the namespace.
    fn analyze_semantics(&self, handler: &impl Handler<base::Error>) -> Result<(), error::Error> {
        let name = self.name();
        Self::validate_str(name.str_content().as_ref()).map_err(|invalid_chars| {
            let err = error::Error::from(InvalidNamespaceName {
                name: name.clone(),
                invalid_chars,
            });
            handler.receive(err.clone());
            err
        })
    }
}

impl Declaration {
    fn add_to_semantic_scope(&self, scope: &SemanticScope) {
        match self {
            Self::Function(func) => {
                let name = func.identifier();
                scope.set_variable(name.span.str(), VariableType::Function);
            }
            Self::GlobalVariable((_, var, _)) => {
                let name = var.identifier();
                let var_type = match var {
                    VariableDeclaration::Array(arr) => match arr.variable_type().keyword {
                        KeywordKind::Bool => VariableType::BooleanStorageArray,
                        KeywordKind::Int => VariableType::ScoreboardArray,
                        _ => unreachable!("variable type is not a valid type"),
                    },
                    VariableDeclaration::Score(_) => VariableType::Scoreboard,
                    VariableDeclaration::Tag(_) => VariableType::Tag,
                    VariableDeclaration::Single(single) => match single.variable_type().keyword {
                        KeywordKind::Bool => VariableType::BooleanStorage,
                        KeywordKind::Int => VariableType::ScoreboardValue,
                        _ => unreachable!("variable type is not a valid type"),
                    },
                    VariableDeclaration::ComptimeValue(_) => VariableType::ComptimeValue,
                };
                scope.set_variable(name.span.str(), var_type);
            }
            Self::Import(_) | Self::Tag(_) => {}
        }
    }

    /// Analyzes the semantics of the declaration.
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Function(func) => func.analyze_semantics(scope, handler),

            Self::Import(imp) => match imp.items() {
                ImportItems::All(_) => {
                    let err = error::Error::Other(
                        "Importing all items is not yet supported.".to_string(),
                    );
                    handler.receive(err.clone());
                    Err(err)
                }
                ImportItems::Named(items) => {
                    for item in items.elements() {
                        if scope.get_variable(item.span.str()) == Some(VariableType::Function) {
                            let err = error::Error::from(error::ConflictingFunctionNames {
                                name: item.span.str().to_string(),
                                definition: item.span(),
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                        scope.set_variable(item.span.str(), VariableType::Function);
                    }

                    Ok(())
                }
            },

            Self::GlobalVariable((_, var, _)) => var.analyze_semantics(scope, handler),

            Self::Tag(_) => Ok(()),
        }
    }
}

impl Function {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let child_scope = SemanticScope::with_parent(scope);

        if let Some(parameters) = self
            .parameters()
            .as_ref()
            .map(ConnectedList::elements)
            .map(Iterator::collect::<Vec<_>>)
        {
            if let Some(incompatible) = self.annotations().iter().find(|a| {
                ["tick", "load", "uninstall"].contains(&a.assignment().identifier.span.str())
            }) {
                let err =
                    error::Error::IncompatibleFunctionAnnotation(IncompatibleFunctionAnnotation {
                        span: incompatible.assignment().identifier.span.clone(),
                        reason:
                            "functions with the `tick`, `load` or `uninstall` annotation cannot have parameters"
                                .to_string(),
                    });
                handler.receive(err.clone());
                return Err(err);
            } else if parameters
                .iter()
                .any(|param| matches!(param.variable_type(), FunctionVariableType::Value(_)))
            {
                if let Some(incompatible) = self
                    .annotations()
                    .iter()
                    .find(|a| a.assignment().identifier.span.str() == "deobfuscate")
                {
                    let err =
                            error::Error::IncompatibleFunctionAnnotation(IncompatibleFunctionAnnotation {
                                span: incompatible.assignment().identifier.span.clone(),
                                reason:
                                    "functions with the `deobfuscate` annotation cannot have compile-time parameters"
                                        .to_string(),
                            });
                    handler.receive(err.clone());
                    return Err(err);
                }
            }

            for param in parameters {
                let name = param.identifier().span.str();
                let var = match param.variable_type() {
                    FunctionVariableType::Boolean(_) => VariableType::BooleanStorage,
                    FunctionVariableType::Integer(_) => VariableType::ScoreboardValue,
                    FunctionVariableType::Macro(_) => VariableType::MacroParameter,
                    FunctionVariableType::Value(_) => VariableType::ComptimeValue,
                };
                child_scope.set_variable(name, var);
            }
        }

        self.block().analyze_semantics(&child_scope, handler)?;

        Ok(())
    }
}

impl Block {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let mut errs = Vec::new();

        for statement in self.statements() {
            if let Err(err) = statement.analyze_semantics(scope, handler) {
                errs.push(err);
            }
        }

        errs.first().map_or(Ok(()), |err| Err(err.clone()))
    }
}

impl Statement {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Block(block) => {
                let child_scope = SemanticScope::with_parent(scope);
                block.analyze_semantics(&child_scope, handler)
            }
            Self::DocComment(_) | Self::LiteralCommand(_) => Ok(()),
            Self::ExecuteBlock(ex) => ex.analyze_semantics(scope, handler),
            Self::Grouping(group) => {
                let child_scope = SemanticScope::with_parent(scope);
                group.analyze_semantics(&child_scope, handler)
            }
            Self::Semicolon(sem) => sem.analyze_semantics(scope, handler),
        }
    }
}

impl ExecuteBlock {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::HeadTail(head, tail) => {
                let head_res = head.analyze_semantics(scope, handler);
                let tail_res = tail.analyze_semantics(scope, handler);

                if head_res.is_err() {
                    head_res
                } else {
                    tail_res
                }
            }
            Self::IfElse(cond, then, el) => {
                let cond_res = cond.analyze_semantics(scope, handler);
                let then_scope = SemanticScope::with_parent(scope);
                let then_res = then.analyze_semantics(&then_scope, handler);
                let el_scope = SemanticScope::with_parent(scope);
                let else_res = el.analyze_semantics(&el_scope, handler);

                if cond_res.is_err() {
                    cond_res
                } else if then_res.is_err() {
                    then_res
                } else {
                    else_res
                }
            }
        }
    }
}

impl ExecuteBlockHead {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Align(align) => align.analyze_semantics(scope, handler),
            Self::Anchored(anchored) => anchored.analyze_semantics(scope, handler),
            Self::As(r#as) => r#as.analyze_semantics(scope, handler),
            Self::At(at) => at.analyze_semantics(scope, handler),
            Self::AsAt(asat) => asat.analyze_semantics(scope, handler),
            Self::Conditional(cond) => cond.analyze_semantics(scope, handler),
            Self::Facing(facing) => facing.analyze_semantics(scope, handler),
            Self::In(r#in) => r#in.analyze_semantics(scope, handler),
            Self::On(on) => on.analyze_semantics(scope, handler),
            Self::Positioned(pos) => pos.analyze_semantics(scope, handler),
            Self::Rotated(rot) => rot.analyze_semantics(scope, handler),
            Self::Store(store) => store.analyze_semantics(scope, handler),
            Self::Summon(summon) => summon.analyze_semantics(scope, handler),
        }
    }
}

impl ExecuteBlockTail {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Block(block) => {
                let child_scope = SemanticScope::with_parent(scope);
                block.analyze_semantics(&child_scope, handler)
            }
            Self::ExecuteBlock(_, ex) => ex.analyze_semantics(scope, handler),
        }
    }
}

impl Else {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let child_scope = SemanticScope::with_parent(scope);
        self.block().analyze_semantics(&child_scope, handler)
    }
}

impl Conditional {
    /// Analyzes the semantics of the conditional.
    pub fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.condition().analyze_semantics(scope, handler)
    }
}

impl Grouping {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.block().analyze_semantics(scope, handler)
    }
}

impl Semicolon {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self.statement() {
            SemicolonStatement::Assignment(assignment) => {
                assignment.analyze_semantics(scope, handler)
            }
            SemicolonStatement::Expression(expr) => expr.analyze_semantics(scope, handler),
            SemicolonStatement::VariableDeclaration(decl) => decl.analyze_semantics(scope, handler),
            SemicolonStatement::Return(ret) => ret.expression().analyze_semantics(scope, handler),
        }
    }
}

impl Assignment {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let variable_type = match self.destination() {
            AssignmentDestination::Identifier(ident) => scope.get_variable(ident.span.str()),
            AssignmentDestination::Indexed(ident, _, index, _) => {
                let var = scope.get_variable(ident.span.str());
                index.analyze_semantics(scope, handler)?;
                let valid_index = match var {
                    Some(VariableType::Tag | VariableType::Scoreboard) => (!index
                        .can_yield_type_semantics(ValueType::String, scope))
                    .then_some(ExpectedType::String),
                    Some(VariableType::ScoreboardArray | VariableType::BooleanStorageArray) => {
                        (!index.can_yield_type_semantics(ValueType::Integer, scope))
                            .then_some(ExpectedType::Integer)
                    }
                    _ => None,
                };
                if let Some(expected) = valid_index {
                    let err = error::Error::IllegalIndexing(IllegalIndexing {
                        expression: index.span(),
                        reason: IllegalIndexingReason::InvalidComptimeType { expected },
                    });
                    handler.receive(err.clone());
                    return Err(err);
                }
                var
            }
        };

        if let Some(variable_type) = variable_type {
            let expected = match variable_type {
                VariableType::BooleanStorage => ValueType::Boolean,
                VariableType::ScoreboardValue => ValueType::Integer,
                VariableType::ComptimeValue => {
                    // TODO: check if the expression is a constant expression
                    return Ok(());
                }
                VariableType::BooleanStorageArray | VariableType::Tag
                    if matches!(self.destination(), AssignmentDestination::Indexed(..)) =>
                {
                    ValueType::Boolean
                }
                VariableType::Scoreboard | VariableType::ScoreboardArray
                    if matches!(self.destination(), AssignmentDestination::Indexed(..)) =>
                {
                    ValueType::Integer
                }
                _ => {
                    let err = error::Error::AssignmentError(AssignmentError {
                        identifier: self.destination().span(),
                        message: "cannot assign to this type".to_string(),
                    });
                    handler.receive(err.clone());
                    return Err(err);
                }
            };

            if !self.expression().can_yield_type_semantics(expected, scope) {
                let err = error::Error::MismatchedTypes(MismatchedTypes {
                    expected_type: expected.into(),
                    expression: self.expression().span(),
                });
                handler.receive(err.clone());
                return Err(err);
            }
        } else {
            let err = error::Error::UnknownIdentifier(UnknownIdentifier {
                identifier: self.destination().span(),
            });
            handler.receive(err.clone());
            return Err(err);
        }
        self.expression().analyze_semantics(scope, handler)
    }
}

impl VariableDeclaration {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let name = self.identifier().span.str();
        let (var, expected) = match self {
            Self::Array(_) => match self.variable_type().keyword {
                KeywordKind::Bool => (VariableType::BooleanStorageArray, ExpectedType::Boolean),
                KeywordKind::Int => (VariableType::ScoreboardArray, ExpectedType::Integer),
                _ => unreachable!("variable type is not a valid type"),
            },
            Self::Score(_) => (VariableType::Scoreboard, ExpectedType::Integer),
            Self::Tag(_) => (VariableType::Tag, ExpectedType::Boolean),
            Self::Single(_) => match self.variable_type().keyword {
                KeywordKind::Bool => (VariableType::BooleanStorage, ExpectedType::Boolean),
                KeywordKind::Int => (VariableType::ScoreboardValue, ExpectedType::Integer),
                _ => unreachable!("variable type is not a valid type"),
            },
            Self::ComptimeValue(decl) => {
                if let Some(assignment) = decl.assignment() {
                    assignment.expression().analyze_semantics(scope, handler)?;
                }
                (VariableType::ComptimeValue, ExpectedType::Any)
            }
        };
        scope.set_variable(name, var);
        let assignment = match self {
            Self::Array(arr) => arr.assignment().as_ref(),
            Self::Score(scr) => scr.assignment().as_ref(),
            Self::Tag(tag) => {
                if let Some((target, assignment)) = tag.target_assignment() {
                    target.analyze_semantics(scope, handler)?;
                    Some(assignment)
                } else {
                    None
                }
            }
            Self::Single(single) => single.assignment().as_ref(),
            Self::ComptimeValue(decl) => decl.assignment().as_ref(),
        };
        if var == VariableType::ComptimeValue {
            // TODO: check if the expression is a constant expression
            return Ok(());
        }
        if let Some(assignment) = assignment {
            let expected = match var {
                VariableType::BooleanStorage | VariableType::Tag => ValueType::Boolean,
                VariableType::ScoreboardValue => ValueType::Integer,
                _ => {
                    let err = error::Error::MismatchedTypes(MismatchedTypes {
                        expected_type: expected,
                        expression: assignment.expression().span(),
                    });
                    handler.receive(err.clone());
                    return Err(err);
                }
            };
            if !assignment
                .expression()
                .can_yield_type_semantics(expected, scope)
            {
                let err = error::Error::MismatchedTypes(MismatchedTypes {
                    expected_type: expected.into(),
                    expression: assignment.expression().span(),
                });
                handler.receive(err.clone());
                return Err(err);
            }
            assignment.expression().analyze_semantics(scope, handler)?;
        }

        Ok(())
    }
}

impl Expression {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Primary(primary) => primary.analyze_semantics(scope, handler),
            Self::Binary(binary) => binary.analyze_semantics(scope, handler),
        }
    }

    #[must_use]
    fn can_yield_type_semantics(&self, expected: ValueType, scope: &SemanticScope) -> bool {
        match self {
            Self::Primary(primary) => primary.can_yield_type_semantics(expected, scope),
            Self::Binary(binary) => binary.can_yield_type_semantics(expected, scope),
        }
    }
}

impl Primary {
    #[expect(clippy::too_many_lines)]
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Identifier(ident) => {
                if scope.get_variable(ident.span.str()).is_none() {
                    let err = error::Error::UnknownIdentifier(UnknownIdentifier {
                        identifier: ident.span.clone(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                } else {
                    Ok(())
                }
            }
            Self::Boolean(_) | Self::Integer(_) | Self::StringLiteral(_) => Ok(()),
            Self::TemplateStringLiteral(lit) => lit.analyze_semantics(scope, handler),
            Self::FunctionCall(call) => {
                let var = scope.get_variable(call.identifier().span.str());
                var.map_or_else(
                    || {
                        let err = error::Error::UnknownIdentifier(UnknownIdentifier {
                            identifier: call.identifier().span.clone(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    },
                    |var| match var {
                        VariableType::Function | VariableType::InternalFunction => Ok(()),
                        _ => {
                            let err = error::Error::UnexpectedExpression(UnexpectedExpression(
                                Box::new(Expression::Primary(self.clone())),
                            ));
                            handler.receive(err.clone());
                            Err(err)
                        }
                    },
                )
            }
            Self::Indexed(indexed) => {
                if let Self::Identifier(ident) = indexed.object().as_ref() {
                    let variable_type = scope.get_variable(ident.span.str());
                    match variable_type {
                        Some(VariableType::BooleanStorageArray | VariableType::ScoreboardArray) => {
                            if !indexed
                                .index()
                                .can_yield_type_semantics(ValueType::Integer, scope)
                            {
                                let err = error::Error::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::Integer,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(err.clone());
                                return Err(err);
                            }
                        }
                        Some(VariableType::Tag | VariableType::Scoreboard) => {
                            if !indexed
                                .index()
                                .can_yield_type_semantics(ValueType::String, scope)
                            {
                                let err = error::Error::IllegalIndexing(IllegalIndexing {
                                    reason: IllegalIndexingReason::InvalidComptimeType {
                                        expected: ExpectedType::String,
                                    },
                                    expression: indexed.index().span(),
                                });
                                handler.receive(err.clone());
                                return Err(err);
                            }
                        }
                        _ => {
                            let err = error::Error::IllegalIndexing(IllegalIndexing {
                                reason: IllegalIndexingReason::NotIndexable,
                                expression: indexed.object().span(),
                            });
                            handler.receive(err.clone());
                            return Err(err);
                        }
                    }
                    indexed.object().analyze_semantics(scope, handler)?;
                    indexed.index().analyze_semantics(scope, handler)
                } else {
                    let err = error::Error::IllegalIndexing(IllegalIndexing {
                        reason: IllegalIndexingReason::NotIdentifier,
                        expression: indexed.object().span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
            Self::MemberAccess(_) => {
                // TODO:
                Ok(())
            }
            Self::Parenthesized(expr) => expr.analyze_semantics(scope, handler),
            Self::Prefix(prefixed) => match prefixed.operator() {
                PrefixOperator::LogicalNot(_) => {
                    if prefixed
                        .operand()
                        .can_yield_type_semantics(ValueType::Boolean, scope)
                    {
                        prefixed.operand().analyze_semantics(scope, handler)
                    } else {
                        let err = error::Error::MismatchedTypes(MismatchedTypes {
                            expected_type: ExpectedType::Boolean,
                            expression: prefixed.operand().span(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    }
                }
                PrefixOperator::Negate(_) => {
                    if prefixed
                        .operand()
                        .can_yield_type_semantics(ValueType::Integer, scope)
                    {
                        prefixed.operand().analyze_semantics(scope, handler)
                    } else {
                        let err = error::Error::MismatchedTypes(MismatchedTypes {
                            expected_type: ExpectedType::Integer,
                            expression: prefixed.operand().span(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    }
                }
                PrefixOperator::Run(_) => {
                    if prefixed
                        .operand()
                        .can_yield_type_semantics(ValueType::String, scope)
                    {
                        prefixed.operand().analyze_semantics(scope, handler)
                    } else {
                        let err = error::Error::MismatchedTypes(MismatchedTypes {
                            expected_type: ExpectedType::String,
                            expression: prefixed.operand().span(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    }
                }
            },
            Self::Lua(lua) => lua.analyze_semantics(scope, handler),
        }
    }

    #[must_use]
    fn can_yield_type_semantics(&self, expected: ValueType, scope: &SemanticScope) -> bool {
        match self {
            Self::Boolean(_) => expected == ValueType::Boolean,
            Self::Integer(_) => expected == ValueType::Integer,
            Self::StringLiteral(_) | Self::TemplateStringLiteral(_) => {
                matches!(expected, ValueType::String | ValueType::Boolean)
            }
            Self::FunctionCall(_) => matches!(expected, ValueType::Boolean | ValueType::Integer),
            Self::Indexed(indexed) => match indexed.object().as_ref() {
                Self::Identifier(ident) => {
                    let var = scope.get_variable(ident.span.str());
                    match (var, expected) {
                        (Some(VariableType::BooleanStorageArray), ValueType::Boolean)
                        | (Some(VariableType::ScoreboardArray), ValueType::Integer) => indexed
                            .index()
                            .can_yield_type_semantics(ValueType::Integer, scope),
                        (Some(VariableType::Tag), ValueType::Boolean)
                        | (Some(VariableType::Scoreboard), ValueType::Integer) => indexed
                            .index()
                            .can_yield_type_semantics(ValueType::String, scope),
                        _ => false,
                    }
                }
                _ => false,
            },
            Self::MemberAccess(_) => {
                // TODO: correct return value after check
                true
            }
            Self::Identifier(ident) => match scope.get_variable(ident.span.str()) {
                Some(VariableType::BooleanStorage) => expected == ValueType::Boolean,
                Some(VariableType::ScoreboardValue) => expected == ValueType::Integer,
                Some(VariableType::Tag) => expected == ValueType::String,
                Some(VariableType::ComptimeValue) => true,
                _ => false,
            },
            Self::Prefix(prefixed) => match prefixed.operator() {
                PrefixOperator::LogicalNot(_) => {
                    expected == ValueType::Boolean
                        && prefixed
                            .operand()
                            .can_yield_type_semantics(ValueType::Boolean, scope)
                }
                PrefixOperator::Negate(_) => {
                    expected == ValueType::Integer
                        && prefixed
                            .operand()
                            .can_yield_type_semantics(ValueType::Integer, scope)
                }
                PrefixOperator::Run(_) => {
                    expected == ValueType::String
                        && prefixed
                            .operand()
                            .can_yield_type_semantics(ValueType::String, scope)
                }
            },
            Self::Parenthesized(parenthesized) => {
                parenthesized.can_yield_type_semantics(expected, scope)
            }
            Self::Lua(lua) => lua.can_yield_type_semantics(expected),
        }
    }
}

impl Binary {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self.operator() {
            BinaryOperator::Add(_) => {
                if (self
                    .left_operand()
                    .can_yield_type_semantics(ValueType::Integer, scope)
                    && self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::Integer, scope))
                    || self
                        .left_operand()
                        .can_yield_type_semantics(ValueType::String, scope)
                    || self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::String, scope)
                {
                    self.left_operand().analyze_semantics(scope, handler)?;
                    self.right_operand().analyze_semantics(scope, handler)
                } else {
                    let err = error::Error::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::AnyOf(vec![
                            ExpectedType::Integer,
                            ExpectedType::String,
                        ]),
                        expression: self.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
            BinaryOperator::Subtract(_)
            | BinaryOperator::Multiply(_)
            | BinaryOperator::Divide(_)
            | BinaryOperator::Modulo(_) => {
                if self
                    .left_operand()
                    .can_yield_type_semantics(ValueType::Integer, scope)
                    && self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::Integer, scope)
                {
                    self.left_operand().analyze_semantics(scope, handler)?;
                    self.right_operand().analyze_semantics(scope, handler)
                } else {
                    let err = error::Error::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Integer,
                        expression: self.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
            BinaryOperator::Equal(..)
            | BinaryOperator::NotEqual(..)
            | BinaryOperator::GreaterThan(_)
            | BinaryOperator::GreaterThanOrEqual(..)
            | BinaryOperator::LessThan(_)
            | BinaryOperator::LessThanOrEqual(..) => {
                self.left_operand().analyze_semantics(scope, handler)?;
                self.right_operand().analyze_semantics(scope, handler)
            }
            BinaryOperator::LogicalAnd(..) | BinaryOperator::LogicalOr(..) => {
                if self
                    .left_operand()
                    .can_yield_type_semantics(ValueType::Boolean, scope)
                    && self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::Boolean, scope)
                {
                    self.left_operand().analyze_semantics(scope, handler)?;
                    self.right_operand().analyze_semantics(scope, handler)
                } else {
                    let err = error::Error::MismatchedTypes(MismatchedTypes {
                        expected_type: ExpectedType::Boolean,
                        expression: self.span(),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
        }
    }

    #[must_use]
    fn can_yield_type_semantics(&self, expected: ValueType, scope: &SemanticScope) -> bool {
        match self.operator() {
            BinaryOperator::Add(_) => {
                if expected == ValueType::Integer {
                    self.left_operand()
                        .can_yield_type_semantics(ValueType::Integer, scope)
                        && self
                            .right_operand()
                            .can_yield_type_semantics(ValueType::Integer, scope)
                } else {
                    expected == ValueType::String
                }
            }
            BinaryOperator::Subtract(_)
            | BinaryOperator::Multiply(_)
            | BinaryOperator::Divide(_)
            | BinaryOperator::Modulo(_) => {
                expected == ValueType::Integer
                    && self
                        .left_operand()
                        .can_yield_type_semantics(ValueType::Integer, scope)
                    && self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::Integer, scope)
            }
            BinaryOperator::Equal(..)
            | BinaryOperator::NotEqual(..)
            | BinaryOperator::GreaterThan(_)
            | BinaryOperator::GreaterThanOrEqual(..)
            | BinaryOperator::LessThan(_)
            | BinaryOperator::LessThanOrEqual(..) => expected == ValueType::Boolean,
            BinaryOperator::LogicalAnd(..) | BinaryOperator::LogicalOr(..) => {
                expected == ValueType::Boolean
                    && self
                        .left_operand()
                        .can_yield_type_semantics(ValueType::Boolean, scope)
                    && self
                        .right_operand()
                        .can_yield_type_semantics(ValueType::Boolean, scope)
            }
        }
    }
}

impl LuaCode {
    #[expect(clippy::unused_self)]
    #[cfg_attr(feature = "lua", expect(unused_variables, clippy::unnecessary_wraps))]
    fn analyze_semantics(
        &self,
        _scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        cfg_if::cfg_if! {
            if #[cfg(feature = "lua")] {
                // TODO: check which type is returned
                Ok(())
            } else {
                let err = error::Error::LuaDisabled;
                handler.receive(err.clone());
                Err(err)
            }
        }
    }

    #[expect(clippy::unused_self)]
    #[must_use]
    fn can_yield_type_semantics(&self, _expected: ValueType) -> bool {
        // TODO: check which type is returned
        cfg!(feature = "lua")
    }
}

impl AnyStringLiteral {
    pub(crate) fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::StringLiteral(_) => Ok(()),
            Self::TemplateStringLiteral(lit) => lit.analyze_semantics(scope, handler),
        }
    }
}

impl TemplateStringLiteral {
    fn analyze_semantics(
        &self,
        scope: &SemanticScope,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let mut errs = Vec::new();

        for part in self.parts() {
            match part {
                TemplateStringLiteralPart::Expression { expression, .. } => {
                    match expression.as_ref() {
                        Expression::Primary(Primary::Identifier(identifier)) => {
                            if let Some(variable_type) = scope.get_variable(identifier.span.str()) {
                                // TODO: correct checks
                                // if variable_type != VariableType::MacroParameter {
                                //     let err = error::Error::UnexpectedExpression(UnexpectedExpression(
                                //         Box::new(Expression::Primary(Primary::Identifier(
                                //             identifier.clone(),
                                //         ))),
                                //     ));
                                //     handler.receive(err.clone());
                                //     errs.push(err);
                                // }
                            } else {
                                let err = error::Error::UnknownIdentifier(UnknownIdentifier {
                                    identifier: identifier.span.clone(),
                                });
                                handler.receive(err.clone());
                                errs.push(err);
                            }
                        }
                        _ => {
                            // TODO: handle other expressions in template string literals
                        }
                    }
                }
                TemplateStringLiteralPart::Text(_) => {}
            }
        }

        errs.into_iter().next().map_or(Ok(()), Err)
    }
}
