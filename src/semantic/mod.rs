//! This module contains the semantic analysis of the AST.

#![allow(clippy::missing_errors_doc)]

use std::collections::HashSet;

use error::{
    IncompatibleFunctionAnnotation, InvalidNamespaceName, MissingFunctionDeclaration,
    UnexpectedExpression, UnresolvedMacroUsage,
};

use crate::{
    base::{self, source_file::SourceElement as _, Handler},
    lexical::token::{MacroStringLiteral, MacroStringLiteralPart},
    syntax::syntax_tree::{
        condition::{
            BinaryCondition, Condition, ParenthesizedCondition, PrimaryCondition, UnaryCondition,
        },
        declaration::{Declaration, Function, ImportItems},
        expression::{Expression, FunctionCall, Primary},
        program::{Namespace, ProgramFile},
        statement::{
            execute_block::{
                Conditional, Else, ExecuteBlock, ExecuteBlockHead, ExecuteBlockHeadItem as _,
                ExecuteBlockTail,
            },
            Block, Grouping, Run, Semicolon, Statement,
        },
        AnyStringLiteral,
    },
};

pub mod error;

impl ProgramFile {
    /// Analyzes the semantics of the program.
    pub fn analyze_semantics(
        &self,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.namespace().analyze_semantics(handler)?;

        let mut errs = Vec::new();
        let function_names = extract_all_function_names(self.declarations(), handler)?;

        for declaration in self.declarations() {
            if let Err(err) = declaration.analyze_semantics(&function_names, handler) {
                errs.push(err);
            }
        }

        #[expect(clippy::option_if_let_else)]
        if let Some(err) = errs.first() {
            Err(err.clone())
        } else {
            Ok(())
        }
    }
}

fn extract_all_function_names(
    declarations: &[Declaration],
    handler: &impl Handler<base::Error>,
) -> Result<HashSet<String>, error::Error> {
    let mut function_names = HashSet::new();
    let mut errs = Vec::new();

    for declaration in declarations {
        match declaration {
            Declaration::Function(func) => {
                let name = func.identifier();
                if function_names.contains(name.span.str()) {
                    let err = error::Error::from(error::ConflictingFunctionNames {
                        name: name.span.str().to_string(),
                        definition: name.span(),
                    });
                    handler.receive(err.clone());
                    errs.push(err);
                }
                function_names.insert(name.span.str().to_string());
            }

            Declaration::Import(imp) => match imp.items() {
                ImportItems::All(_) => {
                    handler.receive(base::Error::Other(
                        "Importing all items is not yet supported.".to_string(),
                    ));
                }
                ImportItems::Named(items) => {
                    for item in items.elements() {
                        if function_names.contains(item.span.str()) {
                            let err = error::Error::from(error::ConflictingFunctionNames {
                                name: item.span.str().to_string(),
                                definition: item.span(),
                            });
                            handler.receive(err.clone());
                            errs.push(err);
                        }
                        function_names.insert(item.span.str().to_string());
                    }
                }
            },

            Declaration::Tag(_) => {}
        }
    }

    #[expect(clippy::option_if_let_else)]
    if let Some(err) = errs.first() {
        Err(err.clone())
    } else {
        Ok(function_names)
    }
}

impl Namespace {
    /// Analyzes the semantics of the namespace.
    pub fn analyze_semantics(
        &self,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let name = self.namespace_name();
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
    /// Analyzes the semantics of the declaration.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Function(func) => func.analyze_semantics(function_names, handler),
            Self::Import(_) | Self::Tag(_) => Ok(()),
        }
    }
}

impl Function {
    /// Analyzes the semantics of the function.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let macro_names = if let Some(parameters) = self.parameters() {
            if let Some(incompatible) = self
                .annotations()
                .iter()
                .find(|a| ["tick", "load"].contains(&a.identifier().span.str()))
            {
                let err =
                    error::Error::IncompatibleFunctionAnnotation(IncompatibleFunctionAnnotation {
                        span: incompatible.identifier().span(),
                        reason:
                            "functions with the `tick` or `load` annotation cannot have parameters"
                                .to_string(),
                    });
                handler.receive(err.clone());
                return Err(err);
            }

            parameters
                .elements()
                .map(|el| el.span.str().to_string())
                .collect()
        } else {
            HashSet::new()
        };

        self.block()
            .analyze_semantics(function_names, &macro_names, handler)
    }
}

impl Block {
    /// Analyzes the semantics of a block.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let mut errs = Vec::new();
        for statement in &self.statements {
            if let Err(err) = match statement {
                Statement::Block(block) => {
                    block.analyze_semantics(function_names, macro_names, handler)
                }
                Statement::DocComment(_) | Statement::LiteralCommand(_) => Ok(()),
                Statement::ExecuteBlock(ex) => {
                    ex.analyze_semantics(function_names, macro_names, handler)
                }
                Statement::Grouping(group) => {
                    group.analyze_semantics(function_names, macro_names, handler)
                }
                Statement::Run(run) => run.analyze_semantics(function_names, macro_names, handler),
                Statement::Semicolon(sem) => {
                    sem.analyze_semantics(function_names, macro_names, handler)
                }
            } {
                errs.push(err);
            };
        }

        #[expect(clippy::option_if_let_else)]
        if let Some(err) = errs.first() {
            Err(err.clone())
        } else {
            Ok(())
        }
    }
}

impl ExecuteBlock {
    /// Analyzes the semantics of the execute block.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::HeadTail(head, tail) => {
                let head_res = head.analyze_semantics(function_names, macro_names, handler);
                let tail_res = tail.analyze_semantics(function_names, macro_names, handler);

                if head_res.is_err() {
                    head_res
                } else {
                    tail_res
                }
            }
            Self::IfElse(cond, then, el) => {
                let cond_res = cond.analyze_semantics(function_names, macro_names, handler);
                let then_res = then.analyze_semantics(function_names, macro_names, handler);
                let else_res = el.analyze_semantics(function_names, macro_names, handler);

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

impl Grouping {
    /// Analyzes the semantics of the grouping.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.block()
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl Run {
    /// Analyzes the semantics of the run statement.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.expression()
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl Semicolon {
    /// Analyzes the semantics of the semicolon statement.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self.expression() {
            Expression::Primary(Primary::FunctionCall(func)) => {
                func.analyze_semantics(function_names, macro_names, handler)
            }
            Expression::Primary(unexpected) => {
                let error = error::Error::UnexpectedExpression(UnexpectedExpression(
                    Expression::Primary(unexpected.clone()),
                ));
                handler.receive(error.clone());
                Err(error)
            }
        }
    }
}

impl ExecuteBlockHead {
    /// Analyzes the semantics of the execute block head.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Align(align) => align.analyze_semantics(macro_names, handler),
            Self::Anchored(anchored) => anchored.analyze_semantics(macro_names, handler),
            Self::As(r#as) => r#as.analyze_semantics(macro_names, handler),
            Self::At(at) => at.analyze_semantics(macro_names, handler),
            Self::AsAt(asat) => asat.analyze_semantics(macro_names, handler),
            Self::Conditional(cond) => cond.analyze_semantics(function_names, macro_names, handler),
            Self::Facing(facing) => facing.analyze_semantics(macro_names, handler),
            Self::In(r#in) => r#in.analyze_semantics(macro_names, handler),
            Self::On(on) => on.analyze_semantics(macro_names, handler),
            Self::Positioned(pos) => pos.analyze_semantics(macro_names, handler),
            Self::Rotated(rot) => rot.analyze_semantics(macro_names, handler),
            Self::Store(store) => store.analyze_semantics(macro_names, handler),
            Self::Summon(summon) => summon.analyze_semantics(macro_names, handler),
        }
    }
}

impl ExecuteBlockTail {
    /// Analyzes the semantics of the execute block tail.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Block(block) => block.analyze_semantics(function_names, macro_names, handler),
            Self::ExecuteBlock(_, ex) => ex.analyze_semantics(function_names, macro_names, handler),
        }
    }
}

impl Conditional {
    /// Analyzes the semantics of the conditional.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.condition()
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl ParenthesizedCondition {
    /// Analyzes the semantics of the parenthesized condition.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.condition
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl Condition {
    /// Analyzes the semantics of the condition.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Primary(prim) => prim.analyze_semantics(function_names, macro_names, handler),
            Self::Binary(bin) => bin.analyze_semantics(function_names, macro_names, handler),
        }
    }
}

impl Else {
    /// Analyzes the semantics of the else block.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.block()
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl MacroStringLiteral {
    /// Analyzes the semantics of the macro string literal.
    pub fn analyze_semantics(
        &self,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let mut errors = Vec::new();
        for part in self.parts() {
            if let MacroStringLiteralPart::MacroUsage { identifier, .. } = part {
                if !macro_names.contains(identifier.span.str()) {
                    let err = error::Error::UnresolvedMacroUsage(UnresolvedMacroUsage {
                        span: identifier.span(),
                    });
                    handler.receive(err.clone());
                    errors.push(err);
                }
            }
        }

        #[expect(clippy::option_if_let_else)]
        if let Some(err) = errors.first() {
            Err(err.clone())
        } else {
            Ok(())
        }
    }
}

impl Expression {
    /// Analyzes the semantics of an expression.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Primary(prim) => prim.analyze_semantics(function_names, macro_names, handler),
        }
    }
}

impl Primary {
    /// Analyzes the semantics of a primary expression.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::FunctionCall(func) => {
                func.analyze_semantics(function_names, macro_names, handler)
            }
            Self::Lua(_) | Self::StringLiteral(_) => Ok(()),
            Self::MacroStringLiteral(literal) => literal.analyze_semantics(macro_names, handler),
        }
    }
}

impl FunctionCall {
    /// Analyzes the semantics of a function call.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let mut errors = Vec::new();

        if !function_names.contains(self.identifier().span.str()) {
            let err = error::Error::MissingFunctionDeclaration(
                MissingFunctionDeclaration::from_context(self.identifier().span(), function_names),
            );
            handler.receive(err.clone());
            errors.push(err);
        }

        for expression in self
            .arguments()
            .iter()
            .flat_map(super::syntax::syntax_tree::ConnectedList::elements)
        {
            if let Err(err) = expression.analyze_semantics(function_names, macro_names, handler) {
                handler.receive(err.clone());
                errors.push(err);
            }
        }

        #[expect(clippy::option_if_let_else)]
        if let Some(err) = errors.first() {
            Err(err.clone())
        } else {
            Ok(())
        }
    }
}

impl AnyStringLiteral {
    /// Analyzes the semantics of any string literal.
    pub fn analyze_semantics(
        &self,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::StringLiteral(_) => Ok(()),
            Self::MacroStringLiteral(literal) => literal.analyze_semantics(macro_names, handler),
        }
    }
}

impl PrimaryCondition {
    /// Analyzes the semantics of a primary condition.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        match self {
            Self::Parenthesized(paren) => {
                paren.analyze_semantics(function_names, macro_names, handler)
            }
            Self::StringLiteral(_) => Ok(()),
            Self::Unary(unary) => unary.analyze_semantics(function_names, macro_names, handler),
        }
    }
}

impl UnaryCondition {
    /// Analyzes the semantics of an unary condition.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        self.operand()
            .analyze_semantics(function_names, macro_names, handler)
    }
}

impl BinaryCondition {
    /// Analyzes the semantics of a binary condition.
    pub fn analyze_semantics(
        &self,
        function_names: &HashSet<String>,
        macro_names: &HashSet<String>,
        handler: &impl Handler<base::Error>,
    ) -> Result<(), error::Error> {
        let a = self
            .left_operand()
            .analyze_semantics(function_names, macro_names, handler)
            .inspect_err(|err| {
                handler.receive(err.clone());
            });
        let b = self
            .right_operand()
            .analyze_semantics(function_names, macro_names, handler)
            .inspect_err(|err| {
                handler.receive(err.clone());
            });
        if a.is_err() {
            a
        } else {
            b
        }
    }
}
