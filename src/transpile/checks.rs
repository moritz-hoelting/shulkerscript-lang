use std::sync::Arc;

use crate::{
    base::{self, Handler},
    syntax::syntax_tree::statement::{
        execute_block::{ExecuteBlock, ExecuteBlockTail},
        AssignmentDestination, Block, SemicolonStatement, Statement,
    },
    transpile::{error::AssignmentError, Scope, TranspileError, TranspileResult, VariableData},
};

impl Block {
    pub(super) fn check_no_comptime_assignments(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<()> {
        for statement in self.statements() {
            match statement {
                Statement::Semicolon(sem) => {
                    if let SemicolonStatement::Assignment(assignment) = sem.statement() {
                        if let AssignmentDestination::Identifier(ident) = assignment.destination() {
                            if let Some(VariableData::ComptimeValue { .. }) =
                                scope.get_variable(ident.span.str()).as_deref()
                            {
                                let err = TranspileError::AssignmentError(AssignmentError {
                                    identifier: ident.span.clone(),
                                    message: "cannot assign to a compile-time variable in a conditional execute block"
                                        .to_string(),
                                });
                                handler.receive(Box::new(err.clone()));
                                return Err(err);
                            }
                        }
                    }
                }
                Statement::Grouping(group) => {
                    group
                        .block()
                        .check_no_comptime_assignments(scope, handler)?;
                }
                Statement::ExecuteBlock(ex) => {
                    ex.check_no_comptime_assignments(scope, handler)?;
                }
                Statement::WhileLoop(while_loop) => {
                    while_loop
                        .block()
                        .check_no_comptime_assignments(scope, handler)?;
                }
                _ => (),
            }
        }

        Ok(())
    }
}

impl ExecuteBlock {
    pub(super) fn check_no_comptime_assignments(
        &self,
        scope: &Arc<Scope>,
        handler: &impl Handler<base::Error>,
    ) -> TranspileResult<()> {
        match self {
            Self::HeadTail(_, tail) => match tail {
                ExecuteBlockTail::Block(block) => {
                    block.check_no_comptime_assignments(scope, handler)?;
                }
                ExecuteBlockTail::ExecuteBlock(_, ex) => {
                    ex.check_no_comptime_assignments(scope, handler)?;
                }
            },
            Self::IfElse(_, then, el) => {
                then.check_no_comptime_assignments(scope, handler)?;
                el.block().check_no_comptime_assignments(scope, handler)?;
            }
        }

        Ok(())
    }
}
