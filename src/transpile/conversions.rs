//! Conversion functions for converting between tokens/ast-nodes and [`shulkerbox`] types

use shulkerbox::datapack::Condition as DpCondition;

use crate::syntax::syntax_tree::expression::{
    BinaryCondition, Condition, ConditionalBinaryOperator, ConditionalPrefixOperator,
    PrimaryCondition,
};

impl From<Condition> for DpCondition {
    fn from(value: Condition) -> Self {
        match value {
            Condition::Primary(primary) => primary.into(),
            Condition::Binary(binary) => binary.into(),
        }
    }
}

impl From<PrimaryCondition> for DpCondition {
    fn from(value: PrimaryCondition) -> Self {
        match value {
            PrimaryCondition::StringLiteral(literal) => {
                Self::Atom(literal.str_content().to_string())
            }
            PrimaryCondition::Parenthesized(cond) => cond.dissolve().1.into(),
            PrimaryCondition::Prefix(prefix) => match prefix.operator() {
                ConditionalPrefixOperator::LogicalNot(_) => {
                    Self::Not(Box::new(prefix.dissolve().1.into()))
                }
            },
        }
    }
}

impl From<BinaryCondition> for DpCondition {
    fn from(value: BinaryCondition) -> Self {
        let (lhs, op, rhs) = value.dissolve();
        match op {
            ConditionalBinaryOperator::LogicalAnd(_, _) => {
                Self::And(Box::new(lhs.into()), Box::new(rhs.into()))
            }
            ConditionalBinaryOperator::LogicalOr(_, _) => {
                Self::Or(Box::new(lhs.into()), Box::new(rhs.into()))
            }
        }
    }
}
