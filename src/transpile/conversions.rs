//! Conversion functions for converting between tokens/ast-nodes and [`shulkerbox`] types

use shulkerbox::{
    datapack::Condition as DpCondition,
    util::{MacroString, MacroStringPart},
};

use crate::{
    lexical::token::{MacroStringLiteral, MacroStringLiteralPart},
    syntax::syntax_tree::{
        condition::{
            BinaryCondition, Condition, ConditionalBinaryOperator, ConditionalPrefixOperator,
            PrimaryCondition,
        },
        AnyStringLiteral,
    },
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
            PrimaryCondition::StringLiteral(literal) => Self::Atom(literal.into()),
            PrimaryCondition::Parenthesized(cond) => cond.dissolve().1.into(),
            PrimaryCondition::Unary(prefix) => match prefix.operator() {
                ConditionalPrefixOperator::LogicalNot(_) => {
                    Self::Not(Box::new(prefix.dissolve().1.into()))
                }
            },
        }
    }
}

impl From<&AnyStringLiteral> for MacroString {
    fn from(value: &AnyStringLiteral) -> Self {
        match value {
            AnyStringLiteral::StringLiteral(literal) => Self::from(literal.str_content().as_ref()),
            AnyStringLiteral::MacroStringLiteral(literal) => Self::from(literal),
        }
    }
}

impl From<AnyStringLiteral> for MacroString {
    fn from(value: AnyStringLiteral) -> Self {
        Self::from(&value)
    }
}

impl From<&MacroStringLiteral> for MacroString {
    fn from(value: &MacroStringLiteral) -> Self {
        if value
            .parts()
            .iter()
            .any(|p| matches!(p, MacroStringLiteralPart::MacroUsage { .. }))
        {
            Self::MacroString(
                value
                    .parts()
                    .iter()
                    .map(|part| match part {
                        MacroStringLiteralPart::Text(span) => {
                            MacroStringPart::String(span.str().to_string())
                        }
                        MacroStringLiteralPart::MacroUsage { identifier, .. } => {
                            MacroStringPart::MacroUsage(
                                crate::transpile::util::identifier_to_macro(identifier.span.str())
                                    .to_string(),
                            )
                        }
                    })
                    .collect(),
            )
        } else {
            Self::String(value.str_content())
        }
    }
}

impl From<MacroStringLiteral> for MacroString {
    fn from(value: MacroStringLiteral) -> Self {
        Self::from(&value)
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
