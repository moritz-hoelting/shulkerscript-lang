//! Syntax tree nodes for statements.

pub mod execute_block;

use std::collections::VecDeque;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;

use crate::{
    base::{
        self,
        source_file::{SourceElement, Span},
        Handler, VoidHandler,
    },
    lexical::{
        token::{
            CommandLiteral, DocComment, Identifier, Integer, Keyword, KeywordKind, Punctuation,
            StringLiteral, Token,
        },
        token_stream::Delimiter,
    },
    syntax::{
        error::{Error, InvalidAnnotation, ParseResult, SyntaxKind, UnexpectedSyntax},
        parser::{Parser, Reading},
    },
};

use self::execute_block::ExecuteBlock;

use super::{expression::Expression, Annotation, AnyStringLiteral};

/// Represents a statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Statement:
///     Block
///     | LiteralCommand
///     | Conditional
///     | Grouping
///     | DocComment
///     | Semicolon
///     | Run
///     ;
/// ```
#[allow(missing_docs)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From)]
pub enum Statement {
    Block(Block),
    LiteralCommand(CommandLiteral),
    ExecuteBlock(ExecuteBlock),
    Grouping(Grouping),
    DocComment(DocComment),
    Semicolon(Semicolon),
    Run(Run),
}

impl SourceElement for Statement {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span(),
            Self::LiteralCommand(literal_command) => literal_command.span(),
            Self::ExecuteBlock(execute_block) => execute_block.span(),
            Self::Grouping(grouping) => grouping.span(),
            Self::DocComment(doc_comment) => doc_comment.span(),
            Self::Semicolon(semi) => semi.span(),
            Self::Run(run) => run.span(),
        }
    }
}

impl Statement {
    /// Adds an annotation to the statement.
    ///
    /// # Errors
    /// - if the annotation is invalid for the statement.
    pub fn with_annotation(self, annotation: Annotation) -> ParseResult<Self> {
        #[expect(clippy::single_match_else)]
        match self {
            Self::Semicolon(Semicolon {
                statement,
                semicolon,
            }) => match statement {
                SemicolonStatement::VariableDeclaration(decl) => {
                    decl.with_annotation(annotation).map(|decl| {
                        Self::Semicolon(Semicolon {
                            statement: SemicolonStatement::VariableDeclaration(decl),
                            semicolon,
                        })
                    })
                }
                SemicolonStatement::Assignment(_) => {
                    let err = Error::InvalidAnnotation(InvalidAnnotation {
                        annotation: annotation.assignment.identifier.span,
                        target: "assignments".to_string(),
                    });

                    Err(err)
                }
                SemicolonStatement::Expression(_) => {
                    let err = Error::InvalidAnnotation(InvalidAnnotation {
                        annotation: annotation.assignment.identifier.span,
                        target: "expressions".to_string(),
                    });

                    Err(err)
                }
            },
            _ => {
                let err = Error::InvalidAnnotation(InvalidAnnotation {
                    annotation: annotation.assignment.identifier.span,
                    target: "statements except variable declarations".to_string(),
                });

                Err(err)
            }
        }
    }
}

/// Represents a block in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Block:
///     '{' Statement* '}'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Block {
    /// The opening brace of the block.
    #[get = "pub"]
    pub open_brace: Punctuation,
    /// The statements within the block.
    #[get = "pub"]
    pub statements: Vec<Statement>,
    /// The closing brace of the block.
    #[get = "pub"]
    pub close_brace: Punctuation,
}

impl Block {
    /// Dissolves the [`Block`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Vec<Statement>, Punctuation) {
        (self.open_brace, self.statements, self.close_brace)
    }
}

impl SourceElement for Block {
    fn span(&self) -> Span {
        self.open_brace
            .span()
            .join(&self.close_brace.span())
            .unwrap()
    }
}

/// Represents a run statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Run:
/// 'run' Expression ';'
/// ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Run {
    /// The `run` keyword.
    #[get = "pub"]
    run_keyword: Keyword,
    /// The expression of the run statement.
    #[get = "pub"]
    expression: Expression,
    /// The semicolon of the run statement.
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Run {
    fn span(&self) -> Span {
        self.run_keyword
            .span()
            .join(&self.semicolon.span())
            .expect("The span of the run statement is invalid.")
    }
}

impl Run {
    /// Dissolves the [`Run`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Expression, Punctuation) {
        (self.run_keyword, self.expression, self.semicolon)
    }
}

/// Represents a grouping statement in the syntax tree.
///
/// Syntax Synopsis:
///
/// ``` ebnf
/// Grouping:
/// 'group' Block
/// ;
/// ````
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Grouping {
    /// The `group` keyword.
    #[get = "pub"]
    group_keyword: Keyword,
    /// The block of the conditional.
    #[get = "pub"]
    block: Block,
}

impl Grouping {
    /// Dissolves the [`Grouping`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Block) {
        (self.group_keyword, self.block)
    }
}

impl SourceElement for Grouping {
    fn span(&self) -> Span {
        self.group_keyword
            .span()
            .join(&self.block.span())
            .expect("The span of the grouping is invalid.")
    }
}

/// Represents a statement that ends with a semicolon in the syntax tree.
///
/// Syntax Synopsis:
/// ``` ebnf
/// Semicolon:
///    SemicolonStatement ';'
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Semicolon {
    /// The expression of the semicolon statement.
    #[get = "pub"]
    statement: SemicolonStatement,
    /// The semicolon of the semicolon statement.
    #[get = "pub"]
    semicolon: Punctuation,
}

impl SourceElement for Semicolon {
    fn span(&self) -> Span {
        self.statement
            .span()
            .join(&self.semicolon.span())
            .expect("The span of the semicolon statement is invalid.")
    }
}

impl Semicolon {
    /// Dissolves the [`Semicolon`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (SemicolonStatement, Punctuation) {
        (self.statement, self.semicolon)
    }
}

/// Represents a statement that ends with a semicolon in the syntax tree.
///
/// Syntax Synopsis:
/// ``` ebnf
/// SemicolonStatement:
///    (Expression | VariableDeclaration | Assignment)
///    ';'
///   ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From, EnumAsInner)]
pub enum SemicolonStatement {
    /// An expression that ends with a semicolon.
    Expression(Expression),
    /// A variable declaration.
    VariableDeclaration(VariableDeclaration),
    /// An assignment.
    Assignment(Assignment),
}

impl SourceElement for SemicolonStatement {
    fn span(&self) -> Span {
        match self {
            Self::Expression(expression) => expression.span(),
            Self::VariableDeclaration(declaration) => declaration.span(),
            Self::Assignment(assignment) => assignment.span(),
        }
    }
}

/// Represents a variable declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// VariableDeclaration:
///    SingleVariableDeclaration
///    | ArrayVariableDeclaration
///    | ScoreVariableDeclaration
///    | TagVariableDeclaration
///    ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, From, EnumAsInner)]
#[allow(missing_docs)]
pub enum VariableDeclaration {
    Single(SingleVariableDeclaration),
    Array(ArrayVariableDeclaration),
    Score(ScoreVariableDeclaration),
    Tag(TagVariableDeclaration),
    ComptimeValue(ComptimeValueDeclaration),
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Span {
        match self {
            Self::Single(declaration) => declaration.span(),
            Self::Array(declaration) => declaration.span(),
            Self::Score(declaration) => declaration.span(),
            Self::Tag(declaration) => declaration.span(),
            Self::ComptimeValue(declaration) => declaration.span(),
        }
    }
}

impl VariableDeclaration {
    /// Get the identifier of the variable declaration
    #[must_use]
    pub fn identifier(&self) -> &Identifier {
        match self {
            Self::Single(declaration) => &declaration.identifier,
            Self::Array(declaration) => &declaration.identifier,
            Self::Score(declaration) => &declaration.identifier,
            Self::Tag(declaration) => &declaration.identifier,
            Self::ComptimeValue(declaration) => &declaration.identifier,
        }
    }

    /// Get the type of the variable declaration
    #[must_use]
    pub fn variable_type(&self) -> &Keyword {
        match self {
            Self::Single(declaration) => &declaration.variable_type,
            Self::Array(declaration) => &declaration.variable_type,
            Self::Score(declaration) => &declaration.int_keyword,
            Self::Tag(declaration) => &declaration.bool_keyword,
            Self::ComptimeValue(declaration) => &declaration.val_keyword,
        }
    }

    /// Adds an annotation to the variable declaration.
    ///
    /// # Errors
    /// - if the annotation is invalid for the variable declaration.
    pub fn with_annotation(self, annotation: Annotation) -> ParseResult<Self> {
        match self {
            Self::Single(mut declaration) => {
                declaration.annotations.push_front(annotation);
                Ok(Self::Single(declaration))
            }
            Self::Array(mut declaration) => {
                declaration.annotations.push_front(annotation);
                Ok(Self::Array(declaration))
            }
            Self::Score(mut declaration) => {
                declaration.annotations.push_front(annotation);
                Ok(Self::Score(declaration))
            }
            Self::Tag(mut declaration) => {
                declaration.annotations.push_front(annotation);
                Ok(Self::Tag(declaration))
            }
            Self::ComptimeValue(_) => {
                let err = Error::InvalidAnnotation(InvalidAnnotation {
                    annotation: annotation.assignment.identifier.span,
                    target: "comptime values".to_string(),
                });

                Err(err)
            }
        }
    }
}

/// Represents a variable assignment.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// VariableDeclarationAssignment:
///     '=' Expression
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct VariableDeclarationAssignment {
    /// The equals sign of the variable declaration.
    #[get = "pub"]
    equals: Punctuation,
    /// The expression of the variable declaration.
    #[get = "pub"]
    expression: Expression,
}

impl SourceElement for VariableDeclarationAssignment {
    fn span(&self) -> Span {
        self.equals
            .span()
            .join(&self.expression.span())
            .expect("The span of the variable declaration assignment is invalid.")
    }
}

impl VariableDeclarationAssignment {
    /// Dissolves the [`VariableDeclarationAssignment`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Punctuation, Expression) {
        (self.equals, self.expression)
    }
}

/// Represents a single variable declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// SingleVariableDeclaration:
///     ('int' | 'bool') identifier VariableDeclarationAssignment?
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct SingleVariableDeclaration {
    /// The type of the variable.
    #[get = "pub"]
    variable_type: Keyword,
    /// The identifier of the variable.
    #[get = "pub"]
    identifier: Identifier,
    /// The optional assignment of the variable.
    #[get = "pub"]
    assignment: Option<VariableDeclarationAssignment>,
    /// The annotations of the variable declaration.
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
}

impl SourceElement for SingleVariableDeclaration {
    fn span(&self) -> Span {
        self.variable_type
            .span()
            .join(
                &self
                    .assignment
                    .as_ref()
                    .map_or_else(|| self.identifier.span(), SourceElement::span),
            )
            .expect("The span of the single variable declaration is invalid.")
    }
}

impl SingleVariableDeclaration {
    /// Dissolves the [`SingleVariableDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Identifier, Option<VariableDeclarationAssignment>) {
        (self.variable_type, self.identifier, self.assignment)
    }
}

/// Represents an array variable declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// ArrayVariableDeclaration:
///     ('int' | 'bool') identifier '[' integer ']' VariableDeclarationAssignment?
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ArrayVariableDeclaration {
    /// The type of the variable.
    #[get = "pub"]
    variable_type: Keyword,
    /// The identifier of the variable.
    #[get = "pub"]
    identifier: Identifier,
    /// The opening bracket of the array.
    #[get = "pub"]
    open_bracket: Punctuation,
    /// The array size
    #[get = "pub"]
    size: Integer,
    /// The closing bracket of the array.
    #[get = "pub"]
    close_bracket: Punctuation,
    /// The optional assignment of the variable.
    #[get = "pub"]
    assignment: Option<VariableDeclarationAssignment>,
    /// The annotations of the variable declaration.
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
}

impl SourceElement for ArrayVariableDeclaration {
    fn span(&self) -> Span {
        self.variable_type
            .span()
            .join(
                &self
                    .assignment
                    .as_ref()
                    .map_or_else(|| self.close_bracket.span(), SourceElement::span),
            )
            .expect("The span of the array variable declaration is invalid.")
    }
}

impl ArrayVariableDeclaration {
    /// Dissolves the [`ArrayVariableDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Punctuation,
        Integer,
        Punctuation,
        Option<VariableDeclarationAssignment>,
    ) {
        (
            self.variable_type,
            self.identifier,
            self.open_bracket,
            self.size,
            self.close_bracket,
            self.assignment,
        )
    }
}

type CriteriaSelection = (Punctuation, StringLiteral, Punctuation);

/// Represents a scoreboard variable declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// ScoreVariableDeclaration:
///     'int' ('<' StringLiteral '>')? identifier '[' AnyStringLiteral? ']' VariableDeclarationAssignment?
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ScoreVariableDeclaration {
    /// `int` keyword
    #[get = "pub"]
    int_keyword: Keyword,
    /// The scoreboard criteria
    #[get = "pub"]
    criteria: Option<CriteriaSelection>,
    /// The identifier of the variable.
    #[get = "pub"]
    identifier: Identifier,
    /// Opening bracket of the score variable
    #[get = "pub"]
    open_bracket: Punctuation,
    /// Closing bracket of the score variable
    #[get = "pub"]
    close_bracket: Punctuation,
    /// The optional assignment of the variable.
    #[get = "pub"]
    assignment: Option<VariableDeclarationAssignment>,
    /// The annotations of the variable declaration.
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
}

impl SourceElement for ScoreVariableDeclaration {
    fn span(&self) -> Span {
        self.int_keyword
            .span()
            .join(
                &self
                    .assignment
                    .as_ref()
                    .map_or_else(|| self.close_bracket.span(), SourceElement::span),
            )
            .expect("The span of the score variable declaration is invalid.")
    }
}

impl ScoreVariableDeclaration {
    /// Dissolves the [`ScoreVariableDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Option<CriteriaSelection>,
        Identifier,
        Punctuation,
        Punctuation,
        Option<VariableDeclarationAssignment>,
    ) {
        (
            self.int_keyword,
            self.criteria,
            self.identifier,
            self.open_bracket,
            self.close_bracket,
            self.assignment,
        )
    }
}

/// Represents a tag variable declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// TagVariableDeclaration:
///     'bool' identifier '[' AnyStringLiteral? ']' VariableDeclarationAssignment?
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct TagVariableDeclaration {
    /// `bool` keyword
    #[get = "pub"]
    bool_keyword: Keyword,
    /// The identifier of the variable.
    #[get = "pub"]
    identifier: Identifier,
    /// Opening bracket of the score variable
    #[get = "pub"]
    open_bracket: Punctuation,
    /// Closing bracket of the score variable
    #[get = "pub"]
    close_bracket: Punctuation,
    /// The optional assignment of the variable.
    #[get = "pub"]
    target_assignment: Option<(AnyStringLiteral, VariableDeclarationAssignment)>,
    /// The annotations of the variable declaration.
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
}

impl SourceElement for TagVariableDeclaration {
    fn span(&self) -> Span {
        self.bool_keyword
            .span()
            .join(&self.target_assignment.as_ref().map_or_else(
                || self.close_bracket.span(),
                |(_, assignment)| assignment.span(),
            ))
            .expect("The span of the tag variable declaration is invalid.")
    }
}

impl TagVariableDeclaration {
    /// Dissolves the [`TagVariableDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(
        self,
    ) -> (
        Keyword,
        Identifier,
        Punctuation,
        Punctuation,
        Option<(AnyStringLiteral, VariableDeclarationAssignment)>,
    ) {
        (
            self.bool_keyword,
            self.identifier,
            self.open_bracket,
            self.close_bracket,
            self.target_assignment,
        )
    }
}

/// Represents a compile time value declaration in the syntax tree.
///
/// Syntax Synopsis:
///
/// ```ebnf
/// ComptimeValueDeclaration:
///     'val' identifier VariableDeclarationAssignment?
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct ComptimeValueDeclaration {
    /// The type of the variable.
    #[get = "pub"]
    val_keyword: Keyword,
    /// The identifier of the variable.
    #[get = "pub"]
    identifier: Identifier,
    /// The optional assignment of the variable.
    #[get = "pub"]
    assignment: Option<VariableDeclarationAssignment>,
    /// The annotations of the variable declaration.
    #[get = "pub"]
    annotations: VecDeque<Annotation>,
}

impl SourceElement for ComptimeValueDeclaration {
    fn span(&self) -> Span {
        self.val_keyword
            .span()
            .join(
                &self
                    .assignment
                    .as_ref()
                    .map_or_else(|| self.identifier.span(), SourceElement::span),
            )
            .expect("The span of the single variable declaration is invalid.")
    }
}

impl ComptimeValueDeclaration {
    /// Dissolves the [`ComptimeValueDeclaration`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (Keyword, Identifier, Option<VariableDeclarationAssignment>) {
        (self.val_keyword, self.identifier, self.assignment)
    }
}

/// Represents an assignment in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// Assignment:
///    AssignmentDestination '=' Expression
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Assignment {
    /// The identifier of the assignment.
    #[get = "pub"]
    destination: AssignmentDestination,
    /// The equals sign of the assignment.
    #[get = "pub"]
    equals: Punctuation,
    /// The expression of the assignment.
    #[get = "pub"]
    expression: Expression,
}

impl SourceElement for Assignment {
    fn span(&self) -> Span {
        self.destination
            .span()
            .join(&self.expression.span())
            .expect("The span of the assignment is invalid.")
    }
}

impl Assignment {
    /// Dissolves the [`Assignment`] into its components.
    #[must_use]
    pub fn dissolve(self) -> (AssignmentDestination, Punctuation, Expression) {
        (self.destination, self.equals, self.expression)
    }
}

/// Represents an assignment destination in the syntax tree.
///
/// Syntax Synopsis:
/// ```ebnf
/// AssignmentDestination:
///     Identifier
///     | Identifier '[' Expression ']'
///     ;
/// ```
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AssignmentDestination {
    /// Assignment to an identifier.
    Identifier(Identifier),
    /// Assignment to an indexed identifier.
    Indexed(Identifier, Punctuation, Expression, Punctuation),
}

impl SourceElement for AssignmentDestination {
    fn span(&self) -> Span {
        match self {
            Self::Identifier(identifier) => identifier.span(),
            Self::Indexed(identifier, _, _, close) => identifier
                .span()
                .join(&close.span())
                .expect("The span of the indexed assignment destination is invalid."),
        }
    }
}

impl Parser<'_> {
    /// Parses a [`Block`].
    ///
    /// # Errors
    /// - if the parser is not at a block.
    pub fn parse_block(&mut self, handler: &impl Handler<base::Error>) -> ParseResult<Block> {
        let token_tree = self.step_into(
            Delimiter::Brace,
            |parser| {
                let mut statements = Vec::new();

                while !parser.is_exhausted() {
                    parser.parse_statement(handler).map_or_else(
                        |_| {
                            // error recovery
                            parser.stop_at(|reading| matches!(
                                reading,
                                Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == ';'
                            ) || matches!(
                                reading,
                                Reading::IntoDelimited(punc) if punc.punctuation == '{'
                            ));

                            // goes after the semicolon or the open brace
                            parser.forward();
                        },
                        |statement| statements.push(statement),
                    );
                }

                Ok(statements)
            },
            handler,
        )?;

        Ok(Block {
            open_brace: token_tree.open,
            statements: token_tree.tree?,
            close_brace: token_tree.close,
        })
    }

    /// Parses a [`Statement`].
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn parse_statement(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Statement> {
        match self.stop_at_significant() {
            // annotations
            Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '#' => {
                let annotation = self.parse_annotation(handler)?;
                let statement = self.parse_statement(handler)?;

                statement
                    .with_annotation(annotation)
                    .inspect_err(|err| handler.receive(err.clone()))
            }
            // variable declaration
            Reading::Atomic(Token::CommandLiteral(command)) => {
                self.forward();
                tracing::trace!("Parsed literal command '{}'", command.clean_command());
                Ok(Statement::LiteralCommand(command))
            }
            // block statement
            Reading::IntoDelimited(open_brace) if open_brace.punctuation == '{' => {
                let block = self.parse_block(handler)?;

                Ok(Statement::Block(block))
            }

            // execute block
            Reading::Atomic(Token::Keyword(execute_keyword))
                if execute_keyword.keyword.starts_execute_block() =>
            {
                self.parse_execute_block_statement(handler)
                    .map(Statement::ExecuteBlock)
            }

            // doc comment
            Reading::Atomic(Token::DocComment(doc_comment)) => {
                self.forward();
                Ok(Statement::DocComment(doc_comment))
            }

            // grouping statement
            Reading::Atomic(Token::Keyword(group_keyword))
                if group_keyword.keyword == KeywordKind::Group =>
            {
                // eat the group keyword
                self.forward();

                let block = self.parse_block(handler)?;

                tracing::trace!("Parsed group command");

                Ok(Statement::Grouping(Grouping {
                    group_keyword,
                    block,
                }))
            }

            // run statement
            Reading::Atomic(Token::Keyword(run_keyword))
                if run_keyword.keyword == KeywordKind::Run =>
            {
                // eat the run keyword
                self.forward();

                let expression = self.parse_expression(handler)?;
                let semicolon = self.parse_punctuation(';', true, handler)?;

                tracing::trace!("Parsed run statement: {:?}", expression);

                Ok(Statement::Run(Run {
                    run_keyword,
                    expression,
                    semicolon,
                }))
            }

            // semicolon statement
            _ => self.parse_semicolon(handler).map(Statement::Semicolon),
        }
    }

    /// Parses a [`Semicolon`].
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn parse_semicolon(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<Semicolon> {
        let statement = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(keyword))
                if matches!(
                    keyword.keyword,
                    KeywordKind::Int | KeywordKind::Bool | KeywordKind::Val
                ) =>
            {
                self.parse_variable_declaration(handler)
                    .map(SemicolonStatement::VariableDeclaration)
            }
            _ => {
                // try to parse assignment
                #[expect(clippy::option_if_let_else)]
                if let Ok(assignment) = self.try_parse(|p| {
                    let destination = {
                        let identifier = p.parse_identifier(&VoidHandler)?;

                        match p.stop_at_significant() {
                            Reading::IntoDelimited(punc) if punc.punctuation == '[' => {
                                let tree = p.step_into(
                                    Delimiter::Bracket,
                                    |pp| pp.parse_expression(&VoidHandler),
                                    &VoidHandler,
                                )?;
                                let open = tree.open;
                                let close = tree.close;
                                let expression = tree.tree?;

                                AssignmentDestination::Indexed(identifier, open, expression, close)
                            }
                            _ => AssignmentDestination::Identifier(identifier),
                        }
                    };
                    let equals = p.parse_punctuation('=', true, &VoidHandler)?;
                    let expression = p.parse_expression(&VoidHandler)?;

                    Ok(SemicolonStatement::Assignment(Assignment {
                        destination,
                        equals,
                        expression,
                    }))
                }) {
                    Ok(assignment)
                } else {
                    self.parse_expression(handler)
                        .map(SemicolonStatement::Expression)
                }
            }
        }?;

        let semicolon = self.parse_punctuation(';', true, handler)?;

        Ok(Semicolon {
            statement,
            semicolon,
        })
    }

    /// Parses a [`VariableDeclaration`].
    #[tracing::instrument(level = "trace", skip_all)]
    pub fn parse_variable_declaration(
        &mut self,
        handler: &impl Handler<base::Error>,
    ) -> ParseResult<VariableDeclaration> {
        #[derive(Debug, Clone)]
        enum IndexingType {
            IntegerSize(Integer),
            None,
        }

        let variable_type = match self.stop_at_significant() {
            Reading::Atomic(Token::Keyword(keyword))
                if matches!(
                    keyword.keyword,
                    KeywordKind::Int | KeywordKind::Bool | KeywordKind::Val
                ) =>
            {
                self.forward();
                keyword
            }
            unexpected => {
                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                    expected: SyntaxKind::Either(&[
                        SyntaxKind::Keyword(KeywordKind::Int),
                        SyntaxKind::Keyword(KeywordKind::Bool),
                    ]),
                    found: unexpected.into_token(),
                });
                handler.receive(err.clone());
                return Err(err);
            }
        };

        let criteria_selection = if variable_type.keyword == KeywordKind::Int {
            self.try_parse(|p| {
                let open = p.parse_punctuation('<', true, &VoidHandler)?;
                let criteria = p.parse_string_literal(&VoidHandler)?;
                let close = p.parse_punctuation('>', true, &VoidHandler)?;
                Ok((open, criteria, close))
            })
            .ok()
        } else {
            None
        };

        // read identifier
        self.stop_at_significant();
        let identifier = self.parse_identifier(handler)?;

        match self.stop_at_significant() {
            Reading::IntoDelimited(punc)
                if punc.punctuation == '['
                    && matches!(variable_type.keyword, KeywordKind::Int | KeywordKind::Bool) =>
            {
                let tree = self.step_into(
                    Delimiter::Bracket,
                    |p| {
                        let res = match p.stop_at_significant() {
                            Reading::Atomic(Token::Integer(int)) => {
                                p.forward();
                                IndexingType::IntegerSize(int)
                            }

                            Reading::DelimitedEnd(punc) if punc.punctuation == ']' => {
                                IndexingType::None
                            }

                            unexpected => {
                                let err = Error::UnexpectedSyntax(UnexpectedSyntax {
                                    expected: SyntaxKind::Integer,
                                    found: unexpected.into_token(),
                                });
                                handler.receive(err.clone());
                                return Err(err);
                            }
                        };

                        Ok(res)
                    },
                    handler,
                )?;

                let open_bracket = tree.open;
                let close_bracket = tree.close;
                let inner = tree.tree?;

                match inner {
                    IndexingType::IntegerSize(size) => {
                        let assignment = self
                            .try_parse(|p| {
                                // read equals sign
                                let equals = p.parse_punctuation('=', true, &VoidHandler)?;

                                // read expression
                                let expression = p.parse_expression(&VoidHandler)?;

                                Ok(VariableDeclarationAssignment { equals, expression })
                            })
                            .ok();

                        Ok(VariableDeclaration::Array(ArrayVariableDeclaration {
                            variable_type,
                            identifier,
                            open_bracket,
                            size,
                            close_bracket,
                            assignment,
                            annotations: VecDeque::new(),
                        }))
                    }
                    IndexingType::None => match variable_type.keyword {
                        KeywordKind::Int => {
                            Ok(VariableDeclaration::Score(ScoreVariableDeclaration {
                                int_keyword: variable_type,
                                criteria: criteria_selection,
                                identifier,
                                open_bracket,
                                close_bracket,
                                assignment: None,
                                annotations: VecDeque::new(),
                            }))
                        }
                        KeywordKind::Bool => Ok(VariableDeclaration::Tag(TagVariableDeclaration {
                            bool_keyword: variable_type,
                            identifier,
                            open_bracket,
                            close_bracket,
                            target_assignment: None,
                            annotations: VecDeque::new(),
                        })),
                        _ => unreachable!(),
                    },
                }
            }
            // SingleVariableDeclaration with Assignment
            Reading::Atomic(Token::Punctuation(punc)) if punc.punctuation == '=' => {
                self.forward();
                let equals = punc;
                let expression = self.parse_expression(handler)?;
                let assignment = VariableDeclarationAssignment { equals, expression };

                if variable_type.keyword == KeywordKind::Val {
                    Ok(VariableDeclaration::ComptimeValue(
                        ComptimeValueDeclaration {
                            val_keyword: variable_type,
                            identifier,
                            assignment: Some(assignment),
                            annotations: VecDeque::new(),
                        },
                    ))
                } else {
                    Ok(VariableDeclaration::Single(SingleVariableDeclaration {
                        variable_type,
                        identifier,
                        assignment: Some(assignment),
                        annotations: VecDeque::new(),
                    }))
                }
            }
            // SingleVariableDeclaration without Assignment
            _ => Ok(VariableDeclaration::Single(SingleVariableDeclaration {
                variable_type,
                identifier,
                assignment: None,
                annotations: VecDeque::new(),
            })),
        }
    }
}
