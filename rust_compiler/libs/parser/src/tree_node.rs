use std::ops::Deref;

use super::sys_call::SysCall;
use tokenizer::token::Number;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Literal {
    Number(Number),
    String(String),
    Boolean(bool),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", if *b { 1 } else { 0 }),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryExpression {
    Add(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Multiply(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Divide(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Subtract(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Exponent(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Modulo(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
}

impl std::fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryExpression::Add(l, r) => write!(f, "({} + {})", l, r),
            BinaryExpression::Multiply(l, r) => write!(f, "({} * {})", l, r),
            BinaryExpression::Divide(l, r) => write!(f, "({} / {})", l, r),
            BinaryExpression::Subtract(l, r) => write!(f, "({} - {})", l, r),
            BinaryExpression::Exponent(l, r) => write!(f, "({} ** {})", l, r),
            BinaryExpression::Modulo(l, r) => write!(f, "({} % {})", l, r),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LogicalExpression {
    And(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Or(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    Not(Box<Spanned<Expression>>),
    Equal(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    NotEqual(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    GreaterThan(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    GreaterThanOrEqual(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    LessThan(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
    LessThanOrEqual(Box<Spanned<Expression>>, Box<Spanned<Expression>>),
}

impl std::fmt::Display for LogicalExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalExpression::And(l, r) => write!(f, "({} && {})", l, r),
            LogicalExpression::Or(l, r) => write!(f, "({} || {})", l, r),
            LogicalExpression::Not(e) => write!(f, "(!{})", e),
            LogicalExpression::Equal(l, r) => write!(f, "({} == {})", l, r),
            LogicalExpression::NotEqual(l, r) => write!(f, "({} != {})", l, r),
            LogicalExpression::GreaterThan(l, r) => write!(f, "({} > {})", l, r),
            LogicalExpression::GreaterThanOrEqual(l, r) => write!(f, "({} >= {})", l, r),
            LogicalExpression::LessThan(l, r) => write!(f, "({} < {})", l, r),
            LogicalExpression::LessThanOrEqual(l, r) => write!(f, "({} <= {})", l, r),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignmentExpression {
    pub identifier: Spanned<String>,
    pub expression: Box<Spanned<Expression>>,
}

impl std::fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} = {})", self.identifier, self.expression)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionExpression {
    pub name: Spanned<String>,
    pub arguments: Vec<Spanned<String>>,
    pub body: BlockExpression,
}

impl std::fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn {}({}) {{ {} }})",
            self.name,
            self.arguments
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockExpression(pub Vec<Spanned<Expression>>);

impl std::fmt::Display for BlockExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {}; }}",
            self.0
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join("; ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvocationExpression {
    pub name: Spanned<String>,
    pub arguments: Vec<Spanned<Expression>>,
}

impl std::fmt::Display for InvocationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.arguments
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LiteralOrVariable {
    Literal(Literal),
    Variable(Spanned<String>),
}

impl std::fmt::Display for LiteralOrVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralOrVariable::Literal(l) => write!(f, "{}", l),
            LiteralOrVariable::Variable(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DeviceDeclarationExpression {
    /// any variable-like name
    pub name: Spanned<String>,
    /// The device port, ex. (db, d0, d1, d2, d3, d4, d5)
    pub device: String,
}

impl std::fmt::Display for DeviceDeclarationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(device {} = {})", self.name, self.device)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfExpression {
    pub condition: Box<Spanned<Expression>>,
    pub body: Spanned<BlockExpression>,
    pub else_branch: Option<Box<Spanned<Expression>>>,
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(if ({}) {}", self.condition, self.body)?;
        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LoopExpression {
    pub body: Spanned<BlockExpression>,
}

impl std::fmt::Display for LoopExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(loop {})", self.body)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileExpression {
    pub condition: Box<Spanned<Expression>>,
    pub body: BlockExpression,
}

impl std::fmt::Display for WhileExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(while {} {})", self.condition, self.body)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start_line: usize,
    pub end_line: usize,
    pub start_col: usize,
    pub end_col: usize,
}

impl From<Span> for lsp_types::Range {
    fn from(value: Span) -> Self {
        Self {
            start: lsp_types::Position {
                line: value.start_line as u32,
                character: value.start_col as u32,
            },
            end: lsp_types::Position {
                line: value.end_line as u32,
                character: value.end_col as u32,
            },
        }
    }
}

impl From<&Span> for lsp_types::Range {
    fn from(value: &Span) -> Self {
        Self {
            start: lsp_types::Position {
                line: value.start_line as u32,
                character: value.start_col as u32,
            },
            end: lsp_types::Position {
                line: value.end_line as u32,
                character: value.end_col as u32,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> std::fmt::Display for Spanned<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Assignment(Spanned<AssignmentExpression>),
    Binary(Spanned<BinaryExpression>),
    Block(Spanned<BlockExpression>),
    Break(Span),
    Continue(Span),
    Declaration(Spanned<String>, Box<Spanned<Expression>>),
    DeviceDeclaration(Spanned<DeviceDeclarationExpression>),
    Function(Spanned<FunctionExpression>),
    If(Spanned<IfExpression>),
    Invocation(Spanned<InvocationExpression>),
    Literal(Spanned<Literal>),
    Logical(Spanned<LogicalExpression>),
    Loop(Spanned<LoopExpression>),
    Negation(Box<Spanned<Expression>>),
    Priority(Box<Spanned<Expression>>),
    Return(Box<Spanned<Expression>>),
    Syscall(Spanned<SysCall>),
    Variable(Spanned<String>),
    While(Spanned<WhileExpression>),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Assignment(e) => write!(f, "{}", e),
            Expression::Binary(e) => write!(f, "{}", e),
            Expression::Block(e) => write!(f, "{}", e),
            Expression::Break(_) => write!(f, "break"),
            Expression::Continue(_) => write!(f, "continue"),
            Expression::Declaration(id, e) => write!(f, "(let {} = {})", id, e),
            Expression::DeviceDeclaration(e) => write!(f, "{}", e),
            Expression::Function(e) => write!(f, "{}", e),
            Expression::If(e) => write!(f, "{}", e),
            Expression::Invocation(e) => write!(f, "{}", e),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Logical(e) => write!(f, "{}", e),
            Expression::Loop(e) => write!(f, "{}", e),
            Expression::Negation(e) => write!(f, "(-{})", e),
            Expression::Priority(e) => write!(f, "({})", e),
            Expression::Return(e) => write!(f, "(return {})", e),
            Expression::Syscall(e) => write!(f, "{}", e),
            Expression::Variable(id) => write!(f, "{}", id),
            Expression::While(e) => write!(f, "{}", e),
        }
    }
}
