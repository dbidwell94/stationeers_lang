use super::sys_call::SysCall;
use crate::sys_call;
use helpers::Span;
use safer_ffi::prelude::*;
use std::{borrow::Cow, ops::Deref};
use tokenizer::token::Number;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Literal<'a> {
    Number(Number),
    String(Cow<'a, str>),
    Boolean(bool),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum LiteralOr<'a, T> {
    Literal(Spanned<Literal<'a>>),
    Or(Spanned<T>),
}

impl<'a, T: std::fmt::Display> std::fmt::Display for LiteralOr<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Or(o) => write!(f, "{o}"),
        }
    }
}

impl<'a> std::fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Boolean(b) => write!(f, "{}", if *b { 1 } else { 0 }),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryExpression<'a> {
    Add(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Multiply(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Divide(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Subtract(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Exponent(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Modulo(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
}

impl<'a> std::fmt::Display for BinaryExpression<'a> {
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
pub enum LogicalExpression<'a> {
    And(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Or(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Not(Box<Spanned<Expression<'a>>>),
    Equal(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    NotEqual(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    GreaterThan(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    GreaterThanOrEqual(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    LessThan(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    LessThanOrEqual(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
}

impl<'a> std::fmt::Display for LogicalExpression<'a> {
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
pub struct AssignmentExpression<'a> {
    pub assignee: Box<Spanned<Expression<'a>>>,
    pub expression: Box<Spanned<Expression<'a>>>,
}

impl<'a> std::fmt::Display for AssignmentExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} = {})", self.assignee, self.expression)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionExpression<'a> {
    pub name: Spanned<Cow<'a, str>>,
    pub arguments: Vec<Spanned<Cow<'a, str>>>,
    pub body: BlockExpression<'a>,
}

impl<'a> std::fmt::Display for FunctionExpression<'a> {
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
pub struct BlockExpression<'a>(pub Vec<Spanned<Expression<'a>>>);

impl<'a> std::fmt::Display for BlockExpression<'a> {
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
pub struct InvocationExpression<'a> {
    pub name: Spanned<Cow<'a, str>>,
    pub arguments: Vec<Spanned<Expression<'a>>>,
}

impl<'a> std::fmt::Display for InvocationExpression<'a> {
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
pub struct MemberAccessExpression<'a> {
    pub object: Box<Spanned<Expression<'a>>>,
    pub member: Spanned<Cow<'a, str>>,
}

impl<'a> std::fmt::Display for MemberAccessExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.object, self.member)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MethodCallExpression<'a> {
    pub object: Box<Spanned<Expression<'a>>>,
    pub method: Spanned<Cow<'a, str>>,
    pub arguments: Vec<Spanned<Expression<'a>>>,
}

impl<'a> std::fmt::Display for MethodCallExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}.{}({})",
            self.object,
            self.method,
            self.arguments
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LiteralOrVariable<'a> {
    Literal(Literal<'a>),
    Variable(Spanned<Cow<'a, str>>),
}

impl<'a> std::fmt::Display for LiteralOrVariable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralOrVariable::Literal(l) => write!(f, "{}", l),
            LiteralOrVariable::Variable(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConstDeclarationExpression<'a> {
    pub name: Spanned<Cow<'a, str>>,
    pub value: LiteralOr<'a, SysCall<'a>>,
}

impl<'a> ConstDeclarationExpression<'a> {
    pub fn is_syscall_supported(call: &SysCall) -> bool {
        use sys_call::System;
        matches!(call, SysCall::System(sys) if matches!(sys, System::Hash(_)))
    }
}

impl<'a> std::fmt::Display for ConstDeclarationExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(const {} = {})", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DeviceDeclarationExpression<'a> {
    /// any variable-like name
    pub name: Spanned<Cow<'a, str>>,
    /// The device port, ex. (db, d0, d1, d2, d3, d4, d5)
    pub device: Cow<'a, str>,
}

impl<'a> std::fmt::Display for DeviceDeclarationExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(device {} = {})", self.name, self.device)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfExpression<'a> {
    pub condition: Box<Spanned<Expression<'a>>>,
    pub body: Spanned<BlockExpression<'a>>,
    pub else_branch: Option<Box<Spanned<Expression<'a>>>>,
}

impl<'a> std::fmt::Display for IfExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(if ({}) {}", self.condition, self.body)?;
        if let Some(else_branch) = &self.else_branch {
            write!(f, " else {}", else_branch)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LoopExpression<'a> {
    pub body: Spanned<BlockExpression<'a>>,
}

impl<'a> std::fmt::Display for LoopExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(loop {})", self.body)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileExpression<'a> {
    pub condition: Box<Spanned<Expression<'a>>>,
    pub body: BlockExpression<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TernaryExpression<'a> {
    pub condition: Box<Spanned<Expression<'a>>>,
    pub true_value: Box<Spanned<Expression<'a>>>,
    pub false_value: Box<Spanned<Expression<'a>>>,
}

impl<'a> std::fmt::Display for TernaryExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} ? {} : {})",
            self.condition, self.true_value, self.false_value
        )
    }
}

impl<'a> std::fmt::Display for WhileExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(while {} {})", self.condition, self.body)
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
pub enum Expression<'a> {
    Assignment(Spanned<AssignmentExpression<'a>>),
    Binary(Spanned<BinaryExpression<'a>>),
    Block(Spanned<BlockExpression<'a>>),
    Break(Span),
    ConstDeclaration(Spanned<ConstDeclarationExpression<'a>>),
    Continue(Span),
    Declaration(Spanned<Cow<'a, str>>, Box<Spanned<Expression<'a>>>),
    DeviceDeclaration(Spanned<DeviceDeclarationExpression<'a>>),
    Function(Spanned<FunctionExpression<'a>>),
    If(Spanned<IfExpression<'a>>),
    Invocation(Spanned<InvocationExpression<'a>>),
    Literal(Spanned<Literal<'a>>),
    Logical(Spanned<LogicalExpression<'a>>),
    Loop(Spanned<LoopExpression<'a>>),
    MemberAccess(Spanned<MemberAccessExpression<'a>>),
    MethodCall(Spanned<MethodCallExpression<'a>>),
    Negation(Box<Spanned<Expression<'a>>>),
    Priority(Box<Spanned<Expression<'a>>>),
    Return(Option<Box<Spanned<Expression<'a>>>>),
    Syscall(Spanned<SysCall<'a>>),
    Ternary(Spanned<TernaryExpression<'a>>),
    Variable(Spanned<Cow<'a, str>>),
    While(Spanned<WhileExpression<'a>>),
}

impl<'a> std::fmt::Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Assignment(e) => write!(f, "{}", e),
            Expression::Binary(e) => write!(f, "{}", e),
            Expression::Block(e) => write!(f, "{}", e),
            Expression::Break(_) => write!(f, "break"),
            Expression::ConstDeclaration(e) => write!(f, "{}", e),
            Expression::Continue(_) => write!(f, "continue"),
            Expression::Declaration(id, e) => write!(f, "(let {} = {})", id, e),
            Expression::DeviceDeclaration(e) => write!(f, "{}", e),
            Expression::Function(e) => write!(f, "{}", e),
            Expression::If(e) => write!(f, "{}", e),
            Expression::Invocation(e) => write!(f, "{}", e),
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Logical(e) => write!(f, "{}", e),
            Expression::Loop(e) => write!(f, "{}", e),
            Expression::MemberAccess(e) => write!(f, "{}", e),
            Expression::MethodCall(e) => write!(f, "{}", e),
            Expression::Negation(e) => write!(f, "(-{})", e),
            Expression::Priority(e) => write!(f, "({})", e),
            Expression::Return(e) => write!(
                f,
                "(return {})",
                if let Some(e) = e {
                    e.to_string()
                } else {
                    "".to_string()
                }
            ),
            Expression::Syscall(e) => write!(f, "{}", e),
            Expression::Ternary(e) => write!(f, "{}", e),
            Expression::Variable(id) => write!(f, "{}", id),
            Expression::While(e) => write!(f, "{}", e),
        }
    }
}
