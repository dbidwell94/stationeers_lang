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
    Add(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Exponent(Box<Expression>, Box<Expression>),
    Modulo(Box<Expression>, Box<Expression>),
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
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    NotEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    GreaterThanOrEqual(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    LessThanOrEqual(Box<Expression>, Box<Expression>),
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
    pub identifier: String,
    pub expression: Box<Expression>,
}

impl std::fmt::Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} = {})", self.identifier, self.expression)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionExpression {
    pub name: String,
    pub arguments: Vec<String>,
    pub body: BlockExpression,
}

impl std::fmt::Display for FunctionExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn {}({}) {{ {} }})",
            self.name,
            self.arguments.to_vec().join(", "),
            self.body
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockExpression(pub Vec<Expression>);

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
    pub name: String,
    pub arguments: Vec<Expression>,
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
    Variable(String),
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
    pub name: String,
    /// The device port, ex. (db, d0, d1, d2, d3, d4, d5)
    pub device: String,
}

impl std::fmt::Display for DeviceDeclarationExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(device {} = {})", self.name, self.device)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Assignment(AssignmentExpression),
    Binary(BinaryExpression),
    Block(BlockExpression),
    Declaration(String, Box<Expression>),
    Function(FunctionExpression),
    Invocation(InvocationExpression),
    Literal(Literal),
    Logical(LogicalExpression),
    Negation(Box<Expression>),
    Priority(Box<Expression>),
    Return(Box<Expression>),
    Variable(String),
    DeviceDeclaration(DeviceDeclarationExpression),
    Syscall(SysCall),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Negation(e) => write!(f, "(-{})", e),
            Expression::Binary(e) => write!(f, "{}", e),
            Expression::Logical(e) => write!(f, "{}", e),
            Expression::Assignment(e) => write!(f, "{}", e),
            Expression::Declaration(id, e) => write!(f, "(let {} = {})", id, e),
            Expression::Function(e) => write!(f, "{}", e),
            Expression::Block(e) => write!(f, "{}", e),
            Expression::Invocation(e) => write!(f, "{}", e),
            Expression::Variable(id) => write!(f, "{}", id),
            Expression::Priority(e) => write!(f, "({})", e),
            Expression::Return(e) => write!(f, "(return {})", e),
            Expression::DeviceDeclaration(e) => write!(f, "{}", e),
            Expression::Syscall(e) => write!(f, "{}", e),
        }
    }
}
