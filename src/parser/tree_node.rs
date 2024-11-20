use crate::tokenizer::token::Number;

#[derive(Debug, Eq, PartialEq)]
pub enum Literal {
    Number(Number),
    String(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryExpression {
    Add(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
}

impl std::fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryExpression::Add(l, r) => write!(f, "({} + {})", l, r),
            BinaryExpression::Multiply(l, r) => write!(f, "({} * {})", l, r),
            BinaryExpression::Divide(l, r) => write!(f, "({} / {})", l, r),
            BinaryExpression::Subtract(l, r) => write!(f, "({} - {})", l, r),
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
pub enum Expression {
    Literal(Literal),
    Negation(Box<Expression>),
    BinaryExpression(BinaryExpression),
    LogicalExpression(LogicalExpression),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Negation(e) => write!(f, "(-{})", e),
            Expression::BinaryExpression(e) => write!(f, "{}", e),
            Expression::LogicalExpression(e) => write!(f, "{}", e),
        }
    }
}
