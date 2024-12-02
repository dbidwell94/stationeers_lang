use rust_decimal::prelude::*;
use rust_decimal::Decimal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    /// The type of the token
    pub token_type: TokenType,
    /// The line where the token was found
    pub line: usize,
    /// The column where the token was found
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, column: usize) -> Self {
        Self {
            token_type,
            line,
            column,
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum Temperature {
    Celsius(Number),
    Fahrenheit(Number),
    Kelvin(Number),
}

impl std::fmt::Display for Temperature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Temperature::Celsius(n) => write!(f, "{}°C", n),
            Temperature::Fahrenheit(n) => write!(f, "{}°F", n),
            Temperature::Kelvin(n) => write!(f, "{}K", n),
        }
    }
}

impl Temperature {
    pub fn to_kelvin(self) -> Number {
        match self {
            Temperature::Celsius(n) => {
                let n = match n {
                    Number::Integer(i) => Decimal::new(i as i64, 0),
                    Number::Decimal(d) => d,
                };
                Number::Decimal(n + Decimal::new(27315, 2))
            }
            Temperature::Fahrenheit(n) => {
                let n = match n {
                    Number::Integer(i) => Decimal::new(i as i64, 0),
                    Number::Decimal(d) => d,
                };

                let a = n - Decimal::new(32, 0);
                let b = Decimal::new(5, 0) / Decimal::new(9, 0);
                Number::Decimal(a * b + Decimal::new(27315, 2))
            }
            Temperature::Kelvin(n) => n,
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum TokenType {
    /// Represents a string token
    String(String),
    /// Represents a number token
    Number(Number),
    /// Represents a boolean token
    Boolean(bool),
    /// Represents a keyword token
    Keyword(Keyword),
    /// Represents an identifier token
    Identifier(String),
    /// Represents a symbol token
    Symbol(Symbol),
    /// Represents an end of file token
    EOF,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Boolean(b) => write!(f, "{}", b),
            TokenType::Keyword(k) => write!(f, "{:?}", k),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Symbol(s) => write!(f, "{:?}", s),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Number {
    /// Represents an integer number
    Integer(u128),
    /// Represents a decimal type number with a precision of 64 bits
    Decimal(Decimal),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Decimal(d) => write!(f, "{}", d.to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Symbol {
    // Single Character Symbols
    /// Represents the `(` symbol
    LParen,
    /// Represents the `)` symbol
    RParen,
    /// Represents the `{` symbol
    LBrace,
    /// Represents the `}` symbol
    RBrace,
    /// Represents the `[` symbol
    LBracket,
    /// Represents the `]` symbol
    RBracket,
    /// Represents the `;` symbol
    Semicolon,
    /// Represents the `:` symbol
    Colon,
    /// Represents the `,` symbol
    Comma,
    /// Represents the `+` symbol
    Plus,
    /// Represents the `-` symbol
    Minus,
    /// Represents the `*` symbol
    Asterisk,
    /// Represents the `/` symbol
    Slash,
    /// Represents the `<` symbol
    LessThan,
    /// Represents the `>` symbol
    GreaterThan,
    /// Represents the `=` symbol
    Assign,
    /// Represents the `!` symbol
    LogicalNot,
    /// Represents the `.` symbol
    Dot,
    /// Represents the `^` symbol
    Caret,

    // Double Character Symbols
    /// Represents the `==` symbol
    Equal,
    /// Represents the `!=` symbol
    NotEqual,
    /// Represents the `&&` Symbol
    LogicalAnd,
    // Represents the `||` Symbol
    LogicalOr,
    /// Represents the `<=` symbol
    LessThanOrEqual,
    /// Represents the `>=` symbol
    GreaterThanOrEqual,
    /// Represents the `**` symbol
    Exp,
}

impl Symbol {
    pub fn is_operator(&self) -> bool {
        match self {
            Symbol::Plus | Symbol::Minus | Symbol::Asterisk | Symbol::Slash | Symbol::Exp => true,
            _ => false,
        }
    }

    pub fn is_comparison(&self) -> bool {
        match self {
            Symbol::LessThan
            | Symbol::GreaterThan
            | Symbol::Equal
            | Symbol::NotEqual
            | Symbol::LessThanOrEqual
            | Symbol::GreaterThanOrEqual => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            Symbol::LogicalAnd | Symbol::LogicalOr => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Keyword {
    /// Represents the `let` keyword
    Let,
    /// Represents the `fn` keyword
    Fn,
    /// Represents the `if` keyword
    If,
    /// Represents the `device` keyword. Useful for defining a device at a specific address (ex. d0, d1, d2, etc.)
    Device,
    /// Represents the `else` keyword
    Else,
    /// Represents the `return` keyword
    Return,
    /// Represents the `enum` keyword
    Enum,
    /// Represents the `loop` keyword
    Loop,
    /// Represents the `break` keyword
    Break,
}
