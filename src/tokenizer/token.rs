#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq, Hash, Eq)]
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

#[derive(Debug, PartialEq, Hash, Eq)]
pub enum Number {
    /// Represents an integer number
    Integer(u64),
    /// Represents a decimal type number with a precision of 64 bits
    Decimal(u64, u64),
}

#[derive(Debug, PartialEq, Hash, Eq)]
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
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub enum Keyword {
    /// Represents the `let` keyword
    Let,
    /// Represents the `fn` keyword
    Fn,
    /// Represents the `if` keyword
    If,
    /// Represents the `else` keyword
    Else,
    /// Represents the `return` keyword
    Return,
    /// Represents the `enum` keyword
    Enum,
    /// Represents an import keyword
    Import,
    /// Represents an export keyword
    Export,
}
