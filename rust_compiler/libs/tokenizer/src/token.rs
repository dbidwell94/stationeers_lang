use helpers::prelude::*;
use rust_decimal::Decimal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    /// The type of the token
    pub token_type: TokenType,
    /// The line where the token was found
    pub line: usize,
    /// The column where the token was found
    pub column: usize,
    pub original_string: Option<String>,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        line: usize,
        column: usize,
        original: Option<String>,
    ) -> Self {
        Self {
            token_type,
            line,
            column,
            original_string: original,
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
            Temperature::Kelvin(n) => write!(f, "{}°K", n),
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

impl Documentation for TokenType {
    fn docs(&self) -> String {
        match self {
            Self::Keyword(k) => k.docs(),
            _ => "".into(),
        }
    }
}

impl From<TokenType> for u32 {
    fn from(value: TokenType) -> Self {
        use TokenType::*;
        match value {
            String(_) => 1,
            Number(_) => 2,
            Boolean(_) => 3,
            Keyword(_) => 4,
            Identifier(_) => 5,
            Symbol(_) => 6,
            EOF => 0,
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Boolean(b) => write!(f, "{}", b),
            TokenType::Keyword(k) => write!(f, "{:?}", k),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Symbol(s) => write!(f, "{}", s),
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
            Number::Decimal(d) => write!(f, "{}", d),
        }
    }
}

impl std::convert::From<Number> for String {
    fn from(value: Number) -> Self {
        value.to_string()
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
    /// Represents the `%` symbol
    Percent,

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
        matches!(
            self,
            Symbol::Plus
                | Symbol::Minus
                | Symbol::Asterisk
                | Symbol::Slash
                | Symbol::Exp
                | Symbol::Percent
        )
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Symbol::LessThan
                | Symbol::GreaterThan
                | Symbol::Equal
                | Symbol::NotEqual
                | Symbol::LessThanOrEqual
                | Symbol::GreaterThanOrEqual,
        )
    }

    pub fn is_logical(&self) -> bool {
        matches!(self, Symbol::LogicalAnd | Symbol::LogicalOr)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Percent => write!(f, "%"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Comma => write!(f, ","),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::Assign => write!(f, "="),
            Self::Equal => write!(f, "=="),
            Self::LogicalAnd => write!(f, "&&"),
            Self::LogicalOr => write!(f, "||"),
            Self::LogicalNot => write!(f, "!"),
            Self::NotEqual => write!(f, "!="),
            Self::Dot => write!(f, "."),
            Self::Caret => write!(f, "^"),
            Self::Exp => write!(f, "**"),
        }
    }
}

documented! {
    #[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
    pub enum Keyword {
        /// Represents the `continue` keyword. This will allow you to bypass the current iteration in a loop and start the next one.
        /// ## Example
        /// ```
        /// let item = 0;
        /// loop {
        ///   if (item % 2 == 0) {
        ///     // This will NOT increment `item` and will continue with the next iteration of the
        ///     // loop
        ///     continue;
        ///   }
        ///   item = item + 1;
        /// }
        /// ```
        Continue,
        /// Represents the `let` keyword, used to declare variables within Slang.
        /// ## Example
        /// ```
        /// // This variable now exists either in a register or the stack depending on how many
        /// // free registers were available when declaring it.
        /// let item = 0;
        /// ```
        Let,
        /// Represents the `fn` keyword, used to declare functions within Slang.
        /// ## Example
        /// ```
        ///  // This allows you to now call `doSomething` with specific arguments.
        ///  fn doSomething(arg1, arg2) {
        ///
        ///  }
        /// ```
        Fn,
        /// Represents the `if` keyword, allowing you to create branched logic.
        /// ## Example
        /// ```
        /// let i = 0;
        /// if (i == 0) {
        ///   i = 1;
        /// }
        /// // At this line, `i` is now `1`
        /// ```
        If,
        /// Represents the `device` keyword. Useful for defining a device at a specific address
        /// (ex. d0, d1, d2, etc.). This also allows you to perform direct operations ON a device.
        /// ## Example
        /// ```
        /// device self = "db";
        ///
        /// // This is the same as `s db Setting 123`
        /// self.Setting = 123;
        /// ```
        Device,
        /// Represents the `else` keyword. Useful if you want to check a condition but run run
        /// seperate logic in case that condition fails.
        /// ## Example
        /// ```
        /// device self = "db";
        /// let i = 0;
        /// if (i < 0) {
        ///   self.Setting = 0;
        /// } else {
        ///   self.Setting = 1;
        /// }
        /// // Here, the `Setting` on the current housing is `1` because i was NOT less than 0
        /// ```
        Else,
        /// Represents the `return` keyword. Allows you to pass values from a function back to
        /// the caller.
        /// ## Example
        /// ```
        /// fn doSomething() {
        ///   return 1 + 2;
        /// }
        ///
        /// // `returnedValue` now holds the value `3`
        /// let returnedValue = doSomething();
        /// ```
        Return,
        /// Represents the `enum` keyword. This is currently not supported, but is kept as a
        /// reserved keyword in the future case that this is implemented.
        Enum,
        /// Represents the `loop` keyword. This allows you to create an infinate loop, but can be
        /// broken with the `break` keyword.
        /// ## Example
        /// ```
        /// device self = "db";
        /// let i = 0;
        /// loop {
        ///   i = i + 1;
        ///   // The current housing will infinately increment it's `Setting` value.
        ///   self.Setting = i;
        /// }
        /// ```
        Loop,
        /// Represents the `break` keyword. This allows you to "break out of" a loop prematurely,
        /// such as when an if() conditon is true, etc.
        /// ## Example
        /// ```
        /// let i = 0;
        /// // This loop will run until the value of `i` is greater than 10,000,
        /// // which will then trigger the `break` keyword and it will stop looping
        /// loop {
        ///  if (i > 10_000) {
        ///    break;
        ///  }
        ///  i = i + 1;
        /// }
        /// ```
        Break,
        /// Represents the `while` keyword. This is similar to the `loop` keyword but different in
        /// that you don't need an `if` statement to break out of a loop, that is handled
        /// automatically when invoking `while`
        /// ## Example
        /// ```
        /// let i = 0;
        /// // This loop will run until the value of `i` is greater than 10,000, in which case the
        /// // while loop will automatically stop running and code will continue AFTER the last
        /// // bracket.
        /// while (i < 10_000) {
        ///   i = i + 1;
        /// }
        /// ```
        While,
    }
}
