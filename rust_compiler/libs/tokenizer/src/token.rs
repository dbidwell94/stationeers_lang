use helpers::prelude::*;
use logos::{Lexer, Logos, Skip, Span};
use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use rust_decimal::Decimal;
use thiserror::Error;

#[derive(Debug, Error, Default, Clone, PartialEq)]
pub enum LexError {
    #[error("Attempted to parse an invalid number: {2}")]
    NumberParseError(usize, Span, String),

    #[error("An invalid character was found in token stream: {2}")]
    InvalidInput(usize, Span, String),

    #[default]
    #[error("An unknown error occurred")]
    Other,
}

impl From<LexError> for Diagnostic {
    fn from(value: LexError) -> Self {
        match value {
            LexError::NumberParseError(line, col, str) | LexError::InvalidInput(line, col, str) => {
                Diagnostic {
                    range: Range {
                        start: Position {
                            character: col.start as u32,
                            line: line as u32,
                        },
                        end: Position {
                            line: line as u32,
                            character: col.end as u32,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: str,
                    ..Default::default()
                }
            }
            _ => todo!(),
        }
    }
}

impl LexError {
    pub fn from_lexer<'a>(lex: &mut Lexer<'a, TokenType<'a>>) -> Self {
        let mut span = lex.span();
        let line = lex.extras.line_count;
        span.start -= lex.extras.line_start_index;
        span.end -= lex.extras.line_start_index;

        Self::InvalidInput(line, span, lex.slice().chars().as_str().to_string())
    }
}

// Define a local macro to consume the list
macro_rules! generate_check {
    ($($name:literal),*) => {
        pub fn is_syscall(s: &str) -> bool {
            matches!(s, $($name)|*)
        }
    }
}

#[derive(Default)]
pub struct Extras {
    pub line_count: usize,
    pub line_start_index: usize,
}

fn update_line_index<'a>(lex: &mut Lexer<'a, TokenType<'a>>) -> Skip {
    lex.extras.line_count += 1;
    lex.extras.line_start_index = lex.span().end;
    Skip
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token<'a> {
    /// The type of the token
    pub token_type: TokenType<'a>,
    /// The line where the token was found
    pub line: usize,
    /// The span where the token starts and ends
    pub span: Span,
}

impl<'a> std::fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType<'a>, line: usize, span: Span) -> Self {
        Self {
            token_type,
            line,
            span,
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

macro_rules! symbol {
    ($var:ident) => {
        |_| Symbol::$var
    };
}

macro_rules! keyword {
    ($var:ident) => {
        |_| Keyword::$var
    };
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Logos)]
#[logos(skip r"[ \t\f]+")]
#[logos(extras = Extras)]
#[logos(error(LexError, LexError::from_lexer))]
pub enum TokenType<'a> {
    #[regex(r"\n", update_line_index)]
    Newline,

    // matches strings with double quotes
    #[regex(r#""(?:[^"\\]|\\.)*""#)]
    // matches strings with single quotes
    #[regex(r#"'(?:[^'\\]|\\.)*'"#)]
    /// Represents a string token
    String(&'a str),

    #[regex(r"[0-9][0-9_]*(\.[0-9][0-9_]*)?([cfk])?", parse_number)]
    /// Represents a number token
    Number(Number),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    /// Represents a boolean token
    Boolean(bool),

    #[token("continue", keyword!(Continue))]
    #[token("const", keyword!(Const))]
    #[token("let", keyword!(Let))]
    #[token("fn", keyword!(Fn))]
    #[token("if", keyword!(If))]
    #[token("device", keyword!(Device))]
    #[token("else", keyword!(Else))]
    #[token("return", keyword!(Return))]
    #[token("enum", keyword!(Enum))]
    #[token("loop", keyword!(Loop))]
    #[token("break", keyword!(Break))]
    #[token("while", keyword!(While))]
    /// Represents a keyword token
    Keyword(Keyword),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    /// Represents an identifier token
    Identifier(&'a str),

    #[token("(", symbol!(LParen))]
    #[token(")", symbol!(RParen))]
    #[token("{", symbol!(LBrace))]
    #[token("}", symbol!(RBrace))]
    #[token("[", symbol!(LBracket))]
    #[token("]", symbol!(RBracket))]
    #[token(";", symbol!(Semicolon))]
    #[token(":", symbol!(Colon))]
    #[token(",", symbol!(Comma))]
    #[token("+", symbol!(Plus))]
    #[token("-", symbol!(Minus))]
    #[token("*", symbol!(Asterisk))]
    #[token("/", symbol!(Slash))]
    #[token("<", symbol!(LessThan))]
    #[token(">", symbol!(GreaterThan))]
    #[token("=", symbol!(Assign))]
    #[token("!", symbol!(LogicalNot))]
    #[token(".", symbol!(Dot))]
    #[token("^", symbol!(Caret))]
    #[token("%", symbol!(Percent))]
    #[token("==", symbol!(Equal))]
    #[token("!=", symbol!(NotEqual))]
    #[token("&&", symbol!(LogicalAnd))]
    #[token("||", symbol!(LogicalOr))]
    #[token("<=", symbol!(LessThanOrEqual))]
    #[token(">=", symbol!(GreaterThanOrEqual))]
    #[token("**", symbol!(Exp))]
    /// Represents a symbol token
    Symbol(Symbol),

    #[regex(r"///[\n]*", |val| Comment::Doc(val.slice()[3..].trim()))]
    #[regex(r"//[\n]*", |val| Comment::Line(val.slice()[2..].trim()))]
    /// Represents a comment, both a line comment and a doc comment
    Comment(Comment<'a>),

    #[end]
    /// Represents an end of file token
    EOF,
}

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub enum Comment<'a> {
    Line(&'a str),
    Doc(&'a str),
}

fn parse_number<'a>(lexer: &mut Lexer<'a, TokenType<'a>>) -> Result<Number, LexError> {
    let slice = lexer.slice();
    let last_char = slice.chars().last().unwrap_or_default();
    let (num_str, suffix) = match last_char {
        'c' | 'k' | 'f' => (&slice[..slice.len() - 1], Some(last_char)),
        _ => (slice, None),
    };

    let clean_str = if num_str.contains('_') {
        num_str.replace('_', "")
    } else {
        num_str.to_string()
    };

    let line = lexer.extras.line_count;
    let mut span = lexer.span();
    span.end -= lexer.extras.line_start_index;
    span.start -= lexer.extras.line_start_index;

    let num = if clean_str.contains('.') {
        Number::Decimal(
            clean_str
                .parse::<Decimal>()
                .map_err(|_| LexError::NumberParseError(line, span, slice.to_string()))?,
        )
    } else {
        Number::Integer(
            clean_str
                .parse::<i128>()
                .map_err(|_| LexError::NumberParseError(line, span, slice.to_string()))?,
        )
    };

    if let Some(suffix) = suffix {
        Ok(match suffix {
            'c' => Temperature::Celsius(num),
            'f' => Temperature::Fahrenheit(num),
            'k' => Temperature::Kelvin(num),
            _ => unreachable!(),
        }
        .to_kelvin())
    } else {
        Ok(num)
    }
}

impl<'a> std::fmt::Display for Comment<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Line(c) => write!(f, "// {}", c),
            Self::Doc(d) => {
                let lines = d
                    .split('\n')
                    .map(|s| format!("/// {s}"))
                    .collect::<Vec<_>>()
                    .join("\n");

                write!(f, "{}", lines)
            }
        }
    }
}

impl<'a> Documentation for TokenType<'a> {
    fn docs(&self) -> String {
        match self {
            Self::Keyword(k) => k.docs(),
            _ => "".into(),
        }
    }

    fn get_all_documentation() -> Vec<(&'static str, String)> {
        Keyword::get_all_documentation()
    }
}

helpers::with_syscalls!(generate_check);

impl<'a> From<TokenType<'a>> for u32 {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::String(_) => 1,
            TokenType::Number(_) => 2,
            TokenType::Boolean(_) => 3,
            TokenType::Keyword(k) => match k {
                Keyword::If
                | Keyword::Else
                | Keyword::Loop
                | Keyword::While
                | Keyword::Break
                | Keyword::Continue
                | Keyword::Return => 4,
                _ => 5,
            },
            TokenType::Comment(_) => 8,
            TokenType::Identifier(s) => {
                if is_syscall(&s) {
                    10
                } else {
                    6
                }
            }
            TokenType::Symbol(s) => {
                if s.is_comparison() {
                    11
                } else if s.is_operator() {
                    12
                } else if s.is_logical() {
                    13
                } else {
                    7
                }
            }
            _ => 0,
        }
    }
}

impl<'a> std::fmt::Display for TokenType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::String(s) => write!(f, "{}", s),
            TokenType::Number(n) => write!(f, "{}", n),
            TokenType::Boolean(b) => write!(f, "{}", b),
            TokenType::Keyword(k) => write!(f, "{:?}", k),
            TokenType::Identifier(i) => write!(f, "{}", i),
            TokenType::Symbol(s) => write!(f, "{}", s),
            TokenType::Comment(c) => write!(f, "{}", c),
            TokenType::EOF => write!(f, "EOF"),
            _ => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Number {
    /// Represents an integer number
    Integer(i128),
    /// Represents a decimal type number with a precision of 64 bits
    Decimal(Decimal),
}

impl From<Number> for Decimal {
    fn from(value: Number) -> Self {
        match value {
            Number::Decimal(d) => d,
            Number::Integer(i) => Decimal::from(i),
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Self::Integer(i) => Self::Integer(-i),
            Self::Decimal(d) => Self::Decimal(-d),
        }
    }
}

impl std::ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l), Self::Integer(r)) => Number::Integer(l + r),
            (Self::Decimal(l), Self::Decimal(r)) => Number::Decimal(l + r),
            (Self::Integer(l), Self::Decimal(r)) => Number::Decimal(Decimal::from(l) + r),
            (Self::Decimal(l), Self::Integer(r)) => Number::Decimal(l + Decimal::from(r)),
        }
    }
}

impl std::ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Integer(l), Self::Integer(r)) => Self::Integer(l - r),
            (Self::Decimal(l), Self::Integer(r)) => Self::Decimal(l - Decimal::from(r)),
            (Self::Integer(l), Self::Decimal(r)) => Self::Decimal(Decimal::from(l) - r),
            (Self::Decimal(l), Self::Decimal(r)) => Self::Decimal(l - r),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Integer(l), Number::Integer(r)) => Number::Integer(l * r),
            (Number::Integer(l), Number::Decimal(r)) => Number::Decimal(Decimal::from(l) * r),
            (Number::Decimal(l), Number::Integer(r)) => Number::Decimal(l * Decimal::from(r)),
            (Number::Decimal(l), Number::Decimal(r)) => Number::Decimal(l * r),
        }
    }
}

impl std::ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        Number::Decimal(Decimal::from(self) / Decimal::from(rhs))
    }
}

impl std::ops::Rem for Number {
    type Output = Number;

    fn rem(self, rhs: Self) -> Self::Output {
        Number::Decimal(Decimal::from(self) % Decimal::from(rhs))
    }
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
        /// Prepresents the `const` keyword. This allows you to define a variable that will never
        /// change throughout the lifetime of the program, similar to `define` in IC10. If you are
        /// not planning on mutating the variable (changing it), it is recommend you store it as a
        /// const, as the compiler will not assign it to a register or stack variable.
        ///
        /// ## Example
        /// ```
        /// const targetTemp = 20c;
        /// device gasSensor = "d0";
        /// device airCon = "d1";
        ///
        /// airCon.On = gasSensor.Temperature > targetTemp;
        /// ```
        Const,
        /// Represents the `let` keyword, used to declare variables within Slang.
        /// ## Example
        /// ```
        /// // This variable now exists either in a register or the stack depending on how many
        /// // free registers were available when declaring it.
        /// let item = 0;
        /// ```
        Let,
        /// Represents the `fn` keyword, used to declare functions within Slang.
        /// # WARNING
        /// Functions are currently unstable and are subject to change until stabilized. Use at
        /// your own risk! (They are also heavily not optimized and produce a LOT of code bloat)
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
