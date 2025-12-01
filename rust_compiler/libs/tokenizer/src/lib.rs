pub mod token;

use quick_error::quick_error;
use rust_decimal::Decimal;
use std::{
    cmp::Ordering,
    collections::VecDeque,
    io::{BufReader, Cursor, Read, Seek, SeekFrom},
    path::PathBuf,
};
use token::{Keyword, Number, Symbol, Temperature, Token, TokenType};

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        IOError(err: std::io::Error) {
            from()
            display("IO Error: {}", err)
            source(err)
        }
        NumberParseError(err: std::num::ParseIntError, line: usize, column: usize, original: String) {
            display("Number Parse Error: {}\nLine: {}, Column: {}", err, line, column)
            source(err)
        }
        DecimalParseError(err: rust_decimal::Error, line: usize, column: usize, original: String) {
            display("Decimal Parse Error: {}\nLine: {}, Column: {}", err, line, column)
            source(err)
        }
        UnknownSymbolError(char: char, line: usize, column: usize, original: String) {
            display("Unknown Symbol: {}\nLine: {}, Column: {}", char, line, column)
        }
        UnknownKeywordOrIdentifierError(val: String, line: usize, column: usize, original: String) {
            display("Unknown Keyword or Identifier: {}\nLine: {}, Column: {}", val, line, column)
        }
    }
}

impl From<Error> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;

        match value {
            IOError(e) => Diagnostic {
                message: e.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            NumberParseError(_, l, c, ref og)
            | DecimalParseError(_, l, c, ref og)
            | UnknownSymbolError(_, l, c, ref og)
            | UnknownKeywordOrIdentifierError(_, l, c, ref og) => Diagnostic {
                range: Range {
                    start: Position {
                        line: l as u32,
                        character: c as u32,
                    },
                    end: Position {
                        line: l as u32,
                        character: (c + og.len()) as u32,
                    },
                },
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
        }
    }
}

pub trait Tokenize: Read + Seek {}

impl<T> Tokenize for T where T: Read + Seek {}

pub struct Tokenizer<'a> {
    reader: BufReader<Box<dyn Tokenize + 'a>>,
    char_buffer: [u8; 1],
    line: usize,
    column: usize,
    returned_eof: bool,
    string_buffer: String,
}

impl<'a> Tokenizer<'a> {
    pub fn from_path(input_file: impl Into<PathBuf>) -> Result<Self, Error> {
        let file = std::fs::File::open(input_file.into())?;
        let reader = BufReader::new(Box::new(file) as Box<dyn Tokenize>);

        Ok(Self {
            reader,
            line: 1,
            column: 0, // Start at 0 so first char becomes 1
            char_buffer: [0],
            returned_eof: false,
            string_buffer: String::new(),
        })
    }
}

impl<'a> From<String> for Tokenizer<'a> {
    fn from(input: String) -> Self {
        let reader = BufReader::new(Box::new(Cursor::new(input)) as Box<dyn Tokenize>);

        Self {
            reader,
            line: 1,
            column: 0,
            char_buffer: [0],
            returned_eof: false,
            string_buffer: String::new(),
        }
    }
}

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            reader: BufReader::new(Box::new(Cursor::new(value)) as Box<dyn Tokenize>),
            char_buffer: [0],
            column: 0,
            line: 1,
            returned_eof: false,
            string_buffer: String::new(),
        }
    }
}

impl<'a> Tokenizer<'a> {
    fn next_char(&mut self) -> Result<Option<char>, Error> {
        let bytes_read = self.reader.read(&mut self.char_buffer)?;

        if bytes_read == 0 {
            return Ok(None);
        }

        let c = self.char_buffer[0] as char;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.string_buffer.push(c);
        Ok(Some(c))
    }

    fn peek_next_char(&mut self) -> Result<Option<char>, Error> {
        let current_pos = self.reader.stream_position()?;
        let to_return = if self.reader.read(&mut self.char_buffer)? == 0 {
            None
        } else {
            self.reader.seek(SeekFrom::Start(current_pos))?;
            Some(self.char_buffer[0] as char)
        };
        Ok(to_return)
    }

    fn skip_line(&mut self) -> Result<(), Error> {
        while let Some(next_char) = self.next_char()? {
            if next_char == '\n' {
                break;
            }
        }
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        self.string_buffer.clear();

        while let Some(next_char) = self.next_char()? {
            if next_char.is_whitespace() {
                self.string_buffer.clear();
                continue;
            }
            if next_char == '/' && self.peek_next_char()? == Some('/') {
                self.skip_line()?;
                self.string_buffer.clear();
                continue;
            }

            // Capture start position before delegating
            let start_line = self.line;
            let start_col = self.column;

            match next_char {
                '0'..='9' => {
                    return self
                        .tokenize_number(next_char, start_line, start_col)
                        .map(Some);
                }
                '"' | '\'' => {
                    return self
                        .tokenize_string(next_char, start_line, start_col)
                        .map(Some);
                }
                char if !char.is_alphanumeric() && char != '"' && char != '\'' => {
                    return self
                        .tokenize_symbol(next_char, start_line, start_col)
                        .map(Some);
                }
                char if char.is_alphabetic() => {
                    return self
                        .tokenize_keyword_or_identifier(next_char, start_line, start_col)
                        .map(Some);
                }
                _ => {
                    return Err(Error::UnknownSymbolError(
                        next_char,
                        start_line,
                        start_col,
                        std::mem::take(&mut self.string_buffer),
                    ));
                }
            }
        }
        if self.returned_eof {
            Ok(None)
        } else {
            self.returned_eof = true;
            Ok(Some(Token::new(
                TokenType::EOF,
                self.line,
                self.column,
                Some(std::mem::take(&mut self.string_buffer)),
            )))
        }
    }

    pub fn peek_next(&mut self) -> Result<Option<Token>, Error> {
        let current_pos = self.reader.stream_position()?;
        let column = self.column;
        let line = self.line;
        let token = self.next_token()?;
        self.reader.seek(SeekFrom::Start(current_pos))?;
        self.column = column;
        self.line = line;
        Ok(token)
    }

    // Updated helper functions to accept start_line and start_col

    fn tokenize_symbol(
        &mut self,
        first_symbol: char,
        line: usize,
        col: usize,
    ) -> Result<Token, Error> {
        macro_rules! symbol {
            ($symbol:ident) => {
                Ok(Token::new(
                    TokenType::Symbol(Symbol::$symbol),
                    line,
                    col,
                    Some(std::mem::take(&mut self.string_buffer)),
                ))
            };
        }

        match first_symbol {
            '(' => symbol!(LParen),
            ')' => symbol!(RParen),
            '{' => symbol!(LBrace),
            '}' => symbol!(RBrace),
            '[' => symbol!(LBracket),
            ']' => symbol!(RBracket),
            ';' => symbol!(Semicolon),
            ':' => symbol!(Colon),
            ',' => symbol!(Comma),
            '+' => symbol!(Plus),
            '-' => symbol!(Minus),
            '/' => symbol!(Slash),
            '.' => symbol!(Dot),
            '^' => symbol!(Caret),
            '%' => symbol!(Percent),
            '<' if self.peek_next_char()? == Some('=') => {
                self.next_char()?;
                symbol!(LessThanOrEqual)
            }
            '<' => symbol!(LessThan),
            '>' if self.peek_next_char()? == Some('=') => {
                self.next_char()?;
                symbol!(GreaterThanOrEqual)
            }
            '>' => symbol!(GreaterThan),
            '=' if self.peek_next_char()? == Some('=') => {
                self.next_char()?;
                symbol!(Equal)
            }
            '=' => symbol!(Assign),
            '!' if self.peek_next_char()? == Some('=') => {
                self.next_char()?;
                symbol!(NotEqual)
            }
            '!' => symbol!(LogicalNot),
            '*' if self.peek_next_char()? == Some('*') => {
                self.next_char()?;
                symbol!(Exp)
            }
            '*' => symbol!(Asterisk),
            '&' if self.peek_next_char()? == Some('&') => {
                self.next_char()?;
                symbol!(LogicalAnd)
            }
            '|' if self.peek_next_char()? == Some('|') => {
                self.next_char()?;
                symbol!(LogicalOr)
            }
            _ => Err(Error::UnknownSymbolError(
                first_symbol,
                line,
                col,
                std::mem::take(&mut self.string_buffer),
            )),
        }
    }

    fn tokenize_number(
        &mut self,
        first_char: char,
        line: usize,
        col: usize,
    ) -> Result<Token, Error> {
        let mut primary = String::with_capacity(16);
        let mut decimal: Option<String> = None;
        let mut reading_decimal = false;
        primary.push(first_char);

        while let Some(next_char) = self.peek_next_char()? {
            if next_char.is_whitespace() {
                break;
            }
            if next_char == '.' {
                reading_decimal = true;
                self.next_char()?;
                continue;
            }
            if next_char == '_' {
                self.next_char()?;
                continue;
            }
            if !next_char.is_numeric() {
                break;
            }

            if reading_decimal {
                decimal.get_or_insert_with(String::new).push(next_char);
            } else {
                primary.push(next_char);
            }
            self.next_char()?;
        }

        let number: Number = if let Some(decimal) = decimal {
            let decimal_scale = decimal.len() as u32;
            let number_str = format!("{}{}", primary, decimal);
            let number = number_str.parse::<i128>().map_err(|e| {
                Error::NumberParseError(e, line, col, std::mem::take(&mut self.string_buffer))
            })?;
            Number::Decimal(
                Decimal::try_from_i128_with_scale(number, decimal_scale).map_err(|e| {
                    Error::DecimalParseError(e, line, col, std::mem::take(&mut self.string_buffer))
                })?,
            )
        } else {
            Number::Integer(primary.parse().map_err(|e| {
                Error::NumberParseError(e, line, col, std::mem::take(&mut self.string_buffer))
            })?)
        };

        if let Some(next_char) = self.peek_next_char()? {
            let temperature = match next_char {
                'c' => Temperature::Celsius(number),
                'f' => Temperature::Fahrenheit(number),
                'k' => Temperature::Kelvin(number),
                _ => {
                    return Ok(Token::new(
                        TokenType::Number(number),
                        line,
                        col,
                        Some(std::mem::take(&mut self.string_buffer)),
                    ));
                }
            }
            .to_kelvin();

            self.next_char()?;
            Ok(Token::new(
                TokenType::Number(temperature),
                line,
                col,
                Some(std::mem::take(&mut self.string_buffer)),
            ))
        } else {
            Ok(Token::new(
                TokenType::Number(number),
                line,
                col,
                Some(std::mem::take(&mut self.string_buffer)),
            ))
        }
    }

    fn tokenize_string(
        &mut self,
        beginning_quote: char,
        line: usize,
        col: usize,
    ) -> Result<Token, Error> {
        let mut buffer = String::with_capacity(16);
        while let Some(next_char) = self.next_char()? {
            if next_char == beginning_quote {
                break;
            }
            buffer.push(next_char);
        }
        Ok(Token::new(
            TokenType::String(buffer),
            line,
            col,
            Some(std::mem::take(&mut self.string_buffer)),
        ))
    }

    fn tokenize_keyword_or_identifier(
        &mut self,
        first_char: char,
        line: usize,
        col: usize,
    ) -> Result<Token, Error> {
        macro_rules! keyword {
            ($keyword:ident) => {{
                return Ok(Token::new(
                    TokenType::Keyword(Keyword::$keyword),
                    line,
                    col,
                    Some(std::mem::take(&mut self.string_buffer)),
                ));
            }};
        }
        macro_rules! next_ws {
            () => { matches!(self.peek_next_char()?, Some(x) if x.is_whitespace() || !x.is_alphanumeric()) || self.peek_next_char()?.is_none() };
        }

        let mut buffer = String::with_capacity(16);
        let mut looped_char = Some(first_char);

        while let Some(next_char) = looped_char {
            if next_char.is_whitespace() || !next_char.is_alphanumeric() {
                break;
            }
            buffer.push(next_char);

            match buffer.as_str() {
                "let" if next_ws!() => keyword!(Let),
                "fn" if next_ws!() => keyword!(Fn),
                "if" if next_ws!() => keyword!(If),
                "else" if next_ws!() => keyword!(Else),
                "return" if next_ws!() => keyword!(Return),
                "enum" if next_ws!() => keyword!(Enum),
                "device" if next_ws!() => keyword!(Device),
                "loop" if next_ws!() => keyword!(Loop),
                "break" if next_ws!() => keyword!(Break),
                "while" if next_ws!() => keyword!(While),
                "continue" if next_ws!() => keyword!(Continue),
                "true" if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Boolean(true),
                        line,
                        col,
                        Some(std::mem::take(&mut self.string_buffer)),
                    ));
                }
                "false" if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Boolean(false),
                        line,
                        col,
                        Some(std::mem::take(&mut self.string_buffer)),
                    ));
                }
                val if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Identifier(val.to_string()),
                        line,
                        col,
                        Some(std::mem::take(&mut self.string_buffer)),
                    ));
                }
                _ => {}
            }
            looped_char = self.next_char()?;
        }
        Err(Error::UnknownKeywordOrIdentifierError(
            buffer,
            line,
            col,
            std::mem::take(&mut self.string_buffer),
        ))
    }
}

// ... Iterator and TokenizerBuffer implementations remain unchanged ...
// They just call the methods above which now use the passed-in start coordinates.
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Some(tok)) => Some(Ok(tok)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub struct TokenizerBuffer<'a> {
    tokenizer: Tokenizer<'a>,
    buffer: VecDeque<Token>,
    history: VecDeque<Token>,
}

impl<'a> TokenizerBuffer<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            buffer: VecDeque::new(),
            history: VecDeque::with_capacity(128),
        }
    }
    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.buffer.pop_front() {
            self.history.push_back(token.clone());
            return Ok(Some(token));
        }
        let token = self.tokenizer.next_token()?;
        if let Some(ref token) = token {
            self.history.push_back(token.clone());
        }
        Ok(token)
    }
    pub fn peek(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.buffer.front() {
            return Ok(Some(token.clone()));
        }
        let token = self.tokenizer.peek_next()?;
        Ok(token)
    }
    fn seek_from_current(&mut self, seek_to: i64) -> Result<(), Error> {
        use Ordering::*;
        match seek_to.cmp(&0) {
            Greater => {
                let mut tokens = Vec::with_capacity(seek_to as usize);
                for _ in 0..seek_to {
                    if let Some(token) = self.tokenizer.next_token()? {
                        tokens.push(token);
                    } else {
                        return Err(Error::IOError(std::io::Error::new(
                            std::io::ErrorKind::UnexpectedEof,
                            "Unexpected EOF",
                        )));
                    }
                }
                self.history.extend(tokens);
            }
            Less => {
                let seek_to = seek_to.unsigned_abs() as usize;
                let mut tokens = Vec::with_capacity(seek_to);
                for _ in 0..seek_to {
                    if let Some(token) = self.history.pop_back() {
                        tokens.push(token);
                    } else {
                        return Err(Error::IOError(std::io::Error::new(
                            std::io::ErrorKind::UnexpectedEof,
                            "Unexpected EOF",
                        )));
                    }
                }
                self.buffer.extend(tokens.into_iter().rev());
            }
            _ => {}
        }
        Ok(())
    }
    pub fn seek(&mut self, from: SeekFrom) -> Result<(), Error> {
        match from {
            SeekFrom::Current(seek_to) => self.seek_from_current(seek_to)?,
            _ => unimplemented!("SeekFrom::End/Start not implemented"),
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use rust_decimal::Decimal;

    const TEST_FILE: &str = "tests/file.stlg";

    const TEST_STRING: &str = r#"
        fn test() {
            let x = 10;
            return x + 2;
        }
    "#;

    #[test]
    fn test_seek_from_current() -> Result<()> {
        let tokenizer = Tokenizer::from(TEST_STRING.to_owned());
        let mut buffer = TokenizerBuffer::new(tokenizer);

        let token = buffer.next_token()?.unwrap();
        assert_eq!(token.token_type, TokenType::Keyword(Keyword::Fn));

        buffer.seek(SeekFrom::Current(1))?;

        let token = buffer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Symbol(Symbol::LParen));

        Ok(())
    }

    #[test]
    fn test_tokenizer_from_path_ok() {
        let tokenizer = Tokenizer::from_path(TEST_FILE);
        assert!(tokenizer.is_ok());
    }

    #[test]
    fn test_tokenizer_from_path_err() {
        let tokenizer = Tokenizer::from_path("non_existent_file.stlg");
        assert!(tokenizer.is_err());
    }

    #[test]
    fn test_next_char() -> Result<()> {
        let mut tokenizer = Tokenizer::from(TEST_STRING.to_owned());

        let char = tokenizer.next_char()?;

        assert_eq!(char, Some('\n'));
        assert_eq!(tokenizer.line, 2);
        assert_eq!(tokenizer.column, 1);

        let mut tokenizer = Tokenizer::from(String::from("fn"));

        let char = tokenizer.next_char()?;

        assert_eq!(char, Some('f'));
        assert_eq!(tokenizer.line, 1);
        assert_eq!(tokenizer.column, 1);

        Ok(())
    }

    #[test]
    fn test_peek_next_char() -> Result<()> {
        let mut tokenizer = Tokenizer::from(TEST_STRING.to_owned());

        let char = tokenizer.peek_next_char()?;

        assert_eq!(char, Some('\n'));
        assert_eq!(tokenizer.line, 1);
        assert_eq!(tokenizer.column, 0);

        let char = tokenizer.next_char()?;
        assert_eq!(char, Some('\n'));
        assert_eq!(tokenizer.line, 2);
        assert_eq!(tokenizer.column, 1);

        let char = tokenizer.peek_next_char()?;
        assert_eq!(char, Some(' '));
        assert_eq!(tokenizer.line, 2);
        assert_eq!(tokenizer.column, 1);

        Ok(())
    }

    #[test]
    fn test_temperature_unit() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("10c 14f 10k"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::Number(Number::Decimal(Decimal::new(28315, 2)))
        );

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::Number(Number::Decimal(Decimal::new(26315, 2)))
        );

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Number(Number::Integer(10)));

        Ok(())
    }

    #[test]
    fn test_parse_integer() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("10"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Number(Number::Integer(10)));

        Ok(())
    }

    #[test]
    fn test_parse_integer_with_underscore() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("1_000"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Number(Number::Integer(1000)));

        Ok(())
    }

    #[test]
    fn test_parse_decimal() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("10.5"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::Number(Number::Decimal(Decimal::new(105, 1))) // 10.5
        );

        Ok(())
    }

    #[test]
    fn test_parse_decimal_with_underscore() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("1_000.000_6"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::Number(Number::Decimal(Decimal::new(10000006, 4))) // 1000.0006
        );

        Ok(())
    }

    #[test]
    fn test_parse_number_with_symbol() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("10;"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Number(Number::Integer(10)));

        let next_char = tokenizer.next_char()?;

        assert_eq!(next_char, Some(';'));

        Ok(())
    }

    #[test]
    fn test_string_parse() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from(r#""Hello, World!""#));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::String(String::from("Hello, World!"))
        );

        let mut tokenizer = Tokenizer::from(String::from(r#"'Hello, World!'"#));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(
            token.token_type,
            TokenType::String(String::from("Hello, World!"))
        );

        Ok(())
    }

    #[test]
    fn test_symbol_parse() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from(
            "^ ! () [] {} , . ; : + - * / < > = != && || >= <=**%",
        ));

        let expected_tokens = vec![
            TokenType::Symbol(Symbol::Caret),
            TokenType::Symbol(Symbol::LogicalNot),
            TokenType::Symbol(Symbol::LParen),
            TokenType::Symbol(Symbol::RParen),
            TokenType::Symbol(Symbol::LBracket),
            TokenType::Symbol(Symbol::RBracket),
            TokenType::Symbol(Symbol::LBrace),
            TokenType::Symbol(Symbol::RBrace),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Symbol(Symbol::Semicolon),
            TokenType::Symbol(Symbol::Colon),
            TokenType::Symbol(Symbol::Plus),
            TokenType::Symbol(Symbol::Minus),
            TokenType::Symbol(Symbol::Asterisk),
            TokenType::Symbol(Symbol::Slash),
            TokenType::Symbol(Symbol::LessThan),
            TokenType::Symbol(Symbol::GreaterThan),
            TokenType::Symbol(Symbol::Assign),
            TokenType::Symbol(Symbol::NotEqual),
            TokenType::Symbol(Symbol::LogicalAnd),
            TokenType::Symbol(Symbol::LogicalOr),
            TokenType::Symbol(Symbol::GreaterThanOrEqual),
            TokenType::Symbol(Symbol::LessThanOrEqual),
            TokenType::Symbol(Symbol::Exp),
            TokenType::Symbol(Symbol::Percent),
        ];

        for expected_token in expected_tokens {
            let token = tokenizer.next_token()?.unwrap();

            assert_eq!(token.token_type, expected_token);
        }

        Ok(())
    }

    #[test]
    fn test_keyword_parse() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("let fn if else return enum"));

        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Let),
            TokenType::Keyword(Keyword::Fn),
            TokenType::Keyword(Keyword::If),
            TokenType::Keyword(Keyword::Else),
            TokenType::Keyword(Keyword::Return),
            TokenType::Keyword(Keyword::Enum),
        ];

        for expected_token in expected_tokens {
            let token = tokenizer.next_token()?.unwrap();

            assert_eq!(token.token_type, expected_token);
        }

        Ok(())
    }

    #[test]
    fn test_identifier_parse() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("fn test"));

        let token = tokenizer.next_token()?.unwrap();
        assert_eq!(token.token_type, TokenType::Keyword(Keyword::Fn));
        let token = tokenizer.next_token()?.unwrap();
        assert_eq!(
            token.token_type,
            TokenType::Identifier(String::from("test"))
        );

        Ok(())
    }

    #[test]
    fn test_boolean_parse() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("true false"));

        let token = tokenizer.next_token()?.unwrap();
        assert_eq!(token.token_type, TokenType::Boolean(true));
        let token = tokenizer.next_token()?.unwrap();
        assert_eq!(token.token_type, TokenType::Boolean(false));

        Ok(())
    }

    #[test]
    fn test_full_source() -> Result<()> {
        let mut tokenizer = Tokenizer::from(TEST_STRING.to_owned());

        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Fn),
            TokenType::Identifier(String::from("test")),
            TokenType::Symbol(Symbol::LParen),
            TokenType::Symbol(Symbol::RParen),
            TokenType::Symbol(Symbol::LBrace),
            TokenType::Keyword(Keyword::Let),
            TokenType::Identifier(String::from("x")),
            TokenType::Symbol(Symbol::Assign),
            TokenType::Number(Number::Integer(10)),
            TokenType::Symbol(Symbol::Semicolon),
            TokenType::Keyword(Keyword::Return),
            TokenType::Identifier(String::from("x")),
            TokenType::Symbol(Symbol::Plus),
            TokenType::Number(Number::Integer(2)),
            TokenType::Symbol(Symbol::Semicolon),
            TokenType::Symbol(Symbol::RBrace),
        ];

        for expected_token in expected_tokens {
            let token = tokenizer.next_token()?.unwrap();

            assert_eq!(token.token_type, expected_token);
        }

        Ok(())
    }

    #[test]
    fn test_peek_next() -> Result<()> {
        let mut tokenizer = Tokenizer::from(TEST_STRING.to_owned());

        let column = tokenizer.column;
        let line = tokenizer.line;

        let peeked_token = tokenizer.peek_next()?;

        assert_eq!(
            peeked_token.unwrap().token_type,
            TokenType::Keyword(Keyword::Fn)
        );
        assert_eq!(tokenizer.column, column);
        assert_eq!(tokenizer.line, line);

        let next_token = tokenizer.next_token()?;

        assert_eq!(
            next_token.unwrap().token_type,
            TokenType::Keyword(Keyword::Fn)
        );
        assert_ne!(tokenizer.column, column);
        assert_ne!(tokenizer.line, line);

        Ok(())
    }

    #[test]
    fn test_compact_syntax() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("if(true) while(false)"));

        // if(true)
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Keyword(Keyword::If)
        );
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Symbol(Symbol::LParen)
        );
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Boolean(true)
        );
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Symbol(Symbol::RParen)
        );

        // while(false)
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Keyword(Keyword::While)
        );
        assert_eq!(
            tokenizer.next_token()?.unwrap().token_type,
            TokenType::Symbol(Symbol::LParen)
        );

        Ok(())
    }

    #[test]
    fn test_identifier_has_correct_length() -> Result<()> {
        let mut tokenizer = Tokenizer::from("hello");
        assert_eq!(
            tokenizer.next_token()?,
            Some(Token {
                token_type: TokenType::Identifier("hello".into()),
                original_string: Some("hello".into()),
                column: 1,
                line: 1
            })
        );
        Ok(())
    }

    #[test]
    fn test_keyword_token_has_correct_length() -> Result<()> {
        let mut tokenizer = Tokenizer::from("while");

        assert_eq!(
            tokenizer.next_token()?,
            Some(Token {
                token_type: TokenType::Keyword(Keyword::While),
                original_string: Some("while".into()),
                column: 1,
                line: 1
            })
        );

        Ok(())
    }
}
