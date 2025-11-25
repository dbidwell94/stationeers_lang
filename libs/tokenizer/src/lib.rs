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
        NumberParseError(err: std::num::ParseIntError, line: usize, column: usize) {
            display("Number Parse Error: {}\nLine: {}, Column: {}", err, line, column)
            source(err)
        }
        DecimalParseError(err: rust_decimal::Error, line: usize, column: usize) {
            display("Decimal Parse Error: {}\nLine: {}, Column: {}", err, line, column)
            source(err)
        }
        UnknownSymbolError(char: char, line: usize, column: usize) {
            display("Unknown Symbol: {}\nLine: {}, Column: {}", char, line, column)
        }
        UnknownKeywordOrIdentifierError(val: String, line: usize, column: usize) {
            display("Unknown Keyword or Identifier: {}\nLine: {}, Column: {}", val, line, column)
        }
    }
}

pub trait Tokenize: Read + Seek {}

impl<T> Tokenize for T where T: Read + Seek {}

pub struct Tokenizer {
    reader: BufReader<Box<dyn Tokenize>>,
    char_buffer: [u8; 1],
    line: usize,
    column: usize,
    returned_eof: bool,
}

impl Tokenizer {
    pub fn from_path(input_file: impl Into<PathBuf>) -> Result<Self, Error> {
        let file = std::fs::File::open(input_file.into())?;
        let reader = BufReader::new(Box::new(file) as Box<dyn Tokenize>);

        Ok(Self {
            reader,
            line: 1,
            column: 1,
            char_buffer: [0],
            returned_eof: false,
        })
    }
}

impl From<String> for Tokenizer {
    fn from(input: String) -> Self {
        let reader = BufReader::new(Box::new(Cursor::new(input)) as Box<dyn Tokenize>);

        Self {
            reader,
            line: 1,
            column: 1,
            char_buffer: [0],
            returned_eof: false,
        }
    }
}

impl Tokenizer {
    /// Consumes the tokenizer and returns the next token in the stream
    /// If there are no more tokens in the stream, this function returns None
    /// If there is an error reading the stream, this function returns an error
    ///
    /// # Important
    /// This function will increment the line and column counters
    fn next_char(&mut self) -> Result<Option<char>, Error> {
        let bytes_read = self.reader.read(&mut self.char_buffer)?;

        if bytes_read == 0 {
            return Ok(None);
        }

        // Safety: The buffer is guaranteed to have 1 value as it is initialized with a size of 1
        let c = self.char_buffer[0] as char;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        Ok(Some(c))
    }

    /// Peeks the next character in the stream without consuming it
    ///
    /// # Important
    /// This does not increment the line or column counters
    fn peek_next_char(&mut self) -> Result<Option<char>, Error> {
        let current_pos = self.reader.stream_position()?;

        let to_return = if self.reader.read(&mut self.char_buffer)? == 0 {
            None
        } else {
            self.reader.seek(SeekFrom::Start(current_pos))?;

            // Safety: The buffer is guaranteed to have 1 value as it is initialized with a size of 1
            Some(self.char_buffer[0] as char)
        };

        Ok(to_return)
    }

    /// Skips the current line in the stream.
    /// Useful for skipping comments or empty lines
    ///
    /// # Important
    /// This function will increment the line and column counters
    fn skip_line(&mut self) -> Result<(), Error> {
        while let Some(next_char) = self.next_char()? {
            if next_char == '\n' {
                break;
            }
        }
        Ok(())
    }

    /// Consumes the tokenizer and returns the next token in the stream
    /// If there are no more tokens in the stream, this function returns None
    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        while let Some(next_char) = self.next_char()? {
            // skip whitespace
            if next_char.is_whitespace() {
                continue;
            }
            // skip comments
            if next_char == '/' && self.peek_next_char()? == Some('/') {
                self.skip_line()?;
                continue;
            }

            match next_char {
                // numbers
                '0'..='9' => {
                    return self.tokenize_number(next_char).map(Some);
                }
                // strings
                '"' | '\'' => return self.tokenize_string(next_char).map(Some),
                // symbols excluding `"` and `'`
                char if !char.is_alphanumeric() && char != '"' && char != '\'' => {
                    return self.tokenize_symbol(next_char).map(Some);
                }
                // keywords and identifiers
                char if char.is_alphabetic() => {
                    return self.tokenize_keyword_or_identifier(next_char).map(Some);
                }
                _ => {
                    return Err(Error::UnknownSymbolError(next_char, self.line, self.column));
                }
            }
        }
        if self.returned_eof {
            Ok(None)
        } else {
            self.returned_eof = true;
            Ok(Some(Token::new(TokenType::EOF, self.line, self.column)))
        }
    }

    /// Peeks the next token in the stream without consuming it
    /// If there are no more tokens in the stream, this function returns None
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

    /// Tokenizes a symbol
    fn tokenize_symbol(&mut self, first_symbol: char) -> Result<Token, Error> {
        /// Helper macro to create a symbol token
        macro_rules! symbol {
            ($symbol:ident) => {
                Ok(Token::new(
                    TokenType::Symbol(Symbol::$symbol),
                    self.line,
                    self.column,
                ))
            };
        }

        match first_symbol {
            // single character symbols
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

            // multi-character symbols
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
                self.line,
                self.column,
            )),
        }
    }

    /// Tokenizes a number literal. Also handles temperatures with a suffix of `c`, `f`, or `k`.
    fn tokenize_number(&mut self, first_char: char) -> Result<Token, Error> {
        let mut primary = String::with_capacity(16);
        let mut decimal: Option<String> = None;
        let mut reading_decimal = false;

        let column = self.column;
        let line = self.line;

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

            // support underscores in numbers for readability
            if next_char == '_' {
                self.next_char()?;
                continue;
            }

            // This is for the times when we have a number followed by a symbol (like a semicolon or =)
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
            let number = format!("{}{}", primary, decimal)
                .parse::<i128>()
                .map_err(|e| Error::NumberParseError(e, self.line, self.column))?;
            Number::Decimal(
                Decimal::try_from_i128_with_scale(number, decimal_scale)
                    .map_err(|e| Error::DecimalParseError(e, line, column))?,
            )
        } else {
            Number::Integer(
                primary
                    .parse()
                    .map_err(|e| Error::NumberParseError(e, line, column))?,
            )
        };

        // check if the next char is a temperature suffix
        if let Some(next_char) = self.peek_next_char()? {
            let temperature = match next_char {
                'c' => Temperature::Celsius(number),
                'f' => Temperature::Fahrenheit(number),
                'k' => Temperature::Kelvin(number),
                _ => return Ok(Token::new(TokenType::Number(number), line, column)),
            }
            .to_kelvin();

            self.next_char()?;
            Ok(Token::new(TokenType::Number(temperature), line, column))
        } else {
            Ok(Token::new(TokenType::Number(number), line, column))
        }
    }

    /// Tokenizes a string literal
    fn tokenize_string(&mut self, beginning_quote: char) -> Result<Token, Error> {
        let mut buffer = String::with_capacity(16);

        let column = self.column;
        let line = self.line;

        while let Some(next_char) = self.next_char()? {
            if next_char == beginning_quote {
                break;
            }

            buffer.push(next_char);
        }

        Ok(Token::new(TokenType::String(buffer), line, column))
    }

    /// Tokenizes a keyword or an identifier. Also handles boolean literals
    fn tokenize_keyword_or_identifier(&mut self, first_char: char) -> Result<Token, Error> {
        macro_rules! keyword {
            ($keyword:ident) => {{
                return Ok(Token::new(
                    TokenType::Keyword(Keyword::$keyword),
                    self.line,
                    self.column,
                ));
            }};
        }

        /// Helper macro to check if the next character is whitespace or not alphanumeric
        macro_rules! next_ws {
            () => {
                matches!(self.peek_next_char()?, Some(x) if x.is_whitespace() || !x.is_alphanumeric()) || self.peek_next_char()?.is_none()
            };
        }

        let mut buffer = String::with_capacity(16);
        let line = self.line;
        let column = self.column;

        let mut looped_char = Some(first_char);

        while let Some(next_char) = looped_char {
            if next_char.is_whitespace() {
                break;
            }

            if !next_char.is_alphanumeric() {
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

                // boolean literals
                "true" if next_ws!() => {
                    return Ok(Token::new(TokenType::Boolean(true), self.line, self.column));
                }
                "false" if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Boolean(false),
                        self.line,
                        self.column,
                    ));
                }
                // if the next character is whitespace or not alphanumeric, then we have an identifier
                // this is because keywords are checked first
                val if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Identifier(val.to_string()),
                        line,
                        column,
                    ));
                }
                _ => {}
            }

            looped_char = self.next_char()?;
        }
        Err(Error::UnknownKeywordOrIdentifierError(buffer, line, column))
    }
}

pub struct TokenizerBuffer {
    tokenizer: Tokenizer,
    buffer: VecDeque<Token>,
    history: VecDeque<Token>,
}

impl TokenizerBuffer {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            buffer: VecDeque::new(),
            history: VecDeque::with_capacity(128),
        }
    }

    /// Reads the next token from the tokenizer, pushing the value to the back of the history
    /// and returning the token
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

    /// Peeks the next token in the stream without adding to the history stack
    pub fn peek(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.buffer.front() {
            return Ok(Some(token.clone()));
        }

        let token = self.tokenizer.peek_next()?;
        Ok(token)
    }

    fn seek_from_current(&mut self, seek_to: i64) -> Result<(), Error> {
        use Ordering::*;
        // if seek_to > 0 then we need to check if the buffer has enough tokens to pop, otherwise we need to read from the tokenizer
        // if seek_to < 0 then we need to pop from the history and push to the front of the buffer. If not enough, then we throw (we reached the front of the history)
        // if seek_to == 0 then we don't need to do anything

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

    /// Adds to or removes from the History stack, allowing the user to move back and forth in the stream
    pub fn seek(&mut self, from: SeekFrom) -> Result<(), Error> {
        match from {
            SeekFrom::Current(seek_to) => self.seek_from_current(seek_to)?,
            SeekFrom::End(_) => unimplemented!("SeekFrom::End will not be implemented"),
            SeekFrom::Start(_) => unimplemented!("SeekFrom::Start will not be implemented"),
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
        assert_eq!(tokenizer.column, 2);

        Ok(())
    }

    #[test]
    fn test_peek_next_char() -> Result<()> {
        let mut tokenizer = Tokenizer::from(TEST_STRING.to_owned());

        let char = tokenizer.peek_next_char()?;

        assert_eq!(char, Some('\n'));
        assert_eq!(tokenizer.line, 1);
        assert_eq!(tokenizer.column, 1);

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
}
