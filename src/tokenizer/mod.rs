mod token;

use std::{
    fs::File,
    io::{BufReader, Cursor, Read, Seek, SeekFrom},
    path::PathBuf,
};
use thiserror::Error;
use token::{Keyword, Number, Symbol, Token, TokenType};

#[derive(Error, Debug)]
pub enum TokenizerError {
    #[error("IO Error: {0}")]
    IOError(#[from] std::io::Error),
    #[error("Number Parse Error \"{0}\"\nLine: {1}, Column: {2}")]
    NumberParseError(std::num::ParseIntError, usize, usize),
    #[error("Unknown Symbol \"{0}\"\nLine: {1}, Column: {2}")]
    UnknownSymbolError(char, usize, usize),
    #[error("Unknown Keyword or Identifier \"{0}\"\nLine: {1}, Column: {2}")]
    UnknownKeywordOrIdentifierError(String, usize, usize),
}

pub(crate) struct Tokenizer<T>
where
    T: Read + Seek,
{
    reader: BufReader<T>,
    char_buffer: [u8; 1],
    line: usize,
    column: usize,
    returned_eof: bool,
}

impl From<String> for Tokenizer<Cursor<Vec<u8>>> {
    fn from(input: String) -> Self {
        let cursor = Cursor::new(input.into_bytes());
        let reader = BufReader::new(cursor);

        Self {
            reader,
            line: 1,
            column: 1,
            char_buffer: [0],
            returned_eof: false,
        }
    }
}

impl Tokenizer<File> {
    pub fn from_path(input_file: impl Into<PathBuf>) -> Result<Self, TokenizerError> {
        let file = std::fs::File::open(input_file.into())?;
        let reader = BufReader::new(file);

        Ok(Self {
            reader,
            line: 1,
            column: 1,
            char_buffer: [0],
            returned_eof: false,
        })
    }
}

impl<T> Tokenizer<T>
where
    T: Read + Seek,
{
    /// Consumes the tokenizer and returns the next token in the stream
    /// If there are no more tokens in the stream, this function returns None
    /// If there is an error reading the stream, this function returns an error
    ///
    /// # Important
    /// This function will increment the line and column counters
    fn next_char(&mut self) -> Result<Option<char>, TokenizerError> {
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
    fn peek_next_char(&mut self) -> Result<Option<char>, TokenizerError> {
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
    fn skip_line(&mut self) -> Result<(), TokenizerError> {
        while let Some(next_char) = self.next_char()? {
            if next_char == '\n' {
                break;
            }
        }
        Ok(())
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, TokenizerError> {
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
                    return self.tokenize_symbol(next_char).map(Some)
                }
                // keywords and identifiers
                char if char.is_alphabetic() => {
                    return self.tokenize_keyword_or_identifier(next_char).map(Some)
                }
                _ => {
                    return Err(TokenizerError::UnknownSymbolError(
                        next_char,
                        self.line,
                        self.column,
                    ))
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

    /// Tokenizes a symbol
    fn tokenize_symbol(&mut self, first_symbol: char) -> Result<Token, TokenizerError> {
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
            '*' => symbol!(Asterisk),
            '.' => symbol!(Dot),

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

            '&' if self.peek_next_char()? == Some('&') => {
                self.next_char()?;
                symbol!(LogicalAnd)
            }
            '|' if self.peek_next_char()? == Some('|') => {
                self.next_char()?;
                symbol!(LogicalOr)
            }
            _ => Err(TokenizerError::UnknownSymbolError(
                first_symbol,
                self.line,
                self.column,
            )),
        }
    }

    /// Tokenizes a number literal
    fn tokenize_number(&mut self, first_char: char) -> Result<Token, TokenizerError> {
        let mut primary = String::with_capacity(16);
        let mut decimal: Option<String> = None;
        let mut reading_decimal = false;

        let column = self.column.clone();
        let line = self.line.clone();

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

        if let Some(decimal) = decimal {
            Ok(Token::new(
                TokenType::Number(Number::Decimal(
                    primary
                        .parse()
                        .map_err(|e| TokenizerError::NumberParseError(e, line, column))?,
                    decimal
                        .parse()
                        .map_err(|e| TokenizerError::NumberParseError(e, line, column))?,
                )),
                line,
                column,
            ))
        } else {
            Ok(Token::new(
                TokenType::Number(Number::Integer(
                    primary
                        .parse()
                        .map_err(|e| TokenizerError::NumberParseError(e, line, column))?,
                )),
                line,
                column,
            ))
        }
    }

    /// Tokenizes a string literal
    fn tokenize_string(&mut self, beginning_quote: char) -> Result<Token, TokenizerError> {
        let mut buffer = String::with_capacity(16);

        let column = self.column.clone();
        let line = self.line.clone();

        while let Some(next_char) = self.next_char()? {
            if next_char == beginning_quote {
                break;
            }

            buffer.push(next_char);
        }

        Ok(Token::new(TokenType::String(buffer), line, column))
    }

    /// Tokenizes a keyword or an identifier. Also handles boolean literals
    fn tokenize_keyword_or_identifier(
        &mut self,
        first_char: char,
    ) -> Result<Token, TokenizerError> {
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
                matches!(self.peek_next_char()?, Some(x) if x.is_whitespace() || !x.is_alphanumeric()) || matches!(self.peek_next_char()?, None)
            };
        }

        let mut buffer = String::with_capacity(16);
        let line = self.line.clone();
        let column = self.column.clone();

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
                "import" if next_ws!() => keyword!(Import),
                "export" if next_ws!() => keyword!(Export),

                // boolean literals
                "true" if next_ws!() => {
                    return Ok(Token::new(TokenType::Boolean(true), self.line, self.column))
                }
                "false" if next_ws!() => {
                    return Ok(Token::new(
                        TokenType::Boolean(false),
                        self.line,
                        self.column,
                    ))
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
        Err(TokenizerError::UnknownKeywordOrIdentifierError(
            buffer, line, column,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    const TEST_FILE: &str = "tests/file.stlg";

    const TEST_STRING: &str = r#"
        fn test() {
            let x = 10;
            return x + 2;
        }
    "#;

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
    fn test_skip_line() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from(
            r#"
This is a skippable line"#,
        ));

        tokenizer.skip_line()?;

        assert_eq!(tokenizer.line, 2);
        assert_eq!(tokenizer.column, 1);

        let next_char = tokenizer.next_char()?;
        assert_eq!(next_char, Some('T'));

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
    fn test_parse_decimal() -> Result<()> {
        let mut tokenizer = Tokenizer::from(String::from("10.5"));

        let token = tokenizer.next_token()?.unwrap();

        assert_eq!(token.token_type, TokenType::Number(Number::Decimal(10, 5)));

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
            "! () [] {} , . ; : + - * / < > = != && || >= <=",
        ));

        let expected_tokens = vec![
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
        ];

        for expected_token in expected_tokens {
            let token = tokenizer.next_token()?.unwrap();

            assert_eq!(token.token_type, expected_token);
        }

        Ok(())
    }

    #[test]
    fn test_keyword_parse() -> Result<()> {
        let mut tokenizer =
            Tokenizer::from(String::from("let fn if else return enum import export"));

        let expected_tokens = vec![
            TokenType::Keyword(Keyword::Let),
            TokenType::Keyword(Keyword::Fn),
            TokenType::Keyword(Keyword::If),
            TokenType::Keyword(Keyword::Else),
            TokenType::Keyword(Keyword::Return),
            TokenType::Keyword(Keyword::Enum),
            TokenType::Keyword(Keyword::Import),
            TokenType::Keyword(Keyword::Export),
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
}
