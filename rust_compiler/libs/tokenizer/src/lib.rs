pub mod token;

use logos::{Lexer, Logos};
use std::{
    cmp::Ordering,
    collections::VecDeque,
    io::{Read, Seek, SeekFrom},
};
use thiserror::Error;
use token::{Token, TokenType};

#[derive(Error, Debug)]
pub enum Error {
    #[error("IO Error: {0}")]
    IOError(#[from()] std::io::Error),
    #[error(transparent)]
    LexError(#[from] token::LexError),
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
            LexError(e) => e.into(),
        }
    }
}

pub trait Tokenize: Read + Seek {}

impl<T> Tokenize for T where T: Read + Seek {}

pub struct Tokenizer<'a> {
    lexer: Lexer<'a, TokenType>,
    returned_eof: bool,
}

impl<'a> From<&'a str> for Tokenizer<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            lexer: TokenType::lexer(value),
            returned_eof: false,
        }
    }
}

impl<'a> Tokenizer<'a> {
    fn get_token(&mut self, t_type: TokenType) -> Token {
        let mut span = self.lexer.span();
        span.start -= self.lexer.extras.line_start_index;
        span.end -= self.lexer.extras.line_start_index;
        Token::new(t_type, self.lexer.extras.line_count, span)
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        let mut current = self.lexer.next().transpose();

        while matches!(current, Ok(Some(TokenType::Comment(_)))) {
            current = self.lexer.next().transpose();
        }

        Ok(current.map(|t| t.map(|t| self.get_token(t)))?)
    }
}

// ... Iterator and TokenizerBuffer implementations remain unchanged ...
// They just call the methods above which now use the passed-in start coordinates.
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            None => {
                if self.returned_eof {
                    None
                } else {
                    self.returned_eof = true;
                    Some(Ok(Token::new(
                        TokenType::EOF,
                        self.lexer.extras.line_count,
                        self.lexer.span(),
                    )))
                }
            }
            Some(t) => match t {
                Err(e) => Some(Err(e.into())),
                Ok(t) => Some(Ok(self.get_token(t))),
            },
        }
    }
}

pub struct TokenizerBuffer<'a> {
    tokenizer: Tokenizer<'a>,
    buffer: VecDeque<Token>,
    history: VecDeque<Token>,
    index: i64,
}

impl<'a> TokenizerBuffer<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            buffer: VecDeque::new(),
            history: VecDeque::with_capacity(128),
            index: 0,
        }
    }
    pub fn next_token(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.buffer.pop_front() {
            self.history.push_back(token.clone());
            self.index += 1;
            return Ok(Some(token));
        }
        let token = self.tokenizer.next_token()?;

        if let Some(ref token) = token {
            self.history.push_back(token.clone());
        }

        self.index += 1;
        Ok(token)
    }
    pub fn peek(&mut self) -> Result<Option<Token>, Error> {
        if let Some(token) = self.buffer.front() {
            return Ok(Some(token.clone()));
        }

        let Some(new_token) = self.tokenizer.next_token()? else {
            return Ok(None);
        };
        self.buffer.push_front(new_token.clone());
        Ok(Some(new_token))
    }
    pub fn loc(&self) -> i64 {
        self.index
    }
    fn seek_from_current(&mut self, seek_to_int: i64) -> Result<(), Error> {
        use Ordering::*;
        match seek_to_int.cmp(&0) {
            Greater => {
                let mut tokens = Vec::with_capacity(seek_to_int as usize);
                for _ in 0..seek_to_int {
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
                let seek_to = seek_to_int.unsigned_abs() as usize;
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
                self.index -= seek_to_int;
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
