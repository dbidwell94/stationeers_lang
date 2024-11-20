use thiserror::Error;

use crate::tokenizer::{
    token::{Keyword, Number, Symbol, Token, TokenType},
    Tokenizer, TokenizerError,
};
use std::io::{Read, Seek};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("{0}")]
    TokenizerError(#[from] TokenizerError),
    #[error("Unexpected EOF\n\nLine: {0}, Column: {1}", token.line, token.column)]
    UnexpectedEOF { token: Token },
    #[error("Unexpected token\n\nLine: {0}, Column: {1}\nToken: {2}", token.line, token.column, token.token_type)]
    UnexpectedToken { token: Token },
    #[error("An unknown error has occurred")]
    UnknownError,
}

#[derive(Debug)]
enum Literal {
    Number(Number),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
struct Identifier(String);

#[derive(Debug)]
pub enum Expression {
    Declaration {
        identifier: Identifier,
        value: Box<Expression>,
    },
    Assignment {
        identifier: Identifier,
        value: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Symbol,
        right: Box<Expression>,
    },
    Literal(Literal),
}

pub struct Parser<T>
where
    T: Read + Seek,
{
    tokenizer: Tokenizer<T>,
}

impl<T> Parser<T>
where
    T: Read + Seek,
{
    pub fn new(tokenizer: Tokenizer<T>) -> Self {
        Self { tokenizer }
    }

    pub fn parse(&mut self) -> Result<Option<Expression>, ParseError> {
        while let Some(token) = self.tokenizer.next_token()? {
            match token.token_type {
                TokenType::Number(n) => {
                    if let Some(Token {
                        token_type: TokenType::Symbol(s),
                        ..
                    }) = self.tokenizer.peek_next()?
                    {
                        if s.is_operator() {
                            self.tokenizer.next_token()?;
                            return Ok(Some(Expression::Binary {
                                left: Box::new(Expression::Literal(Literal::Number(n))),
                                operator: s,
                                right: Box::new(self.parse()?.ok_or(ParseError::UnknownError)?),
                            }));
                        }
                    } else {
                        return Ok(Some(Expression::Literal(Literal::Number(n))));
                    }
                }
                _ => return Err(ParseError::UnexpectedToken { token }),
            }
        }
        return Err(ParseError::UnknownError);
    }

    fn parse_declaration(&mut self) -> Result<Expression, ParseError> {
        let identifier = match self.tokenizer.next_token()? {
            Some(token) => match token.token_type {
                TokenType::Identifier(i) => Identifier(i),
                _ => return Err(ParseError::UnexpectedToken { token }),
            },
            None => return Err(ParseError::UnknownError),
        };

        return Ok(Expression::Declaration {
            identifier,
            value: Box::new(self.parse()?.ok_or(ParseError::UnknownError)?),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_parser() -> Result<()> {
        let input = r#"
            5.3245 + 5




            45 - 2
        "#;

        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expr = parser.parse()?;

        println!("{:?}", expr);

        let expr = parser.parse()?;

        println!("{:?}", expr);

        Ok(())
    }
}
