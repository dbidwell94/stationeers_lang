mod tree_node;

use crate::tokenizer::{
    token::{Symbol, Token, TokenType},
    Tokenizer, TokenizerBuffer, TokenizerError,
};
use std::{
    collections::VecDeque,
    io::{Read, Seek, SeekFrom},
};
use thiserror::Error;
use tree_node::*;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),
    #[error("Unexpected token\n\nLine: {0}, Column: {1}\nToken: {2}", token.line, token.column, token.token_type)]
    UnexpectedToken { token: Token },
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("An unknown error has occurred")]
    UnknownError,
}

pub struct Parser<R: Read + Seek> {
    tokenizer: TokenizerBuffer<R>,
    current_token: Option<Token>,
}

impl<R> Parser<R>
where
    R: Read + Seek,
{
    pub fn new(tokenizer: Tokenizer<R>) -> Self {
        Parser {
            tokenizer: TokenizerBuffer::new(tokenizer),
            current_token: None,
        }
    }

    pub fn parse(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        self.current_token = self.tokenizer.next()?;
        self.expression()
    }

    fn expression(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        /// Helper macro to match the next token in the tokenizer buffer to a pattern
        /// with an optional if condition. The token is peeked and not consumed.
        macro_rules! matches_peek {
            ($pattern:pat) => {
                matches!(self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }))
            };
            ($pattern:pat if $cond:expr) => {
                matches!(self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }) if $cond)
            };
        }

        let Some(current_token) = self.current_token.as_ref() else {
            return Ok(None);
        };

        Ok(match current_token.token_type {
            // Assignment expression
            TokenType::Identifier(_) if matches_peek!(TokenType::Symbol(Symbol::Assign)) => {
                Some(Expression::AssignmentExpression(self.assignment()?))
            }

            // Negation expression
            TokenType::Symbol(Symbol::Minus) if matches_peek!(TokenType::Number(_)) => {
                self.tokenizer.next()?;
                Some(Expression::Negation(Box::new(
                    self.parse()?.ok_or(ParseError::UnexpectedEOF)?,
                )))
            }

            // Literal expression
            TokenType::Number(_) | TokenType::String(_)
                if !matches_peek!(
                    TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
                ) =>
            {
                Some(Expression::Literal(self.literal()?))
            }

            // Logical expression
            TokenType::Number(_) | TokenType::String(_)
                if matches_peek!(
                    TokenType::Symbol(s) if s.is_comparison() || s.is_logical()
                ) =>
            {
                Some(Expression::LogicalExpression(self.logical()?))
            }

            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        })
    }

    fn assignment(&mut self) -> Result<tree_node::AssignmentExpression, ParseError> {
        let Some(Token {
            token_type: TokenType::Identifier(identifier),
            ..
        }) = self.current_token.as_ref()
        else {
            return Err(ParseError::UnexpectedToken {
                // Safety: We have already checked that `self.current_token` is `Some` in the `parse()` function
                token: self.current_token.clone().unwrap(),
            });
        };

        // make sure the next token is `=` for sanity
        if let Some(Token {
            token_type: TokenType::Symbol(Symbol::Assign),
            ..
        }) = self.tokenizer.next()?
        {
        } else {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Err(ParseError::UnexpectedToken {
                token: self.tokenizer.next()?.unwrap(),
            });
        };

        Ok(AssignmentExpression {
            identifier: identifier.clone(),
            expression: Box::new(self.parse()?.ok_or(ParseError::UnexpectedEOF)?),
        })
    }

    fn logical(&mut self) -> Result<tree_node::LogicalExpression, ParseError> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Err(ParseError::UnexpectedEOF);
        };

        todo!()
    }

    fn binary(&mut self) -> Result<tree_node::BinaryExpression, ParseError> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Err(ParseError::UnexpectedEOF);
        };

        todo!()
    }

    fn literal(&mut self) -> Result<tree_node::Literal, ParseError> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Err(ParseError::UnexpectedEOF);
        };

        let to_return = match current_token.token_type {
            TokenType::Number(n) => Literal::Number(n),
            TokenType::String(ref s) => Literal::String(s.clone()),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        };

        // Advance the tokenizer if the next token is a semicolon
        if let Some(Token {
            token_type: TokenType::Symbol(Symbol::Semicolon),
            ..
        }) = self.tokenizer.peek()?
        {
            self.tokenizer.next()?;
        }

        Ok(to_return)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_assignment() -> Result<()> {
        let input = r#"
        x = 10;
        y = "testing";
        "#;
        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expr = parser.parse()?.unwrap();

        assert_eq!("x = 10", format!("{}", expr));

        let expr = parser.parse()?.unwrap();

        assert_eq!("y = \"testing\"", format!("{}", expr));

        Ok(())
    }

    #[test]
    fn test_literal() -> Result<()> {
        let input = r#"
            10;
            "testing";
        "#;

        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expr = parser.parse()?.unwrap();
        assert_eq!("10", format!("{}", expr));

        let expr = parser.parse()?.unwrap();
        assert_eq!("\"testing\"", format!("{}", expr));

        Ok(())
    }
}
