mod tree_node;

use crate::tokenizer::{
    token::{Symbol, Token, TokenType},
    Tokenizer, TokenizerError,
};
use std::{
    collections::VecDeque,
    io::{Read, Seek},
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
    tokenizer: Tokenizer<R>,
    current_token: Option<Token>,
}

impl<R> Parser<R>
where
    R: Read + Seek,
{
    pub fn new(tokenizer: Tokenizer<R>) -> Self {
        Parser {
            tokenizer,
            current_token: None,
        }
    }

    pub fn parse(&mut self) -> Result<tree_node::Expression, ParseError> {
        self.current_token = self.tokenizer.next_token()?;
        self.expression()
    }

    fn expression(&mut self) -> Result<tree_node::Expression, ParseError> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or(ParseError::UnknownError)?;

        Ok(match current_token.token_type {
            // Match a number or string literal as long as the next token is not an operator
            TokenType::Number(_) | TokenType::String(_)
                if !matches!(
                    self.tokenizer.peek_next()?, Some(Token { token_type: TokenType::Symbol(e), .. }) if e.is_operator()
                ) =>
            {
                Expression::Literal(self.literal()?)
            }

            // Match a negation operator
            TokenType::Symbol(Symbol::Minus) => Expression::Negation(Box::new(self.parse()?)),

            _ if matches!(self.tokenizer.peek_next()?, Some(Token { token_type: TokenType::Symbol(e), .. }) if e.is_operator()) => {
                Expression::BinaryExpression(self.binary()?)
            }

            // Something went wrong. Return an error
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        })
    }

    fn binary(&mut self) -> Result<tree_node::BinaryExpression, ParseError> {
        let literal = self.literal()?;

        let Some(Token {
            token_type: TokenType::Symbol(operator),
            ..
        }) = self.current_token
        else {
            return Err(ParseError::UnknownError);
        };
        self.current_token = self.tokenizer.next_token()?;

        Ok(match operator {
            Symbol::Plus => BinaryExpression::Add(
                Box::new(Expression::Literal(literal)),
                Box::new(self.expression()?),
            ),
            Symbol::Asterisk => BinaryExpression::Multiply(
                Box::new(Expression::Literal(literal)),
                Box::new(self.expression()?),
            ),
            Symbol::Slash => BinaryExpression::Divide(
                Box::new(Expression::Literal(literal)),
                Box::new(self.expression()?),
            ),
            Symbol::Minus => BinaryExpression::Subtract(
                Box::new(Expression::Literal(literal)),
                Box::new(self.expression()?),
            ),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: Token {
                        token_type: TokenType::Symbol(operator),
                        line: 0,
                        column: 0,
                    },
                })
            }
        })
    }

    fn literal(&mut self) -> Result<tree_node::Literal, ParseError> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or(ParseError::UnknownError)?;

        let to_return = match current_token.token_type {
            TokenType::Number(ref number) => tree_node::Literal::Number(number.clone()),
            TokenType::String(ref string) => tree_node::Literal::String(string.clone()),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        };

        self.current_token = self.tokenizer.next_token()?;
        Ok(to_return)
    }
}

#[cfg(test)]
mod tests {
    use super::tree_node::*;
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_add_expr() -> Result<()> {
        let input = "123 + 456";

        let mut parser = Parser::new(Tokenizer::from(input.to_owned()));

        let result = parser.parse()?;
        let formatted_output = format!("{}", result);

        assert_eq!(formatted_output, "(123 + 456)");
        Ok(())
    }

    #[test]
    fn test_parse_number() -> Result<()> {
        let input = "123";
        let mut parser = Parser::new(Tokenizer::from(input.to_owned()));
        let result = parser.parse()?;

        let formatted_output = format!("{}", result);

        assert_eq!(formatted_output, "123");

        Ok(())
    }

    #[test]
    fn test_parse_negation() -> Result<()> {
        let input = "-123";
        let mut parser = Parser::new(Tokenizer::from(input.to_owned()));
        let result = parser.parse()?;

        let formatted_output = format!("{}", result);

        assert_eq!(formatted_output, "(-123)");

        Ok(())
    }

    #[test]
    fn test_order_of_operations() -> Result<()> {
        let input = "123 - 456 + 789";

        let mut parser = Parser::new(Tokenizer::from(input.to_owned()));
        let result = parser.parse()?;

        let formatted_output = format!("{}", result);
        println!("{}", formatted_output);

        Ok(())
    }

    #[test]
    fn test_chained_operators() -> Result<()> {
        let input = "123 + 456 * 789";
        let mut parser = Parser::new(Tokenizer::from(input.to_owned()));
        let result = parser.parse()?;

        let formatted_output = format!("{}", result);

        assert_eq!(formatted_output, "(123 + (456 * 789))");

        Ok(())
    }
}
