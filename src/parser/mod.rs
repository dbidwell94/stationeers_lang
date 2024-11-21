mod tree_node;

use crate::tokenizer::{
    token::{Keyword, Symbol, Token, TokenType},
    Tokenizer, TokenizerBuffer, TokenizerError,
};
use std::io::{Read, Seek};
use thiserror::Error;
use tree_node::*;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),
    #[error("Unexpected token\n\nLine: {0}, Column: {1}\nToken: {2}\n", token.line, token.column, token.token_type)]
    UnexpectedToken { token: Token },
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("An unknown error has occurred")]
    UnknownError,
}

macro_rules! self_matches_peek {
    ($self:ident, $pattern:pat) => {
        matches!($self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }))
    };
    ($self:ident, $pattern:pat if $cond:expr) => {
        matches!($self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }) if $cond)
    };
}

macro_rules! token_from_option {
    ($token:expr) => {
        match $token {
            Some(ref token) => token.clone(),
            None => return Err(ParseError::UnexpectedEOF),
        }
    };
}

macro_rules! extract_token_data {
    ($token:ident, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: $token.clone(),
                })
            }
        }
    };
    ($token:expr, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: $token.clone(),
                })
            }
        }
    };
}

macro_rules! self_matches_current {
    ($self:ident, $pattern:pat) => {
        matches!($self.current_token, Some(Token { token_type: $pattern, .. }))
    };
    ($self:ident, $pattern:pat if $cond:expr) => {
        matches!($self.current_token, Some(Token { token_type: $pattern, .. }) if $cond)
    };
}

macro_rules! token_matches {
    ($token:ident, $pattern:pat) => {
        matches!($token.token_type, $pattern)
    };
    ($token:expr, $pattern:pat) => {
        matches!($token.token_type, $pattern)
    };
    ($token:ident, $pattern:pat if $cond:expr) => {
        matches!($token.token_type, $pattern if $cond)
    };
    ($token:expr, $pattern:pat if $cond:expr) => {
        matches!($token.token_type, $pattern if $cond)
    };
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

    /// Parses the input from the tokenizer buffer and returns the resulting expression
    pub fn parse(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        self.assign_next()?;
        self.expression()
    }

    /// Assigns the next token in the tokenizer buffer to the current token
    fn assign_next(&mut self) -> Result<(), ParseError> {
        self.current_token = self.tokenizer.next()?;
        Ok(())
    }

    fn get_next(&mut self) -> Result<Option<&Token>, ParseError> {
        self.assign_next()?;
        Ok(self.current_token.as_ref())
    }

    fn expression(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Ok(None);
        };

        let to_return = Some(match current_token.token_type {
            // match declarations with a `let` keyword
            TokenType::Keyword(Keyword::Let) => self.declaration()?,

            // match functions with a `fn` keyword
            TokenType::Keyword(Keyword::Fn) => Expression::FunctionExpression(self.function()?),

            // match literal expressions with a semi-colon afterwards
            TokenType::Number(_) | TokenType::String(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) =>
            {
                Expression::Literal(self.literal()?)
            }

            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        });

        Ok(to_return)
    }

    fn declaration(&mut self) -> Result<Expression, ParseError> {
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }
        let identifier = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();

        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token,
            });
        }

        let assignment_expression = self.parse()?.ok_or(ParseError::UnexpectedEOF)?;

        // make sure the next token is a semi-colon
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        Ok(Expression::DeclarationExpression(
            identifier,
            Box::new(assignment_expression),
        ))
    }

    fn literal(&mut self) -> Result<Literal, ParseError> {
        let current_token = token_from_option!(self.current_token);
        let literal = match current_token.token_type {
            TokenType::Number(ref num) => Literal::Number(num.clone()),
            TokenType::String(ref string) => Literal::String(string.clone()),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        };

        Ok(literal)
    }

    fn function(&mut self) -> Result<FunctionExpression, ParseError> {
        todo!("Implement function parsing")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    #[test]
    fn test_declarations() -> Result<()> {
        let input = r#"
        let x = 5;
        // The below line should fail
        let y = 234
        "#;
        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expression = parser.parse()?.unwrap();

        assert_eq!("(let x = 5)", expression.to_string());

        assert!(parser.parse().is_err());

        Ok(())
    }
}
