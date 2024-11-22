mod tree_node;

use crate::tokenizer::{
    token::{self, Keyword, Symbol, Token, TokenType},
    Tokenizer, TokenizerBuffer, TokenizerError,
};
use std::{
    collections::HashSet,
    io::{Read, Seek},
};
use thiserror::Error;
use tree_node::*;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),
    #[error("Unexpected token\n\nLine: {0}, Column: {1}\nToken: {2}\n", token.line, token.column, token.token_type)]
    UnexpectedToken { token: Token },
    #[error("Duplicated Identifer\n\nLine: {0}, Column: {1}\nToken: {2}\n", token.line, token.column, token.token_type)]
    DuplicateIdentifier { token: Token },
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

            TokenType::Identifier(ref id) => Expression::Variable(id.clone()),

            // match block expressions with a `{` symbol
            TokenType::Symbol(Symbol::LBrace) => Expression::BlockExpression(self.block()?),

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

    fn block(&mut self) -> Result<BlockExpression, ParseError> {
        let mut expressions = Vec::<Expression>::new();
        let current_token = token_from_option!(self.current_token);

        // sanity check: make sure the current token is a left brace
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        while !token_matches!(
            token_from_option!(self.get_next()?),
            TokenType::Symbol(Symbol::RBrace)
        ) {
            let expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;
            expressions.push(expression);
        }

        Ok(BlockExpression(expressions))
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
        let current_token = token_from_option!(self.current_token);
        // Sanify check that the current token is a `fn` keyword
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Fn)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        let fn_ident = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        // make sure next token is a left parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        let mut arguments = HashSet::<String>::new();

        // iterate through the arguments. While expression while increment the current token
        // with the `token_from_option!(self.get_next()?)` macro
        while !token_matches!(
            token_from_option!(self.get_next()?),
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = token_from_option!(self.current_token);
            let argument =
                extract_token_data!(current_token, TokenType::Identifier(ref id), id.clone());

            if arguments.contains(&argument) {
                return Err(ParseError::DuplicateIdentifier {
                    token: current_token.clone(),
                });
            }

            arguments.insert(argument);

            // make sure the next token is a comma or right parenthesis
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                return Err(ParseError::UnexpectedToken {
                    token: token_from_option!(self.get_next()?).clone(),
                });
            }

            // edge case: if the next token is not a right parenthesis, increment the current token
            //
            // This will allow the loop to break on a right parenthesis with the next iteration
            // which is incremented by the loop
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        // make sure the next token is a left brace
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        };

        Ok(FunctionExpression {
            name: fn_ident,
            arguments,
            body: self.block()?,
        })
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

    #[test]
    fn test_block() -> Result<()> {
        let input = r#"
        {
            let x = 5;
            let y = 10;
        }
        "#;
        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expression = parser.parse()?.unwrap();

        assert_eq!("{ (let x = 5); (let y = 10); }", expression.to_string());

        Ok(())
    }

    #[test]
    fn test_function_expression() -> Result<()> {
        let input = r#"
            // This is a function. The parser is starting to get more complex
            fn add(x, y) {
                let z = x;
            }
        "#;

        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expression = parser.parse()?.unwrap();

        assert_eq!(
            "(fn add(x, y) { { (let z = 5); } })",
            expression.to_string()
        );

        Ok(())
    }
}
