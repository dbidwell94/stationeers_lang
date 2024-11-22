mod tree_node;

use crate::tokenizer::{
    token::{Keyword, Symbol, Token, TokenType},
    Tokenizer, TokenizerBuffer, TokenizerError,
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
    #[error("Invalid Syntax\n\nLine: {0}, Column: {1}\nReason: {reason}", token.line, token.column)]
    InvalidSyntax { token: Token, reason: String },
    #[error("Unexpected EOF")]
    UnexpectedEOF,
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

pub struct Parser {
    tokenizer: TokenizerBuffer,
    current_token: Option<Token>,
}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Parser {
            tokenizer: TokenizerBuffer::new(tokenizer),
            current_token: None,
        }
    }

    /// Parses all the input from the tokenizer buffer and returns the resulting expression
    /// Expressions are returned in a root block expression node
    pub fn parse_all(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        let mut expressions = Vec::<Expression>::new();

        while let Some(expression) = self.parse()? {
            expressions.push(expression);
        }

        Ok(Some(Expression::BlockExpression(BlockExpression(
            expressions,
        ))))
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

    /// Calls `assign_next` and returns the next token in the tokenizer buffer
    fn get_next(&mut self) -> Result<Option<&Token>, ParseError> {
        self.assign_next()?;
        Ok(self.current_token.as_ref())
    }

    fn expression(&mut self) -> Result<Option<tree_node::Expression>, ParseError> {
        let Some(current_token) = self.current_token.as_ref() else {
            return Ok(None);
        };

        if token_matches!(current_token, TokenType::EOF) {
            return Ok(None);
        }

        let expr = Some(match current_token.token_type {
            // match declarations with a `let` keyword
            TokenType::Keyword(Keyword::Let) => self.declaration()?,

            // match functions with a `fn` keyword
            TokenType::Keyword(Keyword::Fn) => Expression::FunctionExpression(self.function()?),

            // match a variable expression with opening parenthesis
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                Expression::InvocationExpression(self.invocation()?)
            }

            // match variable expressions with an identifier
            TokenType::Identifier(ref id) => Expression::Variable(id.clone()),

            // match block expressions with a `{` symbol
            TokenType::Symbol(Symbol::LBrace) => Expression::BlockExpression(self.block()?),

            // match literal expressions with a semi-colon afterwards
            TokenType::Number(_) | TokenType::String(_) if !self_matches_peek!(self, TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()) => {
                Expression::Literal(self.literal()?)
            }

            // match priority expressions with a left parenthesis
            TokenType::Symbol(Symbol::LParen) => Expression::PriorityExpression(self.priority()?),

            _ => {
                return Err(ParseError::UnexpectedToken {
                    token: current_token.clone(),
                })
            }
        });

        let Some(expr) = expr else {
            return Ok(None);
        };

        if self_matches_peek!(self, TokenType::Symbol(s) if s.is_operator()) {
            return Ok(Some(self.binary(expr)?));
        }

        // step 2: check if the next token is an operator and if we should parse a binary expression with the previous expression

        Ok(Some(expr))
    }

    fn binary(&mut self, previous: Expression) -> Result<tree_node::Expression, ParseError> {
        let current_token = token_from_option!(self.get_next()?).clone();

        // first, make sure the previous expression supports binary expressions
        match previous {
            Expression::BinaryExpression(_) // 1 + 2 + 3
            | Expression::InvocationExpression(_) // add() + 3
            | Expression::PriorityExpression(_) // (1 + 2) + 3
            | Expression::Literal(_) // 1 + 2
            | Expression::Variable(_) // x + 2
            | Expression::Negation(_) // -1 + 2
             => {} 
            _ => {
                return Err(ParseError::InvalidSyntax {
                    token: current_token.clone(),
                    reason: "Invalid expression for binary operation".to_owned(),
                })
            }
        }

        // now check the operator. If we have certain operators, we need to wrap in a priority expression
        // Example: subtraction and division. Order of operations is important

        let operator = extract_token_data!(
            current_token,
            TokenType::Symbol(ref s),
            s.clone()
        );

        let expr = match operator {
            Symbol::Plus => {
                let right = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;
                Expression::BinaryExpression(BinaryExpression::Add(Box::new(previous), Box::new(right)))
            },
            Symbol::Minus => {
                let right = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;
                Expression::PriorityExpression(Box::new(Expression::BinaryExpression(BinaryExpression::Subtract(Box::new(previous), Box::new(right)))))
            },
            _ => {
                return Err(ParseError::InvalidSyntax {
                    token: current_token.clone(),
                    reason: "Invalid operator for binary operation".to_owned(),
                })
            }
        };

        todo!()
    }

    fn priority(&mut self) -> Result<Box<Expression>, ParseError> {
        let current_token = token_from_option!(self.current_token);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        let expression = self.parse()?.ok_or(ParseError::UnexpectedEOF)?;

        // make sure the next token is a right parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::RParen)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        Ok(Box::new(expression))
    }

    fn invocation(&mut self) -> Result<InvocationExpression, ParseError> {
        let identifier = extract_token_data!(
            token_from_option!(self.current_token),
            TokenType::Identifier(ref id),
            id.clone()
        );

        // Ensure the next token is a left parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(ParseError::UnexpectedToken {
                token: current_token.clone(),
            });
        }

        let mut arguments = Vec::<Expression>::new();
        // We need to make sure the expressions are NOT BlockExpressions, as they are not allowed

        while !token_matches!(
            token_from_option!(self.get_next()?),
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = token_from_option!(self.current_token);
            let expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;

            if let Expression::BlockExpression(_) = expression {
                return Err(ParseError::InvalidSyntax {
                    token: current_token,
                    reason: "Block expressions are not allowed in function invocations".to_owned(),
                });
            }

            arguments.push(expression);

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

        Ok(InvocationExpression {
            name: identifier,
            arguments,
        })
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

        let mut arguments = Vec::<String>::new();

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

            arguments.push(argument);

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
            "(fn add(x, y) { { (let z = x); } })",
            expression.to_string()
        );

        Ok(())
    }

    #[test]
    fn test_function_invocation() -> Result<()> {
        let input = r#"
                add();
            "#;

        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expression = parser.parse()?.unwrap();

        assert_eq!("add()", expression.to_string());

        Ok(())
    }

    #[test]
    fn test_priority_expression() -> Result<()> {
        let input = r#"
            let x = (4);
        "#;

        let tokenizer = Tokenizer::from(input.to_owned());
        let mut parser = Parser::new(tokenizer);

        let expression = parser.parse()?.unwrap();

        assert_eq!("(let x = (4))", expression.to_string());

        Ok(())
    }
}
