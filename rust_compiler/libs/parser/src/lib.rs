#[cfg(test)]
mod test;

pub mod sys_call;
pub mod tree_node;

use crate::sys_call::System;
use quick_error::quick_error;
use std::io::SeekFrom;
use sys_call::SysCall;
use tokenizer::{
    self, Tokenizer, TokenizerBuffer,
    token::{Keyword, Symbol, Token, TokenType},
};
use tree_node::*;

#[macro_export]
/// A macro to create a boxed value.
macro_rules! boxed {
    ($e:expr) => {
        Box::new($e)
    };
}

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        TokenizerError(err: tokenizer::Error) {
            from()
            display("Tokenizer Error: {}", err)
            source(err)
        }
        UnexpectedToken(span: Span, token: Token) {
            display("Unexpected token: {}", token.token_type)
        }
        DuplicateIdentifier(span: Span, token: Token) {
            display("Duplicate identifier: {}", token.token_type)
        }
        InvalidSyntax(span: Span, reason: String) {
            display("Invalid syntax: {}", reason)
        }
        UnsupportedKeyword(span: Span, token: Token) {
            display("Unsupported keyword: {}", token.token_type)
        }
        UnexpectedEOF {
            display("Unexpected EOF")
        }
    }
}

impl From<Error> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            TokenizerError(e) => e.into(),
            UnexpectedToken(span, _)
            | DuplicateIdentifier(span, _)
            | InvalidSyntax(span, _)
            | UnsupportedKeyword(span, _) => Diagnostic {
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                range: span.into(),
                ..Default::default()
            },
            UnexpectedEOF => Diagnostic {
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
        }
    }
}

macro_rules! self_matches_peek {
    ($self:ident, $pattern:pat) => {
        matches!($self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }))
    };
    ($self:ident, $pattern:pat if $cond:expr) => {
        matches!($self.tokenizer.peek()?, Some(Token { token_type: $pattern, .. }) if $cond)
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

macro_rules! self_matches_current {
    ($self:ident, $pattern:pat) => {
        matches!($self.current_token, Some(Token { token_type: $pattern, .. }))
    };
    ($self:ident, $pattern:pat if $cond:expr) => {
        matches!($self.current_token, Some(Token { token_type: $pattern, .. }) if $cond)
    };
}

pub struct Parser<'a> {
    tokenizer: TokenizerBuffer<'a>,
    current_token: Option<Token>,
    pub errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Parser {
            tokenizer: TokenizerBuffer::new(tokenizer),
            current_token: None,
            errors: Vec::new(),
        }
    }

    /// Calculates a Span from a given Token reference.
    /// This is a static helper to avoid borrowing `self` when we already have a token ref.
    fn token_to_span(t: &Token) -> Span {
        let len = t.original_string.as_ref().map(|s| s.len()).unwrap_or(0);
        Span {
            start_line: t.line,
            start_col: t.column,
            end_line: t.line,
            end_col: t.column + len,
        }
    }

    fn current_span(&self) -> Span {
        self.current_token
            .as_ref()
            .map(Self::token_to_span)
            .unwrap_or(Span {
                start_line: 0,
                start_col: 0,
                end_line: 0,
                end_col: 0,
            })
    }

    /// Helper to run a parsing closure and wrap the result in a Spanned struct
    fn spanned<F, T>(&mut self, parser: F) -> Result<Spanned<T>, Error>
    where
        F: FnOnce(&mut Self) -> Result<T, Error>,
    {
        // Peek at the start token. If no current token (parsing hasn't started), peek the buffer.
        let start_token = if self.current_token.is_some() {
            self.current_token.clone()
        } else {
            self.tokenizer.peek()?
        };

        let (start_line, start_col) = start_token
            .as_ref()
            .map(|t| (t.line, t.column))
            .unwrap_or((1, 1));

        let node = parser(self)?;

        // The end token is the current_token after parsing.
        let end_token = self.current_token.as_ref();

        let (end_line, end_col) = end_token
            .map(|t| {
                let len = t.original_string.as_ref().map(|s| s.len()).unwrap_or(0);
                (t.line, t.column + len)
            })
            .unwrap_or((start_line, start_col));

        Ok(Spanned {
            span: Span {
                start_line,
                start_col,
                end_line,
                end_col,
            },
            node,
        })
    }

    /// Skips tokens until a statement boundary is found to recover from errors.
    fn synchronize(&mut self) -> Result<(), Error> {
        // We advance once to consume the error-causing token if we haven't already
        // But often the error happens after we consumed something.
        // Safe bet: consume current, then look.

        // If we assign next, we might be skipping the very token we want to sync on if the error didn't consume it?
        // Usually, in recursive descent, the error is raised when `current` is unexpected.
        // We want to discard `current` and move on.
        self.assign_next()?;

        while let Some(token) = &self.current_token {
            if token.token_type == TokenType::Symbol(Symbol::Semicolon) {
                // Consuming the semicolon is a good place to stop and resume parsing next statement
                self.assign_next()?;
                return Ok(());
            }

            // Check if the token looks like the start of a statement.
            // If so, we don't consume it; we return so the loop in parse_all can try to parse it.
            match token.token_type {
                TokenType::Keyword(Keyword::Fn)
                | TokenType::Keyword(Keyword::Let)
                | TokenType::Keyword(Keyword::If)
                | TokenType::Keyword(Keyword::While)
                | TokenType::Keyword(Keyword::Loop)
                | TokenType::Keyword(Keyword::Device)
                | TokenType::Keyword(Keyword::Return) => return Ok(()),
                _ => {}
            }

            self.assign_next()?;
        }

        Ok(())
    }

    pub fn parse_all(&mut self) -> Result<Option<tree_node::Expression>, Error> {
        let first_token = self.tokenizer.peek().unwrap_or(None);
        let (start_line, start_col) = first_token
            .as_ref()
            .map(|tok| (tok.line, tok.column))
            .unwrap_or((1, 1));

        let mut expressions = Vec::<Spanned<Expression>>::new();

        loop {
            // Check EOF without unwrapping error
            match self.tokenizer.peek() {
                Ok(None) => break,
                Err(e) => {
                    self.errors.push(Error::TokenizerError(e));
                    break;
                }
                _ => {}
            }

            match self.parse() {
                Ok(Some(expression)) => {
                    expressions.push(expression);
                }
                Ok(None) => break,
                Err(e) => {
                    self.errors.push(e);
                    // Recover
                    if self.synchronize().is_err() {
                        // If sync failed (e.g. EOF during sync), break
                        break;
                    }
                }
            }
        }

        // Even if we had errors, we return whatever partial AST we managed to build.
        // If expressions is empty and we had errors, it's a failed parse, but we return a block.

        // Use the last token position for end span, or start if nothing parsed
        let end_token_opt = self.tokenizer.peek().unwrap_or(None);
        let (end_line, end_col) = end_token_opt
            .map(|tok| {
                let len = tok.original_string.as_ref().map(|s| s.len()).unwrap_or(0);
                (tok.line, tok.column + len)
            })
            .unwrap_or((start_line, start_col));

        let span = Span {
            start_line,
            end_line,
            start_col,
            end_col,
        };

        Ok(Some(Expression::Block(Spanned {
            node: BlockExpression(expressions),
            span,
        })))
    }

    pub fn parse(&mut self) -> Result<Option<Spanned<tree_node::Expression>>, Error> {
        self.assign_next()?;

        // If assign_next hit EOF or error?
        if self.current_token.is_none() {
            return Ok(None);
        }

        let expr = self.expression()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) {
            self.assign_next()?;
        }

        Ok(expr)
    }

    fn assign_next(&mut self) -> Result<(), Error> {
        self.current_token = self.tokenizer.next_token()?;
        Ok(())
    }

    fn get_next(&mut self) -> Result<Option<&Token>, Error> {
        self.assign_next()?;
        Ok(self.current_token.as_ref())
    }

    fn expression(&mut self) -> Result<Option<Spanned<tree_node::Expression>>, Error> {
        // Parse the Left Hand Side (unary/primary expression)
        let lhs = self.unary()?;

        let Some(lhs) = lhs else {
            return Ok(None);
        };

        // check if the next or current token is an operator, comparison, or logical symbol
        if self_matches_peek!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
        ) {
            return Ok(Some(self.infix(lhs)?));
        } else if self_matches_current!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Ok(Some(self.infix(lhs)?));
        }

        Ok(Some(lhs))
    }

    fn unary(&mut self) -> Result<Option<Spanned<tree_node::Expression>>, Error> {
        macro_rules! matches_keyword {
            ($keyword:expr, $($pattern:pat),+) => {
                matches!($keyword, $($pattern)|+)
            };
        }

        let Some(current_token) = self.current_token.as_ref() else {
            return Ok(None);
        };

        if token_matches!(current_token, TokenType::EOF) {
            return Ok(None);
        }

        let expr = match current_token.token_type {
            TokenType::Keyword(e) if matches_keyword!(e, Keyword::Enum) => {
                return Err(Error::UnsupportedKeyword(
                    self.current_span(),
                    current_token.clone(),
                ));
            }

            TokenType::Keyword(Keyword::Let) => {
                // declaration is wrapped in spanned inside the function, but expects 'let' to be current
                Some(self.spanned(|p| p.declaration())?)
            }

            TokenType::Keyword(Keyword::Device) => {
                let spanned_dev = self.spanned(|p| p.device())?;
                Some(Spanned {
                    span: spanned_dev.span,
                    node: Expression::DeviceDeclaration(spanned_dev),
                })
            }

            TokenType::Keyword(Keyword::Fn) => {
                let spanned_fn = self.spanned(|p| p.function())?;
                Some(Spanned {
                    span: spanned_fn.span,
                    node: Expression::Function(spanned_fn),
                })
            }

            TokenType::Keyword(Keyword::If) => {
                let spanned_if = self.spanned(|p| p.if_expression())?;
                Some(Spanned {
                    span: spanned_if.span,
                    node: Expression::If(spanned_if),
                })
            }

            TokenType::Keyword(Keyword::Loop) => {
                let spanned_loop = self.spanned(|p| p.loop_expression())?;
                Some(Spanned {
                    span: spanned_loop.span,
                    node: Expression::Loop(spanned_loop),
                })
            }

            TokenType::Keyword(Keyword::While) => {
                let spanned_while = self.spanned(|p| p.while_expression())?;
                Some(Spanned {
                    span: spanned_while.span,
                    node: Expression::While(spanned_while),
                })
            }

            TokenType::Keyword(Keyword::Break) => {
                let span = self.current_span();
                // make sure the next token is a semi-colon
                let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(next),
                        next.clone(),
                    ));
                }
                Some(Spanned {
                    span,
                    node: Expression::Break(span),
                })
            }

            TokenType::Keyword(Keyword::Continue) => {
                let span = self.current_span();
                let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(next),
                        next.clone(),
                    ));
                }
                Some(Spanned {
                    span,
                    node: Expression::Continue(span),
                })
            }

            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                let spanned_call = self.spanned(|p| p.syscall())?;
                Some(Spanned {
                    span: spanned_call.span,
                    node: Expression::Syscall(spanned_call),
                })
            }

            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                let spanned_invoke = self.spanned(|p| p.invocation())?;
                Some(Spanned {
                    span: spanned_invoke.span,
                    node: Expression::Invocation(spanned_invoke),
                })
            }

            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::Assign)) =>
            {
                let spanned_assign = self.spanned(|p| p.assignment())?;
                Some(Spanned {
                    span: spanned_assign.span,
                    node: Expression::Assignment(spanned_assign),
                })
            }

            TokenType::Identifier(ref id) => {
                let span = self.current_span();
                Some(Spanned {
                    span,
                    node: Expression::Variable(Spanned {
                        span,
                        node: id.clone(),
                    }),
                })
            }

            TokenType::Symbol(Symbol::LBrace) => {
                let spanned_block = self.spanned(|p| p.block())?;
                Some(Spanned {
                    span: spanned_block.span,
                    node: Expression::Block(spanned_block),
                })
            }

            TokenType::Number(_) | TokenType::String(_) | TokenType::Boolean(_) => {
                let spanned_lit = self.spanned(|p| p.literal())?;
                Some(Spanned {
                    span: spanned_lit.span,
                    node: Expression::Literal(spanned_lit),
                })
            }

            TokenType::Symbol(Symbol::LParen) => {
                // Priority handles its own spanning
                self.spanned(|p| p.priority())?.node.map(|node| *node)
            }

            TokenType::Symbol(Symbol::Minus) => {
                // Need to handle span manually because unary call is next
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;
                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_expr.span.end_line,
                    end_col: inner_expr.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Negation(boxed!(inner_expr)),
                })
            }

            TokenType::Symbol(Symbol::LogicalNot) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;
                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_expr.span.end_line,
                    end_col: inner_expr.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Logical(Spanned {
                        span: combined_span,
                        node: LogicalExpression::Not(boxed!(inner_expr)),
                    }),
                })
            }

            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        Ok(expr)
    }

    fn get_infix_child_node(&mut self) -> Result<Spanned<tree_node::Expression>, Error> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;

        match current_token.token_type {
            TokenType::Number(_) | TokenType::Boolean(_) => {
                let lit = self.spanned(|p| p.literal())?;
                Ok(Spanned {
                    span: lit.span,
                    node: Expression::Literal(lit),
                })
            }
            TokenType::Identifier(ref ident)
                if !self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                let span = self.current_span();
                Ok(Spanned {
                    span,
                    node: Expression::Variable(Spanned {
                        span,
                        node: ident.clone(),
                    }),
                })
            }
            TokenType::Symbol(Symbol::LParen) => Ok(*self.spanned(|p| p.priority())?.node.unwrap()),
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                let inv = self.spanned(|p| p.invocation())?;
                Ok(Spanned {
                    span: inv.span,
                    node: Expression::Invocation(inv),
                })
            }
            TokenType::Symbol(Symbol::Minus) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Ok(Spanned {
                    span,
                    node: Expression::Negation(boxed!(inner)),
                })
            }
            TokenType::Symbol(Symbol::LogicalNot) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Ok(Spanned {
                    span,
                    node: Expression::Logical(Spanned {
                        span,
                        node: LogicalExpression::Not(boxed!(inner)),
                    }),
                })
            }
            _ => Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            )),
        }
    }

    fn device(&mut self) -> Result<DeviceDeclarationExpression, Error> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Device)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        let identifier_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(identifier_token),
                    identifier_token.clone(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        }

        let device_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let device = match device_token.token_type {
            TokenType::String(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(device_token),
                    device_token.clone(),
                ));
            }
        };

        Ok(DeviceDeclarationExpression {
            name: Spanned {
                span: identifier_span,
                node: identifier,
            },
            device,
        })
    }

    fn assignment(&mut self) -> Result<AssignmentExpression, Error> {
        let identifier_token = self.current_token.as_ref().unwrap();
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    self.current_token.clone().unwrap(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token.clone(),
            ));
        }
        self.assign_next()?;

        let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        Ok(AssignmentExpression {
            identifier: Spanned {
                span: identifier_span,
                node: identifier,
            },
            expression: boxed!(expression),
        })
    }

    fn infix(&mut self, previous: Spanned<Expression>) -> Result<Spanned<Expression>, Error> {
        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();

        match previous.node {
            Expression::Binary(_)
            | Expression::Logical(_)
            | Expression::Invocation(_)
            | Expression::Priority(_)
            | Expression::Literal(_)
            | Expression::Variable(_)
            | Expression::Negation(_) => {}
            _ => {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    String::from("Invalid expression for binary/logical operation"),
                ));
            }
        }

        let mut expressions = vec![previous];
        let mut operators = Vec::<Symbol>::new();

        let mut temp_token = current_token.clone();

        while token_matches!(
            temp_token,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
        ) {
            let operator = match temp_token.token_type {
                TokenType::Symbol(s) => s,
                _ => unreachable!(),
            };
            operators.push(operator);
            self.assign_next()?;
            expressions.push(self.get_infix_child_node()?);

            temp_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();
        }

        if operators.len() != expressions.len() - 1 {
            return Err(Error::InvalidSyntax(
                self.current_span(),
                String::from("Invalid number of operators"),
            ));
        }

        // --- PRECEDENCE LEVEL 1: Exponent (**) ---
        for (i, operator) in operators.iter().enumerate().rev() {
            if operator == &Symbol::Exp {
                let right = expressions.remove(i + 1);
                let left = expressions.remove(i);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };
                expressions.insert(
                    i,
                    Spanned {
                        span,
                        node: Expression::Binary(Spanned {
                            span,
                            node: BinaryExpression::Exponent(boxed!(left), boxed!(right)),
                        }),
                    },
                );
            }
        }
        operators.retain(|symbol| symbol != &Symbol::Exp);

        // Common macro for binary ops
        macro_rules! process_binary_ops {
            ($ops:pat, $variant:ident) => {
                let mut current_iteration = 0;
                for (i, operator) in operators.iter().enumerate() {
                    if matches!(operator, $ops) {
                        let index = i - current_iteration;
                        let left = expressions.remove(index);
                        let right = expressions.remove(index);
                        let span = Span {
                            start_line: left.span.start_line,
                            start_col: left.span.start_col,
                            end_line: right.span.end_line,
                            end_col: right.span.end_col,
                        };

                        let node = match operator {
                            Symbol::Asterisk => {
                                BinaryExpression::Multiply(boxed!(left), boxed!(right))
                            }
                            Symbol::Slash => BinaryExpression::Divide(boxed!(left), boxed!(right)),
                            Symbol::Percent => {
                                BinaryExpression::Modulo(boxed!(left), boxed!(right))
                            }
                            Symbol::Plus => BinaryExpression::Add(boxed!(left), boxed!(right)),
                            Symbol::Minus => {
                                BinaryExpression::Subtract(boxed!(left), boxed!(right))
                            }
                            _ => unreachable!(),
                        };

                        expressions.insert(
                            index,
                            Spanned {
                                span,
                                node: Expression::Binary(Spanned { span, node }),
                            },
                        );
                        current_iteration += 1;
                    }
                }
                operators.retain(|symbol| !matches!(symbol, $ops));
            };
        }

        // --- PRECEDENCE LEVEL 2: Multiplicative (*, /, %) ---
        process_binary_ops!(
            Symbol::Slash | Symbol::Asterisk | Symbol::Percent,
            BinaryExpression
        );

        // --- PRECEDENCE LEVEL 3: Additive (+, -) ---
        process_binary_ops!(Symbol::Plus | Symbol::Minus, BinaryExpression);

        // --- PRECEDENCE LEVEL 4: Comparison (<, >, <=, >=) ---
        let mut current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if operator.is_comparison() && !matches!(operator, Symbol::Equal | Symbol::NotEqual) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                let node = match operator {
                    Symbol::LessThan => LogicalExpression::LessThan(boxed!(left), boxed!(right)),
                    Symbol::GreaterThan => {
                        LogicalExpression::GreaterThan(boxed!(left), boxed!(right))
                    }
                    Symbol::LessThanOrEqual => {
                        LogicalExpression::LessThanOrEqual(boxed!(left), boxed!(right))
                    }
                    Symbol::GreaterThanOrEqual => {
                        LogicalExpression::GreaterThanOrEqual(boxed!(left), boxed!(right))
                    }
                    _ => unreachable!(),
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned { span, node }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| {
            !symbol.is_comparison() || matches!(symbol, Symbol::Equal | Symbol::NotEqual)
        });

        // --- PRECEDENCE LEVEL 5: Equality (==, !=) ---
        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::Equal | Symbol::NotEqual) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                let node = match operator {
                    Symbol::Equal => LogicalExpression::Equal(boxed!(left), boxed!(right)),
                    Symbol::NotEqual => LogicalExpression::NotEqual(boxed!(left), boxed!(right)),
                    _ => unreachable!(),
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned { span, node }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::Equal | Symbol::NotEqual));

        // --- PRECEDENCE LEVEL 6: Logical AND (&&) ---
        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::LogicalAnd) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned {
                            span,
                            node: LogicalExpression::And(boxed!(left), boxed!(right)),
                        }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::LogicalAnd));

        // --- PRECEDENCE LEVEL 7: Logical OR (||) ---
        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::LogicalOr) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned {
                            span,
                            node: LogicalExpression::Or(boxed!(left), boxed!(right)),
                        }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::LogicalOr));

        if expressions.len() != 1 || !operators.is_empty() {
            return Err(Error::InvalidSyntax(
                self.current_span(),
                String::from("Invalid number of operators"),
            ));
        }

        if token_matches!(
            temp_token,
            TokenType::Symbol(Symbol::Semicolon) | TokenType::Symbol(Symbol::RParen)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
        }

        Ok(expressions.pop().unwrap())
    }

    fn priority(&mut self) -> Result<Option<Box<Spanned<Expression>>>, Error> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        self.assign_next()?;
        let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        }

        Ok(Some(boxed!(expression)))
    }

    fn invocation(&mut self) -> Result<InvocationExpression, Error> {
        let identifier_token = self.current_token.as_ref().unwrap();
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    self.current_token.clone().unwrap(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        }

        let mut arguments = Vec::<Spanned<Expression>>::new();

        while !token_matches!(
            self.get_next()?.ok_or(Error::UnexpectedEOF)?,
            TokenType::Symbol(Symbol::RParen)
        ) {
            let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

            if let Expression::Block(_) = expression.node {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    String::from("Block expressions are not allowed in function invocations"),
                ));
            }

            arguments.push(expression);

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                let next_token = self.get_next()?.unwrap();
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(next_token),
                    next_token.clone(),
                ));
            }

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        Ok(InvocationExpression {
            name: Spanned {
                span: identifier_span,
                node: identifier,
            },
            arguments,
        })
    }

    fn block(&mut self) -> Result<BlockExpression, Error> {
        let mut expressions = Vec::<Spanned<Expression>>::new();
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;

        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        while !self_matches_peek!(
            self,
            TokenType::Symbol(Symbol::RBrace) | TokenType::Keyword(Keyword::Return)
        ) {
            let expression = self.parse()?.ok_or(Error::UnexpectedEOF)?;
            expressions.push(expression);
        }

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;

        if token_matches!(current_token, TokenType::Keyword(Keyword::Return)) {
            // Need to capture return span
            let ret_start_span = Self::token_to_span(current_token);
            self.assign_next()?;
            let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

            let ret_span = Span {
                start_line: ret_start_span.start_line,
                start_col: ret_start_span.start_col,
                end_line: expression.span.end_line,
                end_col: expression.span.end_col,
            };

            let return_expr = Spanned {
                span: ret_span,
                node: Expression::Return(boxed!(expression)),
            };
            expressions.push(return_expr);

            let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
            if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(next),
                    next.clone(),
                ));
            }

            let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
            if !token_matches!(next, TokenType::Symbol(Symbol::RBrace)) {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(next),
                    next.clone(),
                ));
            }
        }

        Ok(BlockExpression(expressions))
    }

    fn declaration(&mut self) -> Result<Expression, Error> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }
        let identifier_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(identifier_token),
                    identifier_token.clone(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();

        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token.clone(),
            ));
        }

        self.assign_next()?;
        let assignment_expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        }

        Ok(Expression::Declaration(
            Spanned {
                span: identifier_span,
                node: identifier,
            },
            boxed!(assignment_expression),
        ))
    }

    fn literal(&mut self) -> Result<Literal, Error> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        let literal = match current_token.token_type {
            TokenType::Number(num) => Literal::Number(num),
            TokenType::String(ref string) => Literal::String(string.clone()),
            TokenType::Boolean(boolean) => Literal::Boolean(boolean),
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        Ok(literal)
    }

    fn if_expression(&mut self) -> Result<IfExpression, Error> {
        // 'if' is current
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }

        let body = self.spanned(|p| p.block())?;

        let else_branch = if self_matches_peek!(self, TokenType::Keyword(Keyword::Else)) {
            self.assign_next()?;

            if self_matches_peek!(self, TokenType::Keyword(Keyword::If)) {
                self.assign_next()?;
                // Recurse for else if
                let if_expr = self.spanned(|p| p.if_expression())?;
                Some(boxed!(Spanned {
                    span: if_expr.span,
                    node: Expression::If(if_expr),
                }))
            } else if self_matches_peek!(self, TokenType::Symbol(Symbol::LBrace)) {
                self.assign_next()?;
                let block = self.spanned(|p| p.block())?;
                Some(boxed!(Spanned {
                    span: block.span,
                    node: Expression::Block(block),
                }))
            } else {
                let next = self.get_next()?.unwrap();
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(next),
                    next.clone(),
                ));
            }
        } else {
            None
        };

        Ok(IfExpression {
            condition: boxed!(condition),
            body,
            else_branch,
        })
    }

    fn loop_expression(&mut self) -> Result<LoopExpression, Error> {
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }

        let body = self.spanned(|p| p.block())?;

        Ok(LoopExpression { body })
    }

    fn while_expression(&mut self) -> Result<WhileExpression, Error> {
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(next),
                next.clone(),
            ));
        }

        let body = self.block()?;

        Ok(WhileExpression {
            condition: boxed!(condition),
            body,
        })
    }

    fn function(&mut self) -> Result<FunctionExpression, Error> {
        // 'fn' is current
        let fn_ident_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let fn_ident_span = Self::token_to_span(fn_ident_token);
        let fn_ident = match fn_ident_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(fn_ident_token),
                    fn_ident_token.clone(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        }

        let mut arguments = Vec::<Spanned<String>>::new();

        while !token_matches!(
            self.get_next()?.ok_or(Error::UnexpectedEOF)?,
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = self.current_token.as_ref().unwrap();
            let arg_span = Self::token_to_span(current_token);
            let argument = match current_token.token_type {
                TokenType::Identifier(ref id) => id.clone(),
                _ => {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(current_token),
                        current_token.clone(),
                    ));
                }
            };

            let spanned_arg = Spanned {
                span: arg_span,
                node: argument,
            };

            if arguments.contains(&spanned_arg) {
                return Err(Error::DuplicateIdentifier(
                    Self::token_to_span(current_token),
                    current_token.clone(),
                ));
            }

            arguments.push(spanned_arg);

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                let next = self.get_next()?.unwrap();
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(next),
                    next.clone(),
                ));
            }

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(current_token),
                current_token.clone(),
            ));
        };

        Ok(FunctionExpression {
            name: Spanned {
                span: fn_ident_span,
                node: fn_ident,
            },
            arguments,
            body: self.block()?,
        })
    }

    fn syscall(&mut self) -> Result<SysCall, Error> {
        fn check_length(
            parser: &Parser,
            arguments: &[Spanned<Expression>],
            length: usize,
        ) -> Result<(), Error> {
            if arguments.len() != length {
                return Err(Error::InvalidSyntax(
                    parser.current_span(),
                    format!("Expected {} arguments", length),
                ));
            }
            Ok(())
        }

        macro_rules! literal_or_variable {
            ($iter:expr) => {
                match $iter {
                    Some(expr) => match &expr.node {
                        Expression::Literal(literal) => {
                            LiteralOrVariable::Literal(literal.node.clone())
                        }
                        Expression::Variable(ident) => LiteralOrVariable::Variable(ident.clone()),
                        _ => {
                            return Err(Error::UnexpectedToken(
                                self.current_span(),
                                self.current_token.clone().unwrap(),
                            ))
                        }
                    },
                    _ => {
                        return Err(Error::UnexpectedToken(
                            self.current_span(),
                            self.current_token.clone().unwrap(),
                        ))
                    }
                }
            };
        }

        macro_rules! get_arg {
            ($matcher: ident, $arg: expr) => {
                match $arg {
                    LiteralOrVariable::$matcher(i) => i,
                    _ => {
                        return Err(Error::InvalidSyntax(
                            self.current_span(),
                            String::from("Expected a variable"),
                        ))
                    }
                }
            };
        }

        let invocation = self.invocation()?;

        match invocation.name.node.as_str() {
            "yield" => {
                check_length(self, &invocation.arguments, 0)?;
                Ok(SysCall::System(sys_call::System::Yield))
            }
            "sleep" => {
                check_length(self, &invocation.arguments, 1)?;
                let mut arg = invocation.arguments.into_iter();
                let expr = arg.next().unwrap();
                Ok(SysCall::System(System::Sleep(boxed!(expr))))
            }
            "hash" => {
                check_length(self, &invocation.arguments, 1)?;
                let mut args = invocation.arguments.into_iter();
                let lit_str = literal_or_variable!(args.next());

                let LiteralOrVariable::Literal(lit_str) = lit_str else {
                    return Err(Error::UnexpectedToken(
                        self.current_span(),
                        self.current_token.clone().unwrap(),
                    ));
                };

                Ok(SysCall::System(System::Hash(lit_str)))
            }
            "loadFromDevice" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.into_iter();

                let device = literal_or_variable!(args.next());
                let next_arg = args.next();

                let variable = match next_arg {
                    Some(expr) => match expr.node {
                        Expression::Literal(spanned_lit) => match spanned_lit.node {
                            Literal::String(s) => s,
                            _ => {
                                return Err(Error::UnexpectedToken(
                                    self.current_span(),
                                    self.current_token.clone().unwrap(),
                                ));
                            }
                        },
                        _ => {
                            return Err(Error::UnexpectedToken(
                                self.current_span(),
                                self.current_token.clone().unwrap(),
                            ));
                        }
                    },
                    _ => {
                        return Err(Error::UnexpectedToken(
                            self.current_span(),
                            self.current_token.clone().unwrap(),
                        ));
                    }
                };

                Ok(SysCall::System(sys_call::System::LoadFromDevice(
                    device,
                    Literal::String(variable),
                )))
            }
            // ... (implementing other syscalls similarly using patterns above)
            "setOnDevice" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.into_iter();
                let device = literal_or_variable!(args.next());
                let logic_type = get_arg!(Literal, literal_or_variable!(args.next()));
                let variable = args.next().unwrap();
                Ok(SysCall::System(sys_call::System::SetOnDevice(
                    device,
                    Literal::String(logic_type.to_string().replace("\"", "")),
                    boxed!(variable),
                )))
            }
            "setOnDeviceBatched" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.into_iter();
                let device_hash = literal_or_variable!(args.next());
                let logic_type = get_arg!(Literal, literal_or_variable!(args.next()));
                let variable = args.next().unwrap();
                Ok(SysCall::System(sys_call::System::SetOnDeviceBatched(
                    device_hash,
                    Literal::String(logic_type.to_string().replace("\"", "")),
                    boxed!(variable),
                )))
            }
            _ => {
                // For Math functions or unknown functions
                if SysCall::is_syscall(&invocation.name.node) {
                    // Attempt to parse as math if applicable, or error if strict
                    // Here we are falling back to simple handling or error.
                    // Since Math isn't fully expanded in this snippet, we return Unsupported.
                    Err(Error::UnsupportedKeyword(
                        self.current_span(),
                        self.current_token.clone().unwrap(),
                    ))
                } else {
                    Err(Error::UnsupportedKeyword(
                        self.current_span(),
                        self.current_token.clone().unwrap(),
                    ))
                }
            }
        }
    }
}
