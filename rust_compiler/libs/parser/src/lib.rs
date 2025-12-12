pub mod sys_call;
#[cfg(test)]
mod test;
pub mod tree_node;

use crate::sys_call::{Math, System};
use helpers::Span;
use std::{borrow::Cow, io::SeekFrom};
use sys_call::SysCall;
use thiserror::Error;
use tokenizer::{
    self, Tokenizer, TokenizerBuffer,
    token::{Keyword, Symbol, Token, TokenType},
};
use tree_node::*;

pub trait Documentation {
    fn docs(&self) -> String;
}

#[macro_export]
/// A macro to create a boxed value.
macro_rules! boxed {
    ($e:expr) => {
        Box::new($e)
    };
}

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error(transparent)]
    Tokenizer(#[from] tokenizer::Error),

    #[error("Unexpected token: {1}")]
    UnexpectedToken(Span, Token<'a>),

    #[error("Duplicate identifier: {1}")]
    DuplicateIdentifier(Span, Token<'a>),

    #[error("Invalid Syntax: {1}")]
    InvalidSyntax(Span, String),

    #[error("Unsupported Keyword: {1}")]
    UnsupportedKeyword(Span, Token<'a>),

    #[error("Unexpected End of File")]
    UnexpectedEOF,
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            Tokenizer(e) => e.into(),
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
    current_token: Option<Token<'a>>,
    pub errors: Vec<Error<'a>>,
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
    fn token_to_span(t: &Token<'a>) -> Span {
        Span {
            start_line: t.line,
            start_col: t.span.start,
            end_line: t.line,
            end_col: t.span.end,
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
    fn spanned<F, T>(&mut self, parser: F) -> Result<Spanned<T>, Error<'a>>
    where
        F: FnOnce(&mut Self) -> Result<T, Error<'a>>,
    {
        let start_token = if self.current_token.is_some() {
            self.current_token.clone()
        } else {
            self.tokenizer.peek()?
        };

        let (start_line, start_col) = start_token
            .as_ref()
            .map(|t| (t.line, t.span.start))
            .unwrap_or((0, 0));

        let node = parser(self)?;

        let end_token = &self.current_token;

        let (end_line, end_col) = end_token
            .clone()
            .map(|t| (t.line, t.span.end))
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

    fn synchronize(&mut self) -> Result<(), Error<'a>> {
        self.assign_next()?;

        while let Some(token) = &self.current_token {
            if token.token_type == TokenType::Symbol(Symbol::Semicolon) {
                self.assign_next()?;
                return Ok(());
            }

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

    pub fn parse_all(&mut self) -> Result<Option<tree_node::Expression<'a>>, Error<'a>> {
        let first_token = self.tokenizer.peek().unwrap_or(None);
        let (start_line, start_col) = first_token
            .as_ref()
            .map(|tok| (tok.line, tok.span.start))
            .unwrap_or((0, 0));

        let mut expressions = Vec::<Spanned<Expression<'a>>>::new();

        loop {
            match self.tokenizer.peek() {
                Ok(None) => break,
                Err(e) => {
                    self.errors.push(Error::Tokenizer(e));
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
                    if self.synchronize().is_err() {
                        break;
                    }
                }
            }
        }

        let end_token_opt = self.tokenizer.peek().unwrap_or(None);
        let (end_line, end_col) = end_token_opt
            .map(|tok| (tok.line, tok.span.end))
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

    pub fn parse(&mut self) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        self.assign_next()?;

        if self.current_token.is_none() {
            return Ok(None);
        }

        let expr = self.expression()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) {
            self.assign_next()?;
        }

        Ok(expr)
    }

    fn assign_next(&mut self) -> Result<(), Error<'a>> {
        self.current_token = self.tokenizer.next_token()?;
        Ok(())
    }

    fn get_next(&mut self) -> Result<Option<Token<'a>>, Error<'a>> {
        self.assign_next()?;
        Ok(self.current_token.clone())
    }

    fn expression(&mut self) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        // Parse the Left Hand Side (unary/primary expression)
        let lhs = self.unary()?;

        let Some(lhs) = lhs else {
            return Ok(None);
        };

        // Handle Postfix operators (Member Access, Method Call) immediately after unary
        let lhs = self.parse_postfix(lhs)?;

        // Handle Infix operators (Binary, Logical, Assignment)
        if self_matches_peek!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || matches!(s, Symbol::Assign | Symbol::Question)
        ) {
            return Ok(Some(self.infix(lhs)?));
        } else if self_matches_current!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || matches!(s, Symbol::Assign | Symbol::Question)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Ok(Some(self.infix(lhs)?));
        }

        Ok(Some(lhs))
    }

    /// Handles dot notation chains: x.y.z()
    fn parse_postfix(
        &mut self,
        mut lhs: Spanned<Expression<'a>>,
    ) -> Result<Spanned<Expression<'a>>, Error<'a>> {
        loop {
            if self_matches_peek!(self, TokenType::Symbol(Symbol::Dot)) {
                self.assign_next()?; // consume Dot

                let identifier_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                let identifier_span = Self::token_to_span(&identifier_token);
                let identifier = match identifier_token.token_type {
                    TokenType::Identifier(ref id) => id.clone(),
                    _ => {
                        return Err(Error::UnexpectedToken(
                            identifier_span,
                            identifier_token.clone(),
                        ));
                    }
                };

                // Check for Method Call '()'
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) {
                    // Method Call
                    self.assign_next()?; // consume '('
                    let mut arguments = Vec::<Spanned<Expression<'a>>>::new();

                    while !token_matches!(
                        self.get_next()?.ok_or(Error::UnexpectedEOF)?,
                        TokenType::Symbol(Symbol::RParen)
                    ) {
                        let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

                        // Block expressions not allowed in args
                        if let Expression::Block(_) = expression.node {
                            return Err(Error::InvalidSyntax(
                                self.current_span(),
                                String::from("Block expressions are not allowed in method calls"),
                            ));
                        }
                        arguments.push(expression);

                        if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                            && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
                        {
                            let next_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                            return Err(Error::UnexpectedToken(
                                Self::token_to_span(&next_token),
                                next_token,
                            ));
                        }

                        if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                            self.assign_next()?;
                        }
                    }

                    // End span is the ')'
                    let end_span = self.current_span();
                    let combined_span = Span {
                        start_line: lhs.span.start_line,
                        start_col: lhs.span.start_col,
                        end_line: end_span.end_line,
                        end_col: end_span.end_col,
                    };

                    lhs = Spanned {
                        span: combined_span,
                        node: Expression::MethodCall(Spanned {
                            span: combined_span,
                            node: MethodCallExpression {
                                object: boxed!(lhs),
                                method: Spanned {
                                    span: identifier_span,
                                    node: identifier,
                                },
                                arguments,
                            },
                        }),
                    };
                } else {
                    // Member Access
                    let combined_span = Span {
                        start_line: lhs.span.start_line,
                        start_col: lhs.span.start_col,
                        end_line: identifier_span.end_line,
                        end_col: identifier_span.end_col,
                    };

                    lhs = Spanned {
                        span: combined_span,
                        node: Expression::MemberAccess(Spanned {
                            span: combined_span,
                            node: MemberAccessExpression {
                                object: boxed!(lhs),
                                member: Spanned {
                                    span: identifier_span,
                                    node: identifier,
                                },
                            },
                        }),
                    };
                }
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn unary(&mut self) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
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

            TokenType::Keyword(Keyword::Let) => Some(self.spanned(|p| p.declaration())?),

            TokenType::Keyword(Keyword::Device) => {
                let spanned_dev = self.spanned(|p| p.device())?;
                Some(Spanned {
                    span: spanned_dev.span,
                    node: Expression::DeviceDeclaration(spanned_dev),
                })
            }

            TokenType::Keyword(Keyword::Const) => {
                let spanned_const = self.spanned(|p| p.const_declaration())?;

                Some(Spanned {
                    span: spanned_const.span,
                    node: Expression::ConstDeclaration(spanned_const),
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
                let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
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
                    return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
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
                self.spanned(|p| p.priority())?.node.map(|node| *node)
            }

            TokenType::Symbol(Symbol::Minus) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;

                let inner_with_postfix = self.parse_postfix(inner_expr)?;

                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_with_postfix.span.end_line,
                    end_col: inner_with_postfix.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Negation(boxed!(inner_with_postfix)),
                })
            }

            TokenType::Symbol(Symbol::LogicalNot) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;
                let inner_with_postfix = self.parse_postfix(inner_expr)?;
                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_with_postfix.span.end_line,
                    end_col: inner_with_postfix.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Logical(Spanned {
                        span: combined_span,
                        node: LogicalExpression::Not(boxed!(inner_with_postfix)),
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

    fn get_infix_child_node(&mut self) -> Result<Spanned<tree_node::Expression<'a>>, Error<'a>> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;

        let start_span = self.current_span();

        let expr = match current_token.token_type {
            TokenType::Number(_) | TokenType::Boolean(_) => {
                let lit = self.spanned(|p| p.literal())?;
                Spanned {
                    span: lit.span,
                    node: Expression::Literal(lit),
                }
            }
            TokenType::Identifier(ref ident)
                if !self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                // This is a Variable. We need to check for Postfix operations on it.
                let span = self.current_span();
                Spanned {
                    span,
                    node: Expression::Variable(Spanned {
                        span,
                        node: ident.clone(),
                    }),
                }
            }
            TokenType::Symbol(Symbol::LParen) => *self
                .spanned(|p| p.priority())?
                .node
                .ok_or(Error::UnexpectedEOF)?,

            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                let spanned_call = self.spanned(|p| p.syscall())?;

                Spanned {
                    span: spanned_call.span,
                    node: Expression::Syscall(spanned_call),
                }
            }

            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                let inv = self.spanned(|p| p.invocation())?;
                Spanned {
                    span: inv.span,
                    node: Expression::Invocation(inv),
                }
            }
            TokenType::Symbol(Symbol::Minus) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Spanned {
                    span,
                    node: Expression::Negation(boxed!(inner)),
                }
            }
            TokenType::Symbol(Symbol::LogicalNot) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Spanned {
                    span,
                    node: Expression::Logical(Spanned {
                        span,
                        node: LogicalExpression::Not(boxed!(inner)),
                    }),
                }
            }
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        // Important: We must check for postfix operations here too
        // e.g. a + b.c
        self.parse_postfix(expr)
    }

    fn device(&mut self) -> Result<DeviceDeclarationExpression<'a>, Error<'a>> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Device)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        let identifier_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let identifier_span = Self::token_to_span(&identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&identifier_token),
                    identifier_token.clone(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token.clone(),
            ));
        }

        let device_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let device = match device_token.token_type {
            TokenType::String(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&device_token),
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

    fn infix(
        &mut self,
        previous: Spanned<Expression<'a>>,
    ) -> Result<Spanned<Expression<'a>>, Error<'a>> {
        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();

        match previous.node {
            Expression::Binary(_)
            | Expression::Logical(_)
            | Expression::Invocation(_)
            | Expression::Syscall(_)
            | Expression::Priority(_)
            | Expression::Literal(_)
            | Expression::Variable(_)
            | Expression::Ternary(_)
            | Expression::Negation(_)
            | Expression::MemberAccess(_)
            | Expression::MethodCall(_) => {}
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

        // Include Assign in the operator loop
        while token_matches!(
            temp_token,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || matches!(s, Symbol::Assign | Symbol::Question | Symbol::Colon)
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

        // -- PRECEDENCE LEVEL 8: Ternary (x ? 1 : 2)
        for i in (0..operators.len()).rev() {
            if matches!(operators[i], Symbol::Question) {
                // Ensure next operator is a colon
                if i + 1 >= operators.len() || !matches!(operators[i + 1], Symbol::Colon) {
                    return Err(Error::InvalidSyntax(
                        self.current_span(),
                        "Ternary operator '?' missing matching ':'".to_string(),
                    ));
                }

                let false_branch = expressions.remove(i + 2);
                let true_branch = expressions.remove(i + 1);
                let condition = expressions.remove(i);

                let span = Span {
                    start_line: condition.span.start_line,
                    end_line: false_branch.span.end_line,
                    start_col: condition.span.start_col,
                    end_col: false_branch.span.end_col,
                };

                let ternary_node = Spanned {
                    span,
                    node: TernaryExpression {
                        condition: Box::new(condition),
                        true_value: Box::new(true_branch),
                        false_value: Box::new(false_branch),
                    },
                };

                expressions.insert(
                    i,
                    Spanned {
                        node: Expression::Ternary(ternary_node),
                        span,
                    },
                );

                // Remove the `?` and the `:` from the operators list
                operators.remove(i);
                operators.remove(i);
            }
        }

        // --- PRECEDENCE LEVEL 9: Assignment (=) ---
        // Assignment is Right Associative: a = b = c => a = (b = c)
        // We iterate Right to Left
        for (i, operator) in operators.iter().enumerate().rev() {
            if matches!(operator, Symbol::Assign) {
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
                        node: Expression::Assignment(Spanned {
                            span,
                            node: AssignmentExpression {
                                assignee: boxed!(left),
                                expression: boxed!(right),
                            },
                        }),
                    },
                );
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::Assign));

        if expressions.len() != 1 || !operators.is_empty() {
            return Err(Error::InvalidSyntax(
                self.current_span(),
                String::from("Invalid number of operators"),
            ));
        }

        if token_matches!(
            temp_token,
            TokenType::Symbol(Symbol::Semicolon)
                | TokenType::Symbol(Symbol::RParen)
                | TokenType::Symbol(Symbol::Comma)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
        }

        expressions.pop().ok_or(Error::UnexpectedEOF)
    }

    fn priority(&mut self) -> Result<Option<Box<Spanned<Expression<'a>>>>, Error<'a>> {
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
                Self::token_to_span(&current_token),
                current_token,
            ));
        }

        Ok(Some(boxed!(expression)))
    }

    fn invocation(&mut self) -> Result<InvocationExpression<'a>, Error<'a>> {
        let identifier_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    self.current_token.clone().ok_or(Error::UnexpectedEOF)?,
                ));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
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
                let next_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&next_token),
                    next_token,
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

    fn block(&mut self) -> Result<BlockExpression<'a>, Error<'a>> {
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
            let ret_start_span = Self::token_to_span(&current_token);
            self.assign_next()?;

            let expr = if token_matches!(
                self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?,
                TokenType::Symbol(Symbol::Semicolon)
            ) {
                // rewind 1 token so we can check for the semicolon at the bottom of this function.
                self.tokenizer.seek(SeekFrom::Current(-1))?;
                None
            } else {
                Some(self.expression()?.ok_or(Error::UnexpectedEOF)?)
            };

            let ret_span = Span {
                start_line: ret_start_span.start_line,
                start_col: ret_start_span.start_col,
                end_line: expr
                    .as_ref()
                    .map(|e| e.span.end_line)
                    .unwrap_or(ret_start_span.end_line),
                end_col: expr
                    .as_ref()
                    .map(|e| e.span.end_col)
                    .unwrap_or(ret_start_span.end_col),
            };

            let return_expr = Spanned {
                span: ret_span,
                node: Expression::Return(expr.map(Box::new)),
            };
            expressions.push(return_expr);

            let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
            if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }

            let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
            if !token_matches!(next, TokenType::Symbol(Symbol::RBrace)) {
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }
        }

        Ok(BlockExpression(expressions))
    }

    fn const_declaration(&mut self) -> Result<ConstDeclarationExpression<'a>, Error<'a>> {
        // const
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Const)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        // variable_name
        let ident_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let ident_span = Self::token_to_span(&ident_token);
        let ident = match ident_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => return Err(Error::UnexpectedToken(ident_span, ident_token.clone())),
        };

        // `=`
        let assign_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?.clone();
        if !token_matches!(assign_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&assign_token),
                assign_token,
            ));
        }

        // literal or syscall, making sure the syscall is supported in hash
        self.assign_next()?;
        // cache the current token location
        let current_token_index = self.tokenizer.loc();

        if let Ok(lit) = self.spanned(|p| p.literal()) {
            Ok(ConstDeclarationExpression {
                name: Spanned {
                    span: ident_span,
                    node: ident,
                },
                value: LiteralOr::Literal(lit),
            })
        } else {
            // we need to rewind our tokenizer to our previous location
            self.tokenizer.seek(SeekFrom::Current(
                self.tokenizer.loc() - current_token_index,
            ))?;
            let syscall = self.spanned(|p| p.syscall())?;

            if !matches!(
                syscall,
                Spanned {
                    node: SysCall::System(sys_call::System::Hash(_)),
                    ..
                }
            ) {
                return Err(Error::UnexpectedToken(
                    syscall.span,
                    self.current_token.clone().ok_or(Error::UnexpectedEOF)?,
                ));
            }

            Ok(ConstDeclarationExpression {
                name: Spanned {
                    span: ident_span,
                    node: ident,
                },
                value: LiteralOr::Or(syscall),
            })
        }
    }

    fn declaration(&mut self) -> Result<Expression<'a>, Error<'a>> {
        let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }
        let identifier_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let identifier_span = Self::token_to_span(&identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&identifier_token),
                    identifier_token,
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
                Self::token_to_span(&current_token),
                current_token,
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

    fn literal(&mut self) -> Result<Literal<'a>, Error<'a>> {
        let current_token = self.current_token.clone().ok_or(Error::UnexpectedEOF)?;
        let literal = match current_token.token_type {
            TokenType::Number(num) => Literal::Number(num),
            TokenType::String(ref string) => Literal::String(string.clone()),
            TokenType::Boolean(boolean) => Literal::Boolean(boolean),
            TokenType::Symbol(Symbol::Minus) => match self.get_next()? {
                Some(Token {
                    token_type: TokenType::Number(num),
                    ..
                }) => Literal::Number(-num),
                Some(wrong_token) => {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(&wrong_token),
                        wrong_token,
                    ));
                }
                None => return Err(Error::UnexpectedEOF),
            },
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        Ok(literal)
    }

    fn if_expression(&mut self) -> Result<IfExpression<'a>, Error<'a>> {
        // 'if' is current
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
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
                let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
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

    fn loop_expression(&mut self) -> Result<LoopExpression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let body = self.spanned(|p| p.block())?;

        Ok(LoopExpression { body })
    }

    fn while_expression(&mut self) -> Result<WhileExpression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let body = self.block()?;

        Ok(WhileExpression {
            condition: boxed!(condition),
            body,
        })
    }

    fn function(&mut self) -> Result<FunctionExpression<'a>, Error<'a>> {
        // 'fn' is current
        let fn_ident_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        let fn_ident_span = Self::token_to_span(&fn_ident_token);
        let fn_ident = match fn_ident_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(fn_ident_span, fn_ident_token));
            }
        };

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
            ));
        }

        let mut arguments = Vec::<Spanned<Cow<'a, str>>>::new();

        while !token_matches!(
            self.get_next()?.ok_or(Error::UnexpectedEOF)?,
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = self.current_token.as_ref().ok_or(Error::UnexpectedEOF)?;
            let arg_span = Self::token_to_span(current_token);
            let argument = match current_token.token_type {
                TokenType::Identifier(ref id) => id.clone(),
                _ => {
                    return Err(Error::UnexpectedToken(arg_span, current_token.clone()));
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
                let next = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        let current_token = self.get_next()?.ok_or(Error::UnexpectedEOF)?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
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

    fn syscall(&mut self) -> Result<SysCall<'a>, Error<'a>> {
        let invocation = self.invocation()?;

        let check_length = |len: usize| -> Result<(), Error> {
            if invocation.arguments.len() != len {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    format!("Expected {} arguments", len),
                ));
            }
            Ok(())
        };

        macro_rules! args {
            ($count:expr) => {{
                check_length($count)?;
                invocation.arguments.into_iter()
            }};
        }

        macro_rules! literal_or_variable {
            ($iter:expr) => {
                match &$iter {
                    Some(expr) => {
                        let span = expr.span;
                        match &expr.node {
                            Expression::Literal(literal) => Spanned {
                                span,
                                node: LiteralOrVariable::Literal(literal.node.clone()),
                            },
                            Expression::Variable(ident) => Spanned {
                                span,
                                node: LiteralOrVariable::Variable(ident.clone()),
                            },
                            _ => {
                                return Err(Error::InvalidSyntax(
                                    expr.span,
                                    "Expected a literal or variable".to_string(),
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(Error::UnexpectedToken(
                            self.current_span(),
                            self.current_token.clone().ok_or(Error::UnexpectedEOF)?,
                        ))
                    }
                }
            };
        }

        macro_rules! get_arg {
            ($matcher: ident, $arg: expr) => {
                match $arg.node {
                    LiteralOrVariable::$matcher(i) => Spanned {
                        node: i,
                        span: $arg.span,
                    },
                    _ => {
                        return Err(Error::InvalidSyntax(
                            $arg.span,
                            format!("Expected a {}", stringify!($matcher).to_lowercase()),
                        ))
                    }
                }
            };
        }

        match invocation.name.node.as_ref() {
            // System SysCalls
            "yield" => {
                check_length(0)?;
                Ok(SysCall::System(sys_call::System::Yield))
            }
            "sleep" => {
                let mut args = args!(1);
                let expr = args.next().ok_or(Error::UnexpectedEOF)?;
                Ok(SysCall::System(System::Sleep(boxed!(expr))))
            }
            "hash" => {
                let mut args = args!(1);
                let lit_str = literal_or_variable!(args.next());

                let Spanned {
                    node: LiteralOrVariable::Literal(lit_str),
                    span,
                } = lit_str
                else {
                    return Err(Error::InvalidSyntax(
                        lit_str.span,
                        "Expected a string literal".to_string(),
                    ));
                };

                Ok(SysCall::System(System::Hash(Spanned {
                    node: lit_str,
                    span,
                })))
            }
            "load" | "l" => {
                let mut args = args!(2);

                let tmp = args.next();
                let device = literal_or_variable!(tmp);
                let next_arg = args.next();

                let variable = match next_arg {
                    Some(expr) => match expr.node {
                        Expression::Literal(spanned_lit) => match spanned_lit.node {
                            Literal::String(s) => Spanned {
                                node: s,
                                span: spanned_lit.span,
                            },
                            _ => {
                                return Err(Error::InvalidSyntax(
                                    spanned_lit.span,
                                    "Expected a string literal".to_string(),
                                ));
                            }
                        },
                        _ => {
                            return Err(Error::InvalidSyntax(
                                expr.span,
                                "Expected a string literal".to_string(),
                            ));
                        }
                    },
                    _ => {
                        return Err(Error::UnexpectedToken(
                            self.current_span(),
                            self.current_token.clone().ok_or(Error::UnexpectedEOF)?,
                        ));
                    }
                };

                Ok(SysCall::System(sys_call::System::LoadFromDevice(
                    device,
                    Spanned {
                        node: Literal::String(variable.node),
                        span: variable.span,
                    },
                )))
            }
            "loadBatched" | "lb" => {
                let mut args = args!(3);
                let tmp = args.next();
                let device_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let logic_type = get_arg!(Literal, literal_or_variable!(tmp));

                let tmp = args.next();
                let batch_mode = get_arg!(Literal, literal_or_variable!(tmp));

                Ok(SysCall::System(System::LoadBatch(
                    device_hash,
                    logic_type,
                    batch_mode,
                )))
            }
            "loadBatchedNamed" | "lbn" => {
                let mut args = args!(4);
                let tmp = args.next();
                let dev_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let name_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let logic_type = get_arg!(Literal, literal_or_variable!(tmp));

                let tmp = args.next();
                let batch_mode = get_arg!(Literal, literal_or_variable!(tmp));

                Ok(SysCall::System(System::LoadBatchNamed(
                    dev_hash, name_hash, logic_type, batch_mode,
                )))
            }
            "set" | "s" => {
                let mut args = args!(3);
                let tmp = args.next();
                let device = literal_or_variable!(tmp);

                let tmp = args.next();
                let logic_type = get_arg!(Literal, literal_or_variable!(tmp));

                let variable = args.next().ok_or(Error::UnexpectedEOF)?;
                Ok(SysCall::System(sys_call::System::SetOnDevice(
                    device,
                    Spanned {
                        node: Literal::String(Cow::from(
                            logic_type.node.to_string().replace("\"", ""),
                        )),
                        span: logic_type.span,
                    },
                    boxed!(variable),
                )))
            }
            "setBatched" | "sb" => {
                let mut args = args!(3);
                let tmp = args.next();
                let device_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let logic_type = get_arg!(Literal, literal_or_variable!(tmp));
                let variable = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::System(sys_call::System::SetOnDeviceBatched(
                    device_hash,
                    Spanned {
                        node: Literal::String(Cow::from(logic_type.to_string().replace("\"", ""))),
                        span: logic_type.span,
                    },
                    boxed!(variable),
                )))
            }
            "setBatchedNamed" | "sbn" => {
                let mut args = args!(4);
                let tmp = args.next();
                let device_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let name_hash = literal_or_variable!(tmp);

                let tmp = args.next();
                let logic_type = get_arg!(Literal, literal_or_variable!(tmp));

                let tmp = args.next();
                let expr = Box::new(tmp.ok_or(Error::UnexpectedEOF)?);

                Ok(SysCall::System(System::SetOnDeviceBatchedNamed(
                    device_hash,
                    name_hash,
                    logic_type,
                    expr,
                )))
            }
            "loadSlot" | "ls" => {
                let mut args = args!(3);
                let next = args.next();
                let dev_name = literal_or_variable!(next);
                let next = args.next();
                let slot_index = get_arg!(Literal, literal_or_variable!(next));
                if !matches!(
                    slot_index,
                    Spanned {
                        node: Literal::Number(_),
                        ..
                    },
                ) {
                    return Err(Error::InvalidSyntax(
                        slot_index.span,
                        "Expected a number".to_string(),
                    ));
                }
                let next = args.next();
                let slot_logic = get_arg!(Literal, literal_or_variable!(next));
                if !matches!(
                    slot_logic,
                    Spanned {
                        node: Literal::String(_),
                        ..
                    }
                ) {
                    return Err(Error::InvalidSyntax(
                        slot_logic.span,
                        "Expected a String".into(),
                    ));
                }

                Ok(SysCall::System(System::LoadSlot(
                    dev_name, slot_index, slot_logic,
                )))
            }
            "setSlot" | "ss" => {
                let mut args = args!(4);
                let next = args.next();
                let dev_name = literal_or_variable!(next);
                let next = args.next();
                let slot_index = get_arg!(Literal, literal_or_variable!(next));
                if !matches!(
                    slot_index,
                    Spanned {
                        node: Literal::Number(_),
                        ..
                    }
                ) {
                    return Err(Error::InvalidSyntax(
                        slot_index.span,
                        "Expected a number".into(),
                    ));
                }
                let next = args.next();
                let slot_logic = get_arg!(Literal, literal_or_variable!(next));
                if !matches!(
                    slot_logic,
                    Spanned {
                        node: Literal::String(_),
                        ..
                    }
                ) {
                    return Err(Error::InvalidSyntax(
                        slot_logic.span,
                        "Expected a string".into(),
                    ));
                }
                let next = args.next();
                let expr = next.ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::System(System::SetSlot(
                    dev_name,
                    slot_index,
                    slot_logic,
                    Box::new(expr),
                )))
            }

            // Math SysCalls
            "acos" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let tmp = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Acos(boxed!(tmp))))
            }
            "asin" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let tmp = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Asin(boxed!(tmp))))
            }
            "atan" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let expr = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Atan(boxed!(expr))))
            }
            "atan2" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or(Error::UnexpectedEOF)?;
                let arg2 = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Atan2(boxed!(arg1), boxed!(arg2))))
            }
            "abs" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let expr = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Abs(boxed!(expr))))
            }
            "ceil" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Ceil(boxed!(arg))))
            }
            "cos" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Cos(boxed!(arg))))
            }
            "floor" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Floor(boxed!(arg))))
            }
            "log" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Log(boxed!(arg))))
            }
            "max" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or(Error::UnexpectedEOF)?;
                let arg2 = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Max(boxed!(arg1), boxed!(arg2))))
            }
            "min" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or(Error::UnexpectedEOF)?;
                let arg2 = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Min(boxed!(arg1), boxed!(arg2))))
            }
            "rand" => {
                check_length(0)?;
                Ok(SysCall::Math(Math::Rand))
            }
            "sin" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Sin(boxed!(arg))))
            }
            "sqrt" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Sqrt(boxed!(arg))))
            }
            "tan" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Tan(boxed!(arg))))
            }
            "trunc" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or(Error::UnexpectedEOF)?;

                Ok(SysCall::Math(Math::Trunc(boxed!(arg))))
            }
            _ => Err(Error::UnsupportedKeyword(
                self.current_span(),
                self.current_token.clone().ok_or(Error::UnexpectedEOF)?,
            )),
        }
    }
}
