pub mod sys_call;
#[cfg(test)]
mod test;
pub mod tree_node;
pub mod visitor;

use crate::sys_call::{Math, System};
use helpers::Span;
use std::{borrow::Cow, io::SeekFrom};
use sys_call::SysCall;
use tokenizer::{
    self, Tokenizer, TokenizerBuffer,
    token::{Keyword, Symbol, Token, TokenType},
};
use tree_node::*;

pub trait Documentation {
    fn docs(&self) -> String;
}

#[macro_export]
macro_rules! parse {
    ($input:expr) => {
        Parser::new(Tokenizer::from($input)).parse_all()
    };
}

#[macro_export]
/// A macro to create a boxed value.
macro_rules! boxed {
    ($e:expr) => {
        Box::new($e)
    };
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

mod calls;
mod control_flow;
mod declarations;
mod error;
mod expressions;
mod recovery;

pub use error::Error;

pub struct Parser<'a> {
    tokenizer: TokenizerBuffer<'a>,
    current_token: Option<Token<'a>>,
    last_token_span: Option<Span>,
    pub errors: Vec<Error<'a>>,
    /// Caches the most recent doc comment for attaching to the next declaration
    cached_doc_comment: Option<String>,
    /// Maps variable/declaration names to their doc comments
    pub declaration_docs: std::collections::HashMap<String, String>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Parser {
            tokenizer: TokenizerBuffer::new(tokenizer),
            current_token: None,
            last_token_span: None,
            errors: Vec::new(),
            cached_doc_comment: None,
            declaration_docs: std::collections::HashMap::new(),
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

    /// Pops and returns the cached doc comment, if any
    pub fn pop_doc_comment(&mut self) -> Option<String> {
        self.cached_doc_comment.take()
    }

    /// Caches a doc comment for attachment to the next declaration
    pub fn cache_doc_comment(&mut self, comment: String) {
        // multi-line doc comments need to be built up across multiple tokens,
        // so we use a buffer to accumulate them until we have no more doc comments to read
        if let Some(existing_comment) = &mut self.cached_doc_comment {
            existing_comment.push('\n');
            existing_comment.push_str(&comment);
        } else {
            self.cached_doc_comment = Some(comment);
        }
    }

    /// Stores a doc comment for a declaration (by name)
    pub fn store_declaration_doc(&mut self, name: String, doc: String) {
        self.declaration_docs.insert(name, doc);
    }

    /// Retrieves and removes a doc comment for a declaration
    pub fn get_declaration_doc(&mut self, name: &str) -> Option<String> {
        self.declaration_docs.get(name).cloned()
    }

    fn unexpected_eof(&self) -> Error<'a> {
        Error::UnexpectedEOF(self.last_token_span)
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

    pub fn parse_all(&mut self) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
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

        Ok(Some(Spanned {
            node: Expression::Block(Spanned {
                node: BlockExpression(expressions),
                span,
            }),
            span,
        }))
    }

    pub fn parse(&mut self) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        self.assign_next()?;

        while self_matches_current!(self, TokenType::Newline) {
            self.assign_next()?;
        }

        if self.current_token.is_none() {
            return Ok(None);
        }

        let expr = self.expression()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) {
            self.assign_next()?;
        }

        Ok(expr)
    }
}
