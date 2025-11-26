#[cfg(test)]
mod test;

pub mod sys_call;
pub mod tree_node;

use quick_error::quick_error;
use std::io::SeekFrom;
use sys_call::SysCall;
use tokenizer::{
    self, Tokenizer, TokenizerBuffer,
    token::{Keyword, Symbol, Token, TokenType},
};
use tree_node::*;

use crate::sys_call::System;

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
        UnexpectedToken(token: Token) {
            display("Unexpected token: {:?}", token)
        }
        DuplicateIdentifier(token: Token) {
            display("Duplicate identifier: {:?}", token)
        }
        InvalidSyntax(token: Token, reason: String) {
            display("Invalid syntax: {:?}, Reason: {}", token, reason)
        }
        UnsupportedKeyword(token: Token) {
            display("Unsupported keyword: {:?}", token)
        }
        UnexpectedEOF {
            display("Unexpected EOF")
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

macro_rules! token_from_option {
    ($token:expr) => {
        match $token {
            Some(ref token) => token.clone(),
            None => return Err(Error::UnexpectedEOF),
        }
    };
    (owned $token:expr) => {
        match $token {
            Some(token) => token,
            None => return Err(Error::UnexpectedEOF),
        }
    };
}

macro_rules! extract_token_data {
    ($token:ident, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => return Err(Error::UnexpectedToken($token.clone())),
        }
    };
    ($token:expr, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => {
                return Err(Error::UnexpectedToken($token.clone()));
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
    pub fn parse_all(&mut self) -> Result<Option<tree_node::Expression>, Error> {
        let mut expressions = Vec::<Expression>::new();

        while let Some(expression) = self.parse()? {
            expressions.push(expression);
        }

        Ok(Some(Expression::Block(BlockExpression(expressions))))
    }

    /// Parses the input from the tokenizer buffer and returns the resulting expression
    pub fn parse(&mut self) -> Result<Option<tree_node::Expression>, Error> {
        self.assign_next()?;
        let expr = self.expression()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) {
            self.assign_next()?;
        }

        Ok(expr)
    }

    /// Assigns the next token in the tokenizer buffer to the current token
    fn assign_next(&mut self) -> Result<(), Error> {
        self.current_token = self.tokenizer.next_token()?;
        Ok(())
    }

    /// Calls `assign_next` and returns the next token in the tokenizer buffer
    fn get_next(&mut self) -> Result<Option<&Token>, Error> {
        self.assign_next()?;
        Ok(self.current_token.as_ref())
    }

    /// Parses an expression, handling binary operations with correct precedence.
    fn expression(&mut self) -> Result<Option<tree_node::Expression>, Error> {
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
        }
        // This is an edge case. We need to move back one token if the current token is an operator
        // so the binary expression can pick up the operator
        else if self_matches_current!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Ok(Some(self.infix(lhs)?));
        }

        Ok(Some(lhs))
    }

    /// Parses a unary or primary expression.
    /// This handles prefix operators (like negation) and atomic expressions (literals, variables, etc.),
    /// but stops before consuming binary operators.
    fn unary(&mut self) -> Result<Option<tree_node::Expression>, Error> {
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
            // match unsupported keywords
            TokenType::Keyword(e) if matches_keyword!(e, Keyword::Enum) => {
                return Err(Error::UnsupportedKeyword(current_token.clone()));
            }

            // match declarations with a `let` keyword
            TokenType::Keyword(Keyword::Let) => self.declaration()?,

            TokenType::Keyword(Keyword::Device) => Expression::DeviceDeclaration(self.device()?),

            // match functions with a `fn` keyword
            TokenType::Keyword(Keyword::Fn) => Expression::Function(self.function()?),

            // match if statements
            TokenType::Keyword(Keyword::If) => Expression::If(self.if_expression()?),

            // match loop statements
            TokenType::Keyword(Keyword::Loop) => Expression::Loop(self.loop_expression()?),

            // match while statements
            TokenType::Keyword(Keyword::While) => Expression::While(self.while_expression()?),

            // match break statements
            TokenType::Keyword(Keyword::Break) => {
                // make sure the next token is a semi-colon
                let next = token_from_option!(self.get_next()?);
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::UnexpectedToken(next.clone()));
                }
                Expression::Break
            }

            // match continue statements
            TokenType::Keyword(Keyword::Continue) => {
                // make sure the next token is a semi-colon
                let next = token_from_option!(self.get_next()?);
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::UnexpectedToken(next.clone()));
                }
                Expression::Continue
            }

            // match syscalls with a `syscall` keyword
            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                Expression::Syscall(self.syscall()?)
            }

            // match a variable expression with opening parenthesis
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                Expression::Invocation(self.invocation()?)
            }

            // match a variable expression with an assignment
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::Assign)) =>
            {
                Expression::Assignment(self.assignment()?)
            }

            // match variable expressions with an identifier
            TokenType::Identifier(ref id) => Expression::Variable(id.clone()),

            // match block expressions with a `{` symbol
            TokenType::Symbol(Symbol::LBrace) => Expression::Block(self.block()?),

            // match literal expressions with a semi-colon afterwards
            TokenType::Number(_) | TokenType::String(_) | TokenType::Boolean(_) => {
                Expression::Literal(self.literal()?)
            }

            // match priority expressions with a left parenthesis
            TokenType::Symbol(Symbol::LParen) => Expression::Priority(self.priority()?),

            // match minus symbols to handle negative numbers or negated expressions
            TokenType::Symbol(Symbol::Minus) => {
                self.assign_next()?; // consume the `-` symbol
                // IMPORTANT: We call `unary()` here, NOT `expression()`.
                // This ensures negation binds tightly to the operand and doesn't consume binary ops.
                // e.g. `-1 + 2` parses as `(-1) + 2`
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;

                Expression::Negation(boxed!(inner_expr))
            }

            // match logical NOT `!`
            TokenType::Symbol(Symbol::LogicalNot) => {
                self.assign_next()?; // consume the `!` symbol
                let inner_expr = self.unary()?.ok_or(Error::UnexpectedEOF)?;
                Expression::Logical(LogicalExpression::Not(boxed!(inner_expr)))
            }

            _ => {
                return Err(Error::UnexpectedToken(current_token.clone()));
            }
        };

        Ok(Some(expr))
    }

    fn get_infix_child_node(&mut self) -> Result<tree_node::Expression, Error> {
        let current_token = token_from_option!(self.current_token);

        match current_token.token_type {
            // A literal number or boolean
            TokenType::Number(_) | TokenType::Boolean(_) => self.literal().map(Expression::Literal),
            // A plain variable
            TokenType::Identifier(ident)
                if !self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                Ok(Expression::Variable(ident))
            }
            // A priority expression ( -> (1 + 2) <- + 3 )
            TokenType::Symbol(Symbol::LParen) => self.priority().map(Expression::Priority),
            // A function invocation
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                self.invocation().map(Expression::Invocation)
            }
            // Handle Negation
            TokenType::Symbol(Symbol::Minus) => {
                self.assign_next()?;
                // recurse to handle double negation or simple negation of atoms
                let inner = self.get_infix_child_node()?;
                Ok(Expression::Negation(boxed!(inner)))
            }
            // Handle Logical Not
            TokenType::Symbol(Symbol::LogicalNot) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                Ok(Expression::Logical(LogicalExpression::Not(boxed!(inner))))
            }
            _ => Err(Error::UnexpectedToken(current_token.clone())),
        }
    }

    fn device(&mut self) -> Result<DeviceDeclarationExpression, Error> {
        // sanity check, make sure current token is a `device` keyword

        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Device)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        let identifier = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(current_token));
        }

        let device = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::String(ref id),
            id.clone()
        );

        Ok(DeviceDeclarationExpression {
            name: identifier,
            device,
        })
    }

    fn assignment(&mut self) -> Result<AssignmentExpression, Error> {
        let identifier = extract_token_data!(
            token_from_option!(self.current_token),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(current_token));
        }
        self.assign_next()?;

        let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        Ok(AssignmentExpression {
            identifier,
            expression: boxed!(expression),
        })
    }

    /// Handles mathmatical and logical expressions in the explicit order of operations
    fn infix(&mut self, previous: Expression) -> Result<Expression, Error> {
        // We cannot use recursion here, as we need to handle the precedence of the operators
        // We need to use a loop to parse the binary expressions.

        let mut current_token = token_from_option!(self.get_next()?).clone();

        // first, make sure the previous expression supports binary expressions
        match previous {
            Expression::Binary(_)
            | Expression::Logical(_)
            | Expression::Invocation(_)
            | Expression::Priority(_)
            | Expression::Literal(_)
            | Expression::Variable(_)
            | Expression::Negation(_) => {}
            _ => {
                return Err(Error::InvalidSyntax(
                    current_token.clone(),
                    String::from("Invalid expression for binary/logical operation"),
                ));
            }
        }

        let mut expressions = vec![previous]; // 1, 2, 3

        // operators Vec should be `expressions.len() - 1`
        let mut operators = Vec::<Symbol>::new(); // +, +

        // build the expressions and operators vectors
        while token_matches!(
            current_token,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical()
        ) {
            // We are guaranteed to have an operator/comparison/logical symbol here as we checked in the while loop
            let operator = extract_token_data!(current_token, TokenType::Symbol(s), s);
            operators.push(operator);
            self.assign_next()?;
            expressions.push(self.get_infix_child_node()?);

            current_token = token_from_option!(self.get_next()?).clone();
        }

        // validate the vectors and make sure operators.len() == expressions.len() - 1
        if operators.len() != expressions.len() - 1 {
            return Err(Error::InvalidSyntax(
                current_token.clone(),
                String::from("Invalid number of operators"),
            ));
        }

        // Every time we find a valid operator, we pop 2 off the expressions and add one back.
        // This means that we need to keep track of the current iteration to ensure we are
        // removing the correct expressions from the vector

        // --- PRECEDENCE LEVEL 1: Exponent (**) ---
        for (i, operator) in operators.iter().enumerate().rev() {
            if operator == &Symbol::Exp {
                let right = expressions.remove(i + 1);
                let left = expressions.remove(i);
                expressions.insert(
                    i,
                    Expression::Binary(BinaryExpression::Exponent(boxed!(left), boxed!(right))),
                );
            }
        }
        operators.retain(|symbol| symbol != &Symbol::Exp);

        // --- PRECEDENCE LEVEL 2: Multiplicative (*, /, %) ---
        let mut current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::Slash | Symbol::Asterisk | Symbol::Percent) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);

                match operator {
                    Symbol::Asterisk => expressions.insert(
                        index,
                        Expression::Binary(BinaryExpression::Multiply(boxed!(left), boxed!(right))),
                    ),
                    Symbol::Slash => expressions.insert(
                        index,
                        Expression::Binary(BinaryExpression::Divide(boxed!(left), boxed!(right))),
                    ),
                    Symbol::Percent => expressions.insert(
                        index,
                        Expression::Binary(BinaryExpression::Modulo(boxed!(left), boxed!(right))),
                    ),
                    _ => unreachable!(),
                }
                current_iteration += 1;
            }
        }
        operators
            .retain(|symbol| !matches!(symbol, Symbol::Asterisk | Symbol::Percent | Symbol::Slash));

        // --- PRECEDENCE LEVEL 3: Additive (+, -) ---
        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::Plus | Symbol::Minus) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);

                match operator {
                    Symbol::Plus => expressions.insert(
                        index,
                        Expression::Binary(BinaryExpression::Add(boxed!(left), boxed!(right))),
                    ),
                    Symbol::Minus => expressions.insert(
                        index,
                        Expression::Binary(BinaryExpression::Subtract(boxed!(left), boxed!(right))),
                    ),
                    _ => unreachable!(),
                }
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::Plus | Symbol::Minus));

        // --- PRECEDENCE LEVEL 4: Comparison (<, >, <=, >=) ---
        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if operator.is_comparison() && !matches!(operator, Symbol::Equal | Symbol::NotEqual) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);

                match operator {
                    Symbol::LessThan => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::LessThan(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    Symbol::GreaterThan => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::GreaterThan(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    Symbol::LessThanOrEqual => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::LessThanOrEqual(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    Symbol::GreaterThanOrEqual => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::GreaterThanOrEqual(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    _ => unreachable!(),
                }
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

                match operator {
                    Symbol::Equal => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::Equal(boxed!(left), boxed!(right))),
                    ),
                    Symbol::NotEqual => expressions.insert(
                        index,
                        Expression::Logical(LogicalExpression::NotEqual(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    _ => unreachable!(),
                }
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

                expressions.insert(
                    index,
                    Expression::Logical(LogicalExpression::And(boxed!(left), boxed!(right))),
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

                expressions.insert(
                    index,
                    Expression::Logical(LogicalExpression::Or(boxed!(left), boxed!(right))),
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::LogicalOr));

        // Ensure there is only one expression left in the expressions vector, and no operators left
        if expressions.len() != 1 || !operators.is_empty() {
            return Err(Error::InvalidSyntax(
                current_token.clone(),
                String::from("Invalid number of operators"),
            ));
        }

        // Edge case. If the current token is a semi-colon, RParen, we need to set current token to the previous token
        if token_matches!(
            current_token,
            TokenType::Symbol(Symbol::Semicolon) | TokenType::Symbol(Symbol::RParen)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
        }

        Ok(expressions.pop().unwrap())
    }

    fn priority(&mut self) -> Result<Box<Expression>, Error> {
        let current_token = token_from_option!(self.current_token);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        self.assign_next()?;
        let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        Ok(boxed!(expression))
    }

    fn invocation(&mut self) -> Result<InvocationExpression, Error> {
        let identifier = extract_token_data!(
            token_from_option!(self.current_token),
            TokenType::Identifier(ref id),
            id.clone()
        );

        // Ensure the next token is a left parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        let mut arguments = Vec::<Expression>::new();
        // We need to make sure the expressions are NOT BlockExpressions, as they are not allowed

        while !token_matches!(
            token_from_option!(self.get_next()?),
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = token_from_option!(self.current_token);
            let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

            if let Expression::Block(_) = expression {
                return Err(Error::InvalidSyntax(
                    current_token,
                    String::from("Block expressions are not allowed in function invocations"),
                ));
            }

            arguments.push(expression);

            // make sure the next token is a comma or right parenthesis
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                return Err(Error::UnexpectedToken(
                    token_from_option!(self.get_next()?).clone(),
                ));
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

    fn block(&mut self) -> Result<BlockExpression, Error> {
        let mut expressions = Vec::<Expression>::new();
        let current_token = token_from_option!(self.current_token);

        // sanity check: make sure the current token is a left brace
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        while !self_matches_peek!(
            self,
            TokenType::Symbol(Symbol::RBrace) | TokenType::Keyword(Keyword::Return)
        ) {
            let expression = self.parse()?.ok_or(Error::UnexpectedEOF)?;
            expressions.push(expression);
        }

        // print the current token for debugging
        let current_token = token_from_option!(self.get_next()?);

        if token_matches!(current_token, TokenType::Keyword(Keyword::Return)) {
            self.assign_next()?;
            let expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;
            let return_expr = Expression::Return(boxed!(expression));
            expressions.push(return_expr);

            // check for semicolon
            let next = token_from_option!(self.get_next()?);
            if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                return Err(Error::UnexpectedToken(next.clone()));
            }

            // check for right brace
            let next = token_from_option!(self.get_next()?);
            if !token_matches!(next, TokenType::Symbol(Symbol::RBrace)) {
                return Err(Error::UnexpectedToken(next.clone()));
            }
        }

        Ok(BlockExpression(expressions))
    }

    fn declaration(&mut self) -> Result<Expression, Error> {
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }
        let identifier = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();

        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        self.assign_next()?;
        let assignment_expression = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        // make sure the next token is a semi-colon
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        Ok(Expression::Declaration(
            identifier,
            boxed!(assignment_expression),
        ))
    }

    fn literal(&mut self) -> Result<Literal, Error> {
        let current_token = token_from_option!(self.current_token);
        let literal = match current_token.token_type {
            TokenType::Number(num) => Literal::Number(num),
            TokenType::String(string) => Literal::String(string),
            TokenType::Boolean(boolean) => Literal::Boolean(boolean),
            _ => return Err(Error::UnexpectedToken(current_token.clone())),
        };

        Ok(literal)
    }

    fn if_expression(&mut self) -> Result<IfExpression, Error> {
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::If)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        // consume 'if'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }
        self.assign_next()?;

        // parse condition
        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        // check for ')'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }

        // check for '{'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }

        // parse body
        let body = self.block()?;

        // check for 'else'
        let else_branch = if self_matches_peek!(self, TokenType::Keyword(Keyword::Else)) {
            self.assign_next()?; // consume 'else'

            if self_matches_peek!(self, TokenType::Keyword(Keyword::If)) {
                // else if ...
                self.assign_next()?;
                Some(boxed!(Expression::If(self.if_expression()?)))
            } else if self_matches_peek!(self, TokenType::Symbol(Symbol::LBrace)) {
                // else { ... }
                self.assign_next()?;
                Some(boxed!(Expression::Block(self.block()?)))
            } else {
                return Err(Error::UnexpectedToken(
                    token_from_option!(self.get_next()?).clone(),
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
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Loop)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        // check for '{'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }

        // parse body
        let body = self.block()?;

        Ok(LoopExpression { body })
    }

    fn while_expression(&mut self) -> Result<WhileExpression, Error> {
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::While)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        // consume 'while'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }
        self.assign_next()?;

        // parse condition
        let condition = self.expression()?.ok_or(Error::UnexpectedEOF)?;

        // check for ')'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }

        // check for '{'
        let next = token_from_option!(self.get_next()?);
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(next.clone()));
        }

        // parse body
        let body = self.block()?;

        Ok(WhileExpression {
            condition: boxed!(condition),
            body,
        })
    }

    fn function(&mut self) -> Result<FunctionExpression, Error> {
        let current_token = token_from_option!(self.current_token);
        // Sanify check that the current token is a `fn` keyword
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Fn)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
        }

        let fn_ident = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        // make sure next token is a left parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(current_token.clone()));
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
                return Err(Error::DuplicateIdentifier(current_token.clone()));
            }

            arguments.push(argument);

            // make sure the next token is a comma or right parenthesis
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                return Err(Error::UnexpectedToken(
                    token_from_option!(self.get_next()?).clone(),
                ));
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
            return Err(Error::UnexpectedToken(current_token.clone()));
        };

        Ok(FunctionExpression {
            name: fn_ident,
            arguments,
            body: self.block()?,
        })
    }

    fn syscall(&mut self) -> Result<SysCall, Error> {
        /// Checks the length of the arguments and returns an error if the length is not equal to the expected length
        fn check_length(
            parser: &Parser,
            arguments: &[Expression],
            length: usize,
        ) -> Result<(), Error> {
            if arguments.len() != length {
                return Err(Error::InvalidSyntax(
                    token_from_option!(parser.current_token).clone(),
                    format!("Expected {} arguments", length),
                ));
            }
            Ok(())
        }
        /// Converts an expression to "literal or variable" expression
        macro_rules! literal_or_variable {
            ($iter:expr) => {
                match $iter {
                    Some(Expression::Literal(literal)) => {
                        LiteralOrVariable::Literal(literal.clone())
                    }
                    Some(Expression::Variable(ident)) => LiteralOrVariable::Variable(ident.clone()),
                    _ => {
                        return Err(Error::UnexpectedToken(
                            token_from_option!(self.current_token).clone(),
                        ))
                    }
                }
            };
        }

        /// Gets the argument from the expression and returns an error if the expression does not match the expected pattern
        macro_rules! get_arg {
            ($matcher: ident, $arg: expr) => {
                match $arg {
                    LiteralOrVariable::$matcher(i) => i,
                    _ => {
                        return Err(Error::InvalidSyntax(
                            token_from_option!(self.current_token).clone(),
                            String::from("Expected a variable"),
                        ))
                    }
                }
            };
        }

        // A syscall is essentially an invocation expression with a syscall identifier. So we can reuse the invocation function
        let invocation = self.invocation()?;

        match invocation.name.as_str() {
            // system calls
            "yield" => {
                check_length(self, &invocation.arguments, 0)?;
                Ok(SysCall::System(sys_call::System::Yield))
            }
            "sleep" => {
                check_length(self, &invocation.arguments, 1)?;
                let mut arg = invocation.arguments.into_iter();
                let expr = token_from_option!(owned arg.next());
                Ok(SysCall::System(System::Sleep(boxed!(expr))))
            }
            "hash" => {
                check_length(self, &invocation.arguments, 1)?;
                let mut args = invocation.arguments.into_iter();
                let lit_str = literal_or_variable!(args.next());

                let LiteralOrVariable::Literal(lit_str) = lit_str else {
                    return Err(Error::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };

                Ok(SysCall::System(System::Hash(lit_str)))
            }
            "loadFromDevice" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.iter();

                let device = literal_or_variable!(args.next());

                let Some(Expression::Literal(Literal::String(variable))) = args.next() else {
                    return Err(Error::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };

                Ok(SysCall::System(sys_call::System::LoadFromDevice(
                    device,
                    Literal::String(variable.clone()),
                )))
            }
            "loadBatch" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.iter();

                let device_hash = literal_or_variable!(args.next());
                let logic_type = get_arg!(Literal, literal_or_variable!(args.next()));
                let batch_mode = get_arg!(Literal, literal_or_variable!(args.next()));

                Ok(SysCall::System(sys_call::System::LoadBatch(
                    device_hash,
                    logic_type,
                    batch_mode,
                )))
            }
            "loadBatchNamed" => {
                check_length(self, &invocation.arguments, 4)?;
                let mut args = invocation.arguments.into_iter();

                let device_hash = literal_or_variable!(args.next());
                let name_hash = token_from_option!(owned args.next());
                let logic_type = get_arg!(Literal, literal_or_variable!(args.next()));
                let batch_mode = get_arg!(Literal, literal_or_variable!(args.next()));

                Ok(SysCall::System(sys_call::System::LoadBatchNamed(
                    device_hash,
                    boxed!(name_hash),
                    logic_type,
                    batch_mode,
                )))
            }
            "setOnDevice" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.into_iter();

                let device = literal_or_variable!(args.next());

                let Literal::String(logic_type) =
                    get_arg!(Literal, literal_or_variable!(args.next()))
                else {
                    return Err(Error::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };

                let variable = token_from_option!(owned args.next());

                Ok(SysCall::System(sys_call::System::SetOnDevice(
                    device,
                    Literal::String(logic_type),
                    boxed!(variable),
                )))
            }
            "setOnDeviceBatched" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.into_iter();

                let device = literal_or_variable!(args.next());
                let Literal::String(logic_type) =
                    get_arg!(Literal, literal_or_variable!(args.next()))
                else {
                    return Err(Error::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };
                let variable = token_from_option!(owned args.next());

                Ok(SysCall::System(System::SetOnDeviceBatched(
                    device,
                    Literal::String(logic_type),
                    boxed!(variable),
                )))
            }
            "setOnDeviceBatchedNamed" => {
                check_length(self, &invocation.arguments, 4)?;
                let mut args = invocation.arguments.into_iter();

                let device = literal_or_variable!(args.next());
                let name = literal_or_variable!(args.next());
                let Literal::String(logic_type) =
                    get_arg!(Literal, literal_or_variable!(args.next()))
                else {
                    return Err(Error::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };
                let variable = token_from_option!(owned args.next());

                Ok(SysCall::System(System::SetOnDeviceBatchedNamed(
                    device,
                    name,
                    Literal::String(logic_type),
                    boxed!(variable),
                )))
            }
            // math calls
            "acos" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Acos(arg)))
            }
            "asin" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Asin(arg)))
            }
            "atan" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Atan(arg)))
            }
            "atan2" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.iter();
                let arg1 = literal_or_variable!(args.next());
                let arg2 = literal_or_variable!(args.next());
                Ok(SysCall::Math(sys_call::Math::Atan2(arg1, arg2)))
            }
            "abs" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Abs(arg)))
            }
            "ceil" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Ceil(arg)))
            }
            "cos" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Cos(arg)))
            }
            "floor" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Floor(arg)))
            }
            "log" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Log(arg)))
            }
            "max" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.iter();
                let arg1 = literal_or_variable!(args.next());
                let arg2 = literal_or_variable!(args.next());
                Ok(SysCall::Math(sys_call::Math::Max(arg1, arg2)))
            }
            "min" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.iter();
                let arg1 = literal_or_variable!(args.next());
                let arg2 = literal_or_variable!(args.next());
                Ok(SysCall::Math(sys_call::Math::Min(arg1, arg2)))
            }
            "rand" => {
                check_length(self, &invocation.arguments, 0)?;
                Ok(SysCall::Math(sys_call::Math::Rand))
            }
            "sin" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Sin(arg)))
            }
            "sqrt" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Sqrt(arg)))
            }
            "tan" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Tan(arg)))
            }
            "trunc" => {
                check_length(self, &invocation.arguments, 1)?;
                let arg = literal_or_variable!(invocation.arguments.first());
                Ok(SysCall::Math(sys_call::Math::Trunc(arg)))
            }
            _ => todo!(),
        }
    }
}
