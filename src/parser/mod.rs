pub mod sys_call;
pub mod tree_node;

use crate::{
    boxed,
    tokenizer::{
        token::{Keyword, Symbol, Token, TokenType},
        Tokenizer, TokenizerBuffer, TokenizerError,
    },
};
use std::io::SeekFrom;
use sys_call::SysCall;
use tree_node::*;

quick_error! {
    #[derive(Debug)]
    pub enum ParseError {
        TokenizerError(err: TokenizerError) {
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
            None => return Err(ParseError::UnexpectedEOF),
        }
    };
}

macro_rules! extract_token_data {
    ($token:ident, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => return Err(ParseError::UnexpectedToken($token.clone())),
        }
    };
    ($token:expr, $pattern:pat, $extraction:expr) => {
        match $token.token_type {
            $pattern => $extraction,
            _ => {
                return Err(ParseError::UnexpectedToken($token.clone()));
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
        let expr = self.expression()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Semicolon)) {
            self.assign_next()?;
        }

        Ok(expr)
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

        let expr = Some(match current_token.token_type {
            // match unsupported keywords
            TokenType::Keyword(e)
                if matches_keyword!(e, Keyword::Enum, Keyword::If, Keyword::Else) =>
            {
                return Err(ParseError::UnsupportedKeyword(current_token.clone()))
            }

            // match declarations with a `let` keyword
            TokenType::Keyword(Keyword::Let) => self.declaration()?,

            TokenType::Keyword(Keyword::Device) => {
                Expression::DeviceDeclarationExpression(self.device()?)
            }

            // match functions with a `fn` keyword
            TokenType::Keyword(Keyword::Fn) => Expression::FunctionExpression(self.function()?),

            // match syscalls with a `syscall` keyword
            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                Expression::SyscallExpression(self.syscall()?)
            }

            // match a variable expression with opening parenthesis
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                Expression::InvocationExpression(self.invocation()?)
            }

            // match a variable expression with an assignment
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::Assign)) =>
            {
                Expression::AssignmentExpression(self.assignment()?)
            }

            // match variable expressions with an identifier
            TokenType::Identifier(ref id) => Expression::Variable(id.clone()),

            // match block expressions with a `{` symbol
            TokenType::Symbol(Symbol::LBrace) => Expression::BlockExpression(self.block()?),

            // match literal expressions with a semi-colon afterwards
            TokenType::Number(_) | TokenType::String(_) => Expression::Literal(self.literal()?),

            // match priority expressions with a left parenthesis
            TokenType::Symbol(Symbol::LParen) => Expression::PriorityExpression(self.priority()?),

            _ => {
                return Err(ParseError::UnexpectedToken(current_token.clone()));
            }
        });

        let Some(expr) = expr else {
            return Ok(None);
        };

        // check if the next or current token is an operator
        if self_matches_peek!(self, TokenType::Symbol(s) if s.is_operator()) {
            return Ok(Some(Expression::BinaryExpression(self.binary(expr)?)));
        }
        // This is an edge case. We need to move back one token if the current token is an operator
        // so the binary expression can pick up the operator
        else if self_matches_current!(self, TokenType::Symbol(s) if s.is_operator()) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Ok(Some(Expression::BinaryExpression(self.binary(expr)?)));
        }

        Ok(Some(expr))
    }

    fn get_binary_child_node(&mut self) -> Result<tree_node::Expression, ParseError> {
        let current_token = token_from_option!(self.current_token);

        match current_token.token_type {
            // A literal number
            TokenType::Number(_) => self.literal().map(Expression::Literal),
            // A plain variable
            TokenType::Identifier(ident)
                if !self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                Ok(Expression::Variable(ident))
            }
            // A priority expression ( -> (1 + 2) <- + 3 )
            TokenType::Symbol(Symbol::LParen) => {
                self.priority().map(Expression::PriorityExpression)
            }
            // A function invocation
            TokenType::Identifier(_)
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                self.invocation().map(Expression::InvocationExpression)
            }
            _ => Err(ParseError::UnexpectedToken(current_token.clone())),
        }
    }

    fn device(&mut self) -> Result<DeviceDeclarationExpression, ParseError> {
        // sanity check, make sure current token is a `device` keyword

        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Device)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        let identifier = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(ParseError::UnexpectedToken(current_token));
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

    fn assignment(&mut self) -> Result<AssignmentExpression, ParseError> {
        let identifier = extract_token_data!(
            token_from_option!(self.current_token),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(ParseError::UnexpectedToken(current_token));
        }
        self.assign_next()?;

        let expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;

        Ok(AssignmentExpression {
            identifier,
            expression: boxed!(expression),
        })
    }

    /// Handles mathmatical expressions in the explicit order of PEMDAS
    fn binary(&mut self, previous: Expression) -> Result<BinaryExpression, ParseError> {
        // We cannot use recursion here, as we need to handle the precedence of the operators
        // We need to use a loop to parse the binary expressions.

        let mut current_token = token_from_option!(self.get_next()?).clone();

        // first, make sure the previous expression supports binary expressions
        match previous {
            Expression::BinaryExpression(_) // 1 + 2 + 3
            | Expression::InvocationExpression(_) // add() + 3
            | Expression::PriorityExpression(_) // (1 + 2) + 3
            | Expression::Literal(Literal::Number(_)) // 1 + 2 (no addition of strings)
            | Expression::Variable(_) // x + 2
            | Expression::Negation(_) // -1 + 2
             => {}
            _ => {
                return Err(ParseError::InvalidSyntax(current_token.clone(), String::from("Invalid expression for binary operation")))
            }
        }

        let mut expressions = vec![previous]; // 1, 2, 3

        // operators Vec should be `expressions.len() - 1`
        let mut operators = Vec::<Symbol>::new(); // +, +

        // build the expressions and operators vectors
        while token_matches!(current_token, TokenType::Symbol(s) if s.is_operator()) {
            // We are guaranteed to have an operator symbol here as we checked in the while loop
            let operator = extract_token_data!(current_token, TokenType::Symbol(s), s);
            operators.push(operator);
            self.assign_next()?;
            expressions.push(self.get_binary_child_node()?);

            current_token = token_from_option!(self.get_next()?).clone();
        }

        // validate the vectors and make sure operators.len() == expressions.len() - 1
        if operators.len() != expressions.len() - 1 {
            return Err(ParseError::InvalidSyntax(
                current_token.clone(),
                String::from("Invalid number of operators"),
            ));
        }

        // Every time we find a valid operator, we pop 2 off the expressions and add one back.
        // This means that we need to keep track of the current iteration to ensure we are
        // removing the correct expressions from the vector
        let mut current_iteration = 0;

        // Loop through operators, and build the binary expressions for exponential operators only
        for (i, operator) in operators.iter().enumerate() {
            if operator == &Symbol::Exp {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                expressions.insert(
                    index,
                    Expression::BinaryExpression(BinaryExpression::Exponent(
                        boxed!(left),
                        boxed!(right),
                    )),
                );
                current_iteration += 1;
            }
        }

        // remove all the exponential operators from the operators vector
        operators.retain(|symbol| symbol != &Symbol::Exp);
        current_iteration = 0;

        // Loop through operators, and build the binary expressions for multiplication and division operators
        for (i, operator) in operators.iter().enumerate() {
            if operator == &Symbol::Asterisk || operator == &Symbol::Slash {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);

                match operator {
                    Symbol::Asterisk => expressions.insert(
                        index,
                        Expression::BinaryExpression(BinaryExpression::Multiply(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    Symbol::Slash => expressions.insert(
                        index,
                        Expression::BinaryExpression(BinaryExpression::Divide(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    // safety: we have already checked for the operator
                    _ => unreachable!(),
                }
                current_iteration += 1;
            }
        }

        // remove all the multiplication and division operators from the operators vector
        operators.retain(|symbol| symbol != &Symbol::Asterisk && symbol != &Symbol::Slash);
        current_iteration = 0;

        // Loop through operators, and build the binary expressions for addition and subtraction operators
        for (i, operator) in operators.iter().enumerate() {
            if operator == &Symbol::Plus || operator == &Symbol::Minus {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);

                match operator {
                    Symbol::Plus => expressions.insert(
                        index,
                        Expression::BinaryExpression(BinaryExpression::Add(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    Symbol::Minus => expressions.insert(
                        index,
                        Expression::BinaryExpression(BinaryExpression::Subtract(
                            boxed!(left),
                            boxed!(right),
                        )),
                    ),
                    // safety: we have already checked for the operator
                    _ => unreachable!(),
                }
                current_iteration += 1;
            }
        }

        // remove all the addition and subtraction operators from the operators vector
        operators.retain(|symbol| symbol != &Symbol::Plus && symbol != &Symbol::Minus);

        // Ensure there is only one expression left in the expressions vector, and no operators left
        if expressions.len() != 1 || !operators.is_empty() {
            return Err(ParseError::InvalidSyntax(
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

        // Ensure the last expression is a binary expression
        match expressions.pop().unwrap() {
            Expression::BinaryExpression(binary) => Ok(binary),
            _ => unreachable!(),
        }
    }

    fn priority(&mut self) -> Result<Box<Expression>, ParseError> {
        let current_token = token_from_option!(self.current_token);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        self.assign_next()?;
        let expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;

        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::RParen)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        Ok(boxed!(expression))
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
            return Err(ParseError::UnexpectedToken(current_token.clone()));
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
                return Err(ParseError::InvalidSyntax(
                    current_token,
                    String::from("Block expressions are not allowed in function invocations"),
                ));
            }

            arguments.push(expression);

            // make sure the next token is a comma or right parenthesis
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                return Err(ParseError::UnexpectedToken(
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

    fn block(&mut self) -> Result<BlockExpression, ParseError> {
        let mut expressions = Vec::<Expression>::new();
        let current_token = token_from_option!(self.current_token);

        // sanity check: make sure the current token is a left brace
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        while !self_matches_peek!(
            self,
            TokenType::Symbol(Symbol::RBrace) | TokenType::Keyword(Keyword::Return)
        ) {
            let expression = self.parse()?.ok_or(ParseError::UnexpectedEOF)?;
            expressions.push(expression);
        }

        // print the current token for debugging
        let current_token = token_from_option!(self.get_next()?);

        if token_matches!(current_token, TokenType::Keyword(Keyword::Return)) {
            self.assign_next()?;
            let expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;
            let return_expr = Expression::ReturnExpression(boxed!(expression));
            expressions.push(return_expr);
            self.assign_next()?;
        }

        self.assign_next()?;

        Ok(BlockExpression(expressions))
    }

    fn declaration(&mut self) -> Result<Expression, ParseError> {
        let current_token = token_from_option!(self.current_token);
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }
        let identifier = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        let current_token = token_from_option!(self.get_next()?).clone();

        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        self.assign_next()?;
        let assignment_expression = self.expression()?.ok_or(ParseError::UnexpectedEOF)?;

        // make sure the next token is a semi-colon
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        Ok(Expression::DeclarationExpression(
            identifier,
            boxed!(assignment_expression),
        ))
    }

    fn literal(&mut self) -> Result<Literal, ParseError> {
        let current_token = token_from_option!(self.current_token);
        let literal = match current_token.token_type {
            TokenType::Number(num) => Literal::Number(num),
            TokenType::String(string) => Literal::String(string),
            _ => return Err(ParseError::UnexpectedToken(current_token.clone())),
        };

        Ok(literal)
    }

    fn function(&mut self) -> Result<FunctionExpression, ParseError> {
        let current_token = token_from_option!(self.current_token);
        // Sanify check that the current token is a `fn` keyword
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Fn)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        }

        let fn_ident = extract_token_data!(
            token_from_option!(self.get_next()?),
            TokenType::Identifier(ref id),
            id.clone()
        );

        // make sure next token is a left parenthesis
        let current_token = token_from_option!(self.get_next()?);
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(ParseError::UnexpectedToken(current_token.clone()));
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
                return Err(ParseError::DuplicateIdentifier(current_token.clone()));
            }

            arguments.push(argument);

            // make sure the next token is a comma or right parenthesis
            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                return Err(ParseError::UnexpectedToken(
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
            return Err(ParseError::UnexpectedToken(current_token.clone()));
        };

        Ok(FunctionExpression {
            name: fn_ident,
            arguments,
            body: self.block()?,
        })
    }

    fn syscall(&mut self) -> Result<SysCall, ParseError> {
        /// Checks the length of the arguments and returns an error if the length is not equal to the expected length
        fn check_length(
            parser: &Parser,
            arguments: &[Expression],
            length: usize,
        ) -> Result<(), ParseError> {
            if arguments.len() != length {
                return Err(ParseError::InvalidSyntax(
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
                        return Err(ParseError::UnexpectedToken(
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
                        return Err(ParseError::InvalidSyntax(
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
            "yield" => Ok(SysCall::System(sys_call::System::Yield)),
            "sleep" => {
                check_length(self, &invocation.arguments, 1)?;
                let mut arg = invocation.arguments.iter();
                let argument = literal_or_variable!(arg.next());
                Ok(SysCall::System(sys_call::System::Sleep(argument)))
            }
            "loadFromDevice" => {
                check_length(self, &invocation.arguments, 2)?;
                let mut args = invocation.arguments.iter();

                let device = literal_or_variable!(args.next());

                let Some(Expression::Literal(Literal::String(variable))) = args.next() else {
                    return Err(ParseError::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };

                Ok(SysCall::System(sys_call::System::LoadFromDevice(
                    device,
                    variable.clone(),
                )))
            }
            "setOnDevice" => {
                check_length(self, &invocation.arguments, 3)?;
                let mut args = invocation.arguments.iter();

                let device = literal_or_variable!(args.next());

                let Literal::String(logic_type) =
                    get_arg!(Literal, literal_or_variable!(args.next()))
                else {
                    return Err(ParseError::UnexpectedToken(
                        token_from_option!(self.current_token).clone(),
                    ));
                };

                let variable = literal_or_variable!(args.next());

                Ok(SysCall::System(sys_call::System::SetOnDevice(
                    device, logic_type, variable,
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

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    macro_rules! parser {
        ($input:expr) => {
            Parser::new(Tokenizer::from($input.to_owned()))
        };
    }

    #[test]
    fn test_unsupported_keywords() -> Result<()> {
        let mut parser = parser!("enum x;");
        assert!(parser.parse().is_err());

        let mut parser = parser!("if x {}");
        assert!(parser.parse().is_err());

        let mut parser = parser!("else {}");
        assert!(parser.parse().is_err());

        Ok(())
    }

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

    #[test]
    fn test_binary_expression() -> Result<()> {
        let expr = parser!("4 ** 2 + 5 ** 2").parse()?.unwrap();
        assert_eq!("((4 ** 2) + (5 ** 2))", expr.to_string());

        let expr = parser!("45 * 2 - 15 / 5 + 5 ** 2").parse()?.unwrap();
        assert_eq!("(((45 * 2) - (15 / 5)) + (5 ** 2))", expr.to_string());

        let expr = parser!("(5 - 2) * 10").parse()?.unwrap();
        assert_eq!("(((5 - 2)) * 10)", expr.to_string());

        Ok(())
    }
}
