use thiserror::Error;

use crate::parser::tree_node::*;
use crate::parser::Parser as ASTParser;
use std::collections::HashMap;

/// Represents the return keyword. Used as a variable name for the register.
const RETURN: &'static str = "ret";

#[derive(Error, Debug)]
pub enum CompileError {
    #[error(transparent)]
    ParseError(#[from] crate::parser::ParseError),
    #[error("A fatal error has occurred with the compiler. Scope could not be found.")]
    ScopeError,
}

pub struct Compiler {
    parser: ASTParser,
    /// Max stack size for the program is by default 512.
    variable_scope: Vec<HashMap<String, usize>>,
    function_scope: Vec<HashMap<String, usize>>,
    output: String,
    stack_pointer: usize,
    /// A map of variable names to register numbers. 0-15 are reserved for variables, 16 is the stack pointer, 17 is the return address
    register: HashMap<String, u8>,
    max_stack_size: usize,
    current_line: usize,
}

impl Compiler {
    pub fn new(parser: ASTParser, max_stack_size: usize) -> Self {
        Self {
            parser,
            variable_scope: Vec::new(),
            function_scope: Vec::new(),
            output: String::new(),
            stack_pointer: 0,
            register: HashMap::new(),
            max_stack_size,
            current_line: 0,
        }
    }

    pub fn compile(mut self) -> Result<String, CompileError> {
        let ast = self.parser.parse_all()?;

        let Some(ast) = ast else {
            return Ok(String::new());
        };

        self.expression(ast)?;

        Ok(self.output)
    }

    fn expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::BlockExpression(block) => self.block_expression(block)?,
            Expression::DeclarationExpression(name, expr) => {
                self.declaration_expression(name, expr)?
            }
            Expression::ReturnExpression(expr) => self.return_expression(*expr)?,
            _ => todo!(),
        };

        todo!()
    }

    fn return_expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        // pop last var off the stack and push it into the first available register
        let register = self.register.len() as u8;
        self.output.push_str(&format!("pop r{}\n", register));
        self.stack_pointer
            .checked_sub(1)
            .ok_or(CompileError::ScopeError)?;
        self.current_line += 1;
        Ok(())
    }

    fn declaration_expression(
        &mut self,
        name: String,
        expression: Box<Expression>,
    ) -> Result<(), CompileError> {
        self.expression(*expression)?;
        self.variable_scope
            .last_mut()
            .ok_or(CompileError::ScopeError)?
            .insert(name, self.stack_pointer);
        self.stack_pointer += 1;
        Ok(())
    }

    fn block_expression(&mut self, expression: BlockExpression) -> Result<(), CompileError> {
        // Push a new scope. This will be read from all the child expressions
        self.variable_scope.push(HashMap::new());

        for expr in expression.0 {
            self.expression(expr)?;
        }

        // Remove the scope we just added, all variables are now out of scope
        self.variable_scope.pop();
        Ok(())
    }
}
