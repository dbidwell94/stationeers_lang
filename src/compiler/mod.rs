use crate::parser::tree_node::*;
use crate::parser::Parser as ASTParser;
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::io::{BufWriter, Write};

/// Represents the return keyword. Used as a variable name for the register.
const RETURN: &'static str = "ret";

quick_error! {
    #[derive(Debug)]
    pub enum CompileError {
        ParseError(err: crate::parser::ParseError) {
            from()
            display("Parse error: {}", err)
        }
        ScopeError {
            display("A fatal error has occurred with the compiler. Scope could not be found.")
        }
        WriteError(err: std::io::Error) {
            from()
            display("Write error: {}", err)
        }
        DuplicateVariable(variable: String) {
            display("A variable with the same name already exists in the current scope: {}", variable)
        }
        VariableNotFound(variable: String) {
            display("Variable {} was not found in the current scope.", variable)
        }
        MissingFunction(name: String) {
            display("Function {} was not found in the function table.", name)
        }
    }
}

pub struct Compiler<'a> {
    parser: ASTParser,
    /// Max stack size for the program is by default 512.
    variable_scope: Vec<HashMap<String, i32>>,
    function_locations: HashMap<String, usize>,
    output: &'a mut BufWriter<Box<dyn Write>>,
    /// A map of variable names to register numbers. 0-15 are reserved for variables, 16 is the stack pointer, 17 is the return address
    register: VecDeque<u8>,
    current_line: usize,
    declared_main: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(parser: ASTParser, writer: &'a mut BufWriter<Box<dyn Write>>) -> Self {
        Self {
            parser,
            variable_scope: Vec::new(),
            function_locations: HashMap::new(),
            output: writer,
            register: VecDeque::new(),
            current_line: 0,
            declared_main: false,
        }
    }

    fn get_variable_index(&self, var_name: &str) -> Result<i32, CompileError> {
        let mut offset = 0;

        for scope in &self.variable_scope {
            if let Some(index) = scope.get(var_name) {
                return Ok(*index + offset);
            }

            offset += scope.len() as i32;
        }

        Err(CompileError::VariableNotFound(var_name.to_owned()))
    }

    fn push_stack(&mut self, var_name: &str) -> Result<(), CompileError> {
        // check to make sure the variable doesn't already exist in the current scope
        if self
            .variable_scope
            .last()
            .ok_or(CompileError::ScopeError)?
            .contains_key(var_name)
        {
            return Err(CompileError::DuplicateVariable(var_name.to_string()));
        }

        let scope_size = self
            .variable_scope
            .last()
            .ok_or(CompileError::ScopeError)?
            .len();

        self.variable_scope
            .last_mut()
            .ok_or(CompileError::ScopeError)?
            .insert(var_name.to_string(), scope_size as i32);

        Ok(())
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), CompileError> {
        self.output.write(output.into().as_bytes())?;
        self.output.write(b"\n")?;
        self.current_line += 1;

        Ok(())
    }

    pub fn compile(mut self) -> Result<(), CompileError> {
        let ast = self.parser.parse_all()?;

        let Some(ast) = ast else {
            return Ok(());
        };

        // Jump directly to the main block. This will avoid executing functions before the main block.
        self.write_output(format!("j main"))?;

        self.expression(ast)?;
        Ok(())
    }

    fn expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::FunctionExpression(expr) => self.function_expression(expr)?,
            Expression::BlockExpression(expr) => self.block_expression(expr)?,
            Expression::InvocationExpression(expr) => self.invocation_expression(expr)?,
            Expression::BinaryExpression(expr) => self.binary_expression(expr)?,
            _ => todo!("{:?}", expression),
        };

        Ok(())
    }

    fn binary_expression(&mut self, expr: BinaryExpression) -> Result<(), CompileError> {
        todo!()
    }

    fn invocation_expression(&mut self, expr: InvocationExpression) -> Result<(), CompileError> {
        let function_name = expr.name;

        let function_line = self
            .function_locations
            .get(&function_name)
            .ok_or(CompileError::MissingFunction(function_name.clone()))?
            .clone();

        let mut to_write = String::new();

        let mut iter_index = 0;
        for arg in expr.arguments {
            match arg {
                Expression::Literal(Literal::Number(num)) => {
                    to_write.push_str(&format!("push {}\n", num));
                }
                Expression::Variable(var_name) => {
                    let index = self.get_variable_index(&var_name)?;
                    to_write.push_str(&format!("sub r0 sp {index}\n"));
                    to_write.push_str("get r0 db r0\n");
                    to_write.push_str("push r0\n");
                    self.push_stack(&format!("{function_name}{iter_index}"))?;
                }
                Expression::BinaryExpression(expr) => {
                    self.binary_expression(expr)?;
                }
                _ => todo!("something is up with the arguments"),
            }

            iter_index += 1;
        }

        // push the return address onto the stack. Current + to write + pushing the return address
        let return_addr = self.current_line + to_write.lines().count() + 2;
        self.write_output(format!("push {return_addr}"))?;
        self.output.write(to_write.as_bytes())?;
        self.current_line = return_addr;

        self.write_output(format!("j {function_line}"))?;

        Ok(())
    }

    fn function_expression(&mut self, expression: FunctionExpression) -> Result<(), CompileError> {
        let func_name = expression.name;

        self.variable_scope.push(HashMap::new());

        self.function_locations.insert(func_name, self.current_line);

        for arg in expression.arguments {
            self.push_stack(&arg)?;
        }

        for expr in expression.body.0 {
            self.expression(expr)?;
        }

        let scope = self.variable_scope.pop().ok_or(CompileError::ScopeError)?;

        self.write_output(format!("sub sp sp {0}", scope.len()))?;
        self.write_output("pop ra")?;
        self.write_output("j ra")?;

        Ok(())
    }

    fn block_expression(&mut self, mut expression: BlockExpression) -> Result<(), CompileError> {
        self.variable_scope.push(HashMap::new());

        // hoist functions to the top of the block
        expression.0.sort_by(|a, b| {
            if matches!(a, Expression::FunctionExpression(_))
                && matches!(b, Expression::FunctionExpression(_))
            {
                Ordering::Equal
            } else if matches!(a, Expression::FunctionExpression(_)) {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        for expr in expression.0 {
            // if we haven't declared main yet and we have already declared all the function expressions, declare main
            if !self.declared_main && !matches!(expr, Expression::FunctionExpression(_)) {
                self.write_output("main:")?;
                self.declared_main = true;
            }
            self.expression(expr)?;
        }

        self.variable_scope.pop();

        Ok(())
    }
}
