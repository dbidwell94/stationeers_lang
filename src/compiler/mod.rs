use crate::parser::tree_node::*;
use crate::parser::Parser as ASTParser;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::io::BufWriter;
use std::io::Write;

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
    }
}

macro_rules! variable_index {
    ($compiler:expr, $name:expr) => {
        $compiler
            .variable_scope
            .iter()
            .rev()
            .find(|v| v.contains_key(&$name))
            .map(|v| v[&$name])
            .ok_or(CompileError::ScopeError)?
    };
}

pub struct Compiler<'a> {
    parser: ASTParser,
    /// Max stack size for the program is by default 512.
    variable_scope: Vec<HashMap<String, usize>>,
    function_locations: HashMap<String, usize>,
    output: &'a mut BufWriter<Box<dyn Write>>,
    stack_pointer: usize,
    /// A map of variable names to register numbers. 0-15 are reserved for variables, 16 is the stack pointer, 17 is the return address
    register: VecDeque<u8>,
    max_stack_size: usize,
    current_line: usize,
}

impl<'a> Compiler<'a> {
    pub fn new(
        parser: ASTParser,
        max_stack_size: usize,
        writer: &'a mut BufWriter<Box<dyn Write>>,
    ) -> Self {
        Self {
            parser,
            variable_scope: Vec::new(),
            function_locations: HashMap::new(),
            output: writer,
            stack_pointer: 0,
            register: VecDeque::new(),
            max_stack_size,
            current_line: 0,
        }
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), CompileError> {
        self.output.write(output.into().as_bytes())?;
        self.current_line += 1;

        Ok(())
    }

    fn push_register(&mut self) -> Result<(), CompileError> {
        if self.register.len() >= 15 {
            return Err(CompileError::ScopeError);
        }
        self.register.push_back(self.register.len() as u8);
        Ok(())
    }

    fn pop_register(&mut self) -> Result<u8, CompileError> {
        self.register.pop_back().ok_or(CompileError::ScopeError)
    }

    pub fn compile(mut self) -> Result<(), CompileError> {
        let ast = self.parser.parse_all()?;

        let Some(ast) = ast else {
            return Ok(());
        };

        self.expression(ast)?;

        Ok(())
    }

    fn expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::BinaryExpression(expr) => self.binary_expression(expr)?,
            Expression::BlockExpression(expr) => self.block_expression(expr)?,
            Expression::DeclarationExpression(ident, expr) => {
                self.declaration_expression(ident, *expr)?
            }
            Expression::FunctionExpression(expr) => self.function_expression(expr)?,

            _ => todo!("{:?}", expression),
        };

        Ok(())
    }

    fn function_expression(&mut self, expr: FunctionExpression) -> Result<(), CompileError> {
        // in stack order: return address, arguments

        self.function_locations
            .insert(expr.name.clone(), self.current_line);

        self.variable_scope.push(HashMap::new());
        let total_args = expr.arguments.len();
        for (index, arg) in expr.arguments.iter().enumerate() {
            
        }

        Ok(())
    }

    fn declaration_expression(
        &mut self,
        ident: String,
        expression: Expression,
    ) -> Result<(), CompileError> {
        let stack_index = self.stack_pointer;
        self.stack_pointer += 1;

        match expression {
            Expression::Literal(Literal::Number(num)) => {
                self.write_output(format!("poke {stack_index} {num}\n"))?;
            }
            _ => {
                self.expression(expression)?;
                let register = self.register.len();
                self.write_output(format!("poke {stack_index} r{register}\n"))?;
                self.register.pop_back();
            }
        };
        self.variable_scope
            .last_mut()
            .ok_or(CompileError::ScopeError)?
            .insert(ident, stack_index);

        Ok(())
    }

    fn block_expression(&mut self, expression: BlockExpression) -> Result<(), CompileError> {
        self.variable_scope.push(HashMap::new());

        for expr in expression.0 {
            self.expression(expr)?;
        }

        self.variable_scope.pop();

        Ok(())
    }

    fn binary_expression(&mut self, expression: BinaryExpression) -> Result<(), CompileError> {
        fn handle_expression<'a>(
            compiler: &'a mut Compiler,
            left: Expression,
            right: Expression,
            operator: &'static str,
            register_number: u8,
        ) -> Result<(), CompileError> {
            let value_left = match left {
                Expression::Literal(Literal::Number(num)) => {
                    format!("{num}")
                }
                Expression::Variable(name) => {
                    let stack_index = variable_index!(compiler, name);
                    compiler.write_output(format!(
                        "get r{0} d0 {stack_index}\n",
                        register_number + 1
                    ))?;
                    format!("r{0}", register_number + 1)
                }
                _ => todo!(),
            };

            let value_right = match right {
                Expression::Literal(Literal::Number(num)) => {
                    format!("{num}")
                }
                Expression::Variable(name) => {
                    let stack_index = variable_index!(compiler, name);
                    compiler.write_output(format!(
                        "get r{0} d0 {stack_index}\n",
                        register_number + 2
                    ))?;
                    format!("r{0}", register_number + 2)
                }
                _ => todo!(),
            };

            compiler.write_output(format!(
                "{operator} r{register_number} {value_left} {value_right}\n"
            ))?;

            Ok(())
        }

        let result_register = self.register.len();

        match expression {
            BinaryExpression::Add(left, right) => {
                handle_expression(self, *left, *right, "add", result_register as u8)?
            }
            BinaryExpression::Subtract(left, right) => {
                handle_expression(self, *left, *right, "sub", result_register as u8)?
            }
            BinaryExpression::Multiply(left, right) => {
                handle_expression(self, *left, *right, "mul", result_register as u8)?
            }
            BinaryExpression::Divide(left, right) => {
                handle_expression(self, *left, *right, "div", result_register as u8)?
            }
            _ => todo!("Exponents have a different instruction set. {{ exp r? a(r?|num) }}"),
        }

        Ok(())
    }
}
