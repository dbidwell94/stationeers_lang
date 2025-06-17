#[cfg(test)]
mod test;

use parser::Parser as ASTParser;
use parser::sys_call::SysCall;
use parser::tree_node::*;
use quick_error::quick_error;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{BufWriter, Write};

quick_error! {
    #[derive(Debug)]
    pub enum CompileError {
        ParseError(err: parser::ParseError) {
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
        MissingDevice(name: String) {
            display("Device {} was not found in the device table.", name)
        }
        InvalidSyscall(syscall: SysCall) {
            display("Syscall {} is not valid.", syscall)
        }
    }
}

pub struct Compiler<'a> {
    parser: ASTParser,
    /// Max stack size for the program is by default 512.
    variable_scope: Vec<HashMap<String, i32>>,
    function_locations: HashMap<String, usize>,
    devices: HashMap<String, String>,
    output: &'a mut BufWriter<Box<dyn Write>>,
    current_line: usize,
    declared_main: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(parser: ASTParser, writer: &'a mut BufWriter<Box<dyn Write>>) -> Self {
        Self {
            parser,
            variable_scope: Vec::new(),
            function_locations: HashMap::new(),
            devices: HashMap::new(),
            output: writer,
            current_line: 0,
            declared_main: false,
        }
    }

    fn get_variable_index(&self, var_name: &str) -> Result<i32, CompileError> {
        let mut offset = 0;

        for scope in &self.variable_scope {
            let scope_size = scope.len() as i32;
            if let Some(index) = scope.get(var_name) {
                let index = (scope_size - *index) + offset;

                return Ok(index);
            }

            offset += scope_size;
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

    /// Pop the given variable from the current stack. Errors if the variable is not found in the
    /// current scope.
    fn pop_current(&mut self, var_name: &str) -> Result<i32, CompileError> {
        let last_scope = self
            .variable_scope
            .last_mut()
            .ok_or(CompileError::ScopeError)?;

        last_scope
            .remove(var_name)
            .ok_or(CompileError::VariableNotFound(var_name.to_string()))
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), CompileError> {
        self.output.write_all(output.into().as_bytes())?;
        self.output.write_all(b"\n")?;
        self.current_line += 1;

        Ok(())
    }

    pub fn compile(mut self) -> Result<(), CompileError> {
        let ast = self.parser.parse_all()?;

        let Some(ast) = ast else {
            return Ok(());
        };

        // Jump directly to the main block. This will avoid executing functions before the main block.
        self.write_output("j main")?;

        self.expression(ast)?;
        Ok(())
    }

    fn expression(&mut self, expression: Expression) -> Result<(), CompileError> {
        match expression {
            Expression::Function(expr) => self.function_expression(expr)?,
            Expression::Block(expr) => self.block_expression(expr)?,
            Expression::Invocation(expr) => self.invocation_expression(expr)?,
            Expression::Binary(expr) => self.binary_expression(expr)?,
            Expression::Declaration(var_name, expr) => {
                self.declaration_expression(&var_name, *expr)?
            }
            Expression::DeviceDeclaration(DeviceDeclarationExpression { name, device }) => {
                self.devices.insert(name, device);
            }
            _ => todo!("{:?}", expression),
        };

        Ok(())
    }

    fn declaration_expression(
        &mut self,
        var_name: &str,
        expr: Expression,
    ) -> Result<(), CompileError> {
        match expr {
            Expression::Literal(Literal::Number(num)) => {
                self.push_stack(var_name)?;
                self.write_output(format!("push {num}"))?;
            }
            Expression::Binary(expr) => {
                self.binary_expression(expr)?;
                self.push_stack(var_name)?;
            }
            Expression::Syscall(expr) => {
                self.syscall_declaration_expression(expr)?;
                self.push_stack(var_name)?;
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn syscall_declaration_expression(&mut self, expr: SysCall) -> Result<(), CompileError> {
        use parser::sys_call::System;
        #[allow(clippy::collapsible_match)]
        match expr {
            SysCall::System(ref sys) => match sys {
                System::LoadFromDevice(LiteralOrVariable::Variable(device), value) => {
                    let device = self
                        .devices
                        .get(device)
                        .ok_or(CompileError::MissingDevice(device.clone()))?;

                    self.write_output(format!("l r15 {device} {value}"))?;
                    self.write_output("push r15")?;
                }
                _ => return Err(CompileError::InvalidSyscall(expr)),
            },
            _ => return Err(CompileError::InvalidSyscall(expr)),
        }

        Ok(())
    }

    fn binary_expression(&mut self, expr: BinaryExpression) -> Result<(), CompileError> {
        self.variable_scope.push(HashMap::new());

        fn perform_operation(
            compiler: &mut Compiler,
            op: &str,
            left: Expression,
            right: Expression,
        ) -> Result<(), CompileError> {
            match left {
                Expression::Literal(Literal::Number(num)) => {
                    compiler.write_output(format!("push {num}"))?;
                    compiler.push_stack(&format!("{op}ExpressionLeft"))?;
                }
                Expression::Variable(var_name) => {
                    let var_offset = compiler.get_variable_index(&var_name)? + 1;
                    compiler.write_output(format!("sub r15 sp {var_offset}"))?;
                    compiler.write_output("get r15 db r15")?;
                    compiler.write_output("push r15")?;
                    compiler.push_stack(&format!("{op}ExpressionLeft"))?;
                }
                Expression::Binary(expr) => {
                    compiler.binary_expression(expr)?;
                    compiler.push_stack(&format!("{op}ExpressionLeft"))?;
                }
                Expression::Priority(expr) => match *expr {
                    Expression::Binary(expr) => {
                        compiler.binary_expression(expr)?;
                        compiler.push_stack(&format!("{op}ExpressionLeft"))?;
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            };

            match right {
                Expression::Literal(Literal::Number(num)) => {
                    compiler.write_output(format!("push {num}"))?;
                    compiler.push_stack(&format!("{op}ExpressionRight"))?;
                }
                Expression::Variable(var_name) => {
                    let var_offset = compiler.get_variable_index(&var_name)? + 1;
                    compiler.write_output(format!("sub r15 sp {}", var_offset))?;
                    compiler.write_output("get r15 db r15")?;
                    compiler.write_output("push r15")?;
                    compiler.push_stack(&format!("{op}ExpressionRight"))?;
                }
                Expression::Binary(expr) => {
                    compiler.binary_expression(expr)?;
                    compiler.push_stack(&format!("{op}ExpressionRight"))?;
                }
                Expression::Priority(expr) => match *expr {
                    Expression::Binary(expr) => {
                        compiler.binary_expression(expr)?;
                        compiler.push_stack(&format!("{op}ExpressionRight"))?;
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            };

            compiler.write_output("pop r1")?;
            compiler.write_output("pop r0")?;
            compiler.write_output(format!("{op} r0 r0 r1"))?;
            compiler.write_output("push r0")?;

            Ok(())
        }

        match expr {
            BinaryExpression::Add(left, right) => {
                perform_operation(self, "add", *left, *right)?;
            }
            BinaryExpression::Subtract(left, right) => {
                perform_operation(self, "sub", *left, *right)?;
            }
            BinaryExpression::Multiply(left, right) => {
                perform_operation(self, "mul", *left, *right)?;
            }
            BinaryExpression::Divide(left, right) => {
                perform_operation(self, "div", *left, *right)?;
            }
            _ => todo!("Operation not currently supported"),
        }
        self.variable_scope.pop();

        Ok(())
    }

    fn invocation_expression(&mut self, expr: InvocationExpression) -> Result<(), CompileError> {
        let function_name = expr.name;
        let args_count = expr.arguments.len();

        let function_line = *self
            .function_locations
            .get(&function_name)
            .ok_or(CompileError::MissingFunction(function_name.clone()))?;

        let mut to_write = String::new();

        self.push_stack(&format!("{function_name}ReturnAddress"))?;

        for (iter_index, arg) in expr.arguments.into_iter().enumerate() {
            match arg {
                Expression::Literal(Literal::Number(num)) => {
                    to_write.push_str(&format!("push {}\n", num));
                }
                Expression::Variable(var_name) => {
                    let index = self.get_variable_index(&var_name)?;

                    to_write.push_str(&format!("sub r15 sp {index}\n"));
                    to_write.push_str("get r15 db r15\n");
                    to_write.push_str("push r15\n");
                }
                Expression::Binary(expr) => {
                    self.binary_expression(expr)?;
                    to_write.push_str("push r0\n");
                }
                _ => todo!("something is up with the arguments: {arg:?}"),
            }
            self.push_stack(&format!("{function_name}Invocation{iter_index}"))?;
        }

        // push the return address onto the stack. Current + to write + pushing the return address
        let return_addr = self.current_line + to_write.lines().count() + 2;
        self.write_output(format!("push {return_addr}"))?;
        self.output.write_all(to_write.as_bytes())?;
        self.current_line = return_addr - 1;

        self.write_output(format!("j {function_line}"))?;

        self.pop_current(&format!("{function_name}ReturnAddress"))?;
        for i in 0..args_count {
            self.pop_current(&format!("{function_name}Invocation{i}"))?;
        }

        Ok(())
    }

    fn function_expression(&mut self, expression: FunctionExpression) -> Result<(), CompileError> {
        let func_name = expression.name;

        self.variable_scope.push(HashMap::new());

        self.function_locations.insert(func_name, self.current_line);

        for arg in expression.arguments.iter().rev() {
            self.push_stack(arg)?;
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
            if matches!(a, Expression::Function(_)) && matches!(b, Expression::Function(_)) {
                Ordering::Equal
            } else if matches!(a, Expression::Function(_)) {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });

        for expr in expression.0 {
            // if we haven't declared main yet and we have already declared all the function expressions, declare main
            if !self.declared_main && !matches!(expr, Expression::Function(_)) {
                self.write_output("main:")?;
                self.declared_main = true;
            }
            self.expression(expr)?;
        }

        self.variable_scope.pop();

        Ok(())
    }
}
