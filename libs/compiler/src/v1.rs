use crate::variable_manager::{self, LocationRequest, VariableLocation, VariableScope};
use parser::{
    Parser as ASTParser,
    tree_node::{
        BinaryExpression, BlockExpression, DeviceDeclarationExpression, Expression,
        FunctionExpression, InvocationExpression, Literal, LogicalExpression,
    },
};
use quick_error::quick_error;
use std::{
    collections::HashMap,
    io::{BufWriter, Write},
};

macro_rules! debug {
    ($self: expr, $debug_value: expr) => {
        if $self.config.debug {
            format!($debug_value)
        } else {
            "".into()
        }
    };
}

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        ParseError(error: parser::Error) {
            from()
        }
        IoError(error: std::io::Error) {
            from()
        }
        ScopeError(error: variable_manager::Error) {
            from()
        }
        DuplicateIdentifier(func_name: String) {
            display("`{func_name}` has already been defined")
        }
        UnknownIdentifier(ident: String) {
            display("`{ident}` is not found in the current scope.")
        }
        InvalidDevice(device: String) {
            display("`{device}` is not valid")
        }
        AgrumentMismatch(func_name: String) {
            display("Incorrect number of arguments passed into `{func_name}`")
        }
        Unknown(reason: String) {
            display("{reason}")
        }
    }
}

#[derive(Default)]
#[repr(C)]
pub struct CompilerConfig {
    pub debug: bool,
}

struct CompilationResult {
    location: VariableLocation,
    /// If Some, this is the name of the temporary variable that holds the result.
    /// It must be freed by the caller when done.
    temp_name: Option<String>,
}

pub struct Compiler<'a, W: std::io::Write> {
    parser: ASTParser,
    function_locations: HashMap<String, usize>,
    function_metadata: HashMap<String, Vec<String>>,
    devices: HashMap<String, String>,
    output: &'a mut BufWriter<W>,
    current_line: usize,
    declared_main: bool,
    config: CompilerConfig,
    temp_counter: usize,
}

impl<'a, W: std::io::Write> Compiler<'a, W> {
    pub fn new(
        parser: ASTParser,
        writer: &'a mut BufWriter<W>,
        config: Option<CompilerConfig>,
    ) -> Self {
        Self {
            parser,
            function_locations: HashMap::new(),
            function_metadata: HashMap::new(),
            devices: HashMap::new(),
            output: writer,
            current_line: 1,
            declared_main: false,
            config: config.unwrap_or_default(),
            temp_counter: 0,
        }
    }

    pub fn compile(mut self) -> Result<(), Error> {
        let expr = self.parser.parse_all()?;

        let Some(expr) = expr else { return Ok(()) };

        self.write_output("j main")?;
        // We ignore the result of the root expression (usually a block)
        let _ = self.expression(expr, &mut VariableScope::default())?;
        Ok(())
    }

    fn write_output(&mut self, output: impl Into<String>) -> Result<(), Error> {
        self.output.write_all(output.into().as_bytes())?;
        self.output.write_all(b"\n")?;
        self.current_line += 1;

        Ok(())
    }

    fn next_temp_name(&mut self) -> String {
        self.temp_counter += 1;
        format!("__binary_temp_{}", self.temp_counter)
    }

    fn expression<'v>(
        &mut self,
        expr: Expression,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<CompilationResult>, Error> {
        match expr {
            Expression::Function(expr_func) => {
                self.expression_function(expr_func, scope)?;
                Ok(None)
            }
            Expression::Block(expr_block) => {
                self.expression_block(expr_block, scope)?;
                Ok(None)
            }
            Expression::DeviceDeclaration(expr_dev) => {
                self.expression_device(expr_dev)?;
                Ok(None)
            }
            Expression::Declaration(var_name, expr) => {
                let loc = self.expression_declaration(var_name, *expr, scope)?;
                Ok(loc.map(|l| CompilationResult {
                    location: l,
                    temp_name: None,
                }))
            }
            Expression::Invocation(expr_invoke) => {
                self.expression_function_invocation(expr_invoke, scope)?;
                // Invocation returns result in r15 (RETURN_REGISTER).
                // If used as an expression, we must move it to a temp to avoid overwrite.
                let temp_name = self.next_temp_name();
                let temp_loc = scope.add_variable(&temp_name, LocationRequest::Temp)?;
                self.emit_variable_assignment(
                    &temp_name,
                    &temp_loc,
                    format!("r{}", VariableScope::RETURN_REGISTER),
                )?;
                Ok(Some(CompilationResult {
                    location: temp_loc,
                    temp_name: Some(temp_name),
                }))
            }
            Expression::Binary(bin_expr) => {
                let result = self.expression_binary(bin_expr, scope)?;
                Ok(Some(result))
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                Ok(Some(result))
            }
            Expression::Literal(Literal::Number(num)) => {
                let temp_name = self.next_temp_name();
                let loc = scope.add_variable(&temp_name, LocationRequest::Temp)?;
                self.emit_variable_assignment(&temp_name, &loc, num.to_string())?;
                Ok(Some(CompilationResult {
                    location: loc,
                    temp_name: Some(temp_name),
                }))
            }
            Expression::Variable(name) => {
                let loc = scope.get_location_of(&name)?;
                Ok(Some(CompilationResult {
                    location: loc,
                    temp_name: None, // User variable, do not free
                }))
            }
            Expression::Priority(inner_expr) => self.expression(*inner_expr, scope),
            Expression::Negation(inner_expr) => {
                // Compile negation as 0 - inner
                let (inner_str, cleanup) = self.compile_operand(*inner_expr, scope)?;
                let result_name = self.next_temp_name();
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp)?;
                let result_reg = self.resolve_register(&result_loc)?;

                self.write_output(format!("sub {result_reg} 0 {inner_str}"))?;

                if let Some(name) = cleanup {
                    scope.free_temp(name)?;
                }

                Ok(Some(CompilationResult {
                    location: result_loc,
                    temp_name: Some(result_name),
                }))
            }
            _ => Err(Error::Unknown(format!(
                "Expression type not yet supported in general expression context: {:?}",
                expr
            ))),
        }
    }

    fn emit_variable_assignment(
        &mut self,
        var_name: &str,
        location: &VariableLocation,
        source_value: impl Into<String>,
    ) -> Result<(), Error> {
        let debug_tag = if self.config.debug {
            format!(" #{var_name}")
        } else {
            String::new()
        };

        match location {
            VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                self.write_output(format!("move r{reg} {}{debug_tag}", source_value.into()))?;
            }
            VariableLocation::Stack(_) => {
                self.write_output(format!("push {}{debug_tag}", source_value.into()))?;
            }
        }

        Ok(())
    }

    fn expression_declaration<'v>(
        &mut self,
        var_name: String,
        expr: Expression,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<VariableLocation>, Error> {
        // optimization. Check for a negated numeric literal
        if let Expression::Negation(box_expr) = &expr
            && let Expression::Literal(Literal::Number(neg_num)) = &**box_expr
        {
            let loc = scope.add_variable(&var_name, LocationRequest::Persist)?;
            self.emit_variable_assignment(&var_name, &loc, format!("-{neg_num}"))?;
            return Ok(Some(loc));
        }

        let loc = match expr {
            Expression::Literal(Literal::Number(num)) => {
                let var_location =
                    scope.add_variable(var_name.clone(), LocationRequest::Persist)?;

                self.emit_variable_assignment(&var_name, &var_location, num)?;
                var_location
            }
            Expression::Invocation(invoke_expr) => {
                self.expression_function_invocation(invoke_expr, scope)?;

                let loc = scope.add_variable(&var_name, LocationRequest::Persist)?;
                self.emit_variable_assignment(
                    &var_name,
                    &loc,
                    format!("r{}", VariableScope::RETURN_REGISTER),
                )?;
                loc
            }
            // Support assigning binary expressions to variables directly
            Expression::Binary(bin_expr) => {
                let result = self.expression_binary(bin_expr, scope)?;
                let var_loc = scope.add_variable(&var_name, LocationRequest::Persist)?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&var_name, &var_loc, result_reg)?;

                // Free the temp result
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
                var_loc
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                let var_loc = scope.add_variable(&var_name, LocationRequest::Persist)?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&var_name, &var_loc, result_reg)?;

                // Free the temp result
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
                var_loc
            }
            Expression::Variable(name) => {
                let src_loc = scope.get_location_of(&name)?;
                let var_loc = scope.add_variable(&var_name, LocationRequest::Persist)?;

                // Handle loading from stack if necessary
                let src_str = match src_loc {
                    VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => {
                        format!("r{r}")
                    }
                    VariableLocation::Stack(offset) => {
                        self.write_output(format!(
                            "sub r{0} sp {offset}",
                            VariableScope::TEMP_STACK_REGISTER
                        ))?;
                        self.write_output(format!(
                            "get r{0} db r{0}",
                            VariableScope::TEMP_STACK_REGISTER
                        ))?;
                        format!("r{}", VariableScope::TEMP_STACK_REGISTER)
                    }
                };
                self.emit_variable_assignment(&var_name, &var_loc, src_str)?;
                var_loc
            }
            Expression::Priority(inner) => {
                return self.expression_declaration(var_name, *inner, scope);
            }
            _ => {
                return Err(Error::Unknown(format!(
                    "`{var_name}` declaration of this type is not supported/implemented."
                )));
            }
        };

        Ok(Some(loc))
    }

    fn expression_function_invocation(
        &mut self,
        invoke_expr: InvocationExpression,
        stack: &mut VariableScope,
    ) -> Result<(), Error> {
        if !self.function_locations.contains_key(&invoke_expr.name) {
            return Err(Error::UnknownIdentifier(invoke_expr.name));
        }

        let Some(args) = self.function_metadata.get(&invoke_expr.name) else {
            return Err(Error::UnknownIdentifier(invoke_expr.name));
        };

        if args.len() != invoke_expr.arguments.len() {
            return Err(Error::AgrumentMismatch(invoke_expr.name));
        }

        // backup all used registers to the stack
        let active_registers = stack.registers().cloned().collect::<Vec<_>>();
        for register in &active_registers {
            stack.add_variable(format!("temp_{register}"), LocationRequest::Stack)?;
            self.write_output(format!("push r{register}"))?;
        }
        for arg in invoke_expr.arguments {
            match arg {
                Expression::Literal(Literal::Number(num)) => {
                    let num_str = num.to_string();
                    self.write_output(format!("push {num_str}"))?;
                }
                Expression::Variable(var_name) => match stack.get_location_of(var_name)? {
                    VariableLocation::Persistant(reg) | VariableLocation::Temporary(reg) => {
                        self.write_output(format!("push r{reg}"))?;
                    }
                    VariableLocation::Stack(stack_offset) => {
                        self.write_output(format!(
                            "sub r{0} sp {stack_offset}",
                            VariableScope::TEMP_STACK_REGISTER
                        ))?;
                        self.write_output(format!(
                            "get r{0} db r{0}",
                            VariableScope::TEMP_STACK_REGISTER
                        ))?;
                        self.write_output(format!(
                            "push r{0}",
                            VariableScope::TEMP_STACK_REGISTER
                        ))?;
                    }
                },
                Expression::Binary(bin_expr) => {
                    // Compile the binary expression to a temp register
                    let result = self.expression_binary(bin_expr, stack)?;
                    let reg_str = self.resolve_register(&result.location)?;
                    self.write_output(format!("push {reg_str}"))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name)?;
                    }
                }
                Expression::Logical(log_expr) => {
                    // Compile the logical expression to a temp register
                    let result = self.expression_logical(log_expr, stack)?;
                    let reg_str = self.resolve_register(&result.location)?;
                    self.write_output(format!("push {reg_str}"))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name)?;
                    }
                }
                _ => {
                    return Err(Error::Unknown(format!(
                        "Attempted to call `{}` with an unsupported argument type",
                        invoke_expr.name
                    )));
                }
            }
        }

        // jump to the function and store current line in ra
        self.write_output(format!("jal {}", invoke_expr.name))?;

        for register in active_registers {
            let VariableLocation::Stack(stack_offset) =
                stack.get_location_of(format!("temp_{register}"))?
            else {
                return Err(Error::UnknownIdentifier(format!("temp_{register}")));
            };
            self.write_output(format!(
                "sub r{0} sp {stack_offset}",
                VariableScope::TEMP_STACK_REGISTER
            ))?;
            self.write_output(format!(
                "get r{register} db r{0}",
                VariableScope::TEMP_STACK_REGISTER
            ))?;
        }

        if stack.stack_offset() > 0 {
            self.write_output(format!("sub sp sp {}", stack.stack_offset()))?;
        }

        Ok(())
    }

    fn expression_device(&mut self, expr: DeviceDeclarationExpression) -> Result<(), Error> {
        if self.devices.contains_key(&expr.name) {
            return Err(Error::DuplicateIdentifier(expr.name));
        }
        self.devices.insert(expr.name, expr.device);

        Ok(())
    }

    /// Helper to resolve a location to a register string (e.g., "r0").
    /// Note: This does not handle Stack locations automatically, as they require
    /// instruction emission to load. Use `compile_operand` for general handling.
    fn resolve_register(&self, loc: &VariableLocation) -> Result<String, Error> {
        match loc {
            VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => Ok(format!("r{r}")),
            VariableLocation::Stack(_) => Err(Error::Unknown(
                "Cannot resolve Stack location directly to register string without context".into(),
            )),
        }
    }

    /// Compiles an expression and ensures the result is available as a string valid for an
    /// IC10 operand (either a register "rX" or a literal value "123").
    /// If the result was stored in a new temporary register, returns the name of that temp
    /// so the caller can free it.
    fn compile_operand(
        &mut self,
        expr: Expression,
        scope: &mut VariableScope,
    ) -> Result<(String, Option<String>), Error> {
        // Optimization for literals
        if let Expression::Literal(Literal::Number(n)) = expr {
            return Ok((n.to_string(), None));
        }

        // Optimization for negated literals used as operands.
        // E.g., `1 + -2` -> return "-2" string, no register used.
        if let Expression::Negation(inner) = &expr
            && let Expression::Literal(Literal::Number(n)) = &**inner
        {
            return Ok((format!("-{}", n), None));
        }

        let result = self
            .expression(expr, scope)?
            .ok_or(Error::Unknown("Expression did not return a value".into()))?;

        match result.location {
            VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => {
                Ok((format!("r{r}"), result.temp_name))
            }
            VariableLocation::Stack(offset) => {
                // If it's on the stack, we must load it into a temp to use it as an operand
                let temp_name = self.next_temp_name();
                let temp_loc = scope.add_variable(&temp_name, LocationRequest::Temp)?;
                let temp_reg = self.resolve_register(&temp_loc)?;

                self.write_output(format!(
                    "sub r{0} sp {offset}",
                    VariableScope::TEMP_STACK_REGISTER
                ))?;
                self.write_output(format!(
                    "get {temp_reg} db r{0}",
                    VariableScope::TEMP_STACK_REGISTER
                ))?;

                // If the original result had a temp name (unlikely for Stack, but possible logic),
                // we technically should free it if it's not needed, but Stack usually implies it's safe there.
                // We return the NEW temp name to be freed.
                Ok((temp_reg, Some(temp_name)))
            }
        }
    }

    fn expression_binary<'v>(
        &mut self,
        expr: BinaryExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<CompilationResult, Error> {
        let (op_str, left_expr, right_expr) = match expr {
            BinaryExpression::Add(l, r) => ("add", l, r),
            BinaryExpression::Multiply(l, r) => ("mul", l, r),
            BinaryExpression::Divide(l, r) => ("div", l, r),
            BinaryExpression::Subtract(l, r) => ("sub", l, r),
            BinaryExpression::Exponent(l, r) => ("pow", l, r),
            BinaryExpression::Modulo(l, r) => ("mod", l, r),
        };

        // Compile LHS
        let (lhs_str, lhs_cleanup) = self.compile_operand(*left_expr, scope)?;
        // Compile RHS
        let (rhs_str, rhs_cleanup) = self.compile_operand(*right_expr, scope)?;

        // Allocate result register
        let result_name = self.next_temp_name();
        let result_loc = scope.add_variable(&result_name, LocationRequest::Temp)?;
        let result_reg = self.resolve_register(&result_loc)?;

        // Emit instruction: op result lhs rhs
        self.write_output(format!("{op_str} {result_reg} {lhs_str} {rhs_str}"))?;

        // Clean up operand temps
        if let Some(name) = lhs_cleanup {
            scope.free_temp(name)?;
        }
        if let Some(name) = rhs_cleanup {
            scope.free_temp(name)?;
        }

        Ok(CompilationResult {
            location: result_loc,
            temp_name: Some(result_name),
        })
    }

    fn expression_logical<'v>(
        &mut self,
        expr: LogicalExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<CompilationResult, Error> {
        match expr {
            LogicalExpression::Not(inner) => {
                let (inner_str, cleanup) = self.compile_operand(*inner, scope)?;

                let result_name = self.next_temp_name();
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // seq rX rY 0  => if rY == 0 set rX = 1 else rX = 0
                self.write_output(format!("seq {result_reg} {inner_str} 0"))?;

                if let Some(name) = cleanup {
                    scope.free_temp(name)?;
                }

                Ok(CompilationResult {
                    location: result_loc,
                    temp_name: Some(result_name),
                })
            }
            _ => {
                let (op_str, left_expr, right_expr) = match expr {
                    LogicalExpression::And(l, r) => ("and", l, r),
                    LogicalExpression::Or(l, r) => ("or", l, r),
                    LogicalExpression::Equal(l, r) => ("seq", l, r),
                    LogicalExpression::NotEqual(l, r) => ("sne", l, r),
                    LogicalExpression::GreaterThan(l, r) => ("sgt", l, r),
                    LogicalExpression::GreaterThanOrEqual(l, r) => ("sge", l, r),
                    LogicalExpression::LessThan(l, r) => ("slt", l, r),
                    LogicalExpression::LessThanOrEqual(l, r) => ("sle", l, r),
                    LogicalExpression::Not(_) => unreachable!(),
                };

                // Compile LHS
                let (lhs_str, lhs_cleanup) = self.compile_operand(*left_expr, scope)?;
                // Compile RHS
                let (rhs_str, rhs_cleanup) = self.compile_operand(*right_expr, scope)?;

                // Allocate result register
                let result_name = self.next_temp_name();
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // Emit instruction: op result lhs rhs
                self.write_output(format!("{op_str} {result_reg} {lhs_str} {rhs_str}"))?;

                // Clean up operand temps
                if let Some(name) = lhs_cleanup {
                    scope.free_temp(name)?;
                }
                if let Some(name) = rhs_cleanup {
                    scope.free_temp(name)?;
                }

                Ok(CompilationResult {
                    location: result_loc,
                    temp_name: Some(result_name),
                })
            }
        }
    }

    fn expression_block<'v>(
        &mut self,
        mut expr: BlockExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        // First, sort the expressions to ensure functions are hoisted
        expr.0.sort_by(|a, b| {
            if matches!(b, Expression::Function(_)) && matches!(a, Expression::Function(_)) {
                std::cmp::Ordering::Equal
            } else if matches!(a, Expression::Function(_)) {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        });

        for expr in expr.0 {
            if !self.declared_main
                && !matches!(expr, Expression::Function(_))
                && !scope.has_parent()
            {
                self.write_output("main:")?;
                self.declared_main = true;
            }

            match expr {
                Expression::Return(ret_expr) => {
                    self.expression_return(*ret_expr, scope)?;
                }
                _ => {
                    let result = self.expression(expr, scope)?;
                    // If the expression was a statement that returned a temp result (e.g. `1 + 2;` line),
                    // we must free it to avoid leaking registers.
                    if let Some(comp_res) = result
                        && let Some(name) = comp_res.temp_name
                    {
                        scope.free_temp(name)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Takes the result of the expression and stores it in VariableScope::RETURN_REGISTER
    fn expression_return<'v>(
        &mut self,
        expr: Expression,
        scope: &mut VariableScope<'v>,
    ) -> Result<VariableLocation, Error> {
        if let Expression::Negation(neg_expr) = &expr
            && let Expression::Literal(Literal::Number(neg_num)) = &**neg_expr
        {
            let loc = VariableLocation::Persistant(VariableScope::RETURN_REGISTER);
            self.emit_variable_assignment("returnValue", &loc, format!("-{neg_num}"))?;
            return Ok(loc);
        };

        match expr {
            Expression::Variable(var_name) => match scope.get_location_of(var_name)? {
                VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                    self.write_output(format!(
                        "move r{} r{reg} {}",
                        VariableScope::RETURN_REGISTER,
                        debug!(self, "#returnValue")
                    ))?;
                }
                VariableLocation::Stack(offset) => {
                    self.write_output(format!(
                        "sub r{} sp {offset}",
                        VariableScope::TEMP_STACK_REGISTER
                    ))?;
                    self.write_output(format!(
                        "get r{} db r{}",
                        VariableScope::RETURN_REGISTER,
                        VariableScope::TEMP_STACK_REGISTER
                    ))?;
                }
            },
            Expression::Literal(Literal::Number(num)) => {
                self.emit_variable_assignment(
                    "returnValue",
                    &VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    num,
                )?;
            }
            Expression::Binary(bin_expr) => {
                let result = self.expression_binary(bin_expr, scope)?;
                let result_reg = self.resolve_register(&result.location)?;
                self.write_output(format!(
                    "move r{} {}",
                    VariableScope::RETURN_REGISTER,
                    result_reg
                ))?;
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                let result_reg = self.resolve_register(&result.location)?;
                self.write_output(format!(
                    "move r{} {}",
                    VariableScope::RETURN_REGISTER,
                    result_reg
                ))?;
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
            }
            _ => {
                return Err(Error::Unknown(format!(
                    "Unsupported `return` statement: {:?}",
                    expr
                )));
            }
        }

        Ok(VariableLocation::Persistant(VariableScope::RETURN_REGISTER))
    }

    /// Compile a function declaration.
    /// Calees are responsible for backing up any registers they wish to use.
    fn expression_function<'v>(
        &mut self,
        expr: FunctionExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let FunctionExpression {
            name,
            arguments,
            body,
        } = expr;

        if self.function_locations.contains_key(&name) {
            return Err(Error::DuplicateIdentifier(name));
        }

        self.function_metadata
            .insert(name.clone(), arguments.clone());

        // Declare the function as a line identifier
        self.write_output(format!("{}:", name))?;

        self.function_locations
            .insert(name.clone(), self.current_line);

        // Create a new block scope for the function body
        let mut block_scope = VariableScope::scoped(scope);

        let mut saved_variables = 0;

        // do a reverse pass to pop variables from the stack and put them into registers
        for var_name in arguments
            .iter()
            .rev()
            .take(VariableScope::PERSIST_REGISTER_COUNT as usize)
        {
            let loc = block_scope.add_variable(var_name, LocationRequest::Persist)?;
            // we don't need to imcrement the stack offset as it's already on the stack from the
            // previous scope

            match loc {
                VariableLocation::Persistant(loc) => {
                    self.write_output(format!("pop r{loc} {}", debug!(self, "#{var_name}")))?;
                }
                VariableLocation::Stack(_) => {
                    return Err(Error::Unknown(
                        "Attempted to save to stack without tracking in scope".into(),
                    ));
                }

                _ => {
                    return Err(Error::Unknown(
                        "Attempted to return a Temporary scoped variable from a Persistant request"
                            .into(),
                    ));
                }
            }
            saved_variables += 1;
        }

        // now do a forward pass in case we have spilled into the stack. We don't need to push
        // anything as they already exist on the stack, but we DO need to let our block_scope be
        // aware that the variables exist on the stack (left to right)
        for var_name in arguments.iter().take(arguments.len() - saved_variables) {
            block_scope.add_variable(var_name, LocationRequest::Stack)?;
        }

        self.write_output("push ra")?;
        block_scope.add_variable(format!("{name}_ra"), LocationRequest::Stack)?;

        for expr in body.0 {
            match expr {
                Expression::Return(ret_expr) => {
                    self.expression_return(*ret_expr, &mut block_scope)?;
                }
                _ => {
                    let result = self.expression(expr, &mut block_scope)?;
                    // Free unused statement results
                    if let Some(comp_res) = result
                        && let Some(name) = comp_res.temp_name
                    {
                        block_scope.free_temp(name)?;
                    }
                }
            }
        }

        // Get the saved return address and save it back into `ra`
        let VariableLocation::Stack(ra_stack_offset) =
            block_scope.get_location_of(format!("{name}_ra"))?
        else {
            return Err(Error::Unknown(
                "Stored return address not in stack as expected".into(),
            ));
        };

        self.write_output(format!(
            "sub r{0} sp {ra_stack_offset}",
            VariableScope::TEMP_STACK_REGISTER
        ))?;
        self.write_output(format!(
            "get ra db r{0}",
            VariableScope::TEMP_STACK_REGISTER
        ))?;

        if block_scope.stack_offset() > 0 {
            self.write_output(format!("sub sp sp {}", block_scope.stack_offset()))?;
        }

        self.write_output("j ra")?;
        Ok(())
    }
}

