#![allow(clippy::result_large_err)]
use crate::variable_manager::{self, LocationRequest, VariableLocation, VariableScope};
use parser::{
    Parser as ASTParser,
    sys_call::{SysCall, System},
    tree_node::{
        AssignmentExpression, BinaryExpression, BlockExpression, DeviceDeclarationExpression,
        Expression, FunctionExpression, IfExpression, InvocationExpression, Literal,
        LiteralOrVariable, LogicalExpression, LoopExpression, Span, Spanned, WhileExpression,
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

    ($self: expr, $debug_value: expr, $args: expr) => {
        if $self.config.debug {
            format!($debug_value, $args)
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
        IoError(error: String) {
            display("IO Error: {}", error)
        }
        ScopeError(error: variable_manager::Error) {
            from()
        }
        DuplicateIdentifier(func_name: String, span: Span) {
            display("`{func_name}` has already been defined")
        }
        UnknownIdentifier(ident: String, span: Span) {
            display("`{ident}` is not found in the current scope.")
        }
        InvalidDevice(device: String, span: Span) {
            display("`{device}` is not valid")
        }
        AgrumentMismatch(func_name: String, span: Span) {
            display("Incorrect number of arguments passed into `{func_name}`")
        }
        Unknown(reason: String, span: Option<Span>) {
            display("{reason}")
        }
    }
}

impl From<Error> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            ParseError(e) => e.into(),
            IoError(e) => Diagnostic {
                message: e.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            ScopeError(e) => Diagnostic {
                message: e.to_string(),
                range: Range::default(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            DuplicateIdentifier(_, span)
            | UnknownIdentifier(_, span)
            | InvalidDevice(_, span)
            | AgrumentMismatch(_, span) => Diagnostic {
                range: span.into(),
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            Unknown(msg, span) => Diagnostic {
                message: msg.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                range: span.map(lsp_types::Range::from).unwrap_or_default(),
                ..Default::default()
            },
        }
    }
}

// Map io::Error to Error manually since we can't clone io::Error
impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IoError(err.to_string())
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
    pub parser: ASTParser<'a>,
    function_locations: HashMap<String, usize>,
    function_metadata: HashMap<String, Vec<String>>,
    devices: HashMap<String, String>,
    output: &'a mut BufWriter<W>,
    current_line: usize,
    declared_main: bool,
    config: CompilerConfig,
    temp_counter: usize,
    label_counter: usize,
    loop_stack: Vec<(String, String)>, // Stores (start_label, end_label)
    pub errors: Vec<Error>,
}

impl<'a, W: std::io::Write> Compiler<'a, W> {
    pub fn new(
        parser: ASTParser<'a>,
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
            label_counter: 0,
            loop_stack: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn compile(mut self) -> Vec<Error> {
        let expr = self.parser.parse_all();

        // Copy errors from parser
        for e in std::mem::take(&mut self.parser.errors) {
            self.errors.push(Error::ParseError(e));
        }

        // We treat parse_all result as potentially partial
        let expr = match expr {
            Ok(Some(expr)) => expr,
            Ok(None) => return self.errors,
            Err(e) => {
                // Should be covered by parser.errors, but just in case
                self.errors.push(Error::ParseError(e));
                return self.errors;
            }
        };

        // Wrap the root expression in a dummy span for consistency
        let span = if let Expression::Block(ref block) = expr {
            block.span
        } else {
            Span {
                start_line: 0,
                end_line: 0,
                start_col: 0,
                end_col: 0,
            }
        };

        let spanned_root = Spanned { node: expr, span };

        if let Err(e) = self.write_output("j main") {
            self.errors.push(e);
            return self.errors;
        }

        // We ignore the result of the root expression (usually a block)
        if let Err(e) = self.expression(spanned_root, &mut VariableScope::default()) {
            self.errors.push(e);
        }

        self.errors
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

    fn next_label_name(&mut self) -> String {
        self.label_counter += 1;
        format!("L{}", self.label_counter)
    }

    fn expression<'v>(
        &mut self,
        expr: Spanned<Expression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<CompilationResult>, Error> {
        match expr.node {
            Expression::Function(expr_func) => {
                self.expression_function(expr_func, scope)?;
                Ok(None)
            }
            Expression::Block(expr_block) => {
                self.expression_block(expr_block.node, scope)?;
                Ok(None)
            }
            Expression::If(expr_if) => {
                self.expression_if(expr_if.node, scope)?;
                Ok(None)
            }
            Expression::Loop(expr_loop) => {
                self.expression_loop(expr_loop.node, scope)?;
                Ok(None)
            }
            Expression::Syscall(Spanned {
                node: SysCall::System(system),
                span,
            }) => self.expression_syscall_system(system, span, scope),
            Expression::While(expr_while) => {
                self.expression_while(expr_while.node, scope)?;
                Ok(None)
            }
            Expression::Break(_) => {
                self.expression_break()?;
                Ok(None)
            }
            Expression::Continue(_) => {
                self.expression_continue()?;
                Ok(None)
            }
            Expression::DeviceDeclaration(expr_dev) => {
                self.expression_device(expr_dev.node, expr_dev.span)?;
                Ok(None)
            }
            Expression::Declaration(var_name, decl_expr) => {
                // decl_expr is Box<Spanned<Expression>>
                self.expression_declaration(var_name, *decl_expr, scope)
            }
            Expression::Assignment(assign_expr) => {
                self.expression_assignment(assign_expr.node, scope)?;
                Ok(None)
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
            Expression::Literal(spanned_lit) => match spanned_lit.node {
                Literal::Number(num) => {
                    let temp_name = self.next_temp_name();
                    let loc = scope.add_variable(&temp_name, LocationRequest::Temp)?;
                    self.emit_variable_assignment(&temp_name, &loc, num.to_string())?;
                    Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                Literal::Boolean(b) => {
                    let val = if b { "1" } else { "0" };
                    let temp_name = self.next_temp_name();
                    let loc = scope.add_variable(&temp_name, LocationRequest::Temp)?;
                    self.emit_variable_assignment(&temp_name, &loc, val)?;
                    Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                _ => Ok(None), // String literals don't return values in this context typically
            },
            Expression::Variable(name) => {
                match scope.get_location_of(&name.node) {
                    Ok(loc) => Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: None, // User variable, do not free
                    })),
                    Err(_) => {
                        self.errors
                            .push(Error::UnknownIdentifier(name.node.clone(), name.span));
                        Ok(Some(CompilationResult {
                            location: VariableLocation::Temporary(0),
                            temp_name: None,
                        }))
                    }
                }
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
            _ => Err(Error::Unknown(
                format!(
                    "Expression type not yet supported in general expression context: {:?}",
                    expr.node
                ),
                Some(expr.span),
            )),
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
        var_name: Spanned<String>,
        expr: Spanned<Expression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<CompilationResult>, Error> {
        let name_str = var_name.node;
        let name_span = var_name.span;

        // optimization. Check for a negated numeric literal
        if let Expression::Negation(box_expr) = &expr.node
            && let Expression::Literal(spanned_lit) = &box_expr.node
            && let Literal::Number(neg_num) = &spanned_lit.node
        {
            let loc = scope.add_variable(&name_str, LocationRequest::Persist)?;
            self.emit_variable_assignment(&name_str, &loc, format!("-{neg_num}"))?;
            return Ok(Some(CompilationResult {
                location: loc,
                temp_name: None,
            }));
        }

        let (loc, temp_name) = match expr.node {
            Expression::Literal(spanned_lit) => match spanned_lit.node {
                Literal::Number(num) => {
                    let var_location =
                        scope.add_variable(name_str.clone(), LocationRequest::Persist)?;

                    self.emit_variable_assignment(&name_str, &var_location, num)?;
                    (var_location, None)
                }
                Literal::Boolean(b) => {
                    let val = if b { "1" } else { "0" };
                    let var_location =
                        scope.add_variable(name_str.clone(), LocationRequest::Persist)?;

                    self.emit_variable_assignment(&name_str, &var_location, val)?;
                    (var_location, None)
                }
                _ => return Ok(None),
            },
            Expression::Invocation(invoke_expr) => {
                self.expression_function_invocation(invoke_expr, scope)?;

                let loc = scope.add_variable(&name_str, LocationRequest::Persist)?;
                self.emit_variable_assignment(
                    &name_str,
                    &loc,
                    format!("r{}", VariableScope::RETURN_REGISTER),
                )?;
                (loc, None)
            }
            Expression::Syscall(spanned_call) => {
                let sys_call = spanned_call.node;
                let SysCall::System(call) = sys_call else {
                    // Math syscalls might be handled differently or here
                    // For now assuming System returns value
                    return Err(Error::Unknown(
                        "Math syscall not yet supported in declaration".into(),
                        Some(spanned_call.span),
                    ));
                };

                if self
                    .expression_syscall_system(call, spanned_call.span, scope)?
                    .is_none()
                {
                    return Err(Error::Unknown(
                        "SysCall did not return a value".into(),
                        Some(spanned_call.span),
                    ));
                };

                let loc = scope.add_variable(&name_str, LocationRequest::Persist)?;
                self.emit_variable_assignment(
                    &name_str,
                    &loc,
                    format!("r{}", VariableScope::RETURN_REGISTER),
                )?;

                (loc, None)
            }
            // Support assigning binary expressions to variables directly
            Expression::Binary(bin_expr) => {
                let result = self.expression_binary(bin_expr, scope)?;
                let var_loc = scope.add_variable(&name_str, LocationRequest::Persist)?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&name_str, &var_loc, result_reg)?;

                // Free the temp result
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
                (var_loc, None)
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                let var_loc = scope.add_variable(&name_str, LocationRequest::Persist)?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&name_str, &var_loc, result_reg)?;

                // Free the temp result
                if let Some(name) = result.temp_name {
                    scope.free_temp(name)?;
                }
                (var_loc, None)
            }
            Expression::Variable(name) => {
                let src_loc_res = scope.get_location_of(&name.node);

                let src_loc = match src_loc_res {
                    Ok(l) => l,
                    Err(_) => {
                        self.errors
                            .push(Error::UnknownIdentifier(name.node.clone(), name.span));
                        VariableLocation::Temporary(0)
                    }
                };

                let var_loc = scope.add_variable(&name_str, LocationRequest::Persist)?;

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
                self.emit_variable_assignment(&name_str, &var_loc, src_str)?;
                (var_loc, None)
            }
            Expression::Priority(inner) => {
                return self.expression_declaration(
                    Spanned {
                        node: name_str,
                        span: name_span,
                    },
                    *inner,
                    scope,
                );
            }
            _ => {
                return Err(Error::Unknown(
                    format!("`{name_str}` declaration of this type is not supported/implemented."),
                    Some(name_span),
                ));
            }
        };

        Ok(Some(CompilationResult {
            location: loc,
            temp_name,
        }))
    }

    fn expression_assignment<'v>(
        &mut self,
        expr: AssignmentExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let AssignmentExpression {
            identifier,
            expression,
        } = expr;

        let location = match scope.get_location_of(&identifier.node) {
            Ok(l) => l,
            Err(_) => {
                self.errors.push(Error::UnknownIdentifier(
                    identifier.node.clone(),
                    identifier.span,
                ));
                VariableLocation::Temporary(0)
            }
        };

        let (val_str, cleanup) = self.compile_operand(*expression, scope)?;

        let debug_tag = if self.config.debug {
            format!(" #{}", identifier.node)
        } else {
            String::new()
        };

        match location {
            VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                self.write_output(format!("move r{reg} {val_str}{debug_tag}"))?;
            }
            VariableLocation::Stack(offset) => {
                // Calculate address: sp - offset
                self.write_output(format!(
                    "sub r{0} sp {offset}",
                    VariableScope::TEMP_STACK_REGISTER
                ))?;
                // Store value to stack/db at address
                self.write_output(format!(
                    "put db r{0} {val_str}{debug_tag}",
                    VariableScope::TEMP_STACK_REGISTER
                ))?;
            }
        }

        if let Some(name) = cleanup {
            scope.free_temp(name)?;
        }

        Ok(())
    }

    fn expression_function_invocation(
        &mut self,
        invoke_expr: Spanned<InvocationExpression>,
        stack: &mut VariableScope,
    ) -> Result<(), Error> {
        let InvocationExpression { name, arguments } = invoke_expr.node;

        if !self.function_locations.contains_key(&name.node) {
            self.errors
                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
            // Don't emit call, just pretend we did?
            // Actually, we should probably emit a dummy call or just skip to avoid logic errors
            // But if we skip, registers might be unbalanced if something expected a return.
            // For now, let's just return early.
            return Ok(());
        }

        let Some(args) = self.function_metadata.get(&name.node) else {
            // Should be covered by check above
            return Err(Error::UnknownIdentifier(name.node.clone(), name.span));
        };

        if args.len() != arguments.len() {
            self.errors
                .push(Error::AgrumentMismatch(name.node.clone(), name.span));
            // Proceed anyway? The assembly will likely crash or act weird.
            // Best to skip generation of this call to prevent bad IC10
            return Ok(());
        }

        // backup all used registers to the stack
        let active_registers = stack.registers().cloned().collect::<Vec<_>>();
        for register in &active_registers {
            stack.add_variable(format!("temp_{register}"), LocationRequest::Stack)?;
            self.write_output(format!("push r{register}"))?;
        }
        for arg in arguments {
            match arg.node {
                Expression::Literal(spanned_lit) => match spanned_lit.node {
                    Literal::Number(num) => {
                        let num_str = num.to_string();
                        self.write_output(format!("push {num_str}"))?;
                    }
                    Literal::Boolean(b) => {
                        let val = if b { "1" } else { "0" };
                        self.write_output(format!("push {val}"))?;
                    }
                    _ => {}
                },
                Expression::Variable(var_name) => {
                    let loc = match stack.get_location_of(var_name.node.clone()) {
                        Ok(l) => l,
                        Err(_) => {
                            self.errors
                                .push(Error::UnknownIdentifier(var_name.node, var_name.span));
                            VariableLocation::Temporary(0)
                        }
                    };

                    match loc {
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
                    }
                }
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
                    return Err(Error::Unknown(
                        format!(
                            "Attempted to call `{}` with an unsupported argument type",
                            name.node
                        ),
                        Some(name.span),
                    ));
                }
            }
        }

        // jump to the function and store current line in ra
        self.write_output(format!("jal {}", name.node))?;

        for register in active_registers {
            let VariableLocation::Stack(stack_offset) = stack
                .get_location_of(format!("temp_{register}"))
                .map_err(Error::ScopeError)?
            else {
                // This shouldn't happen if we just added it
                return Err(Error::Unknown(
                    format!("Failed to recover temp_{register}"),
                    Some(name.span),
                ));
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

    fn expression_device(
        &mut self,
        expr: DeviceDeclarationExpression,
        span: Span,
    ) -> Result<(), Error> {
        if self.devices.contains_key(&expr.name.node) {
            self.errors
                .push(Error::DuplicateIdentifier(expr.name.node.clone(), span));
            // We can overwrite or ignore. Let's ignore new declaration to avoid cascading errors?
            // Actually, for recovery, maybe we want to allow it so subsequent uses work?
            // But we already have it.
            return Ok(());
        }
        self.devices.insert(expr.name.node, expr.device);

        Ok(())
    }

    fn expression_if<'v>(
        &mut self,
        expr: IfExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let end_label = self.next_label_name();
        let else_label = if expr.else_branch.is_some() {
            self.next_label_name()
        } else {
            end_label.clone()
        };

        // Compile Condition
        let (cond_str, cleanup) = self.compile_operand(*expr.condition, scope)?;

        // If condition is FALSE (0), jump to else_label
        self.write_output(format!("beq {cond_str} 0 {else_label}"))?;

        if let Some(name) = cleanup {
            scope.free_temp(name)?;
        }

        // Compile Body
        // Scope variables in body are ephemeral to the block, handled by expression_block
        self.expression_block(expr.body.node, scope)?;

        // If we have an else branch, we need to jump over it after the 'if' body
        if expr.else_branch.is_some() {
            self.write_output(format!("j {end_label}"))?;
            self.write_output(format!("{else_label}:"))?;

            match expr
                .else_branch
                .ok_or(Error::Unknown("Missing else branch. This should not happen and indicates a Compiler Error. Please report to the author.".into(), None))?
                .node
            {
                Expression::Block(block) => self.expression_block(block.node, scope)?,
                Expression::If(if_expr) => self.expression_if(if_expr.node, scope)?,
                _ => unreachable!("Parser ensures else branch is Block or If"),
            }
        }

        self.write_output(format!("{end_label}:"))?;

        Ok(())
    }

    fn expression_loop<'v>(
        &mut self,
        expr: LoopExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Push labels to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone()));

        self.write_output(format!("{start_label}:"))?;

        // Compile Body
        self.expression_block(expr.body.node, scope)?;

        // Jump back to start
        self.write_output(format!("j {start_label}"))?;
        self.write_output(format!("{end_label}:"))?;

        self.loop_stack.pop();

        Ok(())
    }

    fn expression_while<'v>(
        &mut self,
        expr: WhileExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Push labels to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone()));

        self.write_output(format!("{start_label}:"))?;

        // Compile Condition
        let (cond_str, cleanup) = self.compile_operand(*expr.condition, scope)?;

        // If condition is FALSE, jump to end
        self.write_output(format!("beq {cond_str} 0 {end_label}"))?;

        if let Some(name) = cleanup {
            scope.free_temp(name)?;
        }

        // Compile Body
        self.expression_block(expr.body, scope)?;

        // Jump back to start
        self.write_output(format!("j {start_label}"))?;
        self.write_output(format!("{end_label}:"))?;

        self.loop_stack.pop();

        Ok(())
    }

    fn expression_break(&mut self) -> Result<(), Error> {
        if let Some((_, end_label)) = self.loop_stack.last() {
            self.write_output(format!("j {end_label}"))?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Break statement outside of loop".into(),
                None,
            ))
        }
    }

    fn expression_continue(&mut self) -> Result<(), Error> {
        if let Some((start_label, _)) = self.loop_stack.last() {
            self.write_output(format!("j {start_label}"))?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Continue statement outside of loop".into(),
                None,
            ))
        }
    }

    /// Helper to resolve a location to a register string (e.g., "r0").
    /// Note: This does not handle Stack locations automatically, as they require
    /// instruction emission to load. Use `compile_operand` for general handling.
    fn resolve_register(&self, loc: &VariableLocation) -> Result<String, Error> {
        match loc {
            VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => Ok(format!("r{r}")),
            VariableLocation::Stack(_) => Err(Error::Unknown(
                "Cannot resolve Stack location directly to register string without context".into(),
                None,
            )),
        }
    }

    /// Compiles an expression and ensures the result is available as a string valid for an
    /// IC10 operand (either a register "rX" or a literal value "123").
    /// If the result was stored in a new temporary register, returns the name of that temp
    /// so the caller can free it.
    fn compile_operand(
        &mut self,
        expr: Spanned<Expression>,
        scope: &mut VariableScope,
    ) -> Result<(String, Option<String>), Error> {
        // Optimization for literals
        if let Expression::Literal(spanned_lit) = &expr.node {
            if let Literal::Number(n) = spanned_lit.node {
                return Ok((n.to_string(), None));
            }
            if let Literal::Boolean(b) = spanned_lit.node {
                return Ok((if b { "1".to_string() } else { "0".to_string() }, None));
            }
        }

        // Optimization for negated literals used as operands.
        // E.g., `1 + -2` -> return "-2" string, no register used.
        if let Expression::Negation(inner) = &expr.node
            && let Expression::Literal(spanned_lit) = &inner.node
            && let Literal::Number(n) = spanned_lit.node
        {
            return Ok((format!("-{}", n), None));
        }

        let result_opt = self.expression(expr, scope)?;

        let result = match result_opt {
            Some(r) => r,
            None => {
                // Expression failed or returned void. Recover with dummy.
                return Ok(("r0".to_string(), None));
            }
        };

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

    fn compile_literal_or_variable(
        &mut self,
        val: LiteralOrVariable,
        scope: &mut VariableScope,
    ) -> Result<(String, Option<String>), Error> {
        let dummy_span = Span {
            start_line: 0,
            start_col: 0,
            end_line: 0,
            end_col: 0,
        };

        let expr = match val {
            LiteralOrVariable::Literal(l) => Expression::Literal(Spanned {
                node: l,
                span: dummy_span,
            }),
            LiteralOrVariable::Variable(v) => Expression::Variable(v),
        };
        self.compile_operand(
            Spanned {
                node: expr,
                span: dummy_span,
            },
            scope,
        )
    }

    fn expression_binary<'v>(
        &mut self,
        expr: Spanned<BinaryExpression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<CompilationResult, Error> {
        let (op_str, left_expr, right_expr) = match expr.node {
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
        expr: Spanned<LogicalExpression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<CompilationResult, Error> {
        match expr.node {
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
                let (op_str, left_expr, right_expr) = match expr.node {
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
            if matches!(b.node, Expression::Function(_))
                && matches!(a.node, Expression::Function(_))
            {
                std::cmp::Ordering::Equal
            } else if matches!(a.node, Expression::Function(_)) {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        });

        for expr in expr.0 {
            if !self.declared_main
                && !matches!(expr.node, Expression::Function(_))
                && !scope.has_parent()
            {
                self.write_output("main:")?;
                self.declared_main = true;
            }

            match expr.node {
                Expression::Return(ret_expr) => {
                    self.expression_return(*ret_expr, scope)?;
                }
                _ => {
                    // Swallow errors within expressions so block can continue
                    if let Err(e) = self.expression(expr, scope).and_then(|result| {
                        // If the expression was a statement that returned a temp result (e.g. `1 + 2;` line),
                        // we must free it to avoid leaking registers.
                        if let Some(comp_res) = result
                            && let Some(name) = comp_res.temp_name
                        {
                            scope.free_temp(name)?;
                        }
                        Ok(())
                    }) {
                        self.errors.push(e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Takes the result of the expression and stores it in VariableScope::RETURN_REGISTER
    fn expression_return<'v>(
        &mut self,
        expr: Spanned<Expression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<VariableLocation, Error> {
        if let Expression::Negation(neg_expr) = &expr.node
            && let Expression::Literal(spanned_lit) = &neg_expr.node
            && let Literal::Number(neg_num) = &spanned_lit.node
        {
            let loc = VariableLocation::Persistant(VariableScope::RETURN_REGISTER);
            self.emit_variable_assignment("returnValue", &loc, format!("-{neg_num}"))?;
            return Ok(loc);
        };

        match expr.node {
            Expression::Variable(var_name) => match scope.get_location_of(&var_name.node) {
                Ok(loc) => match loc {
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
                Err(_) => {
                    self.errors.push(Error::UnknownIdentifier(
                        var_name.node.clone(),
                        var_name.span,
                    ));
                    // Proceed with dummy
                }
            },
            Expression::Literal(spanned_lit) => match spanned_lit.node {
                Literal::Number(num) => {
                    self.emit_variable_assignment(
                        "returnValue",
                        &VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                        num,
                    )?;
                }
                Literal::Boolean(b) => {
                    let val = if b { "1" } else { "0" };
                    self.emit_variable_assignment(
                        "returnValue",
                        &VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                        val,
                    )?;
                }
                _ => {}
            },
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
                return Err(Error::Unknown(
                    format!("Unsupported `return` statement: {:?}", expr),
                    None,
                ));
            }
        }

        Ok(VariableLocation::Persistant(VariableScope::RETURN_REGISTER))
    }

    // syscalls that return values will be stored in the VariableScope::RETURN_REGISTER
    // register
    fn expression_syscall_system<'v>(
        &mut self,
        expr: System,
        span: Span,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<CompilationResult>, Error> {
        match expr {
            System::Yield => {
                self.write_output("yield")?;
                Ok(None)
            }
            System::Sleep(amt) => {
                let (var, cleanup) = self.compile_operand(*amt, scope)?;
                self.write_output(format!("sleep {var}"))?;
                if let Some(temp) = cleanup {
                    scope.free_temp(temp)?;
                }

                Ok(None)
            }
            System::Hash(hash_arg) => {
                let Literal::String(str_lit) = hash_arg else {
                    return Err(Error::AgrumentMismatch(
                        "Arg1 expected to be a string literal.".into(),
                        span,
                    ));
                };

                let loc = VariableLocation::Persistant(VariableScope::RETURN_REGISTER);
                self.emit_variable_assignment("hash_ret", &loc, format!(r#"HASH("{}")"#, str_lit))?;

                Ok(Some(CompilationResult {
                    location: loc,
                    temp_name: None,
                }))
            }
            System::SetOnDevice(device, logic_type, variable) => {
                let (variable, var_cleanup) = self.compile_operand(*variable, scope)?;

                let LiteralOrVariable::Variable(device_spanned) = device else {
                    return Err(Error::AgrumentMismatch(
                        "Arg1 expected to be a variable".into(),
                        span,
                    ));
                };

                let device_name = device_spanned.node;

                if !self.devices.contains_key(&device_name) {
                    self.errors.push(Error::InvalidDevice(
                        device_name.clone(),
                        device_spanned.span,
                    ));
                }

                let device_val = self
                    .devices
                    .get(&device_name)
                    .cloned()
                    .unwrap_or("d0".to_string());

                let Literal::String(logic_type) = logic_type else {
                    return Err(Error::AgrumentMismatch(
                        "Arg2 expected to be a string".into(),
                        span,
                    ));
                };

                self.write_output(format!("s {} {} {}", device_val, logic_type, variable))?;

                if let Some(temp_var) = var_cleanup {
                    scope.free_temp(temp_var)?;
                }

                Ok(None)
            }
            System::SetOnDeviceBatched(device_hash, logic_type, variable) => {
                let (var, var_cleanup) = self.compile_operand(*variable, scope)?;
                let (device_hash_val, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash, scope)?;
                let Literal::String(logic_type) = logic_type else {
                    return Err(Error::AgrumentMismatch(
                        "Arg2 expected to be a string".into(),
                        span,
                    ));
                };

                self.write_output(format!("sb {} {} {}", device_hash_val, logic_type, var))?;

                if let Some(var_cleanup) = var_cleanup {
                    scope.free_temp(var_cleanup)?;
                }

                if let Some(device_cleanup) = device_hash_cleanup {
                    scope.free_temp(device_cleanup)?;
                }

                Ok(None)
            }
            System::LoadFromDevice(device, logic_type) => {
                let LiteralOrVariable::Variable(device_spanned) = device else {
                    return Err(Error::AgrumentMismatch(
                        "Arg1 expected to be a variable".into(),
                        span,
                    ));
                };

                let device_name = device_spanned.node;

                if !self.devices.contains_key(&device_name) {
                    self.errors.push(Error::InvalidDevice(
                        device_name.clone(),
                        device_spanned.span,
                    ));
                }

                let device_val = self
                    .devices
                    .get(&device_name)
                    .cloned()
                    .unwrap_or("d0".to_string());

                let Literal::String(logic_type) = logic_type else {
                    return Err(Error::AgrumentMismatch(
                        "Arg2 expected to be a string".into(),
                        span,
                    ));
                };

                self.write_output(format!(
                    "l r{} {} {}",
                    VariableScope::RETURN_REGISTER,
                    device_val,
                    logic_type
                ))?;

                Ok(Some(CompilationResult {
                    location: VariableLocation::Temporary(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }

            t => Err(Error::Unknown(
                format!("{t:?}\n\nNot yet implemented"),
                Some(span),
            )),
        }
    }

    /// Compile a function declaration.
    /// Calees are responsible for backing up any registers they wish to use.
    fn expression_function<'v>(
        &mut self,
        expr: Spanned<FunctionExpression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let FunctionExpression {
            name,
            arguments,
            body,
        } = expr.node;

        if self.function_locations.contains_key(&name.node) {
            self.errors
                .push(Error::DuplicateIdentifier(name.node.clone(), name.span));
            // Fallthrough to allow compiling the body anyway?
            // It might be useful to check body for errors.
        }

        self.function_metadata.insert(
            name.node.clone(),
            arguments.iter().map(|a| a.node.clone()).collect(),
        );

        // Declare the function as a line identifier
        self.write_output(format!("{}:", name.node))?;

        self.function_locations
            .insert(name.node.clone(), self.current_line);

        // Create a new block scope for the function body
        let mut block_scope = VariableScope::scoped(scope);

        let mut saved_variables = 0;

        // do a reverse pass to pop variables from the stack and put them into registers
        for var_name in arguments
            .iter()
            .rev()
            .take(VariableScope::PERSIST_REGISTER_COUNT as usize)
        {
            let loc = block_scope.add_variable(var_name.node.clone(), LocationRequest::Persist)?;
            // we don't need to imcrement the stack offset as it's already on the stack from the
            // previous scope

            match loc {
                VariableLocation::Persistant(loc) => {
                    self.write_output(format!(
                        "pop r{loc} {}",
                        debug!(self, "#{}", var_name.node)
                    ))?;
                }
                VariableLocation::Stack(_) => {
                    return Err(Error::Unknown(
                        "Attempted to save to stack without tracking in scope".into(),
                        Some(var_name.span),
                    ));
                }

                _ => {
                    return Err(Error::Unknown(
                        "Attempted to return a Temporary scoped variable from a Persistant request"
                            .into(),
                        Some(var_name.span),
                    ));
                }
            }
            saved_variables += 1;
        }

        // now do a forward pass in case we have spilled into the stack. We don't need to push
        // anything as they already exist on the stack, but we DO need to let our block_scope be
        // aware that the variables exist on the stack (left to right)
        for var_name in arguments.iter().take(arguments.len() - saved_variables) {
            block_scope.add_variable(var_name.node.clone(), LocationRequest::Stack)?;
        }

        self.write_output("push ra")?;
        block_scope.add_variable(format!("{}_ra", name.node), LocationRequest::Stack)?;

        for expr in body.0 {
            match expr.node {
                Expression::Return(ret_expr) => {
                    self.expression_return(*ret_expr, &mut block_scope)?;
                }
                _ => {
                    // Swallow internal errors
                    if let Err(e) = self.expression(expr, &mut block_scope).and_then(|result| {
                        if let Some(comp_res) = result
                            && let Some(name) = comp_res.temp_name
                        {
                            block_scope.free_temp(name)?;
                        }
                        Ok(())
                    }) {
                        self.errors.push(e);
                    }
                }
            }
        }

        // Get the saved return address and save it back into `ra`
        let ra_res = block_scope.get_location_of(format!("{}_ra", name.node));
        let ra_stack_offset = match ra_res {
            Ok(VariableLocation::Stack(offset)) => offset,
            _ => {
                // If we can't find RA, we can't return properly.
                // This usually implies a compiler bug or scope tracking error.
                return Err(Error::Unknown(
                    "Stored return address not in stack as expected".into(),
                    Some(name.span),
                ));
            }
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
