#![allow(clippy::result_large_err)]
use crate::variable_manager::{self, LocationRequest, VariableLocation, VariableScope};
use helpers::prelude::*;
use parser::{
    Parser as ASTParser,
    sys_call::{Math, SysCall, System},
    tree_node::{
        AssignmentExpression, BinaryExpression, BlockExpression, ConstDeclarationExpression,
        DeviceDeclarationExpression, Expression, FunctionExpression, IfExpression,
        InvocationExpression, Literal, LiteralOr, LiteralOrVariable, LogicalExpression,
        LoopExpression, MemberAccessExpression, Span, Spanned, WhileExpression,
    },
};
use quick_error::quick_error;
use std::{
    collections::HashMap,
    io::{BufWriter, Write},
};
use tokenizer::token::Number;

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

fn extract_literal(literal: Literal, allow_strings: bool) -> Result<String, Error> {
    if !allow_strings && matches!(literal, Literal::String(_)) {
        return Err(Error::Unknown(
            "Literal strings are not allowed in this context".to_string(),
            None,
        ));
    }
    Ok(match literal {
        Literal::String(s) => s,
        Literal::Number(n) => n.to_string(),
        Literal::Boolean(b) => if b { "1" } else { "0" }.into(),
    })
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
        ConstAssignment(ident: String, span: Span) {
            display("Attempted to re-assign a value to const variable `{ident}`")
        }
        DeviceAssignment(ident: String, span: Span) {
            display("Attempted to re-assign a value to a device const `{ident}`")
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
            ScopeError(e) => e.into(),
            DuplicateIdentifier(_, span)
            | UnknownIdentifier(_, span)
            | InvalidDevice(_, span)
            | ConstAssignment(_, span)
            | DeviceAssignment(_, span)
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
            Expression::Syscall(Spanned {
                node: SysCall::Math(math),
                ..
            }) => self.expression_syscall_math(math, scope),
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
                self.expression_device(expr_dev.node)?;
                Ok(None)
            }
            Expression::Declaration(var_name, decl_expr) => {
                // decl_expr is Box<Spanned<Expression>>
                self.expression_declaration(var_name, *decl_expr, scope)
            }
            Expression::ConstDeclaration(const_decl_expr) => {
                self.expression_const_declaration(const_decl_expr.node, scope)?;
                Ok(None)
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
                let temp_loc = scope.add_variable(&temp_name, LocationRequest::Temp, None)?;
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
                    let loc = scope.add_variable(&temp_name, LocationRequest::Temp, None)?;
                    self.emit_variable_assignment(&temp_name, &loc, num.to_string())?;
                    Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                Literal::Boolean(b) => {
                    let val = if b { "1" } else { "0" };
                    let temp_name = self.next_temp_name();
                    let loc = scope.add_variable(&temp_name, LocationRequest::Temp, None)?;
                    self.emit_variable_assignment(&temp_name, &loc, val)?;
                    Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                _ => Ok(None), // String literals don't return values in this context typically
            },
            Expression::Variable(name) => {
                match scope.get_location_of(&name.node, Some(name.span)) {
                    Ok(loc) => Ok(Some(CompilationResult {
                        location: loc,
                        temp_name: None, // User variable, do not free
                    })),
                    Err(_) => {
                        // fallback, check devices
                        if let Some(device) = self.devices.get(&name.node) {
                            Ok(Some(CompilationResult {
                                location: VariableLocation::Device(device.clone()),
                                temp_name: None,
                            }))
                        } else {
                            self.errors
                                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
                            Ok(Some(CompilationResult {
                                location: VariableLocation::Temporary(0),
                                temp_name: None,
                            }))
                        }
                    }
                }
            }
            Expression::MemberAccess(access) => {
                // "load" behavior (e.g. `let x = d0.On`)
                let MemberAccessExpression { object, member } = access.node;

                // 1. Resolve the object to a device string (e.g., "d0" or "rX")
                let (device_str, cleanup) = self.resolve_device(*object, scope)?;

                // 2. Allocate a temp register for the result
                let result_name = self.next_temp_name();
                let loc = scope.add_variable(&result_name, LocationRequest::Temp, None)?;
                let reg = self.resolve_register(&loc)?;

                // 3. Emit load instruction: l rX device member
                self.write_output(format!("l {} {} {}", reg, device_str, member.node))?;

                // 4. Cleanup
                if let Some(c) = cleanup {
                    scope.free_temp(c, None)?;
                }

                Ok(Some(CompilationResult {
                    location: loc,
                    temp_name: Some(result_name),
                }))
            }
            Expression::MethodCall(call) => {
                // Methods are not yet fully supported (e.g. `d0.SomeFunc()`).
                // This would likely map to specialized syscalls or batch instructions.
                Err(Error::Unknown(
                    format!(
                        "Method calls are not yet supported: {}",
                        call.node.method.node
                    ),
                    Some(call.span),
                ))
            }
            Expression::Priority(inner_expr) => self.expression(*inner_expr, scope),
            Expression::Negation(inner_expr) => {
                // Compile negation as 0 - inner
                let (inner_str, cleanup) = self.compile_operand(*inner_expr, scope)?;
                let result_name = self.next_temp_name();
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                self.write_output(format!("sub {result_reg} 0 {inner_str}"))?;

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
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

    /// Resolves an expression to a device identifier string for use in instructions like `s` or `l`.
    /// Returns (device_string, optional_cleanup_temp_name).
    fn resolve_device<'v>(
        &mut self,
        expr: Spanned<Expression>,
        scope: &mut VariableScope<'v>,
    ) -> Result<(String, Option<String>), Error> {
        // If it's a direct variable reference, check if it's a known device alias first
        if let Expression::Variable(ref name) = expr.node
            && let Some(device_id) = self.devices.get(&name.node)
        {
            return Ok((device_id.clone(), None));
        }

        // Otherwise, compile it as an operand (e.g. it might be a register holding a device hash/id)
        self.compile_operand(expr, scope)
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
            VariableLocation::Constant(_) => {
                return Err(Error::Unknown(
                    r#"Attempted to emit a variable assignent for a constant value.
                    This is a Compiler bug and should be reported to the developer."#
                        .into(),
                    None,
                ));
            }
            VariableLocation::Device(_) => {
                return Err(Error::Unknown(
                    r#"Attempted to emit a variable assignent for device.
                    This is a Compiler bug and should be reported to the developer."#
                        .into(),
                    None,
                ));
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
            let loc = scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;
            self.emit_variable_assignment(&name_str, &loc, format!("-{neg_num}"))?;
            return Ok(Some(CompilationResult {
                location: loc,
                temp_name: None,
            }));
        }

        let (loc, temp_name) = match expr.node {
            Expression::Literal(spanned_lit) => match spanned_lit.node {
                Literal::Number(num) => {
                    let var_location = scope.add_variable(
                        name_str.clone(),
                        LocationRequest::Persist,
                        Some(name_span),
                    )?;

                    self.emit_variable_assignment(&name_str, &var_location, num)?;
                    (var_location, None)
                }
                Literal::Boolean(b) => {
                    let val = if b { "1" } else { "0" };
                    let var_location = scope.add_variable(
                        name_str.clone(),
                        LocationRequest::Persist,
                        Some(name_span),
                    )?;

                    self.emit_variable_assignment(&name_str, &var_location, val)?;
                    (var_location, None)
                }
                _ => return Ok(None),
            },
            Expression::Invocation(invoke_expr) => {
                self.expression_function_invocation(invoke_expr, scope)?;

                let loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;
                self.emit_variable_assignment(
                    &name_str,
                    &loc,
                    format!("r{}", VariableScope::RETURN_REGISTER),
                )?;
                (loc, None)
            }
            Expression::Syscall(spanned_call) => {
                let sys_call = spanned_call.node;
                let res = match sys_call {
                    SysCall::System(s) => {
                        self.expression_syscall_system(s, spanned_call.span, scope)?
                    }
                    SysCall::Math(m) => self.expression_syscall_math(m, scope)?,
                };

                if res.is_none() {
                    return Err(Error::Unknown(
                        "SysCall did not return a value".into(),
                        Some(spanned_call.span),
                    ));
                };

                let loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;
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
                let var_loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;

                if let CompilationResult {
                    location: VariableLocation::Constant(Literal::Number(num)),
                    ..
                } = result
                {
                    self.emit_variable_assignment(&name_str, &var_loc, num)?;
                    (var_loc, None)
                } else {
                    // Move result from temp to new persistent variable
                    let result_reg = self.resolve_register(&result.location)?;
                    self.emit_variable_assignment(&name_str, &var_loc, result_reg)?;

                    // Free the temp result
                    if let Some(name) = result.temp_name {
                        scope.free_temp(name, None)?;
                    }
                    (var_loc, None)
                }
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                let var_loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&name_str, &var_loc, result_reg)?;

                // Free the temp result
                if let Some(name) = result.temp_name {
                    scope.free_temp(name, None)?;
                }
                (var_loc, None)
            }
            Expression::Variable(name) => {
                let src_loc_res = scope.get_location_of(&name.node, Some(name.span));

                let src_loc = match src_loc_res {
                    Ok(l) => l,
                    Err(_) => {
                        self.errors
                            .push(Error::UnknownIdentifier(name.node.clone(), name.span));
                        VariableLocation::Temporary(0)
                    }
                };

                let var_loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;

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
                    VariableLocation::Constant(_) | VariableLocation::Device(_) => unreachable!(),
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
            Expression::MemberAccess(access) => {
                // Compile the member access (load instruction)
                let result = self.expression(
                    Spanned {
                        node: Expression::MemberAccess(access),
                        span: name_span, // Use declaration span roughly
                    },
                    scope,
                )?;

                // Result is in a temp register
                let Some(comp_res) = result else {
                    return Err(Error::Unknown(
                        "Member access did not return a value".into(),
                        Some(name_span),
                    ));
                };

                let var_loc =
                    scope.add_variable(&name_str, LocationRequest::Persist, Some(name_span))?;
                let result_reg = self.resolve_register(&comp_res.location)?;

                self.emit_variable_assignment(&name_str, &var_loc, result_reg)?;

                if let Some(temp) = comp_res.temp_name {
                    scope.free_temp(temp, None)?;
                }

                (var_loc, None)
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

    fn expression_const_declaration<'v>(
        &mut self,
        expr: ConstDeclarationExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<CompilationResult, Error> {
        let ConstDeclarationExpression {
            name: const_name,
            value: const_value,
        } = expr;

        // check for a hash expression or a literal
        let value = match const_value {
            LiteralOr::Or(Spanned {
                node:
                    SysCall::System(System::Hash(Spanned {
                        node: Literal::String(str_to_hash),
                        ..
                    })),
                ..
            }) => Literal::Number(Number::Integer(crc_hash_signed(&str_to_hash))),
            LiteralOr::Or(Spanned { span, .. }) => {
                return Err(Error::Unknown(
                    "hash only supports string literals in this context.".into(),
                    Some(span),
                ));
            }
            LiteralOr::Literal(Spanned { node, .. }) => node,
        };

        Ok(CompilationResult {
            location: scope.define_const(const_name.node, value, Some(const_name.span))?,
            temp_name: None,
        })
    }

    fn expression_assignment<'v>(
        &mut self,
        expr: AssignmentExpression,
        scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        let AssignmentExpression {
            assignee,
            expression,
        } = expr;

        match assignee.node {
            Expression::Variable(identifier) => {
                let location = match scope.get_location_of(&identifier.node, Some(identifier.span))
                {
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
                    VariableLocation::Constant(_) => {
                        return Err(Error::ConstAssignment(identifier.node, identifier.span));
                    }
                    VariableLocation::Device(_) => {
                        return Err(Error::DeviceAssignment(identifier.node, identifier.span));
                    }
                }

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
                }
            }
            Expression::MemberAccess(access) => {
                // Set instruction: s device member value
                let MemberAccessExpression { object, member } = access.node;

                let (device_str, dev_cleanup) = self.resolve_device(*object, scope)?;
                let (val_str, val_cleanup) = self.compile_operand(*expression, scope)?;

                self.write_output(format!("s {} {} {}", device_str, member.node, val_str))?;

                if let Some(c) = dev_cleanup {
                    scope.free_temp(c, None)?;
                }
                if let Some(c) = val_cleanup {
                    scope.free_temp(c, None)?;
                }
            }
            _ => {
                return Err(Error::Unknown(
                    "Invalid assignment target. Only variables and member access are supported."
                        .into(),
                    Some(assignee.span),
                ));
            }
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
            stack.add_variable(format!("temp_{register}"), LocationRequest::Stack, None)?;
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
                    let loc =
                        match stack.get_location_of(var_name.node.clone(), Some(var_name.span)) {
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
                        VariableLocation::Constant(lit) => {
                            self.write_output(format!("push {}", extract_literal(lit, false)?))?;
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
                        VariableLocation::Device(_) => {
                            return Err(Error::Unknown(
                                r#"Attempted to pass a device contant into a function argument. These values can be used without scope."#.into(),
                                Some(arg.span),
                            ));
                        }
                    }
                }
                Expression::Binary(bin_expr) => {
                    // Compile the binary expression to a temp register
                    let result = self.expression_binary(bin_expr, stack)?;
                    let reg_str = self.resolve_register(&result.location)?;
                    self.write_output(format!("push {reg_str}"))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name, None)?;
                    }
                }
                Expression::Logical(log_expr) => {
                    // Compile the logical expression to a temp register
                    let result = self.expression_logical(log_expr, stack)?;
                    let reg_str = self.resolve_register(&result.location)?;
                    self.write_output(format!("push {reg_str}"))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name, None)?;
                    }
                }
                Expression::MemberAccess(access) => {
                    // Compile member access to temp and push
                    let result_opt = self.expression(
                        Spanned {
                            node: Expression::MemberAccess(access),
                            span: Span {
                                start_col: 0,
                                end_col: 0,
                                start_line: 0,
                                end_line: 0,
                            }, // Dummy span
                        },
                        stack,
                    )?;

                    if let Some(result) = result_opt {
                        let reg_str = self.resolve_register(&result.location)?;
                        self.write_output(format!("push {reg_str}"))?;
                        if let Some(name) = result.temp_name {
                            stack.free_temp(name, None)?;
                        }
                    } else {
                        self.write_output("push 0")?; // Should fail ideally
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
                .get_location_of(format!("temp_{register}"), None)
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

    fn expression_device(&mut self, expr: DeviceDeclarationExpression) -> Result<(), Error> {
        if self.devices.contains_key(&expr.name.node) {
            self.errors.push(Error::DuplicateIdentifier(
                expr.name.node.clone(),
                expr.name.span,
            ));
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
            scope.free_temp(name, None)?;
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
            scope.free_temp(name, None)?;
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
            VariableLocation::Constant(_) => Err(Error::Unknown(
                "Cannot resolve a constant value to register".into(),
                None,
            )),
            VariableLocation::Device(_) => Err(Error::Unknown(
                "Cannot resolve a device to a register".into(),
                None,
            )),
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
            if let Literal::String(ref s) = spanned_lit.node {
                return Ok((s.to_string(), None));
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
            VariableLocation::Constant(lit) => match lit {
                Literal::Number(n) => Ok((n.to_string(), None)),
                Literal::Boolean(b) => Ok((if b { "1" } else { "0" }.to_string(), None)),
                Literal::String(s) => Ok((s, None)),
            },
            VariableLocation::Stack(offset) => {
                // If it's on the stack, we must load it into a temp to use it as an operand
                let temp_name = self.next_temp_name();
                let temp_loc = scope.add_variable(&temp_name, LocationRequest::Temp, None)?;
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
            VariableLocation::Device(d) => Ok((d, None)),
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
        fn fold_binary_expression(expr: &BinaryExpression) -> Option<Number> {
            let (lhs, rhs) = match &expr {
                BinaryExpression::Add(l, r)
                | BinaryExpression::Subtract(l, r)
                | BinaryExpression::Multiply(l, r)
                | BinaryExpression::Divide(l, r)
                | BinaryExpression::Exponent(l, r)
                | BinaryExpression::Modulo(l, r) => (fold_expression(l)?, fold_expression(r)?),
            };

            match expr {
                BinaryExpression::Add(..) => Some(lhs + rhs),
                BinaryExpression::Subtract(..) => Some(lhs - rhs),
                BinaryExpression::Multiply(..) => Some(lhs * rhs),
                BinaryExpression::Divide(..) => Some(lhs / rhs), // Watch out for div by zero panics!
                BinaryExpression::Modulo(..) => Some(lhs % rhs),
                _ => None, // Handle Exponent separately or implement pow
            }
        }

        fn fold_expression(expr: &Expression) -> Option<Number> {
            match expr {
                // 1. Base Case: It's already a number
                Expression::Literal(lit) => match lit.node {
                    Literal::Number(n) => Some(n),
                    _ => None,
                },

                // 2. Handle Parentheses: Just recurse deeper
                Expression::Priority(inner) => fold_expression(&inner.node),

                // 3. Handle Negation: Recurse, then negate
                Expression::Negation(inner) => {
                    let val = fold_expression(&inner.node)?;
                    Some(-val) // Requires impl Neg for Number
                }

                // 4. Handle Binary Ops: Recurse BOTH sides, then combine
                Expression::Binary(bin) => fold_binary_expression(&bin.node),

                // 5. Anything else (Variables, Function Calls) cannot be compile-time folded
                _ => None,
            }
        }

        if let Some(const_lit) = fold_binary_expression(&expr.node) {
            return Ok(CompilationResult {
                location: VariableLocation::Constant(Literal::Number(const_lit)),
                temp_name: None,
            });
        };

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
        let result_loc = scope.add_variable(&result_name, LocationRequest::Temp, None)?;
        let result_reg = self.resolve_register(&result_loc)?;

        // Emit instruction: op result lhs rhs
        self.write_output(format!("{op_str} {result_reg} {lhs_str} {rhs_str}"))?;

        // Clean up operand temps
        if let Some(name) = lhs_cleanup {
            scope.free_temp(name, None)?;
        }
        if let Some(name) = rhs_cleanup {
            scope.free_temp(name, None)?;
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
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // seq rX rY 0  => if rY == 0 set rX = 1 else rX = 0
                self.write_output(format!("seq {result_reg} {inner_str} 0"))?;

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
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
                let result_loc = scope.add_variable(&result_name, LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // Emit instruction: op result lhs rhs
                self.write_output(format!("{op_str} {result_reg} {lhs_str} {rhs_str}"))?;

                // Clean up operand temps
                if let Some(name) = lhs_cleanup {
                    scope.free_temp(name, None)?;
                }
                if let Some(name) = rhs_cleanup {
                    scope.free_temp(name, None)?;
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
        parent_scope: &mut VariableScope<'v>,
    ) -> Result<(), Error> {
        // First, sort the expressions to ensure functions are hoisted
        expr.0.sort_by(|a, b| {
            if matches!(
                b.node,
                Expression::Function(_) | Expression::ConstDeclaration(_)
            ) && matches!(
                a.node,
                Expression::Function(_) | Expression::ConstDeclaration(_)
            ) {
                std::cmp::Ordering::Equal
            } else if matches!(
                a.node,
                Expression::Function(_) | Expression::ConstDeclaration(_)
            ) {
                std::cmp::Ordering::Less
            } else {
                std::cmp::Ordering::Greater
            }
        });

        let mut scope = VariableScope::scoped(parent_scope);

        for expr in expr.0 {
            if !self.declared_main
                && !matches!(expr.node, Expression::Function(_))
                && !parent_scope.has_parent()
            {
                self.write_output("main:")?;
                self.declared_main = true;
            }

            match expr.node {
                Expression::Return(ret_expr) => {
                    self.expression_return(*ret_expr, &mut scope)?;
                }
                _ => {
                    // Swallow errors within expressions so block can continue
                    if let Err(e) = self.expression(expr, &mut scope).and_then(|result| {
                        // If the expression was a statement that returned a temp result (e.g. `1 + 2;` line),
                        // we must free it to avoid leaking registers.
                        if let Some(comp_res) = result
                            && let Some(name) = comp_res.temp_name
                        {
                            scope.free_temp(name, None)?;
                        }
                        Ok(())
                    }) {
                        self.errors.push(e);
                    }
                }
            }
        }

        if scope.stack_offset() > 0 {
            self.write_output(format!("sub sp sp {}", scope.stack_offset()))?;
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
            Expression::Variable(var_name) => {
                match scope.get_location_of(&var_name.node, Some(var_name.span)) {
                    Ok(loc) => match loc {
                        VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                            self.write_output(format!(
                                "move r{} r{reg} {}",
                                VariableScope::RETURN_REGISTER,
                                debug!(self, "#returnValue")
                            ))?;
                        }
                        VariableLocation::Constant(lit) => {
                            let str = extract_literal(lit, false)?;
                            self.write_output(format!(
                                "move r{} {str} {}",
                                VariableScope::RETURN_REGISTER,
                                debug!(self, "#returnValue")
                            ))?
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
                        VariableLocation::Device(_) => {
                            return Err(Error::Unknown(
                                "You can not return a device from a function.".into(),
                                Some(var_name.span),
                            ));
                        }
                    },
                    Err(_) => {
                        self.errors.push(Error::UnknownIdentifier(
                            var_name.node.clone(),
                            var_name.span,
                        ));
                        // Proceed with dummy
                    }
                }
            }
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
                    scope.free_temp(name, None)?;
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
                    scope.free_temp(name, None)?;
                }
            }
            Expression::MemberAccess(access) => {
                // Return result of member access
                let res_opt = self.expression(
                    Spanned {
                        node: Expression::MemberAccess(access),
                        span: expr.span,
                    },
                    scope,
                )?;
                if let Some(res) = res_opt {
                    let reg = self.resolve_register(&res.location)?;
                    self.write_output(format!("move r{} {}", VariableScope::RETURN_REGISTER, reg))?;
                    if let Some(temp) = res.temp_name {
                        scope.free_temp(temp, None)?;
                    }
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
        macro_rules! cleanup {
            ($($to_clean:expr),*) => {
                $(
                    if let Some(to_clean) = $to_clean {
                        scope.free_temp(to_clean, None)?;
                    }
                )*
            };
        }
        match expr {
            System::Yield => {
                self.write_output("yield")?;
                Ok(None)
            }
            System::Sleep(amt) => {
                let (var, var_cleanup) = self.compile_operand(*amt, scope)?;
                self.write_output(format!("sleep {var}"))?;

                cleanup!(var_cleanup);

                Ok(None)
            }
            System::Hash(hash_arg) => {
                let Spanned {
                    node: Literal::String(str_lit),
                    ..
                } = hash_arg
                else {
                    return Err(Error::AgrumentMismatch(
                        "Arg1 expected to be a string literal.".into(),
                        span,
                    ));
                };

                let loc = VariableLocation::Constant(Literal::Number(Number::Integer(
                    crc_hash_signed(&str_lit),
                )));

                Ok(Some(CompilationResult {
                    location: loc,
                    temp_name: None,
                }))
            }
            System::SetOnDevice(device, logic_type, variable) => {
                let (variable, var_cleanup) = self.compile_operand(*variable, scope)?;

                let Spanned {
                    node: LiteralOrVariable::Variable(device_spanned),
                    ..
                } = device
                else {
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

                let Spanned {
                    node: Literal::String(logic_type),
                    ..
                } = logic_type
                else {
                    return Err(Error::AgrumentMismatch(
                        "Arg2 expected to be a string".into(),
                        span,
                    ));
                };

                self.write_output(format!("s {} {} {}", device_val, logic_type, variable))?;

                cleanup!(var_cleanup);

                Ok(None)
            }
            System::SetOnDeviceBatched(device_hash, logic_type, variable) => {
                let (var, var_cleanup) = self.compile_operand(*variable, scope)?;
                let (device_hash_val, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash.node, scope)?;
                let Spanned {
                    node: Literal::String(logic_type),
                    ..
                } = logic_type
                else {
                    return Err(Error::AgrumentMismatch(
                        "Arg2 expected to be a string".into(),
                        span,
                    ));
                };

                self.write_output(format!("sb {} {} {}", device_hash_val, logic_type, var))?;

                cleanup!(var_cleanup, device_hash_cleanup);

                Ok(None)
            }
            System::SetOnDeviceBatchedNamed(device_hash, name_hash, logic_type, val_expr) => {
                let (value, value_cleanup) = self.compile_operand(*val_expr, scope)?;
                let (device_hash, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash.node, scope)?;

                let (name_hash, name_hash_cleanup) =
                    self.compile_literal_or_variable(name_hash.node, scope)?;

                let (logic_type, logic_type_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(logic_type.node),
                    scope,
                )?;

                self.write_output(format!(
                    "sbn {} {} {} {}",
                    device_hash, name_hash, logic_type, value
                ))?;

                cleanup!(
                    value_cleanup,
                    device_hash_cleanup,
                    name_hash_cleanup,
                    logic_type_cleanup
                );

                Ok(None)
            }
            System::LoadFromDevice(device, logic_type) => {
                let Spanned {
                    node: LiteralOrVariable::Variable(device_spanned),
                    ..
                } = device
                else {
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

                let Spanned {
                    node: Literal::String(logic_type),
                    ..
                } = logic_type
                else {
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
            System::LoadBatch(device_hash, logic_type, batch_mode) => {
                let (device_hash, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash.node, scope)?;
                let (logic_type, logic_type_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(logic_type.node),
                    scope,
                )?;
                let (batch_mode, batch_mode_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(batch_mode.node),
                    scope,
                )?;

                self.write_output(format!(
                    "lb r{} {} {} {}",
                    VariableScope::RETURN_REGISTER,
                    device_hash,
                    logic_type,
                    batch_mode
                ))?;

                cleanup!(device_hash_cleanup, logic_type_cleanup, batch_mode_cleanup);

                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadBatchNamed(device_hash, name_hash, logic_type, batch_mode) => {
                let (device_hash, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash.node, scope)?;
                let (name_hash, name_hash_cleanup) =
                    self.compile_literal_or_variable(name_hash.node, scope)?;
                let (logic_type, logic_type_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(logic_type.node),
                    scope,
                )?;
                let (batch_mode, batch_mode_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(batch_mode.node),
                    scope,
                )?;

                self.write_output(format!(
                    "lbn r{} {} {} {} {}",
                    VariableScope::RETURN_REGISTER,
                    device_hash,
                    name_hash,
                    logic_type,
                    batch_mode
                ))?;

                cleanup!(
                    device_hash_cleanup,
                    name_hash_cleanup,
                    logic_type_cleanup,
                    batch_mode_cleanup
                );

                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
        }
    }

    fn expression_syscall_math<'v>(
        &mut self,
        expr: Math,
        scope: &mut VariableScope<'v>,
    ) -> Result<Option<CompilationResult>, Error> {
        macro_rules! cleanup {
            ($($to_clean:expr),*) => {
                $(
                    if let Some(to_clean) = $to_clean {
                        scope.free_temp(to_clean, None)?;
                    }
                )*
            };
        }
        match expr {
            Math::Acos(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("acos r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Asin(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("asin r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Atan(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("atan r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Atan2(expr1, expr2) => {
                let (var1, var1_cleanup) = self.compile_operand(*expr1, scope)?;
                let (var2, var2_cleanup) = self.compile_operand(*expr2, scope)?;

                self.write_output(format!(
                    "atan2 r{} {} {}",
                    VariableScope::RETURN_REGISTER,
                    var1,
                    var2
                ))?;
                cleanup!(var1_cleanup, var2_cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Abs(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("abs r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Ceil(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("ceil r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Cos(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("cos r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Floor(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("floor r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Log(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("log r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(cleanup);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Max(expr1, expr2) => {
                let (var1, clean1) = self.compile_operand(*expr1, scope)?;
                let (var2, clean2) = self.compile_operand(*expr2, scope)?;
                self.write_output(format!(
                    "max r{} {} {}",
                    VariableScope::RETURN_REGISTER,
                    var1,
                    var2
                ))?;

                cleanup!(clean1, clean2);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Min(expr1, expr2) => {
                let (var1, clean1) = self.compile_operand(*expr1, scope)?;
                let (var2, clean2) = self.compile_operand(*expr2, scope)?;
                self.write_output(format!(
                    "min r{} {} {}",
                    VariableScope::RETURN_REGISTER,
                    var1,
                    var2
                ))?;

                cleanup!(clean1, clean2);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Rand => {
                self.write_output(format!("rand r{}", VariableScope::RETURN_REGISTER))?;

                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Sin(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("sin r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(clean);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Sqrt(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("sqrt r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(clean);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Tan(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("tan r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(clean);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Trunc(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_output(format!("trunc r{} {}", VariableScope::RETURN_REGISTER, var))?;

                cleanup!(clean);
                Ok(Some(CompilationResult {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
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
            let loc = block_scope.add_variable(
                var_name.node.clone(),
                LocationRequest::Persist,
                Some(var_name.span),
            )?;
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
            block_scope.add_variable(
                var_name.node.clone(),
                LocationRequest::Stack,
                Some(var_name.span),
            )?;
        }

        self.write_output("push ra")?;
        block_scope.add_variable(
            format!("{}_ra", name.node),
            LocationRequest::Stack,
            Some(name.span),
        )?;

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
                            block_scope.free_temp(name, None)?;
                        }
                        Ok(())
                    }) {
                        self.errors.push(e);
                    }
                }
            }
        }

        // Get the saved return address and save it back into `ra`
        let ra_res = block_scope.get_location_of(format!("{}_ra", name.node), Some(name.span));

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
