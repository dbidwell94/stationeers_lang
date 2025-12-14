#![allow(clippy::result_large_err)]
use crate::variable_manager::{self, LocationRequest, VariableLocation, VariableScope};
use helpers::{Span, prelude::*};
use il::{Instruction, InstructionNode, Instructions, Operand};
use parser::{
    Parser as ASTParser,
    sys_call::{Math, SysCall, System},
    tree_node::{
        AssignmentExpression, BinaryExpression, BlockExpression, ConstDeclarationExpression,
        DeviceDeclarationExpression, Expression, FunctionExpression, IfExpression,
        InvocationExpression, Literal, LiteralOr, LiteralOrVariable, LogicalExpression,
        LoopExpression, MemberAccessExpression, Spanned, TernaryExpression, WhileExpression,
    },
};
use rust_decimal::Decimal;
use std::{borrow::Cow, collections::HashMap};
use thiserror::Error;
use tokenizer::token::Number;

fn extract_literal<'a>(
    literal: Literal<'a>,
    allow_strings: bool,
) -> Result<Operand<'a>, Error<'a>> {
    if !allow_strings && matches!(literal, Literal::String(_)) {
        return Err(Error::Unknown(
            "Literal strings are not allowed in this context".to_string(),
            None,
        ));
    }
    Ok(match literal {
        Literal::String(s) => Operand::LogicType(s),
        Literal::Number(n) => Operand::Number(n.into()),
        Literal::Boolean(b) => Operand::Number(Number::from(b).into()),
    })
}

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error("{0}")]
    Parse(parser::Error<'a>),

    #[error("{0}")]
    Scope(variable_manager::Error<'a>),

    #[error("IO Error: {0}")]
    IO(String),

    #[error("`{0}` has already been defined.")]
    DuplicateIdentifier(Cow<'a, str>, Span),

    #[error("`{0}` is not found in the current scope.")]
    UnknownIdentifier(Cow<'a, str>, Span),

    #[error("`{0}` is not valid.")]
    InvalidDevice(Cow<'a, str>, Span),

    #[error("Incorrent number of arguments passed into `{0}`")]
    AgrumentMismatch(Cow<'a, str>, Span),

    #[error("Attempted to re-assign a value to const variable `{0}`")]
    ConstAssignment(Cow<'a, str>, Span),

    #[error("Attempted to re-assign a value to a device const `{0}`")]
    DeviceAssignment(Cow<'a, str>, Span),

    #[error("{0}")]
    Unknown(String, Option<Span>),
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            Parse(e) => e.into(),
            IO(e) => Diagnostic {
                message: e.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            Scope(e) => e.into(),
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

impl<'a> From<parser::Error<'a>> for Error<'a> {
    fn from(value: parser::Error<'a>) -> Self {
        Self::Parse(value)
    }
}

impl<'a> From<variable_manager::Error<'a>> for Error<'a> {
    fn from(value: variable_manager::Error<'a>) -> Self {
        Self::Scope(value)
    }
}

// Map io::Error to Error manually since we can't clone io::Error
impl<'a> From<std::io::Error> for Error<'a> {
    fn from(err: std::io::Error) -> Self {
        Error::IO(err.to_string())
    }
}

#[derive(Default)]
#[repr(C)]
pub struct CompilerConfig {
    pub debug: bool,
}

#[derive(Debug)]
struct CompileLocation<'a> {
    location: VariableLocation<'a>,
    /// If Some, this is the name of the temporary variable that holds the result.
    /// It must be freed by the caller when done.
    temp_name: Option<Cow<'a, str>>,
}

pub struct CompilationResult<'a> {
    pub errors: Vec<Error<'a>>,
    pub instructions: Instructions<'a>,
}

pub struct Compiler<'a> {
    pub parser: ASTParser<'a>,
    function_locations: HashMap<Cow<'a, str>, usize>,
    function_metadata: HashMap<Cow<'a, str>, Vec<Cow<'a, str>>>,
    devices: HashMap<Cow<'a, str>, Cow<'a, str>>,

    // This holds the IL code which will be used in the
    // optimizer
    pub instructions: Instructions<'a>,

    current_line: usize,
    declared_main: bool,
    _config: CompilerConfig,
    temp_counter: usize,
    label_counter: usize,
    loop_stack: Vec<(Cow<'a, str>, Cow<'a, str>)>, // Stores (start_label, end_label)
    current_return_label: Option<Cow<'a, str>>,
    /// stores (IC10 `line_num`, `Vec<Span>`)
    pub source_map: HashMap<usize, Vec<Span>>,
    /// Accumulative errors from the compilation process
    pub errors: Vec<Error<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(parser: ASTParser<'a>, config: Option<CompilerConfig>) -> Self {
        Self {
            parser,
            function_locations: HashMap::new(),
            function_metadata: HashMap::new(),
            devices: HashMap::new(),
            instructions: Instructions::default(),
            current_line: 1,
            declared_main: false,
            _config: config.unwrap_or_default(),
            temp_counter: 0,
            label_counter: 0,
            loop_stack: Vec::new(),
            current_return_label: None,
            source_map: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn compile(mut self) -> CompilationResult<'a> {
        let expr = self.parser.parse_all();

        // Copy errors from parser
        for e in std::mem::take(&mut self.parser.errors) {
            self.errors.push(Error::Parse(e));
        }

        // We treat parse_all result as potentially partial
        let expr = match expr {
            Ok(Some(expr)) => expr,
            Ok(None) => {
                return CompilationResult {
                    errors: self.errors,
                    instructions: self.instructions,
                };
            }
            Err(e) => {
                // Should be covered by parser.errors, but just in case
                self.errors.push(Error::Parse(e));
                return CompilationResult {
                    errors: self.errors,
                    instructions: self.instructions,
                };
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
        if let Err(e) = self.write_instruction(
            Instruction::Jump(Operand::Label(Cow::from("main"))),
            Some(span),
        ) {
            self.errors.push(e);
            return CompilationResult {
                errors: self.errors,
                instructions: self.instructions,
            };
        }

        let mut scope = VariableScope::default();

        // We ignore the result of the root expression (usually a block)
        if let Err(e) = self.expression(spanned_root, &mut scope) {
            self.errors.push(e);
        }

        CompilationResult {
            errors: self.errors,
            instructions: self.instructions,
        }
    }

    /// Performs a write to the output buffer as well as a push to the IL instructions vec
    fn write_instruction(
        &mut self,
        instr: Instruction<'a>,
        span: Option<Span>,
    ) -> Result<(), Error<'a>> {
        self.current_line += 1;

        self.instructions.push(InstructionNode::new(instr, span));
        Ok(())
    }

    fn next_temp_name(&mut self) -> Cow<'a, str> {
        self.temp_counter += 1;
        Cow::from(format!("__binary_temp_{}", self.temp_counter))
    }

    fn next_label_name(&mut self) -> Cow<'a, str> {
        self.label_counter += 1;
        Cow::from(format!("__internal_L{}", self.label_counter))
    }

    fn expression(
        &mut self,
        expr: Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
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
                span,
            }) => self.expression_syscall_math(math, span, scope),
            Expression::While(expr_while) => {
                self.expression_while(expr_while.node, scope)?;
                Ok(None)
            }
            Expression::Break(span) => {
                self.expression_break(span)?;
                Ok(None)
            }
            Expression::Continue(span) => {
                self.expression_continue(span)?;
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
            Expression::Ternary(tern) => Ok(Some(self.expression_ternary(tern.node, scope)?)),
            Expression::Invocation(expr_invoke) => {
                self.expression_function_invocation(expr_invoke, scope)?;
                // Invocation returns result in r15 (RETURN_REGISTER).
                // If used as an expression, we must move it to a temp to avoid overwrite.
                let temp_name = self.next_temp_name();
                let temp_loc =
                    scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
                self.emit_variable_assignment(
                    &temp_loc,
                    Operand::Register(VariableScope::RETURN_REGISTER),
                )?;
                Ok(Some(CompileLocation {
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
                    let loc = scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
                    self.emit_variable_assignment(&loc, Operand::Number(num.into()))?;
                    Ok(Some(CompileLocation {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                Literal::Boolean(b) => {
                    let temp_name = self.next_temp_name();
                    let loc = scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
                    self.emit_variable_assignment(&loc, Operand::Number(Number::from(b).into()))?;
                    Ok(Some(CompileLocation {
                        location: loc,
                        temp_name: Some(temp_name),
                    }))
                }
                _ => Ok(None), // String literals don't return values in this context typically
            },
            Expression::Variable(name) => {
                match scope.get_location_of(&name.node, Some(name.span)) {
                    Ok(loc) => Ok(Some(CompileLocation {
                        location: loc,
                        temp_name: None, // User variable, do not free
                    })),
                    Err(_) => {
                        // fallback, check devices
                        if let Some(device) = self.devices.get(&name.node) {
                            Ok(Some(CompileLocation {
                                location: VariableLocation::Device(device.clone()),
                                temp_name: None,
                            }))
                        } else {
                            self.errors
                                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
                            Ok(Some(CompileLocation {
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
                let (device, cleanup) = self.resolve_device(*object, scope)?;

                // 2. Allocate a temp register for the result
                let result_name = self.next_temp_name();
                let loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let reg = self.resolve_register(&loc)?;

                // 3. Emit load instruction: l rX device member
                self.write_instruction(
                    Instruction::Load(
                        Operand::Register(reg),
                        device,
                        Operand::LogicType(member.node),
                    ),
                    Some(expr.span),
                )?;

                // 4. Cleanup
                if let Some(c) = cleanup {
                    scope.free_temp(c, None)?;
                }

                Ok(Some(CompileLocation {
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
                let result_loc =
                    scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                self.write_instruction(
                    Instruction::Sub(
                        Operand::Register(result_reg),
                        Operand::Number(0.into()),
                        inner_str,
                    ),
                    Some(expr.span),
                )?;

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
                }

                Ok(Some(CompileLocation {
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
    fn resolve_device(
        &mut self,
        expr: Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        // If it's a direct variable reference, check if it's a known device alias first
        if let Expression::Variable(ref name) = expr.node
            && let Some(device_id) = self.devices.get(&name.node)
        {
            return Ok((Operand::Device(device_id.clone()), None));
        }

        // Otherwise, compile it as an operand (e.g. it might be a register holding a device hash/id)
        self.compile_operand(expr, scope)
    }

    fn emit_variable_assignment(
        &mut self,
        location: &VariableLocation<'a>,
        source_value: Operand<'a>,
    ) -> Result<(), Error<'a>> {
        match location {
            VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                self.write_instruction(
                    Instruction::Move(Operand::Register(*reg), source_value),
                    None,
                )?;
            }
            VariableLocation::Stack(_) => {
                self.write_instruction(Instruction::Push(source_value), None)?;
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

    fn expression_declaration(
        &mut self,
        var_name: Spanned<Cow<'a, str>>,
        expr: Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
        let name_str = var_name.node;
        let name_span = var_name.span;

        // optimization. Check for a negated numeric literal
        if let Expression::Negation(box_expr) = &expr.node
            && let Expression::Literal(spanned_lit) = &box_expr.node
            && let Literal::Number(neg_num) = &spanned_lit.node
        {
            let loc =
                scope.add_variable(name_str.clone(), LocationRequest::Persist, Some(name_span))?;

            self.emit_variable_assignment(&loc, Operand::Number((-*neg_num).into()))?;
            return Ok(Some(CompileLocation {
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

                    self.emit_variable_assignment(&var_location, Operand::Number(num.into()))?;
                    (var_location, None)
                }
                Literal::Boolean(b) => {
                    let var_location = scope.add_variable(
                        name_str.clone(),
                        LocationRequest::Persist,
                        Some(name_span),
                    )?;

                    self.emit_variable_assignment(
                        &var_location,
                        Operand::Number(Number::from(b).into()),
                    )?;
                    (var_location, None)
                }
                _ => return Ok(None),
            },
            Expression::Invocation(invoke_expr) => {
                self.expression_function_invocation(invoke_expr, scope)?;

                let loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;
                self.emit_variable_assignment(
                    &loc,
                    Operand::Register(VariableScope::RETURN_REGISTER),
                )?;
                (loc, None)
            }
            Expression::Syscall(spanned_call) => {
                let sys_call = spanned_call.node;
                let res = match sys_call {
                    SysCall::System(s) => {
                        self.expression_syscall_system(s, spanned_call.span, scope)?
                    }
                    SysCall::Math(m) => {
                        self.expression_syscall_math(m, spanned_call.span, scope)?
                    }
                };

                if res.is_none() {
                    return Err(Error::Unknown(
                        "SysCall did not return a value".into(),
                        Some(spanned_call.span),
                    ));
                };

                let loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;
                self.emit_variable_assignment(
                    &loc,
                    Operand::Register(VariableScope::RETURN_REGISTER),
                )?;

                (loc, None)
            }
            // Support assigning binary expressions to variables directly
            Expression::Binary(bin_expr) => {
                let result = self.expression_binary(bin_expr, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                if let CompileLocation {
                    location: VariableLocation::Constant(Literal::Number(num)),
                    ..
                } = result
                {
                    self.emit_variable_assignment(&var_loc, Operand::Number(num.into()))?;
                    (var_loc, None)
                } else {
                    // Move result from temp to new persistent variable
                    let result_reg = self.resolve_register(&result.location)?;
                    self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

                    // Free the temp result
                    if let Some(name) = result.temp_name {
                        scope.free_temp(name, None)?;
                    }
                    (var_loc, None)
                }
            }
            Expression::Logical(log_expr) => {
                let result = self.expression_logical(log_expr, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                // Move result from temp to new persistent variable
                let result_reg = self.resolve_register(&result.location)?;
                self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

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

                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                // Handle loading from stack if necessary
                let src = match src_loc {
                    VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => {
                        Operand::Register(r)
                    }
                    VariableLocation::Stack(offset) => {
                        self.write_instruction(
                            Instruction::Sub(
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                Operand::StackPointer,
                                Operand::Number(offset.into()),
                            ),
                            Some(expr.span),
                        )?;

                        self.write_instruction(
                            Instruction::Get(
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                Operand::Device(Cow::from("db")),
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                            ),
                            Some(expr.span),
                        )?;

                        Operand::Register(VariableScope::TEMP_STACK_REGISTER)
                    }
                    VariableLocation::Constant(_) | VariableLocation::Device(_) => unreachable!(),
                };
                self.emit_variable_assignment(&var_loc, src)?;
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

                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;
                let result_reg = self.resolve_register(&comp_res.location)?;

                self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

                if let Some(temp) = comp_res.temp_name {
                    scope.free_temp(temp, None)?;
                }

                (var_loc, None)
            }
            Expression::Ternary(ternary) => {
                let res = self.expression_ternary(ternary.node, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                let res_register = self.resolve_register(&res.location)?;
                self.emit_variable_assignment(&var_loc, Operand::Register(res_register))?;

                if let Some(name) = res.temp_name {
                    scope.free_temp(name, None)?;
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

        Ok(Some(CompileLocation {
            location: loc,
            temp_name,
        }))
    }

    fn expression_const_declaration(
        &mut self,
        expr: ConstDeclarationExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
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

        Ok(CompileLocation {
            location: scope.define_const(const_name.node, value, Some(const_name.span))?,
            temp_name: None,
        })
    }

    fn expression_assignment(
        &mut self,
        expr: AssignmentExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let AssignmentExpression {
            assignee,
            expression,
        } = expr;

        let expr_span = expression.span;

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

                let (val, cleanup) = self.compile_operand(*expression, scope)?;

                match location {
                    VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                        self.write_instruction(
                            Instruction::Move(Operand::Register(reg), val),
                            Some(expr_span),
                        )?;
                    }
                    VariableLocation::Stack(offset) => {
                        // Calculate address: sp - offset
                        self.write_instruction(
                            Instruction::Sub(
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                Operand::StackPointer,
                                Operand::Number(offset.into()),
                            ),
                            Some(expr_span),
                        )?;

                        // Store value to stack/db at address
                        self.write_instruction(
                            Instruction::Put(
                                Operand::Device(Cow::from("db")),
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                val,
                            ),
                            Some(expr_span),
                        )?;
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

                let (device, dev_cleanup) = self.resolve_device(*object, scope)?;
                let (val, val_cleanup) = self.compile_operand(*expression, scope)?;

                self.write_instruction(
                    Instruction::Store(device, Operand::LogicType(member.node), val),
                    Some(member.span),
                )?;

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
        invoke_expr: Spanned<InvocationExpression<'a>>,
        parent_scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
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
            return Err(Error::UnknownIdentifier(name.node, name.span));
        };

        if args.len() != arguments.len() {
            self.errors
                .push(Error::AgrumentMismatch(name.node, name.span));
            // Proceed anyway? The assembly will likely crash or act weird.
            // Best to skip generation of this call to prevent bad IC10
            return Ok(());
        }
        let mut stack = VariableScope::scoped(parent_scope);

        // backup all used registers to the stack
        let active_registers = stack.registers();
        for register in &active_registers {
            stack.add_variable(
                Cow::from(format!("temp_{register}")),
                LocationRequest::Stack,
                None,
            )?;
            self.write_instruction(
                Instruction::Push(Operand::Register(*register)),
                Some(name.span),
            )?;
        }
        for arg in arguments {
            match arg.node {
                Expression::Literal(spanned_lit) => match spanned_lit.node {
                    Literal::Number(num) => {
                        self.write_instruction(
                            Instruction::Push(Operand::Number(num.into())),
                            Some(spanned_lit.span),
                        )?;
                    }
                    Literal::Boolean(b) => {
                        self.write_instruction(
                            Instruction::Push(Operand::Number(Number::from(b).into())),
                            Some(spanned_lit.span),
                        )?;
                    }
                    _ => {}
                },
                Expression::Variable(var_name) => {
                    let loc = match stack.get_location_of(&var_name.node, Some(var_name.span)) {
                        Ok(l) => l,
                        Err(_) => {
                            self.errors
                                .push(Error::UnknownIdentifier(var_name.node, var_name.span));
                            VariableLocation::Temporary(0)
                        }
                    };

                    match loc {
                        VariableLocation::Persistant(reg) | VariableLocation::Temporary(reg) => {
                            self.write_instruction(
                                Instruction::Push(Operand::Register(reg)),
                                Some(var_name.span),
                            )?;
                        }
                        VariableLocation::Constant(lit) => {
                            self.write_instruction(
                                Instruction::Push(extract_literal(lit, false)?),
                                Some(var_name.span),
                            )?;
                        }
                        VariableLocation::Stack(stack_offset) => {
                            self.write_instruction(
                                Instruction::Sub(
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    Operand::StackPointer,
                                    Operand::Number(stack_offset.into()),
                                ),
                                Some(var_name.span),
                            )?;

                            self.write_instruction(
                                Instruction::Get(
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    Operand::Device(Cow::from("db")),
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                ),
                                Some(var_name.span),
                            )?;

                            self.write_instruction(
                                Instruction::Push(Operand::Register(
                                    VariableScope::TEMP_STACK_REGISTER,
                                )),
                                Some(var_name.span),
                            )?;
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
                    let span = bin_expr.span;
                    // Compile the binary expression to a temp register
                    let result = self.expression_binary(bin_expr, &mut stack)?;
                    let reg = self.resolve_register(&result.location)?;
                    self.write_instruction(Instruction::Push(Operand::Register(reg)), Some(span))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name, None)?;
                    }
                }
                Expression::Logical(log_expr) => {
                    let span = log_expr.span;
                    // Compile the logical expression to a temp register
                    let result = self.expression_logical(log_expr, &mut stack)?;
                    let reg = self.resolve_register(&result.location)?;
                    self.write_instruction(Instruction::Push(Operand::Register(reg)), Some(span))?;
                    if let Some(name) = result.temp_name {
                        stack.free_temp(name, None)?;
                    }
                }
                Expression::MemberAccess(access) => {
                    let span = access.span;
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
                        &mut stack,
                    )?;

                    if let Some(result) = result_opt {
                        let reg_str = self.resolve_register(&result.location)?;
                        self.write_instruction(
                            Instruction::Push(Operand::Register(reg_str)),
                            Some(span),
                        )?;
                        if let Some(name) = result.temp_name {
                            stack.free_temp(name, None)?;
                        }
                    } else {
                        self.write_instruction(
                            Instruction::Push(Operand::Number(Decimal::from(0))),
                            Some(span),
                        )?;
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
        self.write_instruction(
            Instruction::JumpAndLink(Operand::Label(name.node)),
            Some(name.span),
        )?;

        for register in active_registers {
            let VariableLocation::Stack(stack_offset) = stack
                .get_location_of(&Cow::from(format!("temp_{register}")), None)
                .map_err(Error::Scope)?
            else {
                // This shouldn't happen if we just added it
                return Err(Error::Unknown(
                    format!("Failed to recover temp_{register}"),
                    Some(name.span),
                ));
            };
            self.write_instruction(
                Instruction::Sub(
                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    Operand::StackPointer,
                    Operand::Number(stack_offset.into()),
                ),
                Some(name.span),
            )?;

            self.write_instruction(
                Instruction::Get(
                    Operand::Register(register),
                    Operand::Device(Cow::from("db")),
                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                ),
                Some(name.span),
            )?;
        }

        if stack.stack_offset() > 0 {
            self.write_instruction(
                Instruction::Sub(
                    Operand::StackPointer,
                    Operand::StackPointer,
                    Operand::Number(Decimal::from(stack.stack_offset())),
                ),
                Some(name.span),
            )?;
        }

        Ok(())
    }

    fn expression_device(
        &mut self,
        expr: DeviceDeclarationExpression<'a>,
    ) -> Result<(), Error<'a>> {
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

    fn expression_if(
        &mut self,
        expr: IfExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let end_label = self.next_label_name();
        let else_label = if expr.else_branch.is_some() {
            self.next_label_name()
        } else {
            end_label.clone()
        };

        let cond_span = expr.condition.span;

        // Compile Condition
        let (cond, cleanup) = self.compile_operand(*expr.condition, scope)?;

        // If condition is FALSE (0), jump to else_label
        self.write_instruction(
            Instruction::BranchEqZero(cond, Operand::Label(else_label.clone())),
            Some(cond_span),
        )?;

        if let Some(name) = cleanup {
            scope.free_temp(name, None)?;
        }

        // Compile Body
        // Scope variables in body are ephemeral to the block, handled by expression_block
        self.expression_block(expr.body.node, scope)?;

        // If we have an else branch, we need to jump over it after the 'if' body
        if let Some(else_branch) = expr.else_branch {
            self.write_instruction(
                Instruction::Jump(Operand::Label(end_label.clone())),
                Some(else_branch.span),
            )?;
            self.write_instruction(Instruction::LabelDef(else_label), Some(else_branch.span))?;

            match else_branch.node {
                Expression::Block(block) => self.expression_block(block.node, scope)?,
                Expression::If(if_expr) => self.expression_if(if_expr.node, scope)?,
                _ => unreachable!("Parser ensures else branch is Block or If"),
            }
        }

        self.write_instruction(Instruction::LabelDef(end_label), Some(expr.body.span))?;

        Ok(())
    }

    fn expression_loop(
        &mut self,
        expr: LoopExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Push labels to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone()));

        self.write_instruction(
            Instruction::LabelDef(start_label.clone()),
            Some(expr.body.span),
        )?;

        // Compile Body
        self.expression_block(expr.body.node, scope)?;

        // Jump back to start
        self.write_instruction(
            Instruction::Jump(Operand::Label(start_label)),
            Some(expr.body.span),
        )?;
        self.write_instruction(Instruction::LabelDef(end_label), Some(expr.body.span))?;

        self.loop_stack.pop();

        Ok(())
    }

    fn expression_while(
        &mut self,
        expr: WhileExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Push labels to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone()));

        let span = expr.condition.span;
        self.write_instruction(Instruction::LabelDef(start_label.clone()), Some(span))?;

        // Compile Condition
        let (cond, cleanup) = self.compile_operand(*expr.condition, scope)?;

        // If condition is FALSE, jump to end
        self.write_instruction(
            Instruction::BranchEqZero(cond, Operand::Label(end_label.clone())),
            Some(span),
        )?;

        if let Some(name) = cleanup {
            scope.free_temp(name, None)?;
        }

        // Compile Body
        self.expression_block(expr.body, scope)?;

        // Jump back to start
        self.write_instruction(Instruction::Jump(Operand::Label(start_label)), Some(span))?;
        self.write_instruction(Instruction::LabelDef(end_label), Some(span))?;

        self.loop_stack.pop();

        Ok(())
    }

    fn expression_break(&mut self, span: Span) -> Result<(), Error<'a>> {
        if let Some((_, end_label)) = self.loop_stack.last() {
            self.write_instruction(
                Instruction::Jump(Operand::Label(end_label.clone())),
                Some(span),
            )?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Break statement outside of loop".into(),
                None,
            ))
        }
    }

    fn expression_continue(&mut self, span: Span) -> Result<(), Error<'a>> {
        if let Some((start_label, _)) = self.loop_stack.last() {
            self.write_instruction(
                Instruction::Jump(Operand::Label(start_label.clone())),
                Some(span),
            )?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Continue statement outside of loop".into(),
                None,
            ))
        }
    }

    fn expression_ternary(
        &mut self,
        expr: TernaryExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        let TernaryExpression {
            condition,
            true_value,
            false_value,
        } = expr;

        let span = Span {
            start_line: condition.span.start_line,
            start_col: condition.span.start_col,
            end_line: false_value.span.end_line,
            end_col: false_value.span.end_col,
        };

        let (cond, cond_clean) = self.compile_operand(*condition, scope)?;
        let (true_val, true_clean) = self.compile_operand(*true_value, scope)?;
        let (false_val, false_clean) = self.compile_operand(*false_value, scope)?;

        let result_name = self.next_temp_name();
        let result_loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
        let result_reg = self.resolve_register(&result_loc)?;

        self.write_instruction(
            Instruction::Select(Operand::Register(result_reg), cond, true_val, false_val),
            Some(span),
        )?;

        if let Some(clean) = cond_clean {
            scope.free_temp(clean, None)?;
        }
        if let Some(clean) = true_clean {
            scope.free_temp(clean, None)?;
        }
        if let Some(clean) = false_clean {
            scope.free_temp(clean, None)?;
        }
        Ok(CompileLocation {
            location: result_loc,
            temp_name: Some(result_name),
        })
    }

    /// Helper to resolve a location to a register string (e.g., "r0").
    /// Note: This does not handle Stack locations automatically, as they require
    /// instruction emission to load. Use `compile_operand` for general handling.
    fn resolve_register(&self, loc: &VariableLocation) -> Result<u8, Error<'a>> {
        match loc {
            VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => Ok(*r),
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
        expr: Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        // Optimization for literals
        if let Expression::Literal(spanned_lit) = &expr.node {
            if let Literal::Number(n) = spanned_lit.node {
                return Ok((Operand::Number(n.into()), None));
            }
            if let Literal::Boolean(b) = spanned_lit.node {
                return Ok((Operand::Number(Decimal::from(if b { 1 } else { 0 })), None));
            }
            if let Literal::String(ref s) = spanned_lit.node {
                return Ok((Operand::LogicType(s.clone()), None));
            }
        }

        // Optimization for negated literals used as operands.
        // E.g., `1 + -2` -> return "-2" string, no register used.
        if let Expression::Negation(inner) = &expr.node
            && let Expression::Literal(spanned_lit) = &inner.node
            && let Literal::Number(n) = spanned_lit.node
        {
            return Ok((Operand::Number((-n).into()), None));
        }

        let result_opt = self.expression(expr, scope)?;

        let result = match result_opt {
            Some(r) => r,
            None => {
                // Expression failed or returned void. Recover with dummy.
                return Ok((Operand::Register(0), None));
            }
        };

        match result.location {
            VariableLocation::Temporary(r) | VariableLocation::Persistant(r) => {
                Ok((Operand::Register(r), result.temp_name))
            }
            VariableLocation::Constant(lit) => match lit {
                Literal::Number(n) => Ok((Operand::Number(n.into()), None)),
                Literal::Boolean(b) => Ok((Operand::Number(Number::from(b).into()), None)),
                Literal::String(s) => Ok((Operand::LogicType(s), None)),
            },
            VariableLocation::Stack(offset) => {
                // If it's on the stack, we must load it into a temp to use it as an operand
                let temp_name = self.next_temp_name();
                let temp_loc =
                    scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
                let temp_reg = self.resolve_register(&temp_loc)?;

                self.write_instruction(
                    Instruction::Sub(
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                        Operand::StackPointer,
                        Operand::Number(Decimal::from(offset)),
                    ),
                    None,
                )?;
                self.write_instruction(
                    Instruction::Get(
                        Operand::Register(temp_reg),
                        Operand::Device(Cow::from("db")),
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    ),
                    None,
                )?;

                // If the original result had a temp name (unlikely for Stack, but possible logic),
                // we technically should free it if it's not needed, but Stack usually implies it's safe there.
                // We return the NEW temp name to be freed.
                Ok((Operand::Register(temp_reg), Some(temp_name)))
            }
            VariableLocation::Device(d) => Ok((Operand::Device(d), None)),
        }
    }

    fn compile_literal_or_variable(
        &mut self,
        val: LiteralOrVariable<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
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

    fn expression_binary(
        &mut self,
        expr: Spanned<BinaryExpression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        fn fold_binary_expression<'a>(expr: &BinaryExpression<'a>) -> Option<Number> {
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

        fn fold_expression<'a>(expr: &Expression<'a>) -> Option<Number> {
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
            return Ok(CompileLocation {
                location: VariableLocation::Constant(Literal::Number(const_lit)),
                temp_name: None,
            });
        };

        #[allow(clippy::type_complexity)]
        let (op_instr, left_expr, right_expr): (
            fn(Operand<'a>, Operand<'a>, Operand<'a>) -> Instruction<'a>,
            Box<Spanned<Expression<'a>>>,
            Box<Spanned<Expression<'a>>>,
        ) = match expr.node {
            BinaryExpression::Add(l, r) => {
                (|into, lhs, rhs| Instruction::Add(into, lhs, rhs), l, r)
            }
            BinaryExpression::Multiply(l, r) => {
                (|into, lhs, rhs| Instruction::Mul(into, lhs, rhs), l, r)
            }
            BinaryExpression::Divide(l, r) => {
                (|into, lhs, rhs| Instruction::Div(into, lhs, rhs), l, r)
            }
            BinaryExpression::Subtract(l, r) => {
                (|into, lhs, rhs| Instruction::Sub(into, lhs, rhs), l, r)
            }
            BinaryExpression::Exponent(l, r) => {
                (|into, lhs, rhs| Instruction::Pow(into, lhs, rhs), l, r)
            }
            BinaryExpression::Modulo(l, r) => {
                (|into, lhs, rhs| Instruction::Mod(into, lhs, rhs), l, r)
            }
        };

        let span = Span {
            start_line: left_expr.span.start_line,
            start_col: left_expr.span.start_col,
            end_line: right_expr.span.end_line,
            end_col: right_expr.span.end_col,
        };

        // Compile LHS
        let (lhs, lhs_cleanup) = self.compile_operand(*left_expr, scope)?;
        // Compile RHS
        let (rhs, rhs_cleanup) = self.compile_operand(*right_expr, scope)?;

        // Allocate result register
        let result_name = self.next_temp_name();
        let result_loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
        let result_reg = self.resolve_register(&result_loc)?;

        // Emit instruction: op result lhs rhs
        self.write_instruction(
            op_instr(Operand::Register(result_reg), lhs, rhs),
            Some(span),
        )?;

        // Clean up operand temps
        if let Some(name) = lhs_cleanup {
            scope.free_temp(name, None)?;
        }
        if let Some(name) = rhs_cleanup {
            scope.free_temp(name, None)?;
        }

        Ok(CompileLocation {
            location: result_loc,
            temp_name: Some(result_name),
        })
    }

    fn expression_logical(
        &mut self,
        expr: Spanned<LogicalExpression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        match expr.node {
            LogicalExpression::Not(inner) => {
                let span = inner.span;
                let (inner_str, cleanup) = self.compile_operand(*inner, scope)?;

                let result_name = self.next_temp_name();
                let result_loc =
                    scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // seq rX rY 0  => if rY == 0 set rX = 1 else rX = 0
                self.write_instruction(
                    Instruction::SetEq(
                        Operand::Register(result_reg),
                        inner_str,
                        Operand::Number(0.into()),
                    ),
                    Some(span),
                )?;

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
                }

                Ok(CompileLocation {
                    location: result_loc,
                    temp_name: Some(result_name),
                })
            }
            _ => {
                #[allow(clippy::type_complexity)]
                let (op_instr, left_expr, right_expr): (
                    fn(Operand<'a>, Operand<'a>, Operand<'a>) -> Instruction<'a>,
                    Box<Spanned<Expression<'a>>>,
                    Box<Spanned<Expression<'a>>>,
                ) = match expr.node {
                    LogicalExpression::And(l, r) => {
                        (|into, lhs, rhs| Instruction::And(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::Or(l, r) => {
                        (|into, lhs, rhs| Instruction::Or(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::Equal(l, r) => {
                        (|into, lhs, rhs| Instruction::SetEq(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::NotEqual(l, r) => {
                        (|into, lhs, rhs| Instruction::SetNe(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::GreaterThan(l, r) => {
                        (|into, lhs, rhs| Instruction::SetGt(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::GreaterThanOrEqual(l, r) => {
                        (|into, lhs, rhs| Instruction::SetGe(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::LessThan(l, r) => {
                        (|into, lhs, rhs| Instruction::SetLt(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::LessThanOrEqual(l, r) => {
                        (|into, lhs, rhs| Instruction::SetLe(into, lhs, rhs), l, r)
                    }
                    LogicalExpression::Not(_) => unreachable!(),
                };

                let span = Span {
                    start_line: left_expr.span.start_line,
                    start_col: left_expr.span.start_col,
                    end_line: right_expr.span.end_line,
                    end_col: right_expr.span.end_col,
                };

                // Compile LHS
                let (lhs, lhs_cleanup) = self.compile_operand(*left_expr, scope)?;
                // Compile RHS
                let (rhs, rhs_cleanup) = self.compile_operand(*right_expr, scope)?;

                // Allocate result register
                let result_name = self.next_temp_name();
                let result_loc =
                    scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                // Emit instruction: op result lhs rhs
                self.write_instruction(
                    op_instr(Operand::Register(result_reg), lhs, rhs),
                    Some(span),
                )?;

                // Clean up operand temps
                if let Some(name) = lhs_cleanup {
                    scope.free_temp(name, None)?;
                }
                if let Some(name) = rhs_cleanup {
                    scope.free_temp(name, None)?;
                }

                Ok(CompileLocation {
                    location: result_loc,
                    temp_name: Some(result_name),
                })
            }
        }
    }

    fn expression_block<'v>(
        &mut self,
        mut expr: BlockExpression<'a>,
        parent_scope: &'v mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        fn get_expression_priority<'a>(expr: &Spanned<Expression<'a>>) -> u32 {
            match expr.node {
                Expression::ConstDeclaration(_) => 0,
                Expression::DeviceDeclaration(_) => 1,
                Expression::Function(_) => 2,
                _ => 3,
            }
        }

        // First, sort the expressions to ensure functions are hoisted
        expr.0.sort_by(|a, b| {
            let a_cost = get_expression_priority(a);
            let b_cost = get_expression_priority(b);

            a_cost.cmp(&b_cost)
        });

        let mut scope = VariableScope::scoped(parent_scope);

        for expr in expr.0 {
            if !self.declared_main
                && !matches!(
                    expr.node,
                    Expression::Function(_)
                        | Expression::ConstDeclaration(_)
                        | Expression::DeviceDeclaration(_)
                )
                && !parent_scope.has_parent()
            {
                self.write_instruction(Instruction::LabelDef(Cow::from("main")), Some(expr.span))?;
                self.declared_main = true;
            }

            match expr.node {
                Expression::Return(ret_expr) => {
                    self.expression_return(ret_expr, &mut scope)?;
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
            self.write_instruction(
                Instruction::Sub(
                    Operand::StackPointer,
                    Operand::StackPointer,
                    Operand::Number(scope.stack_offset().into()),
                ),
                None,
            )?;
        }

        Ok(())
    }

    /// Takes the result of the expression and stores it in VariableScope::RETURN_REGISTER
    fn expression_return(
        &mut self,
        expr: Option<Box<Spanned<Expression<'a>>>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        if let Some(expr) = expr {
            let span = expr.span;
            if let Expression::Negation(neg_expr) = &expr.node
                && let Expression::Literal(spanned_lit) = &neg_expr.node
                && let Literal::Number(neg_num) = &spanned_lit.node
            {
                let loc = VariableLocation::Persistant(VariableScope::RETURN_REGISTER);
                self.emit_variable_assignment(&loc, Operand::Number((-*neg_num).into()))?;
                return Ok(loc);
            };

            match expr.node {
                Expression::Variable(var_name) => {
                    match scope.get_location_of(&var_name.node, Some(var_name.span)) {
                        Ok(loc) => match loc {
                            VariableLocation::Temporary(reg)
                            | VariableLocation::Persistant(reg) => {
                                self.write_instruction(
                                    Instruction::Move(
                                        Operand::Register(VariableScope::RETURN_REGISTER),
                                        Operand::Register(reg),
                                    ),
                                    Some(span),
                                )?;
                            }
                            VariableLocation::Constant(lit) => {
                                let op = extract_literal(lit, false)?;
                                self.write_instruction(
                                    Instruction::Move(
                                        Operand::Register(VariableScope::RETURN_REGISTER),
                                        op,
                                    ),
                                    Some(span),
                                )?;
                            }
                            VariableLocation::Stack(offset) => {
                                self.write_instruction(
                                    Instruction::Sub(
                                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                        Operand::StackPointer,
                                        Operand::Number(offset.into()),
                                    ),
                                    Some(span),
                                )?;
                                self.write_instruction(
                                    Instruction::Get(
                                        Operand::Register(VariableScope::RETURN_REGISTER),
                                        Operand::Device(Cow::from("db")),
                                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    ),
                                    Some(span),
                                )?;
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
                            &VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                            Operand::Number(num.into()),
                        )?;
                    }
                    Literal::Boolean(b) => {
                        self.emit_variable_assignment(
                            &VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                            Operand::Number(Number::from(b).into()),
                        )?;
                    }
                    _ => {}
                },
                Expression::Binary(bin_expr) => {
                    let span = bin_expr.span;
                    let result = self.expression_binary(bin_expr, scope)?;
                    let result_reg = self.resolve_register(&result.location)?;
                    self.write_instruction(
                        Instruction::Move(
                            Operand::Register(VariableScope::RETURN_REGISTER),
                            Operand::Register(result_reg),
                        ),
                        Some(span),
                    )?;

                    if let Some(name) = result.temp_name {
                        scope.free_temp(name, None)?;
                    }
                }
                Expression::Logical(log_expr) => {
                    let span = log_expr.span;
                    let result = self.expression_logical(log_expr, scope)?;
                    let result_reg = self.resolve_register(&result.location)?;
                    self.write_instruction(
                        Instruction::Move(
                            Operand::Register(VariableScope::RETURN_REGISTER),
                            Operand::Register(result_reg),
                        ),
                        Some(span),
                    )?;

                    if let Some(name) = result.temp_name {
                        scope.free_temp(name, None)?;
                    }
                }
                Expression::MemberAccess(access) => {
                    let span = access.span;
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
                        self.write_instruction(
                            Instruction::Move(
                                Operand::Register(VariableScope::RETURN_REGISTER),
                                Operand::Register(reg),
                            ),
                            Some(span),
                        )?;

                        if let Some(temp) = res.temp_name {
                            scope.free_temp(temp, Some(span))?;
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
        }

        if let Some(label) = &self.current_return_label {
            self.write_instruction(Instruction::Jump(Operand::Label(label.clone())), None)?;
        } else {
            return Err(Error::Unknown(
                "Return statement used outside of function context.".into(),
                None,
            ));
        }

        Ok(VariableLocation::Persistant(VariableScope::RETURN_REGISTER))
    }

    // syscalls that return values will be stored in the VariableScope::RETURN_REGISTER
    // register
    fn expression_syscall_system(
        &mut self,
        expr: System<'a>,
        span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
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
                self.write_instruction(Instruction::Yield, Some(span))?;
                Ok(None)
            }
            System::Sleep(amt) => {
                let (op, var_cleanup) = self.compile_operand(*amt, scope)?;
                self.write_instruction(Instruction::Sleep(op), Some(span))?;

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

                Ok(Some(CompileLocation {
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
                    .unwrap_or(Cow::from("d0"));

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

                self.write_instruction(
                    Instruction::Store(
                        Operand::Device(device_val),
                        Operand::LogicType(logic_type),
                        variable,
                    ),
                    Some(span),
                )?;
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

                self.write_instruction(
                    Instruction::StoreBatch(device_hash_val, Operand::LogicType(logic_type), var),
                    Some(span),
                )?;
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

                self.write_instruction(
                    Instruction::StoreBatchNamed(device_hash, name_hash, logic_type, value),
                    Some(span),
                )?;
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
                    .unwrap_or(Cow::from("d0"));

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

                self.write_instruction(
                    Instruction::Load(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        Operand::Device(device_val),
                        Operand::LogicType(logic_type),
                    ),
                    Some(span),
                )?;

                Ok(Some(CompileLocation {
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

                self.write_instruction(
                    Instruction::LoadBatch(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        logic_type,
                        batch_mode,
                    ),
                    Some(span),
                )?;
                cleanup!(device_hash_cleanup, logic_type_cleanup, batch_mode_cleanup);

                Ok(Some(CompileLocation {
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

                self.write_instruction(
                    Instruction::LoadBatchNamed(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        name_hash,
                        logic_type,
                        batch_mode,
                    ),
                    Some(span),
                )?;
                cleanup!(
                    device_hash_cleanup,
                    name_hash_cleanup,
                    logic_type_cleanup,
                    batch_mode_cleanup
                );

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadSlot(dev_name, slot_index, logic_type) => {
                let (dev_hash, hash_cleanup) =
                    self.compile_literal_or_variable(dev_name.node, scope)?;
                let (slot_index, slot_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(slot_index.node),
                    scope,
                )?;
                let (logic_type, logic_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(logic_type.node),
                    scope,
                )?;

                self.write_instruction(
                    Instruction::LoadSlot(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        dev_hash,
                        slot_index,
                        logic_type,
                    ),
                    Some(span),
                )?;
                cleanup!(hash_cleanup, slot_cleanup, logic_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::SetSlot(dev_name, slot_index, logic_type, var) => {
                let (dev_name, name_cleanup) =
                    self.compile_literal_or_variable(dev_name.node, scope)?;
                let (slot_index, index_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(slot_index.node),
                    scope,
                )?;
                let (logic_type, type_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Literal(logic_type.node),
                    scope,
                )?;
                let (var, var_cleanup) = self.compile_operand(*var, scope)?;

                self.write_instruction(
                    Instruction::StoreSlot(dev_name, slot_index, logic_type, var),
                    Some(span),
                )?;
                cleanup!(name_cleanup, index_cleanup, type_cleanup, var_cleanup);

                Ok(None)
            }
        }
    }

    fn expression_syscall_math(
        &mut self,
        expr: Math<'a>,
        span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
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
                self.write_instruction(
                    Instruction::Acos(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Asin(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Asin(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Atan(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Atan(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Atan2(expr1, expr2) => {
                let (var1, var1_cleanup) = self.compile_operand(*expr1, scope)?;
                let (var2, var2_cleanup) = self.compile_operand(*expr2, scope)?;

                self.write_instruction(
                    Instruction::Atan2(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        var1,
                        var2,
                    ),
                    Some(span),
                )?;
                cleanup!(var1_cleanup, var2_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Abs(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Abs(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Ceil(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Ceil(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Cos(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;
                self.write_instruction(
                    Instruction::Cos(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Floor(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Floor(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Log(expr) => {
                let (var, cleanup) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Log(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Max(expr1, expr2) => {
                let (var1, clean1) = self.compile_operand(*expr1, scope)?;
                let (var2, clean2) = self.compile_operand(*expr2, scope)?;

                self.write_instruction(
                    Instruction::Max(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        var1,
                        var2,
                    ),
                    Some(span),
                )?;
                cleanup!(clean1, clean2);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Min(expr1, expr2) => {
                let (var1, clean1) = self.compile_operand(*expr1, scope)?;
                let (var2, clean2) = self.compile_operand(*expr2, scope)?;

                self.write_instruction(
                    Instruction::Min(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        var1,
                        var2,
                    ),
                    Some(span),
                )?;
                cleanup!(clean1, clean2);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Rand => {
                self.write_instruction(
                    Instruction::Rand(Operand::Register(VariableScope::RETURN_REGISTER)),
                    Some(span),
                )?;

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Sin(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Sin(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(clean);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Sqrt(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;

                self.write_instruction(
                    Instruction::Sqrt(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(clean);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Tan(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_instruction(
                    Instruction::Tan(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(clean);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            Math::Trunc(expr) => {
                let (var, clean) = self.compile_operand(*expr, scope)?;
                self.write_instruction(
                    Instruction::Trunc(Operand::Register(VariableScope::RETURN_REGISTER), var),
                    Some(span),
                )?;
                cleanup!(clean);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
        }
    }

    /// Compile a function declaration.
    /// Calees are responsible for backing up any registers they wish to use.
    fn expression_function(
        &mut self,
        expr: Spanned<FunctionExpression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let FunctionExpression {
            name,
            arguments,
            body,
        } = expr.node;

        let span = expr.span;

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
        self.write_instruction(Instruction::LabelDef(name.node.clone()), Some(span))?;

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
                    self.write_instruction(
                        Instruction::Pop(Operand::Register(loc)),
                        Some(var_name.span),
                    )?;
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

        self.write_instruction(Instruction::Push(Operand::ReturnAddress), Some(span))?;

        let return_label = self.next_label_name();

        let prev_return_label = self.current_return_label.replace(return_label.clone());

        block_scope.add_variable(
            return_label.clone(),
            LocationRequest::Stack,
            Some(name.span),
        )?;

        for expr in body.0 {
            match expr.node {
                Expression::Return(ret_expr) => {
                    self.expression_return(ret_expr, &mut block_scope)?;
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
        let ra_res = block_scope.get_location_of(&return_label, Some(name.span));

        let ra_stack_offset = match ra_res {
            Ok(VariableLocation::Stack(offset)) => {
                block_scope.free_temp(return_label.clone(), None)?;
                offset
            }
            _ => {
                // If we can't find RA, we can't return properly.
                // This usually implies a compiler bug or scope tracking error.
                return Err(Error::Unknown(
                    "Stored return address not in stack as expected".into(),
                    Some(name.span),
                ));
            }
        };

        self.current_return_label = prev_return_label;

        self.write_instruction(Instruction::LabelDef(return_label.clone()), Some(span))?;

        self.write_instruction(
            Instruction::Sub(
                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                Operand::StackPointer,
                Operand::Number(ra_stack_offset.into()),
            ),
            Some(span),
        )?;

        self.write_instruction(
            Instruction::Get(
                Operand::ReturnAddress,
                Operand::Device(Cow::from("db")),
                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
            ),
            Some(span),
        )?;

        if block_scope.stack_offset() > 0 {
            self.write_instruction(
                Instruction::Sub(
                    Operand::StackPointer,
                    Operand::StackPointer,
                    Operand::Number(block_scope.stack_offset().into()),
                ),
                Some(span),
            )?;
        }

        self.write_instruction(Instruction::Jump(Operand::ReturnAddress), Some(span))?;
        Ok(())
    }
}
