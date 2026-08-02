#![allow(clippy::result_large_err)]
use crate::variable_manager::{LocationRequest, VariableLocation, VariableScope};
use helpers::{Span, prelude::*};
use il::{Instruction, InstructionNode, Instructions, Operand};
use parser::{
    ParseOutput, Parser as ASTParser,
    sys_call::{Math, SysCall, System},
    tree_node::{
        AssignmentExpression, BinaryExpression, BlockExpression, ConstDeclarationExpression,
        DeviceDeclarationExpression, DeviceType, Expression, FunctionExpression, IfExpression,
        IndexAccessExpression, InvocationExpression, Literal, LiteralOr, LiteralOrVariable,
        LogicalExpression, LoopExpression, MemberAccessExpression, Spanned, TernaryExpression,
        TupleAssignmentExpression, TupleDeclarationExpression, WhileExpression,
    },
};
use rust_decimal::Decimal;
use static_analysis::AnalyzeResult;
use std::{borrow::Cow, collections::HashMap};
use tokenizer::token::{Number, Unit};

mod error;

pub use error::Error;

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
    pub metadata: crate::CompilationMetadata<'a>,
}

/// Metadata for the currently compiling function
#[derive(Debug, Default)]
struct FunctionMetadata<'a> {
    /// Maps function name to its instruction location
    locations: HashMap<Cow<'a, str>, usize>,
    /// Maps function name to list of parameter names
    params: HashMap<Cow<'a, str>, Vec<Cow<'a, str>>>,
    /// Maps function name to tuple return size (if it returns a tuple)
    tuple_return_sizes: HashMap<Cow<'a, str>, usize>,
    /// Name of the function currently being compiled
    current_name: Option<Cow<'a, str>>,
    /// Return label for the current function
    return_label: Option<Cow<'a, str>>,
    /// Size of tuple return for the current function (0 if not returning tuple)
    tuple_return_size: u16,
    /// Whether the SP (stack pointer) has been saved for the current function
    sp_saved: bool,
    /// Variable name for the saved SP at function entry (for stack unwinding)
    sp_backup_var: Option<Cow<'a, str>>,
}

pub struct Compiler<'a> {
    analyze_result: AnalyzeResult<'a>,
    function_meta: FunctionMetadata<'a>,
    devices: HashMap<Cow<'a, str>, DeviceType>,

    // This holds the IL code which will be used in the
    // optimizer
    pub instructions: Instructions<'a>,

    current_line: usize,
    declared_main: bool,
    _config: CompilerConfig,
    temp_counter: usize,
    label_counter: usize,
    loop_stack: Vec<(Cow<'a, str>, Cow<'a, str>, u16)>, // Stores (start_label, end_label, stack_depth_at_entry)
    /// stores (IC10 `line_num`, `Vec<Span>`)
    pub source_map: HashMap<usize, Vec<Span>>,
    /// Accumulative errors from the compilation process
    pub errors: Vec<Error<'a>>,
    /// Metadata about symbols encountered during compilation
    pub metadata: crate::CompilationMetadata<'a>,
    pub declaration_docs: std::collections::HashMap<String, String>,
}

/// Chains multiple operand compilations together, injecting a "prevent_return_register_clobbering"
/// inbetween expressions.
///
/// # Example
/// ```
/// let ((expr, cleanup)) = compile_operands!(self, (*operand), scope);
/// let ((expr1, cleanup1), (expr2, cleanup2), (expr3, cleanup3)) = compile_operands!(self, (*operand1, *operand2, *operand3), scope);
/// ```
macro_rules! compile_operands {
    (@increment $self:expr, $scope:expr, [$($acc:tt)*];) => {
        ($($acc)*)
    };
    (@increment $self:expr, $scope:expr, [$($acc:tt)*]; $expr:expr) => {
        compile_operands!{
            @increment $self, $scope, [$($acc)* $self.compile_operand($expr, $scope)?,];
        }
    };
    (@increment $self:expr, $scope:expr, [$($acc:tt)*]; $expr:expr, $($tail:tt)*) => {
        compile_operands!{
            @increment $self, $scope, [$($acc)* {
                let (opr, cleanup) = $self.compile_operand($expr, $scope)?;
                $self.prevent_return_register_clobbering(opr, cleanup, $scope)?
            },]; $($tail)*
        }
    };
    ($self:expr, ($($toks:tt)+), $scope:expr) => { compile_operands! {@increment $self, $scope, []; $($toks)*} };
}

mod control_flow;
mod functions;
mod operands;
mod syscalls;
mod tuples;

impl<'a> Compiler<'a> {
    pub fn new(
        analyze_result: AnalyzeResult<'a>,
        declaration_docs: std::collections::HashMap<String, String>,
        config: Option<CompilerConfig>,
    ) -> Self {
        Self {
            analyze_result,
            function_meta: FunctionMetadata::default(),
            devices: HashMap::new(),
            instructions: Instructions::default(),
            current_line: 1,
            declared_main: false,
            _config: config.unwrap_or_default(),
            temp_counter: 0,
            label_counter: 0,
            loop_stack: Vec::new(),
            source_map: HashMap::new(),
            errors: Vec::new(),
            metadata: crate::CompilationMetadata::new(),
            declaration_docs,
        }
    }

    pub fn compile(mut self, ast: &Spanned<Expression<'a>>) -> CompilationResult<'a> {
        let expr = ast;

        if let Err(e) = self.write_instruction(
            Instruction::Jump(Operand::Label(Cow::from("main"))),
            Some(expr.span.clone()),
        ) {
            self.errors.push(e);
            return CompilationResult {
                errors: self.errors,
                instructions: self.instructions,
                metadata: self.metadata,
            };
        }

        let mut scope = VariableScope::default();

        // We ignore the result of the root expression (usually a block)
        if let Err(e) = self.expression(expr, &mut scope) {
            self.errors.push(e);
        }

        CompilationResult {
            errors: self.errors,
            instructions: self.instructions,
            metadata: self.metadata,
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

    /// Merges two spans into a single span covering both
    fn merge_spans(start: Span, end: Span) -> Span {
        Span {
            start_line: start.start_line,
            start_col: start.start_col,
            end_line: end.end_line,
            end_col: end.end_col,
        }
    }

    /// Cleans up temporary variables, ignoring errors
    fn cleanup_temps(
        scope: &mut VariableScope<'a, '_>,
        temps: &[Option<Cow<'a, str>>],
    ) -> Result<(), Error<'a>> {
        for name in temps.iter().flatten() {
            scope.free_temp(name.clone(), None)?;
        }
        Ok(())
    }

    fn expression(
        &mut self,
        expr: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
        match &expr.node {
            Expression::Function(expr_func) => {
                self.expression_function(expr_func, scope)?;
                Ok(None)
            }
            Expression::Block(expr_block) => {
                self.expression_block(expr_block, scope)?;
                Ok(None)
            }
            Expression::If(expr_if) => {
                self.expression_if(&expr_if.node, scope)?;
                Ok(None)
            }
            Expression::Loop(expr_loop) => {
                self.expression_loop(&expr_loop.node, scope)?;
                Ok(None)
            }
            Expression::Syscall(Spanned {
                node: SysCall::System(system),
                span,
            }) => self.expression_syscall_system(system, *span, scope),
            Expression::Syscall(Spanned {
                node: SysCall::Math(math),
                span,
            }) => self.expression_syscall_math(math, *span, scope),
            Expression::While(expr_while) => {
                self.expression_while(&expr_while.node, scope)?;
                Ok(None)
            }
            Expression::Break(span) => {
                self.expression_break(*span, scope)?;
                Ok(None)
            }
            Expression::Continue(span) => {
                self.expression_continue(*span, scope)?;
                Ok(None)
            }
            Expression::DeviceDeclaration(expr_dev) => {
                self.expression_device(&expr_dev.node)?;
                Ok(None)
            }
            Expression::Declaration(var_name, decl_expr) => {
                // decl_expr is Box<Spanned<Expression>>
                self.expression_declaration(var_name.clone(), &*decl_expr, scope)
            }
            Expression::ConstDeclaration(const_decl_expr) => {
                self.expression_const_declaration(&const_decl_expr.node, scope)?;
                Ok(None)
            }
            Expression::Assignment(assign_expr) => {
                self.expression_assignment(&assign_expr.node, scope)?;
                Ok(None)
            }
            Expression::Ternary(tern) => Ok(Some(self.expression_ternary(&tern.node, scope)?)),
            Expression::Invocation(expr_invoke) => {
                // Special case: hash() with string literal can be evaluated at compile time
                if expr_invoke.node.name.node == "hash"
                    && expr_invoke.node.arguments.len() == 1
                    && let Expression::Literal(Spanned {
                        node: Literal::String(str_to_hash),
                        ..
                    }) = &expr_invoke.node.arguments[0].node
                {
                    // Evaluate hash at compile time
                    let hash_value = crc_hash_signed(str_to_hash);
                    return Ok(Some(CompileLocation {
                        location: VariableLocation::Constant(Literal::Number(Number::Integer(
                            hash_value,
                            Unit::None,
                        ))),
                        temp_name: None,
                    }));
                }

                // Non-constant hash calls or other function calls
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
                    Ok(loc) => {
                        // Track this variable reference in metadata (for tooltips on all usages, not just declaration)
                        let doc_comment: Option<Cow<'a, str>> = self
                            .declaration_docs
                            .get(name.node.as_ref())
                            .map(|s| Cow::Owned(s.to_owned()) as Cow<'a, str>);
                        self.metadata.add_variable_with_doc(
                            name.node.clone(),
                            Some(name.span),
                            doc_comment,
                        );

                        Ok(Some(CompileLocation {
                            location: loc,
                            temp_name: None, // User variable, do not free
                        }))
                    }
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
                let MemberAccessExpression { object, member } = &access.node;

                // 1. Resolve the object to a device string (e.g., "d0" or "rX")
                let (device, cleanup) = self.resolve_device(&object, scope)?;

                // 2. Allocate a temp register for the result
                let result_name = self.next_temp_name();
                let loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let reg = self.resolve_register(&loc)?;

                // 3. Emit load instruction: l rX device member
                self.write_instruction(
                    Instruction::Load(
                        Operand::Register(reg),
                        device,
                        Operand::LogicType(member.node.clone()),
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
            Expression::IndexAccess(access) => {
                // "get" behavior (e.g. `let x = d0[255]`)
                let IndexAccessExpression { object, index } = &access.node;

                // 1. Resolve the object to a device string
                let (device, dev_cleanup) = self.resolve_device(&object, scope)?;

                // Check if device is "db" (not allowed)
                if let Operand::Device(ref dev_str) = device
                    && dev_str.as_ref() == "db"
                {
                    return Err(Error::OperationNotSupported(
                        "Direct stack access on 'db' is not yet supported".to_string(),
                        expr.span,
                    ));
                }

                // 2. Compile the index expression to get the address
                let (addr, addr_cleanup) = self.compile_operand(&index, scope)?;

                // 3. Allocate a temp register for the result
                let result_name = self.next_temp_name();
                let loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let reg = self.resolve_register(&loc)?;

                // 4. Emit get instruction: get rX device address
                self.write_instruction(
                    Instruction::Get(Operand::Register(reg), device, addr),
                    Some(expr.span),
                )?;

                // 5. Cleanup
                if let Some(c) = dev_cleanup {
                    scope.free_temp(c, None)?;
                }
                if let Some(c) = addr_cleanup {
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
            Expression::Priority(inner_expr) => self.expression(&*inner_expr, scope),
            Expression::Negation(inner_expr) => {
                // Compile negation as 0 - inner
                let (inner_str, cleanup) = self.compile_operand(&inner_expr, scope)?;
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
            Expression::BitwiseNot(inner_expr) => {
                // Compile bitwise NOT using the NOT instruction
                let (inner_str, cleanup) = self.compile_operand(&inner_expr, scope)?;
                let result_name = self.next_temp_name();
                let result_loc =
                    scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
                let result_reg = self.resolve_register(&result_loc)?;

                self.write_instruction(
                    Instruction::Not(Operand::Register(result_reg), inner_str),
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
            Expression::TupleDeclaration(tuple_decl) => {
                self.expression_tuple_declaration(&tuple_decl.node, scope)?;
                Ok(None)
            }
            Expression::TupleAssignment(tuple_assign) => {
                self.expression_tuple_assignment(&tuple_assign.node, scope)?;
                Ok(None)
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

    fn expression_const_declaration(
        &mut self,
        expr: &ConstDeclarationExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        let ConstDeclarationExpression {
            name: const_name,
            value: const_value,
        } = expr;

        // Track the const variable in metadata
        let doc_comment = self
            .declaration_docs
            .get(const_name.node.as_ref())
            .map(|s| Cow::Owned(s.to_owned()));
        self.metadata.add_variable_with_doc(
            const_name.node.clone(),
            Some(const_name.span),
            doc_comment,
        );

        // check for a hash expression or a literal
        let value = match const_value {
            LiteralOr::Or(Spanned {
                node:
                    SysCall::System(System::Hash(Spanned {
                        node: Literal::String(str_to_hash),
                        ..
                    })),
                ..
            }) => Literal::Number(Number::Integer(crc_hash_signed(&str_to_hash), Unit::None)),
            LiteralOr::Or(Spanned { span, .. }) => {
                return Err(Error::Unknown(
                    "hash only supports string literals in this context.".into(),
                    Some(*span),
                ));
            }
            LiteralOr::Literal(Spanned { node, .. }) => node.clone(),
        };

        Ok(CompileLocation {
            location: scope.define_const(const_name.node.clone(), value, Some(const_name.span))?,
            temp_name: None,
        })
    }
}
