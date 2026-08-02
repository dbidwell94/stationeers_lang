use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn resolve_device(
        &mut self,
        expr: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        // If it's a direct variable reference, check if it's a known device alias first
        if let Expression::Variable(ref name) = expr.node
            && let Some(device_id) = self.devices.get(&name.node)
        {
            // Track this device reference in metadata (for tooltips on all usages, not just declaration)
            let doc_comment = self
                .declaration_docs
                .get(name.node.as_ref())
                .map(|s| Cow::Owned(s.to_owned()));

            self.metadata
                .add_variable_with_doc(name.node.clone(), Some(expr.span), doc_comment);

            return Ok((Operand::Device(device_id.to_string().into()), None));
        }

        // Otherwise, compile it as an operand (e.g. it might be a register holding a device hash/id)
        self.compile_operand(expr, scope)
    }

    pub(super) fn emit_variable_assignment(
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

    pub(super) fn expression_declaration(
        &mut self,
        var_name: Spanned<Cow<'a, str>>,
        expr: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
        let name_str = var_name.node;
        let name_span = var_name.span;

        // Track the variable in metadata
        let doc_comment = self
            .declaration_docs
            .get(name_str.as_ref())
            .map(|s| Cow::Owned(s.to_owned()));

        self.metadata
            .add_variable_with_doc(name_str.clone(), Some(name_span), doc_comment);

        // optimization. Check for a negated numeric literal (including nested negations)
        // e.g., -5, -(-5), -(-(5)), etc.
        if let Some(num) = self.try_fold_negation(&expr.node) {
            let loc =
                scope.add_variable(name_str.clone(), LocationRequest::Persist, Some(name_span))?;

            self.emit_variable_assignment(&loc, Operand::Number(num.into()))?;
            return Ok(Some(CompileLocation {
                location: loc,
                temp_name: None,
            }));
        }

        let (loc, temp_name) = match &expr.node {
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
                let sys_call = &spanned_call.node;
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
                    VariableLocation::Constant(Literal::Number(num)) => Operand::Number(num.into()),
                    VariableLocation::Constant(Literal::Boolean(b)) => {
                        Operand::Number(Number::from(b).into())
                    }
                    VariableLocation::Constant(Literal::String(s)) => {
                        // String constants can be used in expressions like `let x = STRINGCONST;`
                        Operand::LogicType(s)
                    }
                    VariableLocation::Device(_) => unreachable!(),
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
                    inner,
                    scope,
                );
            }
            Expression::MemberAccess(access) => {
                // Compile the member access (load instruction)
                let result = self.expression(
                    &Spanned {
                        node: Expression::MemberAccess(access.clone()),
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
                let res = self.expression_ternary(&ternary.node, scope)?;
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
            Expression::Negation(_) => {
                // Use try_fold_negation to see if this is a constant folded negation
                if let Some(num) = self.try_fold_negation(&expr.node) {
                    let loc = scope.add_variable(
                        name_str.clone(),
                        LocationRequest::Persist,
                        Some(name_span),
                    )?;
                    self.emit_variable_assignment(&loc, Operand::Number(num.into()))?;
                    return Ok(Some(CompileLocation {
                        location: loc,
                        temp_name: None,
                    }));
                }

                // Otherwise, compile the negation expression
                let result = self.expression(expr, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                if let Some(res) = result {
                    // Move result from temp to new persistent variable
                    let result_reg = self.resolve_register(&res.location)?;
                    self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

                    // Free the temp result
                    if let Some(name) = res.temp_name {
                        scope.free_temp(name, None)?;
                    }
                } else {
                    return Err(Error::Unknown(
                        format!("`{name_str}` negation expression did not produce a value"),
                        Some(name_span),
                    ));
                }
                (var_loc, None)
            }
            Expression::BitwiseNot(_) => {
                // Compile the bitwise NOT expression
                let result = self.expression(expr, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                if let Some(res) = result {
                    // Move result from temp to new persistent variable
                    let result_reg = self.resolve_register(&res.location)?;
                    self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

                    // Free the temp result
                    if let Some(name) = res.temp_name {
                        scope.free_temp(name, None)?;
                    }
                } else {
                    return Err(Error::Unknown(
                        format!("`{name_str}` bitwise NOT expression did not produce a value"),
                        Some(name_span),
                    ));
                }
                (var_loc, None)
            }
            Expression::IndexAccess(_) => {
                // Compile the index access expression
                let result = self.expression(expr, scope)?;
                let var_loc = scope.add_variable(
                    name_str.clone(),
                    LocationRequest::Persist,
                    Some(name_span),
                )?;

                if let Some(res) = result {
                    // Move result from temp to new persistent variable
                    let result_reg = self.resolve_register(&res.location)?;
                    self.emit_variable_assignment(&var_loc, Operand::Register(result_reg))?;

                    // Free the temp result
                    if let Some(name) = res.temp_name {
                        scope.free_temp(name, None)?;
                    }
                } else {
                    return Err(Error::Unknown(
                        format!("`{name_str}` index access expression did not produce a value"),
                        Some(name_span),
                    ));
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

    pub(super) fn expression_assignment(
        &mut self,
        expr: &AssignmentExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let AssignmentExpression {
            assignee,
            expression,
        } = expr;

        let expr_span = expression.span;

        match &assignee.node {
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

                let (val, cleanup) = self.compile_operand(expression, scope)?;

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
                        return Err(Error::ConstAssignment(identifier.node.clone(), identifier.span));
                    }
                    VariableLocation::Device(_) => {
                        return Err(Error::DeviceAssignment(identifier.node.clone(), identifier.span));
                    }
                }

                if let Some(name) = cleanup {
                    scope.free_temp(name, None)?;
                }
            }
            Expression::MemberAccess(access) => {
                // Set instruction: s device member value
                let MemberAccessExpression { object, member } = &access.node;

                let (device, dev_cleanup) = self.resolve_device(object, scope)?;
                let (val, val_cleanup) = self.compile_operand(expression, scope)?;

                self.write_instruction(
                    Instruction::Store(device, Operand::LogicType(member.node.clone()), val),
                    Some(member.span),
                )?;

                if let Some(c) = dev_cleanup {
                    scope.free_temp(c, None)?;
                }
                if let Some(c) = val_cleanup {
                    scope.free_temp(c, None)?;
                }
            }
            Expression::IndexAccess(access) => {
                // Put instruction: put device address value
                let IndexAccessExpression { object, index } = &access.node;

                let (device, dev_cleanup) = self.resolve_device(object, scope)?;

                // Check if device is "db" (not allowed)
                if let Operand::Device(ref dev_str) = device
                    && dev_str.as_ref() == "db"
                {
                    return Err(Error::OperationNotSupported(
                        "Direct stack access on 'db' is not yet supported".to_string(),
                        assignee.span,
                    ));
                }

                let ((addr, addr_cleanup), (val, val_cleanup)) =
                    compile_operands!(self, (index, expression), scope);

                self.write_instruction(Instruction::Put(device, addr, val), Some(assignee.span))?;

                if let Some(c) = dev_cleanup {
                    scope.free_temp(c, None)?;
                }
                if let Some(c) = addr_cleanup {
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

    pub(super) fn expression_device(
        &mut self,
        expr: &DeviceDeclarationExpression<'a>,
    ) -> Result<(), Error<'a>> {
        // Track the device declaration in metadata
        let doc_comment = self
            .declaration_docs
            .get(expr.name.node.as_ref())
            .map(|s| Cow::Owned(s.to_owned()));
        self.metadata.add_variable_with_doc(
            expr.name.node.clone(),
            Some(expr.name.span),
            doc_comment,
        );

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
        self.devices.insert(expr.name.node.clone(), expr.device.node.clone());

        Ok(())
    }

    pub(super) fn resolve_register(&self, loc: &VariableLocation) -> Result<u8, Error<'a>> {
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
    pub(super) fn compile_operand(
        &mut self,
        expr: &Spanned<Expression<'a>>,
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
            VariableLocation::Device(d) => {
                let device = match d {
                    DeviceType::Housing => "db".to_owned(),
                    DeviceType::Pin(pin_id) => format!("d{}", pin_id),
                    DeviceType::Reference(ref_id) => format!("${ref_id:x}"),
                };

                Ok((Operand::Device(Cow::Owned(device)), None))
            }
        }
    }

    /// Prevents clobbering of the return-register in multi-operand expressions
    /// by moving the return register of a parsed operand into a new temporary register.
    pub(super) fn prevent_return_register_clobbering(
        &mut self,
        operand: Operand<'a>,
        cleanup: Option<Cow<'a, str>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        // If opr result landed in RETURN_REGISTER, spill it to a fresh temp before
        // compiling the next operand, which may also emit a syscall and overwrite that register.
        if !matches!(operand, Operand::Register(r) if r == VariableScope::RETURN_REGISTER) {
            return Ok((operand, cleanup));
        }
        let spill_name = self.next_temp_name();
        let spill_loc = scope.add_variable(spill_name.clone(), LocationRequest::Temp, None)?;
        let spill_reg = self.resolve_register(&spill_loc)?;
        self.write_instruction(
            Instruction::Move(Operand::Register(spill_reg), operand),
            None,
        )?;
        if let Some(name) = cleanup {
            scope.free_temp(name, None)?;
        }
        Ok((Operand::Register(spill_reg), Some(spill_name)))
    }

    pub(super) fn compile_literal_or_variable(
        &mut self,
        val: LiteralOrVariable<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        let (span, expr) = match val {
            LiteralOrVariable::Literal(l) => (l.span, Expression::Literal(l)),
            LiteralOrVariable::Variable(v) => (v.span, Expression::Variable(v)),
        };
        self.compile_operand(&Spanned { node: expr, span }, scope)
    }

    /// Compiles an expression and validates that it must result in a constant string value.
    /// Useful for syscall parameters that require string constants (e.g., logic type names).
    ///
    /// Returns the string value if successful, or an error if:
    /// - The expression evaluates to a register (runtime value)
    /// - The expression is not a string
    pub(super) fn compile_const_string(
        &mut self,
        expr: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
        span: Span,
    ) -> Result<Cow<'a, str>, Error<'a>> {
        let (operand, _cleanup) = self.compile_operand(expr, scope)?;

        match operand {
            Operand::LogicType(s) => Ok(s),
            Operand::Register(_) => Err(Error::AgrumentMismatch(
                "String argument must be a constant, not a runtime value".into(),
                span,
            )),
            _ => Err(Error::AgrumentMismatch(
                "Argument must be a string constant".into(),
                span,
            )),
        }
    }

    /// Recursively fold negations of numeric literals, e.g., -5 => 5, -(-5) => 5
    pub(super) fn try_fold_negation(&self, expr: &Expression) -> Option<Number> {
        match expr {
            // Base case: plain number literal
            Expression::Literal(lit) => {
                if let Literal::Number(n) = lit.node {
                    Some(n)
                } else {
                    None
                }
            }
            // Recursive case: negation of something foldable
            Expression::Negation(inner) => self.try_fold_negation(&inner.node).map(|n| -n),
            // Parentheses just pass through
            Expression::Priority(inner) => self.try_fold_negation(&inner.node),
            _ => None,
        }
    }

    pub(super) fn expression_binary(
        &mut self,
        expr: &Spanned<BinaryExpression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        fn fold_binary_expression<'a>(
            expr: &BinaryExpression<'a>,
            scope: &VariableScope<'a, '_>,
        ) -> Option<Number> {
            fn number_to_i64(n: Number) -> Option<i64> {
                match n {
                    Number::Integer(i, _) => i64::try_from(i).ok(),
                    Number::Decimal(d, _) => {
                        // Convert decimal to i64 by truncating
                        let int_part = d.trunc();
                        i64::try_from(int_part.mantissa() / 10_i128.pow(int_part.scale())).ok()
                    }
                }
            }

            fn i64_to_number(i: i64) -> Number {
                Number::Integer(i as i128, Unit::None)
            }

            let (lhs, rhs) = match &expr {
                BinaryExpression::Add(l, r)
                | BinaryExpression::Subtract(l, r)
                | BinaryExpression::Multiply(l, r)
                | BinaryExpression::Divide(l, r)
                | BinaryExpression::Exponent(l, r)
                | BinaryExpression::Modulo(l, r)
                | BinaryExpression::BitwiseAnd(l, r)
                | BinaryExpression::BitwiseOr(l, r)
                | BinaryExpression::BitwiseXor(l, r)
                | BinaryExpression::LeftShift(l, r)
                | BinaryExpression::RightShiftArithmetic(l, r)
                | BinaryExpression::RightShiftLogical(l, r) => {
                    (fold_expression(l, scope)?, fold_expression(r, scope)?)
                }
            };

            match expr {
                BinaryExpression::Add(..) => Some(lhs + rhs),
                BinaryExpression::Subtract(..) => Some(lhs - rhs),
                BinaryExpression::Multiply(..) => Some(lhs * rhs),
                BinaryExpression::Divide(..) => Some(lhs / rhs), // Watch out for div by zero panics!
                BinaryExpression::Modulo(..) => Some(lhs % rhs),
                BinaryExpression::BitwiseAnd(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int & rhs_int))
                }
                BinaryExpression::BitwiseOr(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int | rhs_int))
                }
                BinaryExpression::BitwiseXor(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int ^ rhs_int))
                }
                BinaryExpression::LeftShift(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int << rhs_int))
                }
                BinaryExpression::RightShiftArithmetic(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int >> rhs_int))
                }
                BinaryExpression::RightShiftLogical(..) => {
                    let lhs_int = number_to_i64(lhs)?;
                    let rhs_int = number_to_i64(rhs)?;
                    Some(i64_to_number(lhs_int >> rhs_int))
                }
                _ => None, // Exponent not handled in compile-time folding
            }
        }

        fn fold_expression<'a>(
            expr: &Expression<'a>,
            scope: &VariableScope<'a, '_>,
        ) -> Option<Number> {
            match expr {
                // 1. Base Case: It's already a number
                Expression::Literal(lit) => match lit.node {
                    Literal::Number(n) => Some(n),
                    _ => None,
                },

                // 2. Handle Parentheses: Just recurse deeper
                Expression::Priority(inner) => fold_expression(&inner.node, scope),

                // 3. Handle Negation: Recurse, then negate
                Expression::Negation(inner) => {
                    let val = fold_expression(&inner.node, scope)?;
                    Some(-val) // Requires impl Neg for Number
                }

                // 4. Handle Binary Ops: Recurse BOTH sides, then combine
                Expression::Binary(bin) => fold_binary_expression(&bin.node, scope),

                // 5. Handle Variable Reference: Check if it's a const
                Expression::Variable(var_id) => {
                    if let Ok(var_loc) = scope.get_location_of(var_id, None)
                        && let VariableLocation::Constant(Literal::Number(num)) = var_loc
                    {
                        return Some(num);
                    }
                    None
                }

                // 6. Handle hash() syscall - evaluates to a constant at compile time
                Expression::Syscall(Spanned {
                    node:
                        SysCall::System(System::Hash(Spanned {
                            node: Literal::String(str_to_hash),
                            ..
                        })),
                    ..
                }) => Some(Number::Integer(crc_hash_signed(str_to_hash), Unit::None)),

                // 7. Handle hash() macro as invocation - evaluates to a constant at compile time
                Expression::Invocation(inv) => {
                    if inv.node.name.node == "hash"
                        && inv.node.arguments.len() == 1
                        && let Expression::Literal(Spanned {
                            node: Literal::String(str_to_hash),
                            ..
                        }) = &inv.node.arguments[0].node
                    {
                        // hash() takes a string literal and returns a signed integer
                        return Some(Number::Integer(crc_hash_signed(str_to_hash), Unit::None));
                    }
                    None
                }

                // 8. Anything else cannot be compile-time folded
                _ => None,
            }
        }

        if let Some(const_lit) = fold_binary_expression(&expr.node, scope) {
            return Ok(CompileLocation {
                location: VariableLocation::Constant(Literal::Number(const_lit)),
                temp_name: None,
            });
        };

        #[allow(clippy::type_complexity)]
        let (op_instr, left_expr, right_expr): (
            fn(Operand<'a>, Operand<'a>, Operand<'a>) -> Instruction<'a>,
            &Box<Spanned<Expression<'a>>>,
            &Box<Spanned<Expression<'a>>>,
        ) = match &expr.node {
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
            BinaryExpression::BitwiseAnd(l, r) => {
                (|into, lhs, rhs| Instruction::And(into, lhs, rhs), l, r)
            }
            BinaryExpression::BitwiseOr(l, r) => {
                (|into, lhs, rhs| Instruction::Or(into, lhs, rhs), l, r)
            }
            BinaryExpression::BitwiseXor(l, r) => {
                (|into, lhs, rhs| Instruction::Xor(into, lhs, rhs), l, r)
            }
            BinaryExpression::LeftShift(l, r) => {
                (|into, lhs, rhs| Instruction::Sll(into, lhs, rhs), l, r)
            }
            BinaryExpression::RightShiftArithmetic(l, r) => {
                (|into, lhs, rhs| Instruction::Sra(into, lhs, rhs), l, r)
            }
            BinaryExpression::RightShiftLogical(l, r) => {
                (|into, lhs, rhs| Instruction::Srl(into, lhs, rhs), l, r)
            }
        };

        let span = Self::merge_spans(left_expr.span, right_expr.span);

        // Compile LHS
        let (lhs_tup, rhs_tup) = compile_operands!(self, (&left_expr, &right_expr), scope);

        // Allocate result register
        let result_name = self.next_temp_name();
        let result_loc = scope.add_variable(result_name.clone(), LocationRequest::Temp, None)?;
        let result_reg = self.resolve_register(&result_loc)?;

        // Emit instruction: op result lhs rhs
        self.write_instruction(
            op_instr(Operand::Register(result_reg), lhs_tup.0, rhs_tup.0),
            Some(span),
        )?;

        // Clean up operand temps
        Self::cleanup_temps(scope, &[lhs_tup.1, rhs_tup.1])?;

        Ok(CompileLocation {
            location: result_loc,
            temp_name: Some(result_name),
        })
    }

    pub(super) fn expression_logical(
        &mut self,
        expr: &Spanned<LogicalExpression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<CompileLocation<'a>, Error<'a>> {
        match &expr.node {
            LogicalExpression::Not(inner) => {
                let span = inner.span;
                let (inner_str, cleanup) = self.compile_operand(inner, scope)?;

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
                    &Box<Spanned<Expression<'a>>>,
                    &Box<Spanned<Expression<'a>>>,
                ) = match &expr.node {
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

                let span = Self::merge_spans(left_expr.span, right_expr.span);

                let ((lhs, lhs_cleanup), (rhs, rhs_cleanup)) =
                    compile_operands!(self, (&left_expr, &right_expr), scope);

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
                Self::cleanup_temps(scope, &[lhs_cleanup, rhs_cleanup])?;

                Ok(CompileLocation {
                    location: result_loc,
                    temp_name: Some(result_name),
                })
            }
        }
    }
}
