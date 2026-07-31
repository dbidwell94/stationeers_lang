use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn expression_function_invocation_with_invocation(
        &mut self,
        invoke_expr: &InvocationExpression<'a>,
        parent_scope: &mut VariableScope<'a, '_>,
        backup_registers: bool,
    ) -> Result<(), Error<'a>> {
        let InvocationExpression { name, arguments } = invoke_expr;

        if !self
            .function_meta
            .locations
            .contains_key(name.node.as_ref())
        {
            self.errors
                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
            return Ok(());
        }

        let Some(args) = self.function_meta.params.get(name.node.as_ref()) else {
            return Err(Error::UnknownIdentifier(name.node.clone(), name.span));
        };

        if args.len() != arguments.len() {
            self.errors
                .push(Error::AgrumentMismatch(name.node.clone(), name.span));
            return Ok(());
        }
        let mut stack = VariableScope::scoped(parent_scope);

        // Get the list of active registers (may or may not backup)
        let active_registers = stack.registers();

        // backup all used registers to the stack (unless this is for tuple return handling)
        if backup_registers {
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
        }
        for arg in arguments {
            match &arg.node {
                Expression::Literal(spanned_lit) => match &spanned_lit.node {
                    Literal::Number(num) => {
                        self.write_instruction(
                            Instruction::Push(Operand::Number((*num).into())),
                            Some(spanned_lit.span),
                        )?;
                    }
                    Literal::Boolean(b) => {
                        self.write_instruction(
                            Instruction::Push(Operand::Number(Number::from(*b).into())),
                            Some(spanned_lit.span),
                        )?;
                    }
                    _ => {}
                },
                Expression::Variable(var_name) => {
                    let loc = match stack.get_location_of(&var_name.node, Some(var_name.span)) {
                        Ok(l) => l,
                        Err(_) => {
                            self.errors.push(Error::UnknownIdentifier(
                                var_name.node.clone(),
                                var_name.span,
                            ));
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
                            self.errors.push(Error::Unknown(
                                "Device references not supported in function arguments".into(),
                                Some(var_name.span),
                            ));
                        }
                    }
                }
                _ => {
                    self.errors.push(Error::Unknown(
                        "Only literals and variables supported in function arguments".into(),
                        Some(arg.span),
                    ));
                }
            }
        }

        let Some(_location) = self.function_meta.locations.get(&name.node) else {
            self.errors
                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
            return Ok(());
        };

        self.write_instruction(
            Instruction::JumpAndLink(Operand::Label(name.node.clone())),
            Some(name.span),
        )?;

        // Pop the arguments off the stack (caller cleanup convention)
        // BUT: If the function returns a tuple, it saves SP in r15 and the caller
        // will restore SP with "move sp r15", which automatically cleans up everything.
        // So we only pop arguments for non-tuple-returning functions.
        let returns_tuple = self
            .function_meta
            .tuple_return_sizes
            .get(&name.node)
            .copied()
            .unwrap_or(0)
            > 0;

        if !returns_tuple {
            for _ in 0..arguments.len() {
                self.write_instruction(
                    Instruction::Pop(Operand::Register(VariableScope::TEMP_STACK_REGISTER)),
                    Some(name.span),
                )?;
            }
        }

        // pop all registers back (if they were backed up)
        if backup_registers {
            for register in active_registers.iter().rev() {
                self.write_instruction(
                    Instruction::Pop(Operand::Register(*register)),
                    Some(name.span),
                )?;
            }
        }

        Ok(())
    }

    pub(super) fn expression_function_invocation(
        &mut self,
        invoke_expr: Spanned<InvocationExpression<'a>>,
        parent_scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let InvocationExpression { name, arguments } = invoke_expr.node;

        if !self.function_meta.locations.contains_key(&name.node) {
            self.errors
                .push(Error::UnknownIdentifier(name.node.clone(), name.span));
            // Don't emit call, just pretend we did?
            // Actually, we should probably emit a dummy call or just skip to avoid logic errors
            // But if we skip, registers might be unbalanced if something expected a return.
            // For now, let's just return early.
            return Ok(());
        }

        let Some(args) = self.function_meta.params.get(&name.node) else {
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
            let arg_span = arg.span;
            // Use compile_operand to handle all expression types uniformly
            // This handles literals, variables, binaries, logicals, and importantly INVOCATIONS
            let (operand, temp_cleanup) = self.compile_operand(arg, &mut stack)?;

            // Convert operand to a pushable form
            match operand {
                Operand::Number(n) => {
                    self.write_instruction(Instruction::Push(Operand::Number(n)), Some(arg_span))?;
                }
                Operand::Register(reg) => {
                    self.write_instruction(
                        Instruction::Push(Operand::Register(reg)),
                        Some(arg_span),
                    )?;
                }
                Operand::Device(_) => {
                    return Err(Error::Unknown(
                        r#"Attempted to pass a device constant into a function argument. These values can be used without scope."#.into(),
                        Some(arg_span),
                    ));
                }
                Operand::Label(l) => {
                    self.write_instruction(Instruction::Push(Operand::Label(l)), Some(arg_span))?;
                }
                Operand::LogicType(l) => {
                    self.write_instruction(
                        Instruction::Push(Operand::LogicType(l)),
                        Some(arg_span),
                    )?;
                }
                Operand::StackPointer => {
                    self.write_instruction(
                        Instruction::Push(Operand::StackPointer),
                        Some(arg_span),
                    )?;
                }
                Operand::ReturnAddress => {
                    self.write_instruction(
                        Instruction::Push(Operand::ReturnAddress),
                        Some(arg_span),
                    )?;
                }
            }

            // Clean up any temporary variables created during operand compilation
            if let Some(temp_name) = temp_cleanup {
                stack.free_temp(temp_name, None)?;
            }
        }

        // jump to the function and store current line in ra
        self.write_instruction(
            Instruction::JumpAndLink(Operand::Label(name.node)),
            Some(name.span),
        )?;

        // cleanup spilled temporary variables
        let total_stack_usage = stack.stack_offset();
        let saved_regs_count = active_registers.len() as u16;

        if total_stack_usage > saved_regs_count {
            let spill_amount = total_stack_usage - saved_regs_count;
            self.write_instruction(
                Instruction::Sub(
                    Operand::StackPointer,
                    Operand::StackPointer,
                    Operand::Number(spill_amount.into()),
                ),
                Some(name.span),
            )?;
        }

        // restore the registers in reverse order from the stack, now using `pop`
        for register in active_registers.iter().rev() {
            self.write_instruction(
                Instruction::Pop(Operand::Register(*register)),
                Some(name.span),
            )?;
        }

        Ok(())
    }

    pub(super) fn expression_return(
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
                Expression::Tuple(tuple_expr) => {
                    let span = expr.span;
                    let tuple_elements = tuple_expr.node;
                    let tuple_size = tuple_elements.len();

                    // Push each tuple element onto the stack using compile_operand
                    for element in tuple_elements.into_iter() {
                        let (push_operand, cleanup) = self.compile_operand(element, scope)?;

                        self.write_instruction(Instruction::Push(push_operand), Some(span))?;

                        // Don't track the push in the scope's stack offset because these values
                        // are being returned to the caller, not allocated in this block's scope.
                        // They will be left on the stack when we return.

                        if let Some(temp_name) = cleanup {
                            scope.free_temp(temp_name, Some(span))?;
                        }
                    }

                    // Load the saved SP from stack and move to r15 for caller's stack unwinding
                    if let Some(sp_var_name) = &self.function_meta.sp_backup_var {
                        let sp_var_loc = scope.get_location_of(sp_var_name, Some(span))?;

                        if let VariableLocation::Stack(offset) = sp_var_loc {
                            // Calculate address of saved SP, accounting for tuple values just pushed
                            let adjusted_offset = offset + tuple_size as u16;
                            self.write_instruction(
                                Instruction::Sub(
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    Operand::StackPointer,
                                    Operand::Number(adjusted_offset.into()),
                                ),
                                Some(span),
                            )?;

                            // Load saved SP value
                            self.write_instruction(
                                Instruction::Get(
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    Operand::Device(Cow::from("db")),
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                ),
                                Some(span),
                            )?;

                            // Move to r15 for caller
                            self.write_instruction(
                                Instruction::Move(
                                    Operand::Register(VariableScope::RETURN_REGISTER),
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                ),
                                Some(span),
                            )?;
                        }
                    }

                    // Record the tuple return size for validation at call sites
                    if let Some(func_name) = &self.function_meta.current_name {
                        self.function_meta
                            .tuple_return_sizes
                            .insert(func_name.clone(), tuple_size);
                    }

                    // Track tuple size for epilogue cleanup
                    self.function_meta.tuple_return_size = tuple_size as u16;
                }
                _ => {
                    return Err(Error::Unknown(
                        format!("Unsupported `return` statement: {:?}", expr),
                        None,
                    ));
                }
            }
        }

        if let Some(label) = &self.function_meta.return_label {
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
    pub(super) fn expression_function(
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

        // Track the function definition in metadata
        let param_names: Vec<Cow<'a, str>> = arguments.iter().map(|a| a.node.clone()).collect();
        let doc_comment = self
            .parser
            .get_declaration_doc(name.node.as_ref())
            .map(Cow::Owned);
        self.metadata.add_function_with_doc(
            name.node.clone(),
            param_names,
            Some(name.span),
            doc_comment,
        );

        if self.function_meta.locations.contains_key(&name.node) {
            self.errors
                .push(Error::DuplicateIdentifier(name.node.clone(), name.span));
            // Fallthrough to allow compiling the body anyway?
            // It might be useful to check body for errors.
        }

        self.function_meta.params.insert(
            name.node.clone(),
            arguments.iter().map(|a| a.node.clone()).collect(),
        );

        // Set the current function being compiled
        self.function_meta.current_name = Some(name.node.clone());

        // Declare the function as a line identifier
        self.write_instruction(Instruction::LabelDef(name.node.clone()), Some(span))?;

        self.function_meta
            .locations
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

        // Save the caller's stack pointer FIRST (before any pushes modify it)
        // This is crucial for proper stack unwinding in tuple returns
        let sp_backup_name = self.next_temp_name();
        block_scope.add_variable(
            sp_backup_name.clone(),
            LocationRequest::Stack,
            Some(name.span),
        )?;
        self.write_instruction(Instruction::Push(Operand::StackPointer), Some(span))?;
        self.function_meta.sp_backup_var = Some(sp_backup_name);
        self.function_meta.sp_saved = true;

        // Generate return label name and track it before pushing ra
        let return_label = self.next_label_name();
        let prev_return_label = self
            .function_meta
            .return_label
            .replace(return_label.clone());

        block_scope.add_variable(
            return_label.clone(),
            LocationRequest::Stack,
            Some(name.span),
        )?;

        self.write_instruction(Instruction::Push(Operand::ReturnAddress), Some(span))?;

        for expr in body.node.0 {
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

        self.function_meta.return_label = prev_return_label;

        // Write the return label and epilogue
        self.write_instruction(Instruction::LabelDef(return_label.clone()), Some(span))?;

        // Handle stack cleanup based on whether this is a tuple-returning function
        let is_tuple_return = self.function_meta.tuple_return_size > 0;

        // For tuple returns, account for tuple values pushed onto the stack
        let adjusted_ra_offset = if is_tuple_return {
            ra_stack_offset + self.function_meta.tuple_return_size
        } else {
            ra_stack_offset
        };

        // Load return address from stack
        if adjusted_ra_offset == 1 && !is_tuple_return {
            // Simple case: RA is at top, and we're not returning a tuple
            // Just pop ra, then pop sp to restore
            self.write_instruction(Instruction::Pop(Operand::ReturnAddress), Some(span))?;
            self.write_instruction(Instruction::Pop(Operand::StackPointer), Some(span))?;
        } else {
            // RA is deeper in stack, or we're returning a tuple
            // Load ra from offset
            self.write_instruction(
                Instruction::Sub(
                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    Operand::StackPointer,
                    Operand::Number(adjusted_ra_offset.into()),
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

            if !is_tuple_return {
                // Non-tuple return: restore SP from saved value to clean up.
                // sp was pushed BEFORE ra, so it's one slot deeper:
                // stack layout: ... | saved_sp | saved_ra | locals... | <- sp
                // saved_ra is at sp - adjusted_ra_offset
                // saved_sp is at sp - (adjusted_ra_offset + 1)
                let sp_offset = adjusted_ra_offset + 1;
                self.write_instruction(
                    Instruction::Sub(
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                        Operand::StackPointer,
                        Operand::Number(sp_offset.into()),
                    ),
                    Some(span),
                )?;

                self.write_instruction(
                    Instruction::Get(
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                        Operand::Device(Cow::from("db")),
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    ),
                    Some(span),
                )?;

                self.write_instruction(
                    Instruction::Move(
                        Operand::StackPointer,
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    ),
                    Some(span),
                )?;
            }
            // else: Tuple return - leave tuple values on stack for caller to pop
        }

        self.write_instruction(Instruction::Jump(Operand::ReturnAddress), Some(span))?;

        // Reset the flags for the next function
        self.function_meta.tuple_return_size = 0;
        self.function_meta.sp_saved = false;
        self.function_meta.sp_backup_var = None;
        self.function_meta.current_name = None;
        Ok(())
    }
}
