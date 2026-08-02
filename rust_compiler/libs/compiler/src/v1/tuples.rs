use super::*;

impl<'a> Compiler<'a> {
    /// Helper: Validate tuple size from function return
    pub(super) fn validate_tuple_function_size(
        &mut self,
        func_name: &str,
        expected_count: usize,
        span: Span,
    ) {
        if let Some(&actual_size) = self.function_meta.tuple_return_sizes.get(func_name)
            && actual_size != expected_count
        {
            self.errors
                .push(Error::TupleSizeMismatch(actual_size, expected_count, span));
        }
    }

    /// Helper: Pop tuple values from stack into variables (for function returns)
    /// Variables are popped in reverse order (LIFO)
    pub(super) fn pop_tuple_values(
        &mut self,
        var_locations: Vec<(Option<VariableLocation>, Span)>,
    ) -> Result<(), Error<'a>> {
        for (var_loc_opt, span) in var_locations.into_iter().rev() {
            if let Some(var_location) = var_loc_opt {
                match var_location {
                    VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                        self.write_instruction(
                            Instruction::Pop(Operand::Register(reg)),
                            Some(span),
                        )?;
                    }
                    VariableLocation::Stack(offset) => {
                        // Pop into temp register, then write to stack
                        self.write_instruction(
                            Instruction::Pop(Operand::Register(VariableScope::TEMP_STACK_REGISTER)),
                            Some(span),
                        )?;

                        self.write_instruction(
                            Instruction::Sub(
                                Operand::Register(0),
                                Operand::StackPointer,
                                Operand::Number(offset.into()),
                            ),
                            Some(span),
                        )?;

                        self.write_instruction(
                            Instruction::Put(
                                Operand::Device(Cow::from("db")),
                                Operand::Register(0),
                                Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                            ),
                            Some(span),
                        )?;
                    }
                    VariableLocation::Constant(_) => {
                        return Err(Error::ConstAssignment(Cow::from("tuple element"), span));
                    }
                    VariableLocation::Device(_) => {
                        return Err(Error::DeviceAssignment(Cow::from("tuple element"), span));
                    }
                }
            } else {
                // Underscore: pop into temp register to discard
                self.write_instruction(
                    Instruction::Pop(Operand::Register(VariableScope::TEMP_STACK_REGISTER)),
                    Some(span),
                )?;
            }
        }

        // Restore stack pointer from r15 to clean up remaining tuple values
        // (r15 contains the caller's SP from before the function was called)
        self.write_instruction(
            Instruction::Move(
                Operand::StackPointer,
                Operand::Register(VariableScope::RETURN_REGISTER),
            ),
            None,
        )?;

        Ok(())
    }

    pub(super) fn expression_tuple_declaration(
        &mut self,
        tuple_decl: &TupleDeclarationExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let TupleDeclarationExpression { names, value } = tuple_decl;

        // Track each variable in the tuple declaration
        // Get doc for the first variable
        let first_var_name = names
            .iter()
            .find(|n| n.node.as_ref() != "_")
            .map(|n| n.node.to_string());
        let doc_comment = first_var_name.as_ref().and_then(|name| {
            self.declaration_docs
                .get(name)
                .map(|s| Cow::Owned(s.to_owned()))
        });

        for (i, name_spanned) in names.iter().enumerate() {
            if name_spanned.node.as_ref() != "_" {
                // Only attach doc comment to the first variable
                let comment = if i == 0 { doc_comment.clone() } else { None };
                self.metadata.add_variable_with_doc(
                    name_spanned.node.clone(),
                    Some(name_spanned.span),
                    comment,
                );
            }
        }

        match &value.node {
            Expression::Invocation(invoke_expr) => {
                // Execute the function call - tuple values will be on the stack
                self.expression_function_invocation_with_invocation(invoke_expr, scope, false)?;

                // Validate tuple return size matches the declaration
                self.validate_tuple_function_size(
                    &invoke_expr.node.name.node,
                    names.len(),
                    value.span,
                );

                // Allocate variables and collect their locations
                let var_locations: Vec<_> = names
                    .iter()
                    .map(|name_spanned| {
                        if name_spanned.node.as_ref() == "_" {
                            Ok((None, name_spanned.span))
                        } else {
                            let var_location = scope.add_variable(
                                name_spanned.node.clone(),
                                LocationRequest::Persist,
                                Some(name_spanned.span),
                            )?;
                            Ok((Some(var_location), name_spanned.span))
                        }
                    })
                    .collect::<Result<_, Error<'a>>>()?;

                // Pop tuple values from stack into variables
                self.pop_tuple_values(var_locations)?;
            }
            Expression::Tuple(tuple_expr) => {
                // Direct tuple literal: (value1, value2, ...)
                let tuple_elements = &tuple_expr.node;

                // Validate tuple size matches names
                if tuple_elements.len() != names.len() {
                    return Err(Error::TupleSizeMismatch(
                        names.len(),
                        tuple_elements.len(),
                        value.span,
                    ));
                }

                // Compile each element and assign to corresponding variable
                for (name_spanned, element) in names.iter().zip(tuple_elements) {
                    // Skip underscores
                    if name_spanned.node.as_ref() == "_" {
                        continue;
                    }

                    // Add variable to scope
                    let var_location = scope.add_variable(
                        name_spanned.node.clone(),
                        LocationRequest::Persist,
                        Some(name_spanned.span),
                    )?;

                    // Compile the element expression - use compile_operand to handle all expression types
                    let (value_operand, cleanup) = self.compile_operand(element, scope)?;
                    self.emit_variable_assignment(&var_location, value_operand)?;

                    // Clean up any temporary registers used for complex expressions
                    if let Some(temp_name) = cleanup {
                        scope.free_temp(temp_name, None)?;
                    }
                }
            }
            _ => {
                return Err(Error::Unknown(
                    "Tuple declaration only supports function invocations or tuple literals as RHS"
                        .into(),
                    Some(value.span),
                ));
            }
        }

        Ok(())
    }

    pub(super) fn expression_tuple_assignment(
        &mut self,
        tuple_assign: &TupleAssignmentExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let TupleAssignmentExpression { names, value } = tuple_assign;

        match &value.node {
            Expression::Invocation(invoke_expr) => {
                // Execute the function call - tuple values will be on the stack
                self.expression_function_invocation_with_invocation(invoke_expr, scope, false)?;

                // Validate tuple return size matches the assignment
                self.validate_tuple_function_size(
                    &invoke_expr.node.name.node,
                    names.len(),
                    value.span,
                );

                // Look up existing variable locations
                let var_locations: Vec<_> = names
                    .iter()
                    .map(|name_spanned| {
                        if name_spanned.node.as_ref() == "_" {
                            Ok((None, name_spanned.span))
                        } else {
                            let var_location = scope
                                .get_location_of(&name_spanned.node, Some(name_spanned.span))
                                .unwrap_or_else(|_| {
                                    self.errors.push(Error::UnknownIdentifier(
                                        name_spanned.node.clone(),
                                        name_spanned.span,
                                    ));
                                    VariableLocation::Temporary(0)
                                });
                            Ok((Some(var_location), name_spanned.span))
                        }
                    })
                    .collect::<Result<_, Error<'a>>>()?;

                // Pop tuple values from stack into variables
                self.pop_tuple_values(var_locations)?;
            }
            Expression::Tuple(tuple_expr) => {
                // Direct tuple literal: (value1, value2, ...)
                let tuple_elements = &tuple_expr.node;

                // Validate tuple size matches names
                if tuple_elements.len() != names.len() {
                    return Err(Error::TupleSizeMismatch(
                        tuple_elements.len(),
                        names.len(),
                        value.span,
                    ));
                }

                // Compile each element and assign to corresponding variable
                for (name_spanned, element) in names.iter().zip(tuple_elements) {
                    // Skip underscores
                    if name_spanned.node.as_ref() == "_" {
                        continue;
                    }

                    // Get the existing variable location
                    let var_location =
                        match scope.get_location_of(&name_spanned.node, Some(name_spanned.span)) {
                            Ok(l) => l,
                            Err(_) => {
                                self.errors.push(Error::UnknownIdentifier(
                                    name_spanned.node.clone(),
                                    name_spanned.span,
                                ));
                                VariableLocation::Temporary(0)
                            }
                        };

                    // Compile the element expression - use compile_operand to handle all expression types
                    let (value_operand, cleanup) = self.compile_operand(element, scope)?;

                    // Assign the compiled value to the target variable location
                    match &var_location {
                        VariableLocation::Temporary(reg) | VariableLocation::Persistant(reg) => {
                            self.write_instruction(
                                Instruction::Move(Operand::Register(*reg), value_operand),
                                Some(name_spanned.span),
                            )?;
                        }
                        VariableLocation::Stack(offset) => {
                            self.write_instruction(
                                Instruction::Sub(
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    Operand::StackPointer,
                                    Operand::Number((*offset).into()),
                                ),
                                Some(name_spanned.span),
                            )?;

                            self.write_instruction(
                                Instruction::Put(
                                    Operand::Device(Cow::from("db")),
                                    Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                                    value_operand,
                                ),
                                Some(name_spanned.span),
                            )?;
                        }
                        VariableLocation::Constant(_) => {
                            return Err(Error::ConstAssignment(
                                name_spanned.node.clone(),
                                name_spanned.span,
                            ));
                        }
                        VariableLocation::Device(_) => {
                            return Err(Error::DeviceAssignment(
                                name_spanned.node.clone(),
                                name_spanned.span,
                            ));
                        }
                    }

                    // Clean up any temporary registers used for complex expressions
                    if let Some(temp_name) = cleanup {
                        scope.free_temp(temp_name, None)?;
                    }
                }
            }
            _ => {
                return Err(Error::Unknown(
                    "Tuple assignment only supports function invocations or tuple literals as RHS"
                        .into(),
                    Some(value.span),
                ));
            }
        }

        Ok(())
    }
}
