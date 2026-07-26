use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn expression_if(
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

    pub(super) fn expression_loop(
        &mut self,
        expr: LoopExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Track the stack depth before entering the loop body
        let entry_stack_depth = scope.total_stack_depth();

        // Push labels and stack depth to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone(), entry_stack_depth));

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

    pub(super) fn expression_while(
        &mut self,
        expr: WhileExpression<'a>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        let start_label = self.next_label_name();
        let end_label = self.next_label_name();

        // Track the stack depth before entering the loop body
        let entry_stack_depth = scope.total_stack_depth();

        // Push labels and stack depth to stack for 'break' and 'continue'
        self.loop_stack
            .push((start_label.clone(), end_label.clone(), entry_stack_depth));

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

    pub(super) fn expression_break(
        &mut self,
        span: Span,
        scope: &VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        if let Some((_, end_label, entry_depth)) = self.loop_stack.last() {
            let end_label = end_label.clone();
            let entry_depth = *entry_depth;

            // Calculate how much stack to clean up: current depth - depth at loop entry
            let current_depth = scope.total_stack_depth();
            let cleanup_amount = current_depth.saturating_sub(entry_depth);

            // Clean up stack before jumping out of the loop
            if cleanup_amount > 0 {
                self.write_instruction(
                    Instruction::Sub(
                        Operand::StackPointer,
                        Operand::StackPointer,
                        Operand::Number(cleanup_amount.into()),
                    ),
                    Some(span),
                )?;
            }
            self.write_instruction(Instruction::Jump(Operand::Label(end_label)), Some(span))?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Break statement outside of loop".into(),
                None,
            ))
        }
    }

    pub(super) fn expression_continue(
        &mut self,
        span: Span,
        scope: &VariableScope<'a, '_>,
    ) -> Result<(), Error<'a>> {
        if let Some((start_label, _, entry_depth)) = self.loop_stack.last() {
            let start_label = start_label.clone();
            let entry_depth = *entry_depth;

            // Calculate how much stack to clean up: current depth - depth at loop entry
            let current_depth = scope.total_stack_depth();
            let cleanup_amount = current_depth.saturating_sub(entry_depth);

            // Clean up stack before jumping back to loop start
            if cleanup_amount > 0 {
                self.write_instruction(
                    Instruction::Sub(
                        Operand::StackPointer,
                        Operand::StackPointer,
                        Operand::Number(cleanup_amount.into()),
                    ),
                    Some(span),
                )?;
            }
            self.write_instruction(Instruction::Jump(Operand::Label(start_label)), Some(span))?;
            Ok(())
        } else {
            Err(Error::Unknown(
                "Continue statement outside of loop".into(),
                None,
            ))
        }
    }

    pub(super) fn expression_ternary(
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

        let ((cond, cond_clean), (true_val, true_clean), (false_val, false_clean)) =
            compile_operands!(self, (*condition, *true_value, *false_value), scope);

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
    pub(super) fn expression_block<'v>(
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
}
