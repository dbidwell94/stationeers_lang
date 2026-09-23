use super::operands::fold_expression;
use super::*;
use parser::tree_node::ArrayRepeatExpression;

impl<'a> Compiler<'a> {
    /// Folds an array-repeat's `size` expression into a compile-time constant.
    /// Array sizes must always be known at compile time (no runtime bounds checking).
    fn resolve_array_size(
        &self,
        size_expr: &Spanned<Expression<'a>>,
        scope: &VariableScope<'a, '_>,
    ) -> Result<u16, Error<'a>> {
        let folded = fold_expression(&size_expr.node, scope).ok_or_else(|| {
            Error::OperationNotSupported(
                "Array size must be a compile-time constant.".to_string(),
                size_expr.span,
            )
        })?;

        let as_i128 = match folded {
            Number::Integer(i, _) => i,
            Number::Decimal(d, _) => {
                let trunc = d.trunc();
                trunc.mantissa() / 10_i128.pow(trunc.scale())
            }
        };

        u16::try_from(as_i128).map_err(|_| {
            Error::OperationNotSupported(
                "Array size must be a non-negative integer that fits within the array region."
                    .to_string(),
                size_expr.span,
            )
        })
    }

    /// Compiles `let arr = [1, 2, 3];`: allocates a fixed-size array and emits a
    /// `put` instruction to `db` for each element.
    pub(super) fn expression_array_literal(
        &mut self,
        items: &Spanned<Vec<Spanned<Expression<'a>>>>,
        name: Cow<'a, str>,
        name_span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        let len = u16::try_from(items.node.len()).map_err(|_| {
            Error::OperationNotSupported(
                format!("Array `{name}` has too many elements."),
                name_span,
            )
        })?;

        let loc = scope.define_array(name, len, Some(name_span))?;
        let VariableLocation::Array { base, .. } = loc else {
            unreachable!("define_array always returns VariableLocation::Array")
        };

        for (i, item) in items.node.iter().enumerate() {
            let (value, cleanup) = self.compile_operand(item, scope)?;
            self.write_instruction(
                Instruction::Put(
                    Operand::Device(DeviceType::Housing),
                    Operand::Number((base + i as u16).into()),
                    value,
                ),
                Some(item.span),
            )?;
            if let Some(c) = cleanup {
                scope.free_temp(c, None)?;
            }
        }

        Ok(loc)
    }

    /// Compiles `let arr = [|5| 0];` (zero-filled) and `let arr = [|5|];`
    /// (uninitialized - allocation only, no instructions emitted).
    pub(super) fn expression_array_repeat(
        &mut self,
        repeat: &Spanned<ArrayRepeatExpression<'a>>,
        name: Cow<'a, str>,
        name_span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<VariableLocation<'a>, Error<'a>> {
        let len = self.resolve_array_size(&repeat.node.size, scope)?;

        let loc = scope.define_array(name, len, Some(name_span))?;
        let VariableLocation::Array { base, .. } = loc else {
            unreachable!("define_array always returns VariableLocation::Array")
        };

        if let Some(fill) = &repeat.node.fill {
            let (value, cleanup) = self.compile_operand(fill, scope)?;
            for i in 0..len {
                self.write_instruction(
                    Instruction::Put(
                        Operand::Device(DeviceType::Housing),
                        Operand::Number((base + i).into()),
                        value.clone(),
                    ),
                    Some(fill.span),
                )?;
            }
            if let Some(c) = cleanup {
                scope.free_temp(c, None)?;
            }
        }

        Ok(loc)
    }

    /// Computes the absolute `db` address for `arr[index]`, given the array's
    /// base address. Folds a compile-time-constant index into a single literal
    /// operand; otherwise emits an `add` and returns a temp register holding
    /// the computed address (caller must free the returned temp name).
    pub(super) fn compile_array_index_address(
        &mut self,
        base: u16,
        index: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        if let Some(folded) = fold_expression(&index.node, scope) {
            let base_num = Number::Integer(base as i128, Unit::None);
            return Ok((Operand::Number((base_num + folded).into()), None));
        }

        let (idx_operand, idx_cleanup) = self.compile_operand(index, scope)?;

        let temp_name = self.next_temp_name();
        let temp_loc = scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
        let temp_reg = self.resolve_register(&temp_loc)?;

        self.write_instruction(
            Instruction::Add(
                Operand::Register(temp_reg),
                Operand::Number(base.into()),
                idx_operand,
            ),
            Some(index.span),
        )?;

        if let Some(c) = idx_cleanup {
            scope.free_temp(c, None)?;
        }

        Ok((Operand::Register(temp_reg), Some(temp_name)))
    }

    /// If `object` is an identifier bound to an array, returns its base address.
    pub(super) fn array_base_of(
        object: &Spanned<Expression<'a>>,
        scope: &VariableScope<'a, '_>,
    ) -> Option<u16> {
        let Expression::Variable(name) = &object.node else {
            return None;
        };
        match scope.get_location_of(&name.node, Some(name.span)) {
            Ok(VariableLocation::Array { base, .. }) => Some(base),
            _ => None,
        }
    }

    /// If `object` is an identifier bound to an array, returns its length.
    pub(super) fn array_len_of(
        object: &Spanned<Expression<'a>>,
        scope: &VariableScope<'a, '_>,
    ) -> Option<u16> {
        let Expression::Variable(name) = &object.node else {
            return None;
        };
        match scope.get_location_of(&name.node, Some(name.span)) {
            Ok(VariableLocation::Array { len, .. }) => Some(len),
            _ => None,
        }
    }
}
