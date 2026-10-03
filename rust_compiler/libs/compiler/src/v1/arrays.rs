use super::operands::fold_expression;
use super::*;
use crate::variable_manager::ArrayPointerLocation;
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
                if d != trunc {
                    return Err(Error::OperationNotSupported(
                        "Array size must be a whole number.".to_string(),
                        size_expr.span,
                    ));
                }
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
        self.array_high_water = self.array_high_water.max(base + len);

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
        self.array_high_water = self.array_high_water.max(base + len);

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
        array: &VariableLocation<'a>,
        index: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        if let VariableLocation::Array { base, len } = array {
            if let Some(folded) = fold_expression(&index.node, scope) {
                let index_value = match folded {
                    Number::Integer(value, _) => i128::from(value),
                    Number::Decimal(value, _) => {
                        let trunc = value.trunc();
                        if value != trunc {
                            return Err(Error::OperationNotSupported(
                                "Array index must be a whole number when compile-time constant."
                                    .to_string(),
                                index.span,
                            ));
                        }
                        trunc.mantissa() / 10_i128.pow(trunc.scale())
                    }
                };
                if index_value < 0 || index_value >= i128::from(*len) {
                    return Err(Error::OperationNotSupported(
                        format!("Array index {index_value} is outside the valid range 0..{len}."),
                        index.span,
                    ));
                }
                let base_num = Number::Integer(*base as i128, Unit::None);
                return Ok((
                    Operand::Number((base_num + Number::Integer(index_value, Unit::None)).into()),
                    None,
                ));
            }

            let (idx_operand, idx_cleanup) = self.compile_operand(index, scope)?;
            if *base == 0 {
                return Ok((idx_operand, idx_cleanup));
            }
            let temp_name = self.next_temp_name();
            let temp_loc = scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
            let temp_reg = self.resolve_register(&temp_loc)?;
            self.write_instruction(
                Instruction::Add(
                    temp_reg.clone(),
                    Operand::Number((*base).into()),
                    idx_operand,
                ),
                Some(index.span),
            )?;
            if let Some(c) = idx_cleanup {
                scope.free_temp(c, None)?;
            }
            return Ok((temp_reg, Some(temp_name)));
        }

        let VariableLocation::ArrayParameter { base } = array else {
            return Err(Error::OperationNotSupported(
                "Expected an array location.".to_string(),
                index.span,
            ));
        };

        let (base_operand, base_cleanup) = self.array_pointer_operand(base, scope)?;
        let (index_operand, index_cleanup) = self.compile_operand(index, scope)?;
        let temp_name = self.next_temp_name();
        let temp_loc = scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
        let temp_reg = self.resolve_register(&temp_loc)?;

        self.write_instruction(
            Instruction::Add(temp_reg.clone(), base_operand, index_operand),
            Some(index.span),
        )?;

        if let Some(c) = base_cleanup {
            scope.free_temp(c, None)?;
        }
        if let Some(c) = index_cleanup {
            scope.free_temp(c, None)?;
        }

        Ok((temp_reg, Some(temp_name)))
    }

    pub(super) fn array_pointer_operand(
        &mut self,
        base: &ArrayPointerLocation,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        match base {
            ArrayPointerLocation::Register(reg) => Ok((Operand::Register(*reg), None)),
            ArrayPointerLocation::Stack(offset) => {
                let temp_name = self.next_temp_name();
                let temp_loc =
                    scope.add_variable(temp_name.clone(), LocationRequest::Temp, None)?;
                let temp_reg = self.resolve_register(&temp_loc)?;
                self.write_instruction(
                    Instruction::Sub(
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                        Operand::StackPointer,
                        Operand::Number((*offset).into()),
                    ),
                    None,
                )?;
                self.write_instruction(
                    Instruction::Get(
                        temp_reg.clone(),
                        Operand::Device(DeviceType::Housing),
                        Operand::Register(VariableScope::TEMP_STACK_REGISTER),
                    ),
                    None,
                )?;
                Ok((temp_reg, Some(temp_name)))
            }
        }
    }

    pub(super) fn compile_array_argument_base(
        &mut self,
        expr: &Spanned<Expression<'a>>,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        match &expr.node {
            Expression::ArrayLiteral(items) => {
                let name = self.next_temp_name();
                let name_span = expr.span;
                let location = self.expression_array_literal(items, name, name_span, scope)?;
                self.array_argument_base(&location, scope, expr.span)
            }
            Expression::ArrayRepeat(repeat) => {
                let name = self.next_temp_name();
                let name_span = expr.span;
                let location = self.expression_array_repeat(repeat, name, name_span, scope)?;
                self.array_argument_base(&location, scope, expr.span)
            }
            Expression::Priority(inner) => self.compile_array_argument_base(inner, scope),
            Expression::Variable(name) => {
                let location = scope.get_location_of(&name.node, Some(name.span))?;
                self.array_argument_base(&location, scope, name.span)
            }
            _ => Err(Error::OperationNotSupported(
                "Function array arguments must be array literals or array variables.".to_string(),
                expr.span,
            )),
        }
    }

    fn array_argument_base(
        &mut self,
        location: &VariableLocation<'a>,
        scope: &mut VariableScope<'a, '_>,
        span: Span,
    ) -> Result<(Operand<'a>, Option<Cow<'a, str>>), Error<'a>> {
        match location {
            VariableLocation::Array { base, .. } => Ok((Operand::Number((*base).into()), None)),
            VariableLocation::ArrayParameter { base } => self.array_pointer_operand(base, scope),
            _ => Err(Error::OperationNotSupported(
                "Function argument is not an array.".to_string(),
                span,
            )),
        }
    }

    /// If `object` is an identifier bound to an array, returns its base address.
    pub(super) fn array_location_of(
        object: &Spanned<Expression<'a>>,
        scope: &VariableScope<'a, '_>,
    ) -> Option<VariableLocation<'a>> {
        let Expression::Variable(name) = &object.node else {
            return None;
        };
        match scope.get_location_of(&name.node, Some(name.span)) {
            Ok(location @ VariableLocation::Array { .. })
            | Ok(location @ VariableLocation::ArrayParameter { .. }) => Some(location),
            _ => None,
        }
    }
}
