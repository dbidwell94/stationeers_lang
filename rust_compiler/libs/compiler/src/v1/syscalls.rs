use super::*;

impl<'a> Compiler<'a> {
    pub(super) fn expression_syscall_system(
        &mut self,
        expr: System<'a>,
        span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
        // Track the syscall in metadata
        let syscall_name = expr.name();
        let doc = expr.docs().into();
        self.metadata.add_syscall_with_doc(
            Cow::Borrowed(syscall_name),
            crate::SyscallType::System,
            expr.arg_count(),
            Some(span),
            Some(doc),
        );

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
            System::Clr(device) => {
                let (op, var_cleanup) = self.compile_operand(*device, scope)?;
                self.write_instruction(Instruction::Clr(op), Some(span))?;

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
                    Unit::None,
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

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::Store(
                        Operand::Device(device_val),
                        Operand::LogicType(logic_type_str),
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

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::StoreBatch(
                        device_hash_val,
                        Operand::LogicType(logic_type_str),
                        var,
                    ),
                    Some(span),
                )?;
                cleanup!(var_cleanup, device_hash_cleanup);

                Ok(None)
            }
            System::SetOnDeviceBatchedNamed(device_hash, name_hash, logic_type, val_expr) => {
                let (value, value_cleanup) = self.compile_operand(*val_expr, scope)?;
                let (device_hash, device_hash_cleanup) =
                    self.compile_literal_or_variable(device_hash.node, scope)?;

                let (name_hash, name_hash_cleanup) = self.compile_operand(*name_hash, scope)?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_operand = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::StoreBatchNamed(
                        device_hash,
                        name_hash,
                        Operand::LogicType(logic_type_operand),
                        value,
                    ),
                    Some(span),
                )?;
                cleanup!(value_cleanup, device_hash_cleanup, name_hash_cleanup);

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

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::Load(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        Operand::Device(device_val),
                        Operand::LogicType(logic_type_str),
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
                    self.compile_operand(*device_hash, scope)?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let batch_mode_expr = match batch_mode.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: batch_mode.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let batch_mode_str = self.compile_const_string(
                    Spanned {
                        node: batch_mode_expr,
                        span: batch_mode.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::LoadBatch(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        Operand::LogicType(logic_type_str),
                        Operand::LogicType(batch_mode_str),
                    ),
                    Some(span),
                )?;
                cleanup!(device_hash_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadBatchNamed(device_hash, name_hash, logic_type, batch_mode) => {
                let ((device_hash, device_hash_cleanup), (name_hash, name_hash_cleanup)) =
                    compile_operands!(self, (*device_hash, *name_hash), scope);

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let batch_mode_expr = match batch_mode.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: batch_mode.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let batch_mode_str = self.compile_const_string(
                    Spanned {
                        node: batch_mode_expr,
                        span: batch_mode.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::LoadBatchNamed(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        name_hash,
                        Operand::LogicType(logic_type_str),
                        Operand::LogicType(batch_mode_str),
                    ),
                    Some(span),
                )?;
                cleanup!(device_hash_cleanup, name_hash_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadBatchSlot(device_hash, slot_index, logic_slot_type, batch_mode) => {
                let ((device_hash, device_hash_cleanup), (slot_index, slot_cleanup)) =
                    compile_operands!(self, (*device_hash, *slot_index), scope);

                let logic_slot_type_expr = match logic_slot_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_slot_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_slot_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_slot_type_expr,
                        span: logic_slot_type.span,
                    },
                    scope,
                    span,
                )?;

                let batch_mode_expr = match batch_mode.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: batch_mode.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let batch_mode_str = self.compile_const_string(
                    Spanned {
                        node: batch_mode_expr,
                        span: batch_mode.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::LoadBatchSlot(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        slot_index,
                        Operand::LogicType(logic_slot_type_str),
                        Operand::LogicType(batch_mode_str),
                    ),
                    Some(span),
                )?;
                cleanup!(device_hash_cleanup, slot_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadBatchNamedSlot(
                device_hash,
                name_hash,
                slot_index,
                logic_slot_type,
                batch_mode,
            ) => {
                let (
                    (device_hash, device_hash_cleanup),
                    (name_hash, name_hash_cleanup),
                    (slot_index, slot_cleanup),
                ) = compile_operands!(self, (*device_hash, *name_hash, *slot_index), scope);

                let logic_slot_type_expr = match logic_slot_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_slot_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_slot_type_str = self.compile_const_string(
                    Spanned {
                        node: logic_slot_type_expr,
                        span: logic_slot_type.span,
                    },
                    scope,
                    span,
                )?;

                let batch_mode_expr = match batch_mode.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: batch_mode.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let batch_mode_str = self.compile_const_string(
                    Spanned {
                        node: batch_mode_expr,
                        span: batch_mode.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::LoadBatchNamedSlot(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device_hash,
                        name_hash,
                        slot_index,
                        Operand::LogicType(logic_slot_type_str),
                        Operand::LogicType(batch_mode_str),
                    ),
                    Some(span),
                )?;
                cleanup!(device_hash_cleanup, name_hash_cleanup, slot_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::LoadSlot(dev_name, slot_index, logic_type) => {
                let (dev_hash, hash_cleanup) =
                    self.compile_literal_or_variable(dev_name.node, scope)?;
                let (slot_index, slot_cleanup) = self.compile_operand(*slot_index, scope)?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_operand = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::LoadSlot(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        dev_hash,
                        slot_index,
                        Operand::LogicType(logic_type_operand),
                    ),
                    Some(span),
                )?;
                cleanup!(hash_cleanup, slot_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::SetSlot(dev_name, slot_index, logic_type, var) => {
                let (dev_name, name_cleanup) =
                    self.compile_literal_or_variable(dev_name.node, scope)?;
                let ((slot_index, index_cleanup), (var, var_cleanup)) =
                    compile_operands!(self, (*slot_index, *var), scope);

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let logic_type_expr = match logic_type.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: logic_type.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let logic_type_operand = self.compile_const_string(
                    Spanned {
                        node: logic_type_expr,
                        span: logic_type.span,
                    },
                    scope,
                    span,
                )?;

                self.write_instruction(
                    Instruction::StoreSlot(
                        dev_name,
                        slot_index,
                        Operand::LogicType(logic_type_operand),
                        var,
                    ),
                    Some(span),
                )?;
                cleanup!(name_cleanup, index_cleanup, var_cleanup);

                Ok(None)
            }
            System::LoadReagent(device, reagent_mode, reagent_hash) => {
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

                let (device, device_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Variable(device_spanned),
                    scope,
                )?;

                // Convert LiteralOrVariable to Expression and validate it's a constant string
                let reagent_mode_expr = match reagent_mode.node {
                    LiteralOrVariable::Literal(lit) => Expression::Literal(Spanned {
                        node: lit,
                        span: reagent_mode.span,
                    }),
                    LiteralOrVariable::Variable(var) => Expression::Variable(var),
                };
                let reagent_mode_str = self.compile_const_string(
                    Spanned {
                        node: reagent_mode_expr,
                        span: reagent_mode.span,
                    },
                    scope,
                    span,
                )?;

                let (reagent_hash, reagent_hash_cleanup) =
                    self.compile_operand(*reagent_hash, scope)?;

                self.write_instruction(
                    Instruction::LoadReagent(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device,
                        Operand::LogicType(reagent_mode_str),
                        reagent_hash,
                    ),
                    Some(span),
                )?;

                cleanup!(reagent_hash_cleanup, device_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
            System::Rmap(device, reagent_hash) => {
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

                let (device, device_cleanup) = self.compile_literal_or_variable(
                    LiteralOrVariable::Variable(device_spanned),
                    scope,
                )?;

                let (reagent_hash, reagent_hash_cleanup) =
                    self.compile_operand(*reagent_hash, scope)?;

                self.write_instruction(
                    Instruction::Rmap(
                        Operand::Register(VariableScope::RETURN_REGISTER),
                        device,
                        reagent_hash,
                    ),
                    Some(span),
                )?;

                cleanup!(reagent_hash_cleanup, device_cleanup);

                Ok(Some(CompileLocation {
                    location: VariableLocation::Persistant(VariableScope::RETURN_REGISTER),
                    temp_name: None,
                }))
            }
        }
    }

    pub(super) fn expression_syscall_math(
        &mut self,
        expr: Math<'a>,
        span: Span,
        scope: &mut VariableScope<'a, '_>,
    ) -> Result<Option<CompileLocation<'a>>, Error<'a>> {
        // Track the syscall in metadata
        let syscall_name = expr.name();
        let doc = expr.docs().into();
        self.metadata.add_syscall_with_doc(
            Cow::Borrowed(syscall_name),
            crate::SyscallType::Math,
            expr.arg_count(),
            Some(span),
            Some(doc),
        );

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
                let ((var1, var1_cleanup), (var2, var2_cleanup)) =
                    compile_operands!(self, (*expr1, *expr2), scope);

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
                let ((var1, clean1), (var2, clean2)) =
                    compile_operands!(self, (*expr1, *expr2), scope);

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
                let ((var1, clean1), (var2, clean2)) =
                    compile_operands!(self, (*expr1, *expr2), scope);

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
}
