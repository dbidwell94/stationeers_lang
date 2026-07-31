use super::*;

impl<'a> Parser<'a> {
    pub(super) fn invocation(&mut self) -> Result<InvocationExpression<'a>, Error<'a>> {
        let identifier_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;
        let identifier_span = Self::token_to_span(identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    self.current_token
                        .clone()
                        .ok_or_else(|| self.unexpected_eof())?,
                ));
            }
        };

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
            ));
        }

        let mut arguments = Vec::<Spanned<Expression>>::new();

        while !token_matches!(
            self.get_next()?.ok_or_else(|| self.unexpected_eof())?,
            TokenType::Symbol(Symbol::RParen)
        ) {
            let expression = self.expression()?.ok_or_else(|| self.unexpected_eof())?;

            if let Expression::Block(_) = expression.node {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    String::from("Block expressions are not allowed in function invocations"),
                ));
            }

            arguments.push(expression);

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                let next_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&next_token),
                    next_token,
                ));
            }

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        Ok(InvocationExpression {
            name: Spanned {
                span: identifier_span,
                node: identifier,
            },
            arguments,
        })
    }

    pub(super) fn syscall(&mut self) -> Result<SysCall<'a>, Error<'a>> {
        let invocation = self.invocation()?;

        let check_length = |len: usize| -> Result<(), Error> {
            if invocation.arguments.len() != len {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    format!("Expected {} arguments", len),
                ));
            }
            Ok(())
        };

        macro_rules! args {
            ($count:expr) => {{
                check_length($count)?;
                invocation.arguments.into_iter()
            }};
        }

        macro_rules! literal_or_variable {
            ($iter:expr) => {
                match &$iter {
                    Some(expr) => {
                        let span = expr.span;
                        match &expr.node {
                            Expression::Literal(literal) => Spanned {
                                span,
                                node: LiteralOrVariable::Literal(literal.node.clone()),
                            },
                            Expression::Variable(ident) => Spanned {
                                span,
                                node: LiteralOrVariable::Variable(ident.clone()),
                            },
                            _ => {
                                return Err(Error::InvalidSyntax(
                                    expr.span,
                                    "Expected a literal or variable".to_string(),
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(Error::UnexpectedToken(
                            self.current_span(),
                            self.current_token
                                .clone()
                                .ok_or_else(|| self.unexpected_eof())?,
                        ))
                    }
                }
            };
        }

        match invocation.name.node.as_ref() {
            "yield" => {
                check_length(0)?;
                Ok(SysCall::System(sys_call::System::Yield))
            }
            "sleep" => {
                let mut args = args!(1);
                let expr = args.next().ok_or_else(|| self.unexpected_eof())?;
                Ok(SysCall::System(System::Sleep(boxed!(expr))))
            }
            "clr" => {
                let mut args = args!(1);
                let expr = args.next().ok_or_else(|| self.unexpected_eof())?;
                Ok(SysCall::System(System::Clr(boxed!(expr))))
            }
            "hash" => {
                let mut args = args!(1);
                let lit_str = literal_or_variable!(args.next());

                let Spanned {
                    node: LiteralOrVariable::Literal(lit_str),
                    span,
                } = lit_str
                else {
                    return Err(Error::InvalidSyntax(
                        lit_str.span,
                        "Expected a string literal".to_string(),
                    ));
                };

                Ok(SysCall::System(System::Hash(Spanned {
                    node: lit_str,
                    span,
                })))
            }
            "load" | "l" => {
                let mut args = args!(2);
                let device = literal_or_variable!(args.next());
                let logic_type = literal_or_variable!(args.next());

                Ok(SysCall::System(sys_call::System::LoadFromDevice(
                    device, logic_type,
                )))
            }
            "loadBatched" | "lb" => {
                let mut args = args!(3);
                let device_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let logic_type = literal_or_variable!(args.next());
                let batch_mode = literal_or_variable!(args.next());

                Ok(SysCall::System(System::LoadBatch(
                    boxed!(device_hash),
                    logic_type,
                    batch_mode,
                )))
            }
            "loadBatchedSlot" | "lbs" => {
                let mut args = args!(4);
                let device_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let slot_index = args.next().ok_or_else(|| self.unexpected_eof())?;
                let logic_slot_type = literal_or_variable!(args.next());
                let batch_mode = literal_or_variable!(args.next());

                Ok(SysCall::System(System::LoadBatchSlot(
                    boxed!(device_hash),
                    boxed!(slot_index),
                    logic_slot_type,
                    batch_mode,
                )))
            }
            "loadBatchedNamedSlot" | "lbns" => {
                let mut args = args!(5);
                let device_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let name_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let slot_index = args.next().ok_or_else(|| self.unexpected_eof())?;
                let logic_slot_type = literal_or_variable!(args.next());
                let batch_mode = literal_or_variable!(args.next());

                Ok(SysCall::System(System::LoadBatchNamedSlot(
                    boxed!(device_hash),
                    boxed!(name_hash),
                    boxed!(slot_index),
                    logic_slot_type,
                    batch_mode,
                )))
            }
            "loadBatchedNamed" | "lbn" => {
                let mut args = args!(4);
                let dev_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let name_hash = args.next().ok_or_else(|| self.unexpected_eof())?;
                let logic_type = literal_or_variable!(args.next());
                let batch_mode = literal_or_variable!(args.next());

                Ok(SysCall::System(System::LoadBatchNamed(
                    boxed!(dev_hash),
                    boxed!(name_hash),
                    logic_type,
                    batch_mode,
                )))
            }
            "set" | "s" => {
                let mut args = args!(3);
                let device = literal_or_variable!(args.next());
                let logic_type = literal_or_variable!(args.next());
                let variable = args.next().ok_or_else(|| self.unexpected_eof())?;
                Ok(SysCall::System(sys_call::System::SetOnDevice(
                    device,
                    logic_type,
                    boxed!(variable),
                )))
            }
            "setBatched" | "sb" => {
                let mut args = args!(3);
                let device_hash = literal_or_variable!(args.next());
                let logic_type = literal_or_variable!(args.next());
                let variable = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::System(sys_call::System::SetOnDeviceBatched(
                    device_hash,
                    logic_type,
                    boxed!(variable),
                )))
            }
            "setBatchedNamed" | "sbn" => {
                let mut args = args!(4);
                let device_hash = literal_or_variable!(args.next());
                let name_hash = Box::new(args.next().ok_or_else(|| self.unexpected_eof())?);
                let logic_type = literal_or_variable!(args.next());
                let expr = Box::new(args.next().ok_or_else(|| self.unexpected_eof())?);

                Ok(SysCall::System(System::SetOnDeviceBatchedNamed(
                    device_hash,
                    name_hash,
                    logic_type,
                    expr,
                )))
            }
            "loadSlot" | "ls" => {
                let mut args = args!(3);
                let dev_name = literal_or_variable!(args.next());
                let slot_index = args.next().ok_or_else(|| self.unexpected_eof())?;
                let slot_logic = literal_or_variable!(args.next());

                Ok(SysCall::System(System::LoadSlot(
                    dev_name,
                    boxed!(slot_index),
                    slot_logic,
                )))
            }
            "setSlot" | "ss" => {
                let mut args = args!(4);
                let dev_name = literal_or_variable!(args.next());
                let slot_index = args.next().ok_or_else(|| self.unexpected_eof())?;
                let slot_logic = literal_or_variable!(args.next());
                let expr = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::System(System::SetSlot(
                    dev_name,
                    boxed!(slot_index),
                    slot_logic,
                    boxed!(expr),
                )))
            }
            "loadReagent" | "lr" => {
                let mut args = args!(3);
                let device = literal_or_variable!(args.next());
                let reagent_mode = literal_or_variable!(args.next());
                let reagent_hash = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::System(System::LoadReagent(
                    device,
                    reagent_mode,
                    Box::new(reagent_hash),
                )))
            }
            "rmap" => {
                let mut args = args!(2);
                let device = literal_or_variable!(args.next());
                let reagent_hash = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::System(System::Rmap(
                    device,
                    Box::new(reagent_hash),
                )))
            }
            "acos" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let tmp = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Acos(boxed!(tmp))))
            }
            "asin" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let tmp = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Asin(boxed!(tmp))))
            }
            "atan" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let expr = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Atan(boxed!(expr))))
            }
            "atan2" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or_else(|| self.unexpected_eof())?;
                let arg2 = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Atan2(boxed!(arg1), boxed!(arg2))))
            }
            "abs" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let expr = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Abs(boxed!(expr))))
            }
            "ceil" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Ceil(boxed!(arg))))
            }
            "cos" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Cos(boxed!(arg))))
            }
            "floor" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Floor(boxed!(arg))))
            }
            "log" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Log(boxed!(arg))))
            }
            "max" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or_else(|| self.unexpected_eof())?;
                let arg2 = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Max(boxed!(arg1), boxed!(arg2))))
            }
            "min" => {
                check_length(2)?;
                let mut args = invocation.arguments.into_iter();
                let arg1 = args.next().ok_or_else(|| self.unexpected_eof())?;
                let arg2 = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Min(boxed!(arg1), boxed!(arg2))))
            }
            "rand" => {
                check_length(0)?;
                Ok(SysCall::Math(Math::Rand))
            }
            "sin" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Sin(boxed!(arg))))
            }
            "sqrt" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Sqrt(boxed!(arg))))
            }
            "tan" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Tan(boxed!(arg))))
            }
            "trunc" => {
                check_length(1)?;
                let mut args = invocation.arguments.into_iter();
                let arg = args.next().ok_or_else(|| self.unexpected_eof())?;

                Ok(SysCall::Math(Math::Trunc(boxed!(arg))))
            }
            _ => Err(Error::UnsupportedKeyword(
                self.current_span(),
                self.current_token
                    .clone()
                    .ok_or_else(|| self.unexpected_eof())?,
            )),
        }
    }
}
