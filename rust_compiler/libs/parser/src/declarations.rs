use super::*;

impl<'a> Parser<'a> {
    pub(super) fn device(&mut self) -> Result<DeviceDeclarationExpression<'a>, Error<'a>> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Device)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        let identifier_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        let identifier_span = Self::token_to_span(&identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&identifier_token),
                    identifier_token.clone(),
                ));
            }
        };

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token.clone(),
            ));
        }

        let device_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        let span = self.current_span();
        let device_type = Spanned {
            span,
            node: DeviceType::try_from(&device_token)?,
        };

        Ok(DeviceDeclarationExpression {
            name: Spanned {
                span: identifier_span,
                node: identifier,
            },
            device: device_type,
        })
    }

    pub(super) fn tuple_declaration(&mut self) -> Result<Expression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let mut names = Vec::new();
        while !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
            let token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
            let span = Self::token_to_span(&token);
            if let TokenType::Identifier(id) = token.token_type {
                names.push(Spanned { span, node: id });
            } else {
                return Err(Error::UnexpectedToken(span, token));
            }

            if self_matches_peek!(self, TokenType::Symbol(Symbol::Comma)) {
                self.assign_next()?;
            }
        }
        self.assign_next()?;

        let assign = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;

        if !token_matches!(assign, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&assign), assign));
        }

        self.assign_next()?;

        let value = self.expression()?.ok_or_else(|| self.unexpected_eof())?;
        let value_span = value.span;

        let semi = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(semi, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(Error::MissingSemicolon(value_span));
        }

        Ok(Expression::TupleDeclaration(Spanned {
            span: names.first().map(|n| n.span).unwrap_or(value.span),
            node: TupleDeclarationExpression {
                names,
                value: boxed!(value),
            },
        }))
    }

    pub(super) fn block(&mut self) -> Result<BlockExpression<'a>, Error<'a>> {
        let mut expressions = Vec::<Spanned<Expression>>::new();
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;

        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        while !self_matches_peek!(
            self,
            TokenType::Symbol(Symbol::RBrace) | TokenType::Keyword(Keyword::Return)
        ) {
            let expression = self.parse()?.ok_or_else(|| self.unexpected_eof())?;
            expressions.push(expression);
        }

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;

        if token_matches!(current_token, TokenType::Keyword(Keyword::Return)) {
            let ret_start_span = Self::token_to_span(&current_token);
            self.assign_next()?;

            let expr = if token_matches!(
                self.current_token
                    .as_ref()
                    .ok_or_else(|| self.unexpected_eof())?,
                TokenType::Symbol(Symbol::Semicolon)
            ) {
                self.tokenizer.seek(SeekFrom::Current(-1))?;
                None
            } else {
                Some(self.expression()?.ok_or_else(|| self.unexpected_eof())?)
            };

            let ret_span = Span {
                start_line: ret_start_span.start_line,
                start_col: ret_start_span.start_col,
                end_line: expr
                    .as_ref()
                    .map(|e| e.span.end_line)
                    .unwrap_or(ret_start_span.end_line),
                end_col: expr
                    .as_ref()
                    .map(|e| e.span.end_col)
                    .unwrap_or(ret_start_span.end_col),
            };

            let return_expr = Spanned {
                span: ret_span,
                node: Expression::Return(expr.map(Box::new)),
            };
            expressions.push(return_expr);

            let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
            if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                return Err(Error::MissingSemicolon(ret_span));
            }

            let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
            if !token_matches!(next, TokenType::Symbol(Symbol::RBrace)) {
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }
        }

        Ok(BlockExpression(expressions))
    }

    pub(super) fn const_declaration(
        &mut self,
    ) -> Result<ConstDeclarationExpression<'a>, Error<'a>> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Const)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        let ident_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        let ident_span = Self::token_to_span(&ident_token);
        let ident = match ident_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => return Err(Error::UnexpectedToken(ident_span, ident_token.clone())),
        };

        let assign_token = self
            .get_next()?
            .ok_or_else(|| self.unexpected_eof())?
            .clone();
        if !token_matches!(assign_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&assign_token),
                assign_token,
            ));
        }

        self.assign_next()?;
        let current_token_index = self.tokenizer.loc();

        if let Ok(lit) = self.spanned(|p| p.literal()) {
            Ok(ConstDeclarationExpression {
                name: Spanned {
                    span: ident_span,
                    node: ident,
                },
                value: LiteralOr::Literal(lit),
            })
        } else {
            self.tokenizer.seek(SeekFrom::Current(
                current_token_index - self.tokenizer.loc(),
            ))?;
            let syscall = self.spanned(|p| p.syscall())?;

            if !matches!(
                syscall,
                Spanned {
                    node: SysCall::System(sys_call::System::Hash(_)),
                    ..
                }
            ) {
                return Err(Error::UnexpectedToken(
                    syscall.span,
                    self.current_token
                        .clone()
                        .ok_or_else(|| self.unexpected_eof())?,
                ));
            }

            Ok(ConstDeclarationExpression {
                name: Spanned {
                    span: ident_span,
                    node: ident,
                },
                value: LiteralOr::Or(syscall),
            })
        }
    }

    pub(super) fn declaration(&mut self) -> Result<Expression<'a>, Error<'a>> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;
        if !self_matches_current!(self, TokenType::Keyword(Keyword::Let)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }
        let identifier_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        let identifier_span = Self::token_to_span(&identifier_token);
        let identifier = match identifier_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(
                    Self::token_to_span(&identifier_token),
                    identifier_token,
                ));
            }
        };

        let current_token = self
            .get_next()?
            .ok_or_else(|| self.unexpected_eof())?
            .clone();

        if !token_matches!(current_token, TokenType::Symbol(Symbol::Assign)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token.clone(),
            ));
        }

        self.assign_next()?;
        let assignment_expression = self.expression()?.ok_or_else(|| self.unexpected_eof())?;
        let expr_span = assignment_expression.span;

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::Semicolon)) {
            return Err(Error::MissingSemicolon(expr_span));
        }

        Ok(Expression::Declaration(
            Spanned {
                span: identifier_span,
                node: identifier,
            },
            boxed!(assignment_expression),
        ))
    }

    pub(super) fn function(&mut self) -> Result<FunctionExpression<'a>, Error<'a>> {
        let fn_ident_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        let fn_ident_span = Self::token_to_span(&fn_ident_token);
        let fn_ident = match fn_ident_token.token_type {
            TokenType::Identifier(ref id) => id.clone(),
            _ => {
                return Err(Error::UnexpectedToken(fn_ident_span, fn_ident_token));
            }
        };

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
            ));
        }

        let mut arguments = Vec::<Spanned<Cow<'a, str>>>::new();

        while !token_matches!(
            self.get_next()?.ok_or_else(|| self.unexpected_eof())?,
            TokenType::Symbol(Symbol::RParen)
        ) {
            let current_token = self
                .current_token
                .as_ref()
                .ok_or_else(|| self.unexpected_eof())?;
            let arg_span = Self::token_to_span(current_token);
            let argument = match current_token.token_type {
                TokenType::Identifier(ref id) => id.clone(),
                _ => {
                    return Err(Error::UnexpectedToken(arg_span, current_token.clone()));
                }
            };

            let spanned_arg = Spanned {
                span: arg_span,
                node: argument,
            };

            if arguments.contains(&spanned_arg) {
                return Err(Error::DuplicateIdentifier(
                    Self::token_to_span(current_token),
                    current_token.clone(),
                ));
            }

            arguments.push(spanned_arg);

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
            {
                let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }

            if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                self.assign_next()?;
            }
        }

        let current_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(current_token, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(
                Self::token_to_span(&current_token),
                current_token,
            ));
        };

        Ok(FunctionExpression {
            name: Spanned {
                span: fn_ident_span,
                node: fn_ident,
            },
            arguments,
            body: self.block()?,
        })
    }
}
