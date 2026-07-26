use super::*;

impl<'a> Parser<'a> {
    pub(super) fn expression(
        &mut self,
    ) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        let lhs = self.unary()?;

        let Some(lhs) = lhs else {
            return Ok(None);
        };

        let lhs = self.parse_postfix(lhs)?;

        if self_matches_peek!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || s.is_bitwise() || matches!(s, Symbol::Assign | Symbol::Question)
        ) {
            return Ok(Some(self.infix(lhs)?));
        } else if self_matches_current!(
            self,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || s.is_bitwise() || matches!(s, Symbol::Assign | Symbol::Question)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
            return Ok(Some(self.infix(lhs)?));
        }

        Ok(Some(lhs))
    }

    pub(super) fn parse_postfix(
        &mut self,
        mut lhs: Spanned<Expression<'a>>,
    ) -> Result<Spanned<Expression<'a>>, Error<'a>> {
        loop {
            if self_matches_peek!(self, TokenType::Symbol(Symbol::Dot)) {
                self.assign_next()?;

                let identifier_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                let identifier_span = Self::token_to_span(&identifier_token);
                let identifier = match identifier_token.token_type {
                    TokenType::Identifier(ref id) => id.clone(),
                    _ => {
                        return Err(Error::UnexpectedToken(
                            identifier_span,
                            identifier_token.clone(),
                        ));
                    }
                };

                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) {
                    self.assign_next()?;
                    let mut arguments = Vec::<Spanned<Expression<'a>>>::new();

                    while !token_matches!(
                        self.get_next()?.ok_or_else(|| self.unexpected_eof())?,
                        TokenType::Symbol(Symbol::RParen)
                    ) {
                        let expression = self.expression()?.ok_or_else(|| self.unexpected_eof())?;

                        if let Expression::Block(_) = expression.node {
                            return Err(Error::InvalidSyntax(
                                self.current_span(),
                                String::from("Block expressions are not allowed in method calls"),
                            ));
                        }
                        arguments.push(expression);

                        if !self_matches_peek!(self, TokenType::Symbol(Symbol::Comma))
                            && !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen))
                        {
                            let next_token =
                                self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                            return Err(Error::UnexpectedToken(
                                Self::token_to_span(&next_token),
                                next_token,
                            ));
                        }

                        if !self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
                            self.assign_next()?;
                        }
                    }

                    let end_span = self.current_span();
                    let combined_span = Span {
                        start_line: lhs.span.start_line,
                        start_col: lhs.span.start_col,
                        end_line: end_span.end_line,
                        end_col: end_span.end_col,
                    };

                    lhs = Spanned {
                        span: combined_span,
                        node: Expression::MethodCall(Spanned {
                            span: combined_span,
                            node: MethodCallExpression {
                                object: boxed!(lhs),
                                method: Spanned {
                                    span: identifier_span,
                                    node: identifier,
                                },
                                arguments,
                            },
                        }),
                    };
                } else {
                    let combined_span = Span {
                        start_line: lhs.span.start_line,
                        start_col: lhs.span.start_col,
                        end_line: identifier_span.end_line,
                        end_col: identifier_span.end_col,
                    };

                    lhs = Spanned {
                        span: combined_span,
                        node: Expression::MemberAccess(Spanned {
                            span: combined_span,
                            node: MemberAccessExpression {
                                object: boxed!(lhs),
                                member: Spanned {
                                    span: identifier_span,
                                    node: identifier,
                                },
                            },
                        }),
                    };
                }
            } else if self_matches_peek!(self, TokenType::Symbol(Symbol::LBracket)) {
                self.assign_next()?;
                self.assign_next()?;
                let index = self.expression()?.ok_or_else(|| self.unexpected_eof())?;

                let rbracket_token = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                if !token_matches!(rbracket_token, TokenType::Symbol(Symbol::RBracket)) {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(&rbracket_token),
                        rbracket_token.clone(),
                    ));
                }

                let end_span = Self::token_to_span(&rbracket_token);
                let combined_span = Span {
                    start_line: lhs.span.start_line,
                    start_col: lhs.span.start_col,
                    end_line: end_span.end_line,
                    end_col: end_span.end_col,
                };

                lhs = Spanned {
                    span: combined_span,
                    node: Expression::IndexAccess(Spanned {
                        span: combined_span,
                        node: IndexAccessExpression {
                            object: boxed!(lhs),
                            index: boxed!(index),
                        },
                    }),
                };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    pub(super) fn unary(
        &mut self,
    ) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        macro_rules! matches_keyword {
            ($keyword:expr, $($pattern:pat),+) => {
                matches!($keyword, $($pattern)|+)
            };
        }

        let Some(current_token) = self.current_token.as_ref() else {
            return Ok(None);
        };

        if token_matches!(current_token, TokenType::EOF) {
            return Ok(None);
        }

        let expr = match current_token.token_type {
            TokenType::Keyword(e) if matches_keyword!(e, Keyword::Enum) => {
                return Err(Error::UnsupportedKeyword(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
            TokenType::Keyword(Keyword::Let) => {
                if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) {
                    Some(self.spanned(|p| p.tuple_declaration())?)
                } else {
                    Some(self.spanned(|p| p.declaration())?)
                }
            }
            TokenType::Keyword(Keyword::Device) => {
                let spanned_dev = self.spanned(|p| p.device())?;
                Some(Spanned {
                    span: spanned_dev.span,
                    node: Expression::DeviceDeclaration(spanned_dev),
                })
            }
            TokenType::Keyword(Keyword::Const) => {
                let spanned_const = self.spanned(|p| p.const_declaration())?;
                Some(Spanned {
                    span: spanned_const.span,
                    node: Expression::ConstDeclaration(spanned_const),
                })
            }
            TokenType::Keyword(Keyword::Fn) => {
                let spanned_fn = self.spanned(|p| p.function())?;
                Some(Spanned {
                    span: spanned_fn.span,
                    node: Expression::Function(spanned_fn),
                })
            }
            TokenType::Keyword(Keyword::If) => {
                let spanned_if = self.spanned(|p| p.if_expression())?;
                Some(Spanned {
                    span: spanned_if.span,
                    node: Expression::If(spanned_if),
                })
            }
            TokenType::Keyword(Keyword::Loop) => {
                let spanned_loop = self.spanned(|p| p.loop_expression())?;
                Some(Spanned {
                    span: spanned_loop.span,
                    node: Expression::Loop(spanned_loop),
                })
            }
            TokenType::Keyword(Keyword::While) => {
                let spanned_while = self.spanned(|p| p.while_expression())?;
                Some(Spanned {
                    span: spanned_while.span,
                    node: Expression::While(spanned_while),
                })
            }
            TokenType::Keyword(Keyword::Break) => {
                let span = self.current_span();
                let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::MissingSemicolon(span));
                }
                Some(Spanned {
                    span,
                    node: Expression::Break(span),
                })
            }
            TokenType::Keyword(Keyword::Continue) => {
                let span = self.current_span();
                let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                if !token_matches!(next, TokenType::Symbol(Symbol::Semicolon)) {
                    return Err(Error::MissingSemicolon(span));
                }
                Some(Spanned {
                    span,
                    node: Expression::Continue(span),
                })
            }
            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                let spanned_call = self.spanned(|p| p.syscall())?;
                Some(Spanned {
                    span: spanned_call.span,
                    node: Expression::Syscall(spanned_call),
                })
            }
            TokenType::Identifier(_) if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) => {
                let spanned_invoke = self.spanned(|p| p.invocation())?;
                Some(Spanned {
                    span: spanned_invoke.span,
                    node: Expression::Invocation(spanned_invoke),
                })
            }
            TokenType::Identifier(ref id) => {
                let span = self.current_span();
                Some(Spanned {
                    span,
                    node: Expression::Variable(Spanned {
                        span,
                        node: id.clone(),
                    }),
                })
            }
            TokenType::Symbol(Symbol::LBrace) => {
                let spanned_block = self.spanned(|p| p.block())?;
                Some(Spanned {
                    span: spanned_block.span,
                    node: Expression::Block(spanned_block),
                })
            }
            TokenType::Number(_) | TokenType::String(_) | TokenType::Boolean(_) => {
                let spanned_lit = self.spanned(|p| p.literal())?;
                Some(Spanned {
                    span: spanned_lit.span,
                    node: Expression::Literal(spanned_lit),
                })
            }
            TokenType::Symbol(Symbol::LParen) => self.parenthesized_or_tuple()?,
            TokenType::Symbol(Symbol::Minus) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or_else(|| self.unexpected_eof())?;
                let inner_with_postfix = self.parse_postfix(inner_expr)?;

                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_with_postfix.span.end_line,
                    end_col: inner_with_postfix.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Negation(boxed!(inner_with_postfix)),
                })
            }
            TokenType::Symbol(Symbol::LogicalNot) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or_else(|| self.unexpected_eof())?;
                let inner_with_postfix = self.parse_postfix(inner_expr)?;
                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_with_postfix.span.end_line,
                    end_col: inner_with_postfix.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::Logical(Spanned {
                        span: combined_span,
                        node: LogicalExpression::Not(boxed!(inner_with_postfix)),
                    }),
                })
            }
            TokenType::Symbol(Symbol::BitwiseNot) => {
                let start_span = self.current_span();
                self.assign_next()?;
                let inner_expr = self.unary()?.ok_or_else(|| self.unexpected_eof())?;
                let inner_with_postfix = self.parse_postfix(inner_expr)?;
                let combined_span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner_with_postfix.span.end_line,
                    end_col: inner_with_postfix.span.end_col,
                };
                Some(Spanned {
                    span: combined_span,
                    node: Expression::BitwiseNot(boxed!(inner_with_postfix)),
                })
            }
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        Ok(expr)
    }

    pub(super) fn get_infix_child_node(
        &mut self,
    ) -> Result<Spanned<tree_node::Expression<'a>>, Error<'a>> {
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;

        let start_span = self.current_span();

        let expr = match current_token.token_type {
            TokenType::Number(_) | TokenType::Boolean(_) => {
                let lit = self.spanned(|p| p.literal())?;
                Spanned {
                    span: lit.span,
                    node: Expression::Literal(lit),
                }
            }
            TokenType::Identifier(ref ident)
                if !self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) =>
            {
                let span = self.current_span();
                Spanned {
                    span,
                    node: Expression::Variable(Spanned {
                        span,
                        node: ident.clone(),
                    }),
                }
            }
            TokenType::Symbol(Symbol::LParen) => *self
                .parenthesized_or_tuple()?
                .map(Box::new)
                .ok_or_else(|| self.unexpected_eof())?,
            TokenType::Identifier(ref id) if SysCall::is_syscall(id) => {
                let spanned_call = self.spanned(|p| p.syscall())?;

                Spanned {
                    span: spanned_call.span,
                    node: Expression::Syscall(spanned_call),
                }
            }
            TokenType::Identifier(_) if self_matches_peek!(self, TokenType::Symbol(Symbol::LParen)) => {
                let inv = self.spanned(|p| p.invocation())?;
                Spanned {
                    span: inv.span,
                    node: Expression::Invocation(inv),
                }
            }
            TokenType::Symbol(Symbol::Minus) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Spanned {
                    span,
                    node: Expression::Negation(boxed!(inner)),
                }
            }
            TokenType::Symbol(Symbol::LogicalNot) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Spanned {
                    span,
                    node: Expression::Logical(Spanned {
                        span,
                        node: LogicalExpression::Not(boxed!(inner)),
                    }),
                }
            }
            TokenType::Symbol(Symbol::BitwiseNot) => {
                self.assign_next()?;
                let inner = self.get_infix_child_node()?;
                let span = Span {
                    start_line: start_span.start_line,
                    start_col: start_span.start_col,
                    end_line: inner.span.end_line,
                    end_col: inner.span.end_col,
                };
                Spanned {
                    span,
                    node: Expression::BitwiseNot(boxed!(inner)),
                }
            }
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        self.parse_postfix(expr)
    }

    pub(super) fn infix(
        &mut self,
        previous: Spanned<Expression<'a>>,
    ) -> Result<Spanned<Expression<'a>>, Error<'a>> {
        let current_token = self
            .get_next()?
            .ok_or_else(|| self.unexpected_eof())?
            .clone();

        match previous.node {
            Expression::Binary(_)
            | Expression::Logical(_)
            | Expression::Invocation(_)
            | Expression::Syscall(_)
            | Expression::Priority(_)
            | Expression::Literal(_)
            | Expression::Variable(_)
            | Expression::Ternary(_)
            | Expression::Negation(_)
            | Expression::BitwiseNot(_)
            | Expression::MemberAccess(_)
            | Expression::MethodCall(_)
            | Expression::IndexAccess(_)
            | Expression::Tuple(_) => {}
            _ => {
                return Err(Error::InvalidSyntax(
                    self.current_span(),
                    String::from("Invalid expression for binary/logical operation"),
                ));
            }
        }

        let mut expressions = vec![previous];
        let mut operators = Vec::<Symbol>::new();

        let mut temp_token = current_token.clone();

        while token_matches!(
            temp_token,
            TokenType::Symbol(s) if s.is_operator() || s.is_comparison() || s.is_logical() || s.is_bitwise() || matches!(s, Symbol::Assign | Symbol::Question | Symbol::Colon)
        ) {
            let operator = match temp_token.token_type {
                TokenType::Symbol(s) => s,
                _ => unreachable!(),
            };
            operators.push(operator);
            self.assign_next()?;
            expressions.push(self.get_infix_child_node()?);

            temp_token = self
                .get_next()?
                .ok_or_else(|| self.unexpected_eof())?
                .clone();
        }

        if operators.len() != expressions.len() - 1 {
            return Err(Error::InvalidSyntax(
                self.current_span(),
                String::from("Invalid number of operators"),
            ));
        }

        for (i, operator) in operators.iter().enumerate().rev() {
            if operator == &Symbol::Exp {
                let right = expressions.remove(i + 1);
                let left = expressions.remove(i);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };
                expressions.insert(
                    i,
                    Spanned {
                        span,
                        node: Expression::Binary(Spanned {
                            span,
                            node: BinaryExpression::Exponent(boxed!(left), boxed!(right)),
                        }),
                    },
                );
            }
        }
        operators.retain(|symbol| symbol != &Symbol::Exp);

        macro_rules! process_binary_ops {
            ($ops:pat, $variant:ident) => {
                let mut current_iteration = 0;
                for (i, operator) in operators.iter().enumerate() {
                    if matches!(operator, $ops) {
                        let index = i - current_iteration;
                        let left = expressions.remove(index);
                        let right = expressions.remove(index);
                        let span = Span {
                            start_line: left.span.start_line,
                            start_col: left.span.start_col,
                            end_line: right.span.end_line,
                            end_col: right.span.end_col,
                        };

                        let node = match operator {
                            Symbol::Asterisk => {
                                BinaryExpression::Multiply(boxed!(left), boxed!(right))
                            }
                            Symbol::Slash => BinaryExpression::Divide(boxed!(left), boxed!(right)),
                            Symbol::Percent => {
                                BinaryExpression::Modulo(boxed!(left), boxed!(right))
                            }
                            Symbol::Plus => BinaryExpression::Add(boxed!(left), boxed!(right)),
                            Symbol::Minus => {
                                BinaryExpression::Subtract(boxed!(left), boxed!(right))
                            }
                            Symbol::LeftShift => {
                                BinaryExpression::LeftShift(boxed!(left), boxed!(right))
                            }
                            Symbol::RightShiftArithmetic => {
                                BinaryExpression::RightShiftArithmetic(boxed!(left), boxed!(right))
                            }
                            Symbol::RightShiftLogical => {
                                BinaryExpression::RightShiftLogical(boxed!(left), boxed!(right))
                            }
                            Symbol::BitwiseAnd => {
                                BinaryExpression::BitwiseAnd(boxed!(left), boxed!(right))
                            }
                            Symbol::BitwiseOr => {
                                BinaryExpression::BitwiseOr(boxed!(left), boxed!(right))
                            }
                            Symbol::Caret => {
                                BinaryExpression::BitwiseXor(boxed!(left), boxed!(right))
                            }
                            _ => unreachable!(),
                        };

                        expressions.insert(
                            index,
                            Spanned {
                                span,
                                node: Expression::Binary(Spanned { span, node }),
                            },
                        );
                        current_iteration += 1;
                    }
                }
                operators.retain(|symbol| !matches!(symbol, $ops));
            };
        }

        process_binary_ops!(
            Symbol::Slash | Symbol::Asterisk | Symbol::Percent,
            BinaryExpression
        );
        process_binary_ops!(Symbol::Plus | Symbol::Minus, BinaryExpression);
        process_binary_ops!(
            Symbol::LeftShift | Symbol::RightShiftArithmetic | Symbol::RightShiftLogical,
            BinaryExpression
        );
        process_binary_ops!(Symbol::BitwiseAnd, BinaryExpression);
        process_binary_ops!(Symbol::Caret, BinaryExpression);
        process_binary_ops!(Symbol::BitwiseOr, BinaryExpression);

        let mut current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if operator.is_comparison() && !matches!(operator, Symbol::Equal | Symbol::NotEqual) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                let node = match operator {
                    Symbol::LessThan => LogicalExpression::LessThan(boxed!(left), boxed!(right)),
                    Symbol::GreaterThan => {
                        LogicalExpression::GreaterThan(boxed!(left), boxed!(right))
                    }
                    Symbol::LessThanOrEqual => {
                        LogicalExpression::LessThanOrEqual(boxed!(left), boxed!(right))
                    }
                    Symbol::GreaterThanOrEqual => {
                        LogicalExpression::GreaterThanOrEqual(boxed!(left), boxed!(right))
                    }
                    _ => unreachable!(),
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned { span, node }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| {
            !symbol.is_comparison() || matches!(symbol, Symbol::Equal | Symbol::NotEqual)
        });

        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::Equal | Symbol::NotEqual) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                let node = match operator {
                    Symbol::Equal => LogicalExpression::Equal(boxed!(left), boxed!(right)),
                    Symbol::NotEqual => LogicalExpression::NotEqual(boxed!(left), boxed!(right)),
                    _ => unreachable!(),
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned { span, node }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::Equal | Symbol::NotEqual));

        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::LogicalAnd) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned {
                            span,
                            node: LogicalExpression::And(boxed!(left), boxed!(right)),
                        }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::LogicalAnd));

        current_iteration = 0;
        for (i, operator) in operators.iter().enumerate() {
            if matches!(operator, Symbol::LogicalOr) {
                let index = i - current_iteration;
                let left = expressions.remove(index);
                let right = expressions.remove(index);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                expressions.insert(
                    index,
                    Spanned {
                        span,
                        node: Expression::Logical(Spanned {
                            span,
                            node: LogicalExpression::Or(boxed!(left), boxed!(right)),
                        }),
                    },
                );
                current_iteration += 1;
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::LogicalOr));

        for i in (0..operators.len()).rev() {
            if matches!(operators[i], Symbol::Question) {
                if i + 1 >= operators.len() || !matches!(operators[i + 1], Symbol::Colon) {
                    return Err(Error::InvalidSyntax(
                        self.current_span(),
                        "Ternary operator '?' missing matching ':'".to_string(),
                    ));
                }

                let false_branch = expressions.remove(i + 2);
                let true_branch = expressions.remove(i + 1);
                let condition = expressions.remove(i);

                let span = Span {
                    start_line: condition.span.start_line,
                    end_line: false_branch.span.end_line,
                    start_col: condition.span.start_col,
                    end_col: false_branch.span.end_col,
                };

                let ternary_node = Spanned {
                    span,
                    node: TernaryExpression {
                        condition: Box::new(condition),
                        true_value: Box::new(true_branch),
                        false_value: Box::new(false_branch),
                    },
                };

                expressions.insert(
                    i,
                    Spanned {
                        node: Expression::Ternary(ternary_node),
                        span,
                    },
                );

                operators.remove(i);
                operators.remove(i);
            }
        }

        for (i, operator) in operators.iter().enumerate().rev() {
            if matches!(operator, Symbol::Assign) {
                let right = expressions.remove(i + 1);
                let left = expressions.remove(i);
                let span = Span {
                    start_line: left.span.start_line,
                    start_col: left.span.start_col,
                    end_line: right.span.end_line,
                    end_col: right.span.end_col,
                };

                let node = if let Expression::Tuple(tuple_expr) = &left.node {
                    let mut names = Vec::new();
                    for item in &tuple_expr.node {
                        if let Expression::Variable(var) = &item.node {
                            names.push(var.clone());
                        } else {
                            return Err(Error::InvalidSyntax(
                                item.span,
                                String::from("Tuple assignment can only contain variable names"),
                            ));
                        }
                    }

                    Expression::TupleAssignment(Spanned {
                        span,
                        node: TupleAssignmentExpression {
                            names,
                            value: boxed!(right),
                        },
                    })
                } else {
                    Expression::Assignment(Spanned {
                        span,
                        node: AssignmentExpression {
                            assignee: boxed!(left),
                            expression: boxed!(right),
                        },
                    })
                };

                expressions.insert(i, Spanned { span, node });
            }
        }
        operators.retain(|symbol| !matches!(symbol, Symbol::Assign));

        if expressions.len() != 1 || !operators.is_empty() {
            return Err(Error::InvalidSyntax(
                self.current_span(),
                String::from("Invalid number of operators"),
            ));
        }

        if token_matches!(
            temp_token,
            TokenType::Symbol(Symbol::Semicolon)
                | TokenType::Symbol(Symbol::RParen)
                | TokenType::Symbol(Symbol::Comma)
        ) {
            self.tokenizer.seek(SeekFrom::Current(-1))?;
        }

        expressions.pop().ok_or_else(|| self.unexpected_eof())
    }

    pub(super) fn parenthesized_or_tuple(
        &mut self,
    ) -> Result<Option<Spanned<tree_node::Expression<'a>>>, Error<'a>> {
        let start_span = self.current_span();
        let current_token = self
            .current_token
            .as_ref()
            .ok_or_else(|| self.unexpected_eof())?;

        if !token_matches!(current_token, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(
                self.current_span(),
                current_token.clone(),
            ));
        }

        self.assign_next()?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::RParen)) {
            self.assign_next()?;
            let end_span = self.current_span();
            let span = Span {
                start_line: start_span.start_line,
                start_col: start_span.start_col,
                end_line: end_span.end_line,
                end_col: end_span.end_col,
            };
            return Ok(Some(Spanned {
                span,
                node: Expression::Tuple(Spanned { span, node: vec![] }),
            }));
        }

        let first_expression = self.expression()?.ok_or_else(|| self.unexpected_eof())?;

        if self_matches_peek!(self, TokenType::Symbol(Symbol::Comma)) {
            let mut items = vec![first_expression];
            while self_matches_peek!(self, TokenType::Symbol(Symbol::Comma)) {
                self.assign_next()?;
                self.assign_next()?;
                items.push(self.expression()?.ok_or_else(|| self.unexpected_eof())?);
            }

            let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
            if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }

            let end_span = Self::token_to_span(&next);
            let span = Span {
                start_line: start_span.start_line,
                start_col: start_span.start_col,
                end_line: end_span.end_line,
                end_col: end_span.end_col,
            };

            Ok(Some(Spanned {
                span,
                node: Expression::Tuple(Spanned { span, node: items }),
            }))
        } else {
            let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
            if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }

            Ok(Some(Spanned {
                span: first_expression.span,
                node: Expression::Priority(boxed!(first_expression)),
            }))
        }
    }

    pub(super) fn literal(&mut self) -> Result<Literal<'a>, Error<'a>> {
        let current_token = self
            .current_token
            .clone()
            .ok_or_else(|| self.unexpected_eof())?;
        let literal = match current_token.token_type {
            TokenType::Number(num) => Literal::Number(num),
            TokenType::String(ref string) => Literal::String(string.clone()),
            TokenType::Boolean(boolean) => Literal::Boolean(boolean),
            TokenType::Symbol(Symbol::Minus) => match self.get_next()? {
                Some(Token {
                    token_type: TokenType::Number(num),
                    ..
                }) => Literal::Number(-num),
                Some(wrong_token) => {
                    return Err(Error::UnexpectedToken(
                        Self::token_to_span(&wrong_token),
                        wrong_token,
                    ));
                }
                None => return Err(self.unexpected_eof()),
            },
            _ => {
                return Err(Error::UnexpectedToken(
                    self.current_span(),
                    current_token.clone(),
                ));
            }
        };

        Ok(literal)
    }
}