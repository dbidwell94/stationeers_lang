use super::*;

impl<'a> Parser<'a> {
    pub(super) fn if_expression(&mut self) -> Result<IfExpression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or_else(|| self.unexpected_eof())?;
        self.validate_condition_expression(&condition)?;

        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let body = self.spanned(|p| p.block())?;

        let else_branch = if self_matches_peek!(self, TokenType::Keyword(Keyword::Else)) {
            self.assign_next()?;

            if self_matches_peek!(self, TokenType::Keyword(Keyword::If)) {
                self.assign_next()?;
                let if_expr = self.spanned(|p| p.if_expression())?;
                Some(boxed!(Spanned {
                    span: if_expr.span,
                    node: Expression::If(if_expr),
                }))
            } else if self_matches_peek!(self, TokenType::Symbol(Symbol::LBrace)) {
                self.assign_next()?;
                let block = self.spanned(|p| p.block())?;
                Some(boxed!(Spanned {
                    span: block.span,
                    node: Expression::Block(block),
                }))
            } else {
                let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
                return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
            }
        } else {
            None
        };

        Ok(IfExpression {
            condition: boxed!(condition),
            body,
            else_branch,
        })
    }

    pub(super) fn loop_expression(&mut self) -> Result<LoopExpression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let body = self.spanned(|p| p.block())?;

        Ok(LoopExpression { body })
    }

    pub(super) fn while_expression(&mut self) -> Result<WhileExpression<'a>, Error<'a>> {
        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }
        self.assign_next()?;

        let condition = self.expression()?.ok_or_else(|| self.unexpected_eof())?;
        self.validate_condition_expression(&condition)?;

        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::RParen)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        let next = self.get_next()?.ok_or_else(|| self.unexpected_eof())?;
        if !token_matches!(next, TokenType::Symbol(Symbol::LBrace)) {
            return Err(Error::UnexpectedToken(Self::token_to_span(&next), next));
        }

        Ok(WhileExpression {
            condition: boxed!(condition),
            body: self.spanned(|p| p.block())?,
        })
    }

    pub(super) fn validate_condition_expression(
        &self,
        expression: &Spanned<tree_node::Expression<'a>>,
    ) -> Result<(), Error<'a>> {
        if self.condition_contains_assignment(expression) {
            return Err(Error::InvalidSyntax(
                expression.span,
                String::from("Assignment expressions are not allowed in condition expressions"),
            ));
        }

        Ok(())
    }

    pub(super) fn condition_contains_assignment(
        &self,
        expression: &Spanned<tree_node::Expression<'a>>,
    ) -> bool {
        match &expression.node {
            Expression::Assignment(_) | Expression::TupleAssignment(_) => true,
            Expression::Binary(binary) => match &binary.node {
                BinaryExpression::Add(left, right)
                | BinaryExpression::Multiply(left, right)
                | BinaryExpression::Divide(left, right)
                | BinaryExpression::Subtract(left, right)
                | BinaryExpression::Exponent(left, right)
                | BinaryExpression::Modulo(left, right)
                | BinaryExpression::BitwiseAnd(left, right)
                | BinaryExpression::BitwiseOr(left, right)
                | BinaryExpression::BitwiseXor(left, right)
                | BinaryExpression::LeftShift(left, right)
                | BinaryExpression::RightShiftArithmetic(left, right)
                | BinaryExpression::RightShiftLogical(left, right) => {
                    self.condition_contains_assignment(left)
                        || self.condition_contains_assignment(right)
                }
            },
            Expression::Logical(logical) => match &logical.node {
                LogicalExpression::And(left, right)
                | LogicalExpression::Or(left, right)
                | LogicalExpression::Equal(left, right)
                | LogicalExpression::NotEqual(left, right)
                | LogicalExpression::GreaterThan(left, right)
                | LogicalExpression::GreaterThanOrEqual(left, right)
                | LogicalExpression::LessThan(left, right)
                | LogicalExpression::LessThanOrEqual(left, right) => {
                    self.condition_contains_assignment(left)
                        || self.condition_contains_assignment(right)
                }
                LogicalExpression::Not(inner) => self.condition_contains_assignment(inner),
            },
            Expression::BitwiseNot(inner)
            | Expression::Negation(inner)
            | Expression::Priority(inner) => self.condition_contains_assignment(inner),
            Expression::Ternary(ternary) => {
                self.condition_contains_assignment(&ternary.condition)
                    || self.condition_contains_assignment(&ternary.true_value)
                    || self.condition_contains_assignment(&ternary.false_value)
            }
            Expression::Tuple(tuple_items) => tuple_items
                .node
                .iter()
                .any(|item| self.condition_contains_assignment(item)),
            Expression::Invocation(invocation) => invocation
                .arguments
                .iter()
                .any(|arg| self.condition_contains_assignment(arg)),
            Expression::MethodCall(method_call) => {
                self.condition_contains_assignment(&method_call.object)
                    || method_call
                        .arguments
                        .iter()
                        .any(|arg| self.condition_contains_assignment(arg))
            }
            Expression::MemberAccess(member_access) => {
                self.condition_contains_assignment(&member_access.object)
            }
            Expression::IndexAccess(index_access) => {
                self.condition_contains_assignment(&index_access.object)
                    || self.condition_contains_assignment(&index_access.index)
            }
            Expression::Declaration(_, value) => self.condition_contains_assignment(value),
            Expression::ConstDeclaration(_) => false,
            Expression::TupleDeclaration(tuple_decl) => {
                self.condition_contains_assignment(&tuple_decl.value)
            }
            Expression::Return(Some(value)) => self.condition_contains_assignment(value),
            Expression::If(_)
            | Expression::While(_)
            | Expression::Loop(_)
            | Expression::Function(_)
            | Expression::Block(_)
            | Expression::Break(_)
            | Expression::Continue(_)
            | Expression::Return(None)
            | Expression::DeviceDeclaration(_)
            | Expression::Syscall(_)
            | Expression::Literal(_)
            | Expression::Variable(_) => false,
        }
    }
}
