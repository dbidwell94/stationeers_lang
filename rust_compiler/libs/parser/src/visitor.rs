use crate::{
    sys_call::SysCall,
    tree_node::{
        AssignmentExpression, BinaryExpression, BlockExpression, ConstDeclarationExpression,
        DeviceDeclarationExpression, Expression, FunctionExpression, IfExpression,
        IndexAccessExpression, InvocationExpression, Literal, LogicalExpression, LoopExpression,
        MemberAccessExpression, MethodCallExpression, Spanned, TernaryExpression,
        TupleAssignmentExpression, TupleDeclarationExpression, WhileExpression,
    },
};
use helpers::Span;
use std::borrow::Cow;

/// Helper trait to aid in the traversal of the AST
pub trait AstVisitor<'a>: Sized {
    fn visit_expression(&mut self, spanned: &'a Spanned<Expression<'a>>) {
        walk_expression(self, spanned);
    }

    fn visit_const_decl_expression(
        &mut self,
        _spanned: &'a Spanned<ConstDeclarationExpression<'a>>,
    ) {
    }

    fn visit_literal(&mut self, _spanned: &'a Spanned<Literal<'a>>) {}

    fn visit_variable(&mut self, _spanned: &'a Spanned<Cow<'a, str>>) {}

    fn visit_assignent_expression(&mut self, spanned: &'a Spanned<AssignmentExpression<'a>>) {
        self.visit_expression(&spanned.node.assignee);
        self.visit_expression(&spanned.node.expression);
    }

    fn visit_binary_expression(&mut self, spanned: &'a Spanned<BinaryExpression<'a>>) {
        walk_binary_expression(self, spanned);
    }

    fn visit_block_expression(&mut self, spanned: &'a Spanned<BlockExpression<'a>>) {
        for expr in &spanned.0 {
            self.visit_expression(expr);
        }
    }

    fn visit_break_expression(&mut self, _span: &Span) {}

    fn visit_bitwise_not(&mut self, spanned: &'a Spanned<Expression<'a>>) {
        self.visit_expression(spanned);
    }

    fn visit_continue_expression(&mut self, _span: &Span) {}

    fn visit_declaration_expression(
        &mut self,
        _name: &'a Spanned<Cow<'a, str>>,
        spanned: &'a Spanned<Expression<'a>>,
    ) {
        self.visit_expression(spanned);
    }

    fn visit_device_declaration_expression(
        &mut self,
        _spanned: &'a Spanned<DeviceDeclarationExpression<'a>>,
    ) {
    }

    fn visit_function_expression(&mut self, spanned: &'a Spanned<FunctionExpression<'a>>) {
        self.visit_block_expression(&spanned.body);
    }

    fn visit_if_expression(&mut self, spanned: &'a Spanned<IfExpression<'a>>) {
        self.visit_expression(&spanned.condition);
        self.visit_block_expression(&spanned.body);
        if let Some(ref exp) = spanned.else_branch {
            self.visit_expression(exp);
        }
    }

    fn visit_invocation_expression(&mut self, spanned: &'a Spanned<InvocationExpression<'a>>) {
        for expr in &spanned.arguments {
            self.visit_expression(expr);
        }
    }

    fn visit_logical_expression(&mut self, spanned: &'a Spanned<LogicalExpression<'a>>) {
        walk_logical_expression(self, spanned);
    }

    fn visit_loop_expression(&mut self, spanned: &'a Spanned<LoopExpression<'a>>) {
        self.visit_block_expression(&spanned.body);
    }

    fn visit_negation_expression(&mut self, spanned: &'a Spanned<Expression<'a>>) {
        self.visit_expression(spanned);
    }

    fn visit_member_access_expression(&mut self, spanned: &'a Spanned<MemberAccessExpression<'a>>) {
        self.visit_expression(&spanned.object);
    }

    fn visit_method_call_expression(&mut self, spanned: &'a Spanned<MethodCallExpression<'a>>) {
        self.visit_expression(&spanned.object);
        for expr in &spanned.arguments {
            self.visit_expression(expr);
        }
    }

    fn visit_priority_expression(&mut self, spanned: &'a Spanned<Expression<'a>>) {
        self.visit_expression(spanned);
    }

    fn visit_return_expression(&mut self, spanned: &'a Option<Box<Spanned<Expression<'a>>>>) {
        if let Some(exp) = spanned {
            self.visit_expression(exp);
        }
    }

    fn visit_syscall_expression(&mut self, spanned: &'a Spanned<SysCall<'a>>) {
        walk_syscall_expression(self, spanned);
    }

    fn visit_ternary_expression(&mut self, spanned: &'a Spanned<TernaryExpression<'a>>) {
        self.visit_expression(&spanned.condition);
        self.visit_expression(&spanned.true_value);
        self.visit_expression(&spanned.false_value);
    }

    fn visit_tuple_expression(&mut self, spanned: &'a Spanned<Vec<Spanned<Expression<'a>>>>) {
        for expr in &spanned.node {
            self.visit_expression(expr);
        }
    }

    fn visit_tuple_assignment_expression(
        &mut self,
        spanned: &'a Spanned<TupleAssignmentExpression<'a>>,
    ) {
        self.visit_expression(&spanned.value);
    }

    fn visit_tuple_declaration_expression(
        &mut self,
        spanned: &'a Spanned<TupleDeclarationExpression<'a>>,
    ) {
        self.visit_expression(&spanned.value);
    }

    fn visit_while_expression(&mut self, spanned: &'a Spanned<WhileExpression<'a>>) {
        self.visit_expression(&spanned.condition);
        self.visit_block_expression(&spanned.body);
    }

    fn visit_index_access_expression(&mut self, spanned: &'a Spanned<IndexAccessExpression<'a>>) {
        self.visit_expression(&spanned.object);
        self.visit_expression(&spanned.index);
    }
}

/// Walks through a binary expression, calling the appropriate visitor functions for l and r nodes
pub fn walk_binary_expression<'a, V: AstVisitor<'a>>(
    visitor: &mut V,
    spanned: &'a Spanned<BinaryExpression<'a>>,
) {
    let (l, r) = match &spanned.node {
        BinaryExpression::Add(l, r)
        | BinaryExpression::Multiply(l, r)
        | BinaryExpression::Divide(l, r)
        | BinaryExpression::Subtract(l, r)
        | BinaryExpression::Exponent(l, r)
        | BinaryExpression::Modulo(l, r)
        | BinaryExpression::BitwiseAnd(l, r)
        | BinaryExpression::BitwiseOr(l, r)
        | BinaryExpression::BitwiseXor(l, r)
        | BinaryExpression::LeftShift(l, r)
        | BinaryExpression::RightShiftArithmetic(l, r)
        | BinaryExpression::RightShiftLogical(l, r) => (l, r),
    };

    visitor.visit_expression(l);
    visitor.visit_expression(r);
}

/// Walks through a binary expression, calling the appropriate visitor functions for l and r nodes
pub fn walk_logical_expression<'a, V: AstVisitor<'a>>(
    visitor: &mut V,
    spanned: &'a Spanned<LogicalExpression<'a>>,
) {
    let (l, r) = match &spanned.node {
        LogicalExpression::And(l, r)
        | LogicalExpression::Or(l, r)
        | LogicalExpression::NotEqual(l, r)
        | LogicalExpression::Equal(l, r)
        | LogicalExpression::GreaterThan(l, r)
        | LogicalExpression::LessThan(l, r)
        | LogicalExpression::GreaterThanOrEqual(l, r)
        | LogicalExpression::LessThanOrEqual(l, r) => (Some(l), Some(r)),
        LogicalExpression::Not(l) => (Some(l), None),
    };

    if let Some(l) = l {
        visitor.visit_expression(l);
    }
    if let Some(r) = r {
        visitor.visit_expression(r);
    }
}

pub fn walk_syscall_expression<'a, V: AstVisitor<'a>>(
    visitor: &mut V,
    spanned: &'a Spanned<SysCall<'a>>,
) {
    spanned.walk(visitor);
}

/// Calls the appropriate visitor function for each `Expression` type
pub fn walk_expression<'a, V: AstVisitor<'a>>(
    visitor: &mut V,
    spanned: &'a Spanned<Expression<'a>>,
) {
    match &spanned.node {
        Expression::ConstDeclaration(expr) => visitor.visit_const_decl_expression(expr),
        Expression::Literal(lit) => visitor.visit_literal(lit),
        Expression::Assignment(expr) => visitor.visit_assignent_expression(expr),
        Expression::Binary(expr) => visitor.visit_binary_expression(expr),
        Expression::Block(expr) => visitor.visit_block_expression(expr),
        Expression::Break(expr) => visitor.visit_break_expression(expr),
        Expression::BitwiseNot(expr) => visitor.visit_bitwise_not(expr),
        Expression::Continue(span) => visitor.visit_continue_expression(span),
        Expression::Declaration(name, expr) => visitor.visit_declaration_expression(name, expr),
        Expression::DeviceDeclaration(expr) => visitor.visit_device_declaration_expression(expr),
        Expression::Function(expr) => visitor.visit_function_expression(expr),
        Expression::If(exp) => visitor.visit_if_expression(exp),
        Expression::Invocation(exp) => visitor.visit_invocation_expression(exp),
        Expression::Logical(exp) => visitor.visit_logical_expression(exp),
        Expression::Loop(exp) => visitor.visit_loop_expression(exp),
        Expression::Negation(exp) => visitor.visit_negation_expression(exp),
        Expression::MemberAccess(exp) => visitor.visit_member_access_expression(exp),
        Expression::MethodCall(exp) => visitor.visit_method_call_expression(exp),
        Expression::Priority(exp) => visitor.visit_priority_expression(exp),
        Expression::Return(exp) => visitor.visit_return_expression(exp),
        Expression::Syscall(exp) => visitor.visit_syscall_expression(exp),
        Expression::Ternary(exp) => visitor.visit_ternary_expression(exp),
        Expression::Tuple(spanned) => visitor.visit_tuple_expression(spanned),
        Expression::TupleAssignment(exp) => visitor.visit_tuple_assignment_expression(exp),
        Expression::TupleDeclaration(exp) => visitor.visit_tuple_declaration_expression(exp),
        Expression::Variable(spanned) => visitor.visit_variable(spanned),
        Expression::While(exp) => visitor.visit_while_expression(exp),
        Expression::IndexAccess(exp) => visitor.visit_index_access_expression(exp),
    }
}
