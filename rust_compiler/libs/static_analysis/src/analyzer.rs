use std::borrow::Cow;

use helpers::Span;
use parser::sys_call::{SysCall, System};
use parser::tree_node::{Expression, Literal, LiteralOr, Spanned};
use tokenizer::token::{Number, Unit};

use crate::error::Error;
use crate::symbol::*;

#[cfg(test)]
mod tests;

#[derive(Default)]
pub struct Analyzer<'a> {
    pub symbol_table: SymbolTable<'a>,
    pub errors: Vec<Error>,

    is_lhs: bool,
    lhs_vars: Vec<Cow<'a, str>>,
}

impl<'a> Analyzer<'a> {
    /// Takes the root of the AST tree and analyzes it, creating a symbol table
    /// and / or populating errors along the way.
    pub fn analyze(&mut self, tree: &'a Spanned<Expression<'a>>) {
        use parser::visitor::AstVisitor;
        self.visit_expression(tree);
    }

    fn declare(&mut self, name: &'a str, kind: SymbolKind<'a>, span: Span) {
        if let Err(e) = self.symbol_table.declare(name, kind, span) {
            self.errors.push(e);
        }
    }
}

impl<'a> parser::visitor::AstVisitor<'a> for Analyzer<'a> {
    fn visit_device_declaration_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::DeviceDeclarationExpression<'a>>,
    ) {
        self.declare(
            &spanned.name.node,
            SymbolKind::Device(spanned.device.node.clone()),
            spanned.span,
        );
    }

    fn visit_function_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::FunctionExpression<'a>>,
    ) {
        self.declare(
            &spanned.name.node,
            SymbolKind::Function {
                param_count: spanned.arguments.len(),
            },
            spanned.span,
        );
        self.symbol_table.enter_scope();
        for arg in &spanned.arguments {
            self.declare(&arg.node, SymbolKind::Variable, arg.span);
        }
        self.visit_block_expression(&spanned.body);
        self.symbol_table.exit_scope();
    }

    fn visit_block_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::BlockExpression<'a>>,
    ) {
        self.symbol_table.enter_scope();
        for expr in &spanned.0 {
            self.visit_expression(expr);
        }
        self.symbol_table.exit_scope();
    }

    fn visit_const_decl_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::ConstDeclarationExpression<'a>>,
    ) {
        let var_name = &spanned.name;

        let computed_literal = match &spanned.value {
            LiteralOr::Literal(lit) => Ok(lit.node.clone()),
            LiteralOr::Or(Spanned { node: s, .. }) => match s {
                SysCall::System(System::Hash(to_hash)) => Ok(Literal::Number(Number::Integer(
                    helpers::prelude::crc_hash_signed(&to_hash.node.to_string()),
                    Unit::None,
                ))),
                _ => Err(Error::InvalidArgType {
                    error: "This syscall is not allowed here.".into(),
                    span: spanned.span,
                }),
            },
        };

        let computed_literal = match computed_literal {
            Ok(lit) => lit,
            Err(e) => {
                self.errors.push(e);
                return;
            }
        };

        self.declare(
            &var_name.node,
            SymbolKind::Constant(computed_literal),
            spanned.span,
        );
    }

    fn visit_variable(&mut self, spanned: &'a Spanned<Cow<'a, str>>) {
        let Some(symbol_id) = self.symbol_table.lookup(&spanned.node) else {
            return;
        };

        if self.is_lhs {
            self.lhs_vars.push(spanned.node.clone());
            self.symbol_table.mark_written(&symbol_id);
        } else {
            self.symbol_table.mark_read(&symbol_id);
        }
    }

    fn visit_assignent_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::AssignmentExpression<'a>>,
    ) {
        self.is_lhs = true;
        self.visit_expression(&spanned.assignee);
        self.is_lhs = false;

        let Some(lhs_var) = self.lhs_vars.pop() else {
            self.errors
                .push(Error::MissingAsignee { span: spanned.span });
            return;
        };
        if self.symbol_table.lookup(&lhs_var).is_none() {
            self.errors.push(Error::InvalidVariable {
                name: lhs_var.to_string(),
                span: spanned.assignee.span,
            });
        }

        self.visit_expression(&spanned.expression);
    }

    fn visit_return_expression(&mut self, spanned: &'a Option<Box<Spanned<Expression<'a>>>>) {
        let Some(spanned) = spanned else {
            return;
        };
        // safety check, ensure we can not return forbidden symbols like:
        // functions, arrays, blocks, etc.
        match spanned.node {
            Expression::Function(_) | Expression::Block(_) => {
                self.errors
                    .push(Error::InvalidReturnType { span: spanned.span });
            }

            _ => {}
        }
        self.visit_expression(spanned);
    }

    fn visit_declaration_expression(
        &mut self,
        name: &'a Spanned<Cow<'a, str>>,
        spanned: &'a Spanned<Expression<'a>>,
    ) {
        self.declare(&name.node, SymbolKind::Variable, name.span);
        self.visit_expression(spanned);
    }
}
