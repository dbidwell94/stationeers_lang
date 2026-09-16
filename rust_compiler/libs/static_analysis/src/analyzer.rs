use std::borrow::Cow;
use std::collections::HashMap;

use helpers::Span;
use parser::sys_call::{SysCall, System};
use parser::tree_node::DeviceType;
use parser::tree_node::{Expression, Literal, LiteralOr, Spanned};
use tokenizer::token::{Number, Unit};

use crate::error::{AnalyzeErrors, Error};
use crate::symbol::*;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum ParameterKind {
    #[default]
    Unknown,
    Value,
    DevicePin,
    DeviceReference,
    DeviceHousing,
}

impl ParameterKind {
    fn as_str(self) -> &'static str {
        match self {
            ParameterKind::Unknown => "unknown",
            ParameterKind::Value => "value",
            ParameterKind::DevicePin => "device pin",
            ParameterKind::DeviceReference => "device reference",
            ParameterKind::DeviceHousing => "device housing",
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata<'a> {
    pub symbol: Symbol<'a>,
    pub parameter_kinds: Vec<ParameterKind>,
    pub call_sites: Vec<Span>,
}

#[cfg(test)]
mod tests;

pub struct AnalyzeResult<'a> {
    pub symbol_table: SymbolTable<'a>,
    pub functions: HashMap<SymbolId, FunctionMetadata<'a>>,
    pub documentation: HashMap<SymbolId, String>,
}

#[derive(Default)]
pub struct Analyzer<'a> {
    symbol_table: SymbolTable<'a>,
    errors: Vec<Error>,
    functions: HashMap<SymbolId, FunctionMetadata<'a>>,

    is_lhs: bool,
    lhs_vars: Vec<Cow<'a, str>>,
}

impl<'a> Analyzer<'a> {
    /// Takes the root of the AST tree and analyzes it, creating a symbol table
    /// and / or populating errors along the way.
    pub fn analyze(
        mut self,
        tree: &'a Spanned<Expression<'a>>,
    ) -> Result<AnalyzeResult<'a>, AnalyzeErrors> {
        use parser::visitor::AstVisitor;
        self.visit_expression(tree);

        if self.errors.is_empty() {
            Ok(AnalyzeResult {
                symbol_table: self.symbol_table,
                functions: self.functions,
                documentation: HashMap::new(),
            })
        } else {
            Err(AnalyzeErrors(self.errors))
        }
    }

    fn declare(&mut self, name: &'a str, kind: SymbolKind<'a>, span: Span) {
        if let Err(e) = self.symbol_table.declare(name, kind, span) {
            self.errors.push(e);
        }
    }

    fn ensure_function_metadata(&mut self, symbol: Symbol<'a>) {
        let param_count = match symbol.kind {
            SymbolKind::Function { param_count } => param_count,
            _ => return,
        };

        self.functions
            .entry(symbol.id)
            .or_insert_with(|| FunctionMetadata {
                symbol,
                parameter_kinds: vec![ParameterKind::Unknown; param_count],
                call_sites: Vec::new(),
            });
    }

    fn symbol_kind_for_declaration(&mut self, expr: &'a Spanned<Expression<'a>>) -> SymbolKind<'a> {
        match &expr.node {
            Expression::Variable(name) => self
                .symbol_table
                .lookup(&name.node)
                .and_then(|id| self.symbol_table.get(&id))
                .map(|symbol| match symbol.kind {
                    SymbolKind::Device(device) => SymbolKind::Device(device),
                    _ => SymbolKind::Variable,
                })
                .unwrap_or(SymbolKind::Variable),
            Expression::Priority(inner) => self.symbol_kind_for_declaration(inner),
            _ => SymbolKind::Variable,
        }
    }

    fn infer_argument_kind(&mut self, expr: &'a Spanned<Expression<'a>>) -> ParameterKind {
        match &expr.node {
            Expression::Literal(_) => ParameterKind::Value,
            Expression::Variable(name) => self
                .symbol_table
                .lookup(&name.node)
                .and_then(|id| self.symbol_table.get(&id))
                .map(|symbol| Self::parameter_kind_from_symbol_kind(&symbol.kind))
                .unwrap_or(ParameterKind::Unknown),
            Expression::Binary(_)
            | Expression::BitwiseNot(_)
            | Expression::IndexAccess(_)
            | Expression::Logical(_)
            | Expression::MemberAccess(_)
            | Expression::Negation(_)
            | Expression::Syscall(_)
            | Expression::Ternary(_)
            | Expression::Tuple(_) => ParameterKind::Value,
            Expression::Priority(inner) => self.infer_argument_kind(inner),
            _ => ParameterKind::Unknown,
        }
    }

    fn parameter_kind_from_symbol_kind(kind: &SymbolKind<'a>) -> ParameterKind {
        match kind {
            SymbolKind::Device(DeviceType::Pin(_)) => ParameterKind::DevicePin,
            SymbolKind::Device(DeviceType::Reference(_)) => ParameterKind::DeviceReference,
            SymbolKind::Device(DeviceType::Housing) => ParameterKind::DeviceHousing,
            SymbolKind::Function { .. } => ParameterKind::Unknown,
            _ => ParameterKind::Value,
        }
    }

    fn merge_parameter_kind(
        &mut self,
        function_symbol: Symbol<'a>,
        parameter_index: usize,
        inferred_kind: ParameterKind,
        span: Span,
    ) {
        if inferred_kind == ParameterKind::Unknown {
            return;
        }

        self.ensure_function_metadata(function_symbol.clone());
        let Some(metadata) = self.functions.get_mut(&function_symbol.id) else {
            return;
        };

        let Some(existing_kind) = metadata.parameter_kinds.get_mut(parameter_index) else {
            return;
        };

        if *existing_kind == ParameterKind::Unknown {
            *existing_kind = inferred_kind;
            return;
        }

        if *existing_kind != inferred_kind {
            self.errors.push(Error::ConflictingFunctionParameterType {
                function: function_symbol.name.to_string(),
                parameter_index,
                expected: existing_kind.as_str().to_string(),
                actual: inferred_kind.as_str().to_string(),
                span,
            });
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

        if let Some(symbol_id) = self.symbol_table.lookup(&spanned.name.node)
            && let Some(symbol) = self.symbol_table.get(&symbol_id)
        {
            self.ensure_function_metadata(symbol);
        }

        self.symbol_table.enter_scope();
        for arg in &spanned.arguments {
            self.declare(&arg.node, SymbolKind::Variable, arg.span);
        }
        self.visit_block_expression(&spanned.body);
        self.symbol_table.exit_scope();
    }

    fn visit_invocation_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::InvocationExpression<'a>>,
    ) {
        let Some(function_symbol_id) = self.symbol_table.lookup(&spanned.name.node) else {
            self.errors.push(Error::MissingSymbol {
                name: spanned.name.node.to_string(),
                span: spanned.name.span,
            });
            return;
        };

        let Some(function_symbol) = self.symbol_table.get(&function_symbol_id) else {
            self.errors.push(Error::MissingSymbol {
                name: spanned.name.node.to_string(),
                span: spanned.name.span,
            });
            return;
        };

        self.ensure_function_metadata(function_symbol.clone());
        if let Some(metadata) = self.functions.get_mut(&function_symbol.id) {
            metadata.call_sites.push(spanned.span);
        }

        for (index, argument) in spanned.arguments.iter().enumerate() {
            let inferred_kind = self.infer_argument_kind(argument);
            self.merge_parameter_kind(function_symbol.clone(), index, inferred_kind, argument.span);
            self.visit_expression(argument);
        }
    }

    fn visit_block_expression(
        &mut self,
        spanned: &'a Spanned<parser::tree_node::BlockExpression<'a>>,
    ) {
        self.symbol_table.enter_scope();

        for expr in spanned.hoisted() {
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
        let kind = self.symbol_kind_for_declaration(spanned);
        self.declare(&name.node, kind, name.span);
        self.visit_expression(spanned);
    }
}
