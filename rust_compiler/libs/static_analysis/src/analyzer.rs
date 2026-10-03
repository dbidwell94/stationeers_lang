use std::borrow::Cow;
use std::collections::HashMap;

use helpers::Span;
use parser::sys_call::{SysCall, System};
use parser::tree_node::DeviceType;
use parser::tree_node::{ArrayRepeatExpression, Expression, Literal, LiteralOr, Spanned};
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
    Array,
}

impl ParameterKind {
    fn as_str(self) -> &'static str {
        match self {
            ParameterKind::Unknown => "unknown",
            ParameterKind::Value => "value",
            ParameterKind::DevicePin => "device pin",
            ParameterKind::DeviceReference => "device reference",
            ParameterKind::DeviceHousing => "device housing",
            ParameterKind::Array => "array",
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunctionMetadata<'a> {
    pub symbol: Symbol<'a>,
    pub parameter_kinds: Vec<ParameterKind>,
    pub parameter_symbols: Vec<SymbolId>,
    pub call_sites: Vec<Span>,
}

#[cfg(test)]
mod tests;

#[derive(Clone)]
pub struct AnalyzeResult<'a> {
    pub symbol_table: SymbolTable<'a>,
    pub functions: HashMap<SymbolId, FunctionMetadata<'a>>,
    pub documentation: HashMap<SymbolId, String>,
    /// Whether the program declares any arrays. Used by the compiler to decide
    /// whether to reserve the array region of the `db` stack at all.
    pub uses_arrays: bool,
}

#[derive(Default)]
pub struct Analyzer<'a> {
    symbol_table: SymbolTable<'a>,
    errors: Vec<Error>,
    functions: HashMap<SymbolId, FunctionMetadata<'a>>,

    is_lhs: bool,
    lhs_vars: Vec<Cow<'a, str>>,
    uses_arrays: bool,
    parameter_symbol_owner: HashMap<SymbolId, (SymbolId, usize)>,
    parameter_forwarding: Vec<(SymbolId, usize, SymbolId, usize, Span)>,
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
        self.propagate_array_parameter_kinds();

        if self.errors.is_empty() {
            Ok(AnalyzeResult {
                symbol_table: self.symbol_table,
                functions: self.functions,
                documentation: HashMap::new(),
                uses_arrays: self.uses_arrays,
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
                parameter_symbols: Vec::new(),
                call_sites: Vec::new(),
            });
    }

    fn symbol_kind_for_declaration(&mut self, expr: &'a Spanned<Expression<'a>>) -> SymbolKind<'a> {
        match &expr.node {
            Expression::ArrayLiteral(_) | Expression::ArrayRepeat(_) => SymbolKind::Array,
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
            Expression::Variable(name) => {
                let Some(symbol_id) = self.symbol_table.lookup(&name.node) else {
                    return ParameterKind::Unknown;
                };
                if let Some((function_id, parameter_index)) =
                    self.parameter_symbol_owner.get(&symbol_id)
                {
                    return self
                        .functions
                        .get(function_id)
                        .and_then(|metadata| metadata.parameter_kinds.get(*parameter_index))
                        .copied()
                        .unwrap_or(ParameterKind::Unknown);
                }
                self.symbol_table
                    .get(&symbol_id)
                    .map(|symbol| Self::parameter_kind_from_symbol_kind(&symbol.kind))
                    .unwrap_or(ParameterKind::Unknown)
            }
            Expression::Binary(_)
            | Expression::BitwiseNot(_)
            | Expression::IndexAccess(_)
            | Expression::Logical(_)
            | Expression::MemberAccess(_)
            | Expression::Negation(_)
            | Expression::Syscall(_)
            | Expression::Ternary(_)
            | Expression::Tuple(_) => ParameterKind::Value,
            Expression::ArrayLiteral(_) | Expression::ArrayRepeat(_) => ParameterKind::Array,
            Expression::Priority(inner) => self.infer_argument_kind(inner),
            _ => ParameterKind::Unknown,
        }
    }

    fn parameter_kind_from_symbol_kind(kind: &SymbolKind<'a>) -> ParameterKind {
        match kind {
            SymbolKind::Device(DeviceType::Pin(_)) => ParameterKind::DevicePin,
            SymbolKind::Device(DeviceType::Reference(_)) => ParameterKind::DeviceReference,
            SymbolKind::Device(DeviceType::Housing) => ParameterKind::DeviceHousing,
            SymbolKind::Array => ParameterKind::Array,
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
            if inferred_kind == ParameterKind::Array
                && let Some(parameter_symbol_id) = metadata.parameter_symbols.get(parameter_index)
                && let Some(parameter_symbol) =
                    self.symbol_table.symbols.get_mut(parameter_symbol_id.0)
            {
                parameter_symbol.kind = SymbolKind::Array;
            }
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
        } else if inferred_kind == ParameterKind::Array
            && let Some(parameter_symbol_id) = metadata.parameter_symbols.get(parameter_index)
            && let Some(parameter_symbol) = self.symbol_table.symbols.get_mut(parameter_symbol_id.0)
        {
            parameter_symbol.kind = SymbolKind::Array;
        }
    }

    fn propagate_array_parameter_kinds(&mut self) {
        let mut reported_conflicts = std::collections::HashSet::new();
        loop {
            let mut changed = false;
            for (target_id, target_index, source_id, source_index, _span) in
                self.parameter_forwarding.iter().copied()
            {
                let source_is_array = self
                    .functions
                    .get(&source_id)
                    .and_then(|metadata| metadata.parameter_kinds.get(source_index))
                    == Some(&ParameterKind::Array);
                if !source_is_array {
                    continue;
                }

                let target_kind = self
                    .functions
                    .get(&target_id)
                    .and_then(|metadata| metadata.parameter_kinds.get(target_index))
                    .copied();
                if let Some(kind) = target_kind
                    && kind != ParameterKind::Unknown
                    && kind != ParameterKind::Array
                    && reported_conflicts.insert((target_id, target_index, source_id, source_index))
                {
                    let target_name = self
                        .functions
                        .get(&target_id)
                        .map(|metadata| metadata.symbol.name.to_string())
                        .unwrap_or_default();
                    self.errors.push(Error::ConflictingFunctionParameterType {
                        function: target_name,
                        parameter_index: target_index,
                        expected: kind.as_str().to_string(),
                        actual: ParameterKind::Array.as_str().to_string(),
                        span: _span,
                    });
                }

                let parameter_symbol_id = if let Some(target) = self.functions.get_mut(&target_id)
                    && let Some(target_kind) = target.parameter_kinds.get_mut(target_index)
                    && *target_kind == ParameterKind::Unknown
                {
                    *target_kind = ParameterKind::Array;
                    target.parameter_symbols.get(target_index).copied()
                } else {
                    None
                };
                if let Some(parameter_symbol_id) = parameter_symbol_id {
                    if let Some(parameter_symbol) =
                        self.symbol_table.symbols.get_mut(parameter_symbol_id.0)
                    {
                        parameter_symbol.kind = SymbolKind::Array;
                    }
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }
}

impl<'a> parser::visitor::AstVisitor<'a> for Analyzer<'a> {
    fn visit_array_literal_expression(
        &mut self,
        spanned: &'a Spanned<Vec<Spanned<Expression<'a>>>>,
    ) {
        self.uses_arrays = true;
        for expr in &spanned.node {
            self.visit_expression(expr);
        }
    }

    fn visit_array_repeat_expression(&mut self, spanned: &'a Spanned<ArrayRepeatExpression<'a>>) {
        self.uses_arrays = true;
        self.visit_expression(&spanned.node.size);
        if let Some(fill) = &spanned.node.fill {
            self.visit_expression(fill);
        }
    }

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
        let mut parameter_symbols = Vec::with_capacity(spanned.arguments.len());
        for arg in &spanned.arguments {
            self.declare(&arg.node, SymbolKind::Variable, arg.span);
            if let Some(symbol_id) = self.symbol_table.lookup(&arg.node) {
                parameter_symbols.push(symbol_id);
            }
        }
        if let Some(function_symbol_id) = self.symbol_table.lookup(&spanned.name.node)
            && let Some(metadata) = self.functions.get_mut(&function_symbol_id)
        {
            metadata.parameter_symbols = parameter_symbols;
        }
        if let Some(function_symbol_id) = self.symbol_table.lookup(&spanned.name.node)
            && let Some(metadata) = self.functions.get(&function_symbol_id)
        {
            for (parameter_index, parameter_symbol_id) in
                metadata.parameter_symbols.iter().copied().enumerate()
            {
                self.parameter_symbol_owner
                    .insert(parameter_symbol_id, (function_symbol_id, parameter_index));
            }
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
            if let Expression::Variable(name) = &argument.node
                && let Some(source_symbol_id) = self.symbol_table.lookup(&name.node)
                && let Some((source_function_id, source_parameter_index)) =
                    self.parameter_symbol_owner.get(&source_symbol_id).copied()
            {
                self.parameter_forwarding.push((
                    function_symbol.id,
                    index,
                    source_function_id,
                    source_parameter_index,
                    argument.span,
                ));
            }
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
