use crate::error::Error;
use helpers::Span;
use parser::tree_node::{DeviceType, Literal};
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum SymbolKind<'a> {
    /// A variable that can be read and written to
    Variable,
    /// A non-changing compile-time constant value
    Constant(Literal<'a>),
    /// A variable representing a device pin or device refId
    Device(DeviceType),
    /// A function with a specific number of parameters
    Function { param_count: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub(crate) usize);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Symbol<'a> {
    pub id: SymbolId,
    pub name: &'a str,
    pub kind: SymbolKind<'a>,
    pub scope_id: usize,
    // Diagnostic tracking
    pub is_read: bool,
    pub is_written: bool,
    pub span: Span,
}

#[derive(Debug, Default)]
pub struct Scope<'a> {
    pub id: usize,
    pub parent: Option<usize>,
    pub bindings: HashMap<&'a str, SymbolId>,
}

pub struct SymbolTable<'a> {
    /// Arena storage for all symbols across all scopes
    pub symbols: Vec<Symbol<'a>>,
    /// All scopes created during static analysis
    pub scopes: Vec<Scope<'a>>,
    /// Stack of scope IDs representing current lexical depth during AST traversal
    pub scope_stack: Vec<usize>,
}

impl<'a> Default for SymbolTable<'a> {
    fn default() -> Self {
        let mut table = Self {
            symbols: Vec::new(),
            scopes: vec![],
            scope_stack: vec![],
        };
        // Create the root (global) scope at index 0
        table.enter_scope();
        table
    }
}

impl<'a> SymbolTable<'a> {
    pub fn enter_scope(&mut self) -> usize {
        let parent = self.scope_stack.last().copied();
        let new_scope_id = self.scopes.len();

        self.scopes.push(Scope {
            id: new_scope_id,
            parent,
            bindings: HashMap::new(),
        });
        self.scope_stack.push(new_scope_id);
        new_scope_id
    }

    pub fn get(&self, symbol_id: &SymbolId) -> Option<Symbol<'a>> {
        self.symbols.get(symbol_id.0).cloned()
    }

    pub fn exit_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    pub fn current_scope_id(&self) -> usize {
        *self.scope_stack.last().unwrap_or(&0)
    }

    pub fn declare(
        &mut self,
        name: &'a str,
        kind: SymbolKind<'a>,
        span: Span,
    ) -> Result<SymbolId, Error> {
        let current_scope_index = self.current_scope_id();
        let current_scope = &self.scopes[current_scope_index];

        if let Some(original) = current_scope.bindings.get(name) {
            let original_span = &self.symbols[original.0].span;

            return Err(Error::DuplicateDeclaration {
                name: name.to_string(),
                original: *original_span,
                current: span,
            });
        }

        let id = SymbolId(self.symbols.len());

        let symbol = Symbol {
            id,
            name,
            kind,
            scope_id: current_scope_index,
            is_read: false,
            is_written: false,
            span,
        };

        self.symbols.push(symbol);
        self.scopes[current_scope_index].bindings.insert(name, id);

        Ok(id)
    }

    pub fn mark_read(&mut self, symbol_id: &SymbolId) {
        self.symbols[symbol_id.0].is_read = true;
    }

    pub fn mark_written(&mut self, symbol_id: &SymbolId) {
        self.symbols[symbol_id.0].is_written = true;
    }

    pub fn lookup(&mut self, name: &str) -> Option<SymbolId> {
        let mut current_scope_opt = self.scope_stack.last().copied();

        while let Some(scope_idx) = current_scope_opt {
            let scope = &self.scopes[scope_idx];

            if let Some(&symbol_id) = scope.bindings.get(&name) {
                return Some(symbol_id);
            }

            current_scope_opt = scope.parent;
        }

        None
    }
}
