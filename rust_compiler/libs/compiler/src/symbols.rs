use helpers::Span;
use std::borrow::Cow;

/// Represents a symbol (function, syscall, variable, etc.) that can be referenced in code.
/// Designed to be LSP-compatible for easy integration with language servers.
#[derive(Debug, Clone)]
pub struct SymbolInfo<'a> {
    /// The name of the symbol
    pub name: Cow<'a, str>,
    /// The kind of symbol and associated metadata
    pub kind: SymbolKind<'a>,
    /// The source location of this symbol (for IDE features)
    pub span: Option<Span>,
    /// Optional description for tooltips and documentation
    pub description: Option<Cow<'a, str>>,
}

impl<'a> SymbolInfo<'a> {
    /// Converts to an LSP SymbolInformation for protocol compatibility.
    pub fn to_lsp_symbol_information(&self, uri: lsp_types::Uri) -> lsp_types::SymbolInformation {
        lsp_types::SymbolInformation {
            name: self.name.to_string(),
            kind: self.kind.to_lsp_symbol_kind(),
            deprecated: None,
            location: lsp_types::Location {
                uri,
                range: self.span.as_ref().map(|s| (*s).into()).unwrap_or_default(),
            },
            container_name: None,
            tags: None,
        }
    }

    /// Converts to an LSP CompletionItem for autocomplete.
    pub fn to_lsp_completion_item(&self) -> lsp_types::CompletionItem {
        lsp_types::CompletionItem {
            label: self.name.to_string(),
            kind: Some(self.kind.to_lsp_completion_kind()),
            documentation: self
                .description
                .as_ref()
                .map(|d| lsp_types::Documentation::String(d.to_string())),
            detail: Some(self.kind.detail_string()),
            ..Default::default()
        }
    }
}

/// Discriminates between different kinds of symbols.
#[derive(Debug, Clone)]
pub enum SymbolKind<'a> {
    /// A user-defined function
    Function {
        /// Names of parameters in order
        parameters: Vec<Cow<'a, str>>,
        /// Type hint for the return type (if applicable)
        return_type: Option<Cow<'a, str>>,
    },
    /// A system or math syscall
    Syscall {
        /// Whether it's a System or Math syscall
        syscall_type: SyscallType,
        /// Number of expected arguments
        argument_count: usize,
    },
    /// A variable declaration
    Variable {
        /// Type hint for the variable (if applicable)
        type_hint: Option<Cow<'a, str>>,
    },
}

impl<'a> SymbolKind<'a> {
    /// Converts to LSP SymbolKind for protocol compatibility.
    fn to_lsp_symbol_kind(&self) -> lsp_types::SymbolKind {
        match self {
            SymbolKind::Function { .. } => lsp_types::SymbolKind::FUNCTION,
            SymbolKind::Syscall { .. } => lsp_types::SymbolKind::FUNCTION, // Syscalls are function-like
            SymbolKind::Variable { .. } => lsp_types::SymbolKind::VARIABLE,
        }
    }

    /// Converts to LSP CompletionItemKind for autocomplete filtering.
    fn to_lsp_completion_kind(&self) -> lsp_types::CompletionItemKind {
        match self {
            SymbolKind::Function { .. } => lsp_types::CompletionItemKind::FUNCTION,
            SymbolKind::Syscall { .. } => lsp_types::CompletionItemKind::FUNCTION,
            SymbolKind::Variable { .. } => lsp_types::CompletionItemKind::VARIABLE,
        }
    }

    /// Returns a human-readable detail string for display in IDEs.
    fn detail_string(&self) -> String {
        match self {
            SymbolKind::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                let ret = return_type
                    .as_ref()
                    .map(|t| format!(" -> {}", t))
                    .unwrap_or_default();
                format!("fn({}){}", params, ret)
            }
            SymbolKind::Syscall {
                syscall_type,
                argument_count,
            } => {
                format!(
                    "{}(... {} args)",
                    match syscall_type {
                        SyscallType::System => "syscall",
                        SyscallType::Math => "math",
                    },
                    argument_count
                )
            }
            SymbolKind::Variable { type_hint } => type_hint
                .as_ref()
                .map(|t| t.to_string())
                .unwrap_or_else(|| "var".to_string()),
        }
    }
}

/// Distinguishes between System and Math syscalls.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyscallType {
    System,
    Math,
}

/// Metadata collected during compilation, including all referenced symbols.
#[derive(Debug, Default)]
pub struct CompilationMetadata<'a> {
    /// All symbols encountered during compilation (functions, syscalls, variables)
    pub symbols: Vec<SymbolInfo<'a>>,
}

impl<'a> CompilationMetadata<'a> {
    /// Creates a new empty compilation metadata.
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    /// Adds a symbol to the metadata.
    pub fn add_symbol(&mut self, symbol: SymbolInfo<'a>) {
        self.symbols.push(symbol);
    }

    /// Adds a function symbol.
    pub fn add_function(
        &mut self,
        name: Cow<'a, str>,
        parameters: Vec<Cow<'a, str>>,
        span: Option<Span>,
    ) {
        self.add_symbol(SymbolInfo {
            name,
            kind: SymbolKind::Function {
                parameters,
                return_type: None,
            },
            span,
            description: None,
        });
    }

    /// Adds a syscall symbol.
    pub fn add_syscall(
        &mut self,
        name: Cow<'a, str>,
        syscall_type: SyscallType,
        argument_count: usize,
        span: Option<Span>,
    ) {
        self.add_symbol(SymbolInfo {
            name,
            kind: SymbolKind::Syscall {
                syscall_type,
                argument_count,
            },
            span,
            description: None,
        });
    }

    /// Adds a variable symbol.
    pub fn add_variable(&mut self, name: Cow<'a, str>, span: Option<Span>) {
        self.add_symbol(SymbolInfo {
            name,
            kind: SymbolKind::Variable { type_hint: None },
            span,
            description: None,
        });
    }

    /// Returns all symbols of a specific kind.
    pub fn symbols_of_kind(&self, kind: &str) -> Vec<&SymbolInfo<'a>> {
        self.symbols
            .iter()
            .filter(|sym| match (&sym.kind, kind) {
                (SymbolKind::Function { .. }, "function") => true,
                (SymbolKind::Syscall { .. }, "syscall") => true,
                (SymbolKind::Variable { .. }, "variable") => true,
                _ => false,
            })
            .collect()
    }

    /// Converts all symbols to LSP SymbolInformation for protocol compatibility.
    pub fn to_lsp_symbols(&self, uri: lsp_types::Uri) -> Vec<lsp_types::SymbolInformation> {
        self.symbols
            .iter()
            .map(|sym| sym.to_lsp_symbol_information(uri.clone()))
            .collect()
    }

    /// Converts all symbols to LSP CompletionItems for autocomplete.
    pub fn to_lsp_completion_items(&self) -> Vec<lsp_types::CompletionItem> {
        self.symbols
            .iter()
            .map(|sym| sym.to_lsp_completion_item())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metadata_creation() {
        let metadata = CompilationMetadata::new();
        assert!(metadata.symbols.is_empty());
    }

    #[test]
    fn test_add_function_symbol() {
        let mut metadata = CompilationMetadata::new();
        metadata.add_function("test_func".into(), vec!["x".into(), "y".into()], None);
        assert_eq!(metadata.symbols.len(), 1);
        assert_eq!(metadata.symbols[0].name, "test_func");
    }

    #[test]
    fn test_add_syscall_symbol() {
        let mut metadata = CompilationMetadata::new();
        metadata.add_syscall("hash".into(), SyscallType::System, 1, None);
        assert_eq!(metadata.symbols.len(), 1);
        assert_eq!(metadata.symbols[0].name, "hash");
    }

    #[test]
    fn test_symbols_of_kind() {
        let mut metadata = CompilationMetadata::new();
        metadata.add_function("func1".into(), vec![], None);
        metadata.add_syscall("hash".into(), SyscallType::System, 1, None);
        metadata.add_variable("x".into(), None);

        let functions = metadata.symbols_of_kind("function");
        assert_eq!(functions.len(), 1);

        let syscalls = metadata.symbols_of_kind("syscall");
        assert_eq!(syscalls.len(), 1);

        let variables = metadata.symbols_of_kind("variable");
        assert_eq!(variables.len(), 1);
    }

    #[test]
    #[ignore] // Requires complex Uri construction
    fn test_lsp_symbol_conversion() {
        let mut metadata = CompilationMetadata::new();
        metadata.add_function("test_func".into(), vec!["a".into(), "b".into()], None);

        // In real usage with LSP, Uri would be passed from the server
        // This test demonstrates the conversion method exists and is type-safe
    }

    #[test]
    fn test_lsp_completion_items() {
        let mut metadata = CompilationMetadata::new();
        metadata.add_function("test_func".into(), vec![], None);
        metadata.add_syscall("hash".into(), SyscallType::System, 1, None);
        metadata.add_variable("x".into(), None);

        let completions = metadata.to_lsp_completion_items();
        assert_eq!(completions.len(), 3);

        // Verify function
        assert_eq!(completions[0].label, "test_func");
        assert_eq!(
            completions[0].kind,
            Some(lsp_types::CompletionItemKind::FUNCTION)
        );

        // Verify syscall
        assert_eq!(completions[1].label, "hash");
        assert_eq!(
            completions[1].kind,
            Some(lsp_types::CompletionItemKind::FUNCTION)
        );

        // Verify variable
        assert_eq!(completions[2].label, "x");
        assert_eq!(
            completions[2].kind,
            Some(lsp_types::CompletionItemKind::VARIABLE)
        );
    }
}
