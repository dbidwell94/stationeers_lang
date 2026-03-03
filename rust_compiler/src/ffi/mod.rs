use compiler::{CompilationResult, Compiler};
use helpers::{Documentation, Span};
use parser::{sys_call::SysCall, Parser};
use safer_ffi::prelude::*;
use std::io::BufWriter;
use tokenizer::{
    token::{Token, TokenType},
    Tokenizer,
};

#[derive_ReprC]
#[repr(C)]
pub struct FfiSourceMapEntry {
    pub line_number: u32,
    pub span: FfiRange,
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiCompilationResult {
    pub output_code: safer_ffi::String,
    pub source_map: safer_ffi::Vec<FfiSourceMapEntry>,
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiToken {
    pub tooltip: safer_ffi::String,
    pub error: safer_ffi::String,
    pub column: i32,
    pub length: i32,
    pub token_kind: u32,
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiRange {
    start_col: u32,
    end_col: u32,
    start_line: u32,
    end_line: u32,
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiDocumentedItem {
    item_name: safer_ffi::String,
    docs: safer_ffi::String,
}

impl From<Span> for FfiRange {
    fn from(value: Span) -> Self {
        Self {
            start_line: value.start_line as u32,
            end_line: value.end_line as u32,
            start_col: value.start_col as u32,
            end_col: value.end_col as u32,
        }
    }
}

impl From<lsp_types::Range> for FfiRange {
    fn from(value: lsp_types::Range) -> Self {
        Self {
            start_col: value.start.character,
            end_col: value.end.character,
            start_line: value.start.line,
            end_line: value.end.line,
        }
    }
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiDiagnostic {
    message: safer_ffi::String,
    severity: i32,
    range: FfiRange,
}

impl From<lsp_types::Diagnostic> for FfiDiagnostic {
    fn from(value: lsp_types::Diagnostic) -> Self {
        use lsp_types::*;
        Self {
            message: value.message.into(),
            severity: match value.severity.unwrap_or(DiagnosticSeverity::ERROR) {
                DiagnosticSeverity::WARNING => 2,
                DiagnosticSeverity::INFORMATION => 3,
                DiagnosticSeverity::HINT => 4,
                _ => 1,
            },
            range: value.range.into(),
        }
    }
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiSymbolKindData {
    pub kind: u32, // 0=Function, 1=Syscall, 2=Variable
    pub arg_count: u32,
    pub syscall_type: u32, // 0=System, 1=Math (only for Syscall kind)
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiSymbolInfo {
    pub name: safer_ffi::String,
    pub kind_data: FfiSymbolKindData,
    pub span: FfiRange,
    pub description: safer_ffi::String,
}

#[derive_ReprC]
#[repr(C)]
pub struct FfiDiagnosticsAndSymbols {
    pub diagnostics: safer_ffi::Vec<FfiDiagnostic>,
    pub symbols: safer_ffi::Vec<FfiSymbolInfo>,
}

#[ffi_export]
pub fn free_ffi_compilation_result(input: FfiCompilationResult) {
    drop(input)
}

#[ffi_export]
pub fn free_ffi_token_vec(v: safer_ffi::Vec<FfiToken>) {
    drop(v)
}

#[ffi_export]
pub fn free_ffi_diagnostic_vec(v: safer_ffi::Vec<FfiDiagnostic>) {
    drop(v)
}

#[ffi_export]
pub fn free_ffi_diagnostics_and_symbols(v: FfiDiagnosticsAndSymbols) {
    drop(v)
}

#[ffi_export]
pub fn free_string(s: safer_ffi::String) {
    drop(s)
}

#[ffi_export]
pub fn free_docs_vec(v: safer_ffi::Vec<FfiDocumentedItem>) {
    drop(v)
}

/// C# handles strings as UTF16. We do NOT want to allocate that memory in C# because
/// we want to avoid GC. So we pass it to Rust to handle all the memory allocations.
/// This should result in the ability to compile many times without triggering frame drops
/// from the GC from a `GetBytes()` call on a string in C#.
#[ffi_export]
pub fn compile_from_string(input: safer_ffi::slice::Ref<'_, u16>) -> FfiCompilationResult {
    let res = std::panic::catch_unwind(|| {
        let input = String::from_utf16_lossy(input.as_slice());

        let tokenizer = Tokenizer::from(input.as_str());
        let parser = Parser::new(tokenizer);
        let compiler = Compiler::new(parser, None);

        let res = compiler.compile();

        if !res.errors.is_empty() {
            return (safer_ffi::String::EMPTY, res.instructions.source_map());
        }

        let mut writer = BufWriter::new(Vec::new());

        // writing into a Vec<u8>. This should not fail.
        let optimized = optimizer::optimize(res.instructions);
        let map = optimized.source_map();
        _ = optimized.write(&mut writer);

        let Ok(compiled_vec) = writer.into_inner() else {
            return (safer_ffi::String::EMPTY, map);
        };

        // Safety: I know the compiler only outputs valid utf8
        (
            safer_ffi::String::from(unsafe { String::from_utf8_unchecked(compiled_vec) }),
            map,
        )
    });

    if let Ok((res_str, source_map)) = res {
        FfiCompilationResult {
            source_map: source_map
                .into_iter()
                .map(|(line_num, span)| FfiSourceMapEntry {
                    span: span.into(),
                    line_number: line_num as u32,
                })
                .collect::<Vec<_>>()
                .into(),
            output_code: res_str,
        }
    } else {
        FfiCompilationResult {
            output_code: "".into(),
            source_map: vec![].into(),
        }
    }
}

#[ffi_export]
pub fn tokenize_line(input: safer_ffi::slice::Ref<'_, u16>) -> safer_ffi::Vec<FfiToken> {
    let res = std::panic::catch_unwind(|| {
        let input = String::from_utf16_lossy(input.as_slice());
        let tokenizer = Tokenizer::from(input.as_str());

        // Build a lookup table for syscall documentation
        let syscall_docs: std::collections::HashMap<&'static str, String> =
            SysCall::get_all_documentation().into_iter().collect();

        let mut tokens = Vec::new();

        for token in tokenizer {
            if matches!(
                token,
                Ok(Token {
                    token_type: TokenType::EOF,
                    ..
                })
            ) {
                continue;
            }
            match token {
                Err(ref e) => {
                    use tokenizer::token::LexError;
                    use tokenizer::Error::*;
                    let (err_str, _, span) = match e {
                        LexError(LexError::NumberParse(line, span, err))
                        | LexError(LexError::InvalidInput(line, span, err)) => {
                            (err.to_string(), line, span)
                        }

                        _ => continue,
                    };

                    tokens.push(FfiToken {
                        column: span.start as i32,
                        error: err_str.into(),
                        tooltip: "".into(),
                        length: (span.end - span.start) as i32,
                        token_kind: 0,
                    })
                }
                Ok(Token {
                    span, token_type, ..
                }) => {
                    let mut tooltip = token_type.docs();

                    // If no docs from token type, check if it's a syscall
                    if tooltip.is_empty() {
                        if let TokenType::Identifier(id) = &token_type {
                            if let Some(doc) = syscall_docs.get(id.as_ref()) {
                                tooltip = doc.clone();
                            }
                        }
                    }

                    tokens.push(FfiToken {
                        column: span.start as i32,
                        error: "".into(),
                        length: (span.end - span.start) as i32,
                        tooltip: tooltip.into(),
                        token_kind: token_type.into(),
                    })
                }
            }
        }

        tokens.into()
    });

    res.unwrap_or(vec![].into())
}

#[ffi_export]
pub fn diagnose_source(input: safer_ffi::slice::Ref<'_, u16>) -> safer_ffi::Vec<FfiDiagnostic> {
    let res = std::panic::catch_unwind(|| {
        let input = String::from_utf16_lossy(input.as_slice());

        let tokenizer = Tokenizer::from(input.as_str());
        let compiler = Compiler::new(Parser::new(tokenizer), None);

        let CompilationResult {
            errors: diagnosis, ..
        } = compiler.compile();

        let mut result_vec: Vec<FfiDiagnostic> = Vec::with_capacity(diagnosis.len());

        for err in diagnosis {
            result_vec.push(lsp_types::Diagnostic::from(err).into());
        }

        result_vec.into()
    });

    res.unwrap_or(vec![].into())
}

#[ffi_export]
pub fn diagnose_source_with_symbols(
    input: safer_ffi::slice::Ref<'_, u16>,
) -> FfiDiagnosticsAndSymbols {
    let res = std::panic::catch_unwind(|| {
        let input = String::from_utf16_lossy(input.as_slice());

        let tokenizer = Tokenizer::from(input.as_str());
        let compiler = Compiler::new(Parser::new(tokenizer), None);

        let CompilationResult {
            errors: diagnosis,
            metadata,
            ..
        } = compiler.compile();

        // Convert diagnostics
        let mut diagnostics_vec: Vec<FfiDiagnostic> = Vec::with_capacity(diagnosis.len());
        for err in diagnosis {
            diagnostics_vec.push(lsp_types::Diagnostic::from(err).into());
        }

        // Convert symbols
        let mut symbols_vec: Vec<FfiSymbolInfo> = Vec::with_capacity(metadata.symbols.len());
        for symbol in &metadata.symbols {
            let (kind, arg_count, syscall_type) = match &symbol.kind {
                compiler::SymbolKind::Function { parameters, .. } => {
                    (0, parameters.len() as u32, 0)
                }
                compiler::SymbolKind::Syscall {
                    syscall_type,
                    argument_count,
                } => {
                    let sc_type = match syscall_type {
                        compiler::SyscallType::System => 0,
                        compiler::SyscallType::Math => 1,
                    };
                    (1, *argument_count as u32, sc_type)
                }
                compiler::SymbolKind::Variable { .. } => (2, 0, 0),
            };

            let span = symbol
                .span
                .as_ref()
                .map(|s| (*s).into())
                .unwrap_or(FfiRange {
                    start_line: 0,
                    end_line: 0,
                    start_col: 0,
                    end_col: 0,
                });

            symbols_vec.push(FfiSymbolInfo {
                name: symbol.name.to_string().into(),
                kind_data: FfiSymbolKindData {
                    kind,
                    arg_count,
                    syscall_type,
                },
                span,
                description: symbol
                    .description
                    .as_ref()
                    .map(|d| d.to_string())
                    .unwrap_or_default()
                    .into(),
            });
        }

        FfiDiagnosticsAndSymbols {
            diagnostics: diagnostics_vec.into(),
            symbols: symbols_vec.into(),
        }
    });

    res.unwrap_or(FfiDiagnosticsAndSymbols {
        diagnostics: vec![].into(),
        symbols: vec![].into(),
    })
}

#[ffi_export]
pub fn get_docs() -> safer_ffi::Vec<FfiDocumentedItem> {
    let res = std::panic::catch_unwind(|| {
        let mut docs = SysCall::get_all_documentation();
        docs.extend(TokenType::get_all_documentation());

        docs
    });

    let Ok(result) = res else {
        return vec![].into();
    };

    result
        .into_iter()
        .map(|(key, doc)| FfiDocumentedItem {
            item_name: key.into(),
            docs: doc.into(),
        })
        .collect::<Vec<_>>()
        .into()
}
