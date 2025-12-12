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
        let mut writer = BufWriter::new(Vec::new());

        let tokenizer = Tokenizer::from(input.as_str());
        let parser = Parser::new(tokenizer);
        let compiler = Compiler::new(parser, &mut writer, None);

        let res = compiler.compile();

        if !res.errors.is_empty() {
            return (safer_ffi::String::EMPTY, res.source_map);
        }

        let Ok(compiled_vec) = writer.into_inner() else {
            return (safer_ffi::String::EMPTY, res.source_map);
        };

        // Safety: I know the compiler only outputs valid utf8
        (
            safer_ffi::String::from(unsafe { String::from_utf8_unchecked(compiled_vec) }),
            res.source_map,
        )
    });

    if let Ok((res_str, source_map)) = res {
        FfiCompilationResult {
            source_map: source_map
                .into_iter()
                .flat_map(|(k, v)| {
                    v.into_iter()
                        .map(|span| FfiSourceMapEntry {
                            span: span.into(),
                            line_number: k as u32,
                        })
                        .collect::<Vec<_>>()
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
                }) => tokens.push(FfiToken {
                    column: span.start as i32,
                    error: "".into(),
                    length: (span.end - span.start) as i32,
                    tooltip: token_type.docs().into(),
                    token_kind: token_type.into(),
                }),
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

        let mut writer = BufWriter::new(Vec::new());
        let tokenizer = Tokenizer::from(input.as_str());
        let compiler = Compiler::new(Parser::new(tokenizer), &mut writer, None);

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
