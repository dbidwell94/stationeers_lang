use compiler::Compiler;
use parser::Parser;
use safer_ffi::prelude::*;
use std::io::BufWriter;
use tokenizer::{Error as TokenizerError, Tokenizer};

#[derive_ReprC]
#[repr(C)]
pub struct FfiToken {
    pub text: safer_ffi::String,
    pub tooltip: safer_ffi::String,
    pub error: safer_ffi::String,
    pub status: safer_ffi::String,
    pub column: i32,
}

/// C# handles strings as UTF16. We do NOT want to allocate that memory in C# because
/// we want to avoid GC. So we pass it to Rust to handle all the memory allocations.
/// This should result in the ability to compile many times without triggering frame drops
/// from the GC from a `GetBytes()` call on a string in C#.
#[ffi_export]
pub fn compile_from_string(input: safer_ffi::slice::Ref<'_, u16>) -> safer_ffi::String {
    let mut writer = BufWriter::new(Vec::new());

    let tokenizer = Tokenizer::from(String::from_utf16_lossy(input.as_slice()));
    let parser = Parser::new(tokenizer);
    let compiler = Compiler::new(parser, &mut writer, None);

    if compiler.compile().is_err() {
        return safer_ffi::String::EMPTY;
    }

    let Ok(compiled_vec) = writer.into_inner() else {
        return safer_ffi::String::EMPTY;
    };

    // Safety: I know the compiler only outputs valid utf8
    safer_ffi::String::from(unsafe { String::from_utf8_unchecked(compiled_vec) })
}
/// C# handles strings as UTF16. We do NOT want to allocate that memory in C# because
/// we want to avoid GC. So we pass it to Rust to handle all the memory allocations.
/// This should result in the ability to tokenize many times without triggering frame drops
/// from the GC from a `GetBytes()` call on a string in C#.
#[ffi_export]
pub fn tokenize_line(input: safer_ffi::slice::Ref<'_, u16>) -> safer_ffi::Vec<FfiToken> {
    let tokenizer = Tokenizer::from(String::from_utf16_lossy(input.as_slice()));

    let mut tokens = Vec::<FfiToken>::new();

    for token in tokenizer {
        match token {
            Err(TokenizerError::NumberParseError(_, _, col))
            | Err(TokenizerError::UnknownSymbolError(_, _, col))
            | Err(TokenizerError::DecimalParseError(_, _, col))
            | Err(TokenizerError::UnknownKeywordOrIdentifierError(_, _, col)) => {
                tokens.push(FfiToken {
                    column: col as i32,
                    text: "".into(),
                    tooltip: "".into(),
                    // Safety: it's okay to unwrap the err here because we are matching on the `Err` variant
                    error: token.unwrap_err().to_string().into(),
                    status: "".into(),
                });
            }
            Err(_) => return safer_ffi::Vec::EMPTY,
            Ok(token) => tokens.push(FfiToken {
                text: token.token_type.to_string().into(),
                tooltip: "".into(),
                error: "".into(),
                status: "".into(),
                column: token.column as i32,
            }),
        }
    }

    tokens.into()
}

#[ffi_export]
pub fn free_ffi_token_vec(v: safer_ffi::Vec<FfiToken>) {
    drop(v)
}

#[ffi_export]
pub fn free_string(s: safer_ffi::String) {
    drop(s)
}

#[cfg(feature = "headers")]
pub fn generate_headers() -> std::io::Result<()> {
    let file_name = "../csharp_mod/FfiGlue.cs";
    ::safer_ffi::headers::builder()
        .with_language(safer_ffi::headers::Language::CSharp)
        .to_file(file_name)?
        .generate()?;

    let content = std::fs::read_to_string(file_name)?;

    let content = content.replace(
        "private const string RustLib = \"slang\";",
        "public const string RustLib = \"slang_compiler.dll\";",
    );

    std::fs::write(file_name, content)?;
    Ok(())
}
