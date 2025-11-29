use compiler::Compiler;
use parser::Parser;
use safer_ffi::prelude::*;
use std::io::BufWriter;
use tokenizer::{token::TokenType, Error as TokenizerError, Tokenizer};

#[derive_ReprC]
#[repr(C)]
pub struct FfiToken {
    pub tooltip: safer_ffi::String,
    pub error: safer_ffi::String,
    pub column: i32,
    pub length: i32,
    pub token_kind: u32,
}

fn map_token_kind(t: &TokenType) -> u32 {
    use TokenType::*;
    match t {
        Keyword(_) => 1,
        Identifier(_) => 2,
        Number(_) => 3,
        String(_) => 4,
        Boolean(_) => 5,
        Symbol(_) => 6,
        _ => 0,
    }
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
            Err(TokenizerError::NumberParseError(_, _, col, ref str))
            | Err(TokenizerError::UnknownSymbolError(_, _, col, ref str))
            | Err(TokenizerError::DecimalParseError(_, _, col, ref str))
            | Err(TokenizerError::UnknownKeywordOrIdentifierError(_, _, col, ref str)) => {
                tokens.push(FfiToken {
                    column: col as i32 - 1,
                    tooltip: "".into(),
                    length: str.len() as i32,
                    token_kind: 0,
                    // Safety: it's okay to unwrap the err here because we are matching on the `Err` variant
                    error: token.unwrap_err().to_string().into(),
                });
            }
            Err(_) => return safer_ffi::Vec::EMPTY,
            Ok(token) if !matches!(token.token_type, TokenType::EOF) => tokens.push(FfiToken {
                tooltip: "".into(),
                error: "".into(),
                length: token
                    .original_string
                    .map(|s| s.len() as i32)
                    .unwrap_or_default(),
                token_kind: map_token_kind(&token.token_type),
                column: token.column as i32 - 1,
            }),
            _ => {}
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
