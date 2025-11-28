use compiler::Compiler;
use parser::Parser;
use safer_ffi::prelude::*;
use std::io::BufWriter;
use tokenizer::{Error as TokenizerError, Tokenizer};

#[derive_ReprC]
#[repr(C)]
pub struct FfiToken {
    pub text: safer_ffi::String,
    pub tooltip: Option<safer_ffi::String>,
    pub error: Option<safer_ffi::String>,
    pub status: Option<safer_ffi::String>,
    pub column: i32,
}

#[ffi_export]
pub fn compile_from_string(input: safer_ffi::char_p::char_p_ref<'_>) -> safer_ffi::String {
    let mut writer = BufWriter::new(Vec::new());

    let tokenizer = Tokenizer::from(input.to_str());
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

#[ffi_export]
pub fn tokenize_line(input: safer_ffi::char_p::char_p_ref<'_>) -> safer_ffi::Vec<FfiToken> {
    let tokenizer = Tokenizer::from(input.to_str());

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
                    tooltip: None,
                    // Safety: it's okay to unwrap the err here because we are matching on the `Err` variant
                    error: Some(token.unwrap_err().to_string().into()),
                    status: None,
                });
            }
            Err(_) => return safer_ffi::Vec::EMPTY,
            Ok(token) => tokens.push(FfiToken {
                text: token.token_type.to_string().into(),
                tooltip: None,
                error: None,
                status: None,
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
    ::safer_ffi::headers::builder()
        .with_language(safer_ffi::headers::Language::CSharp)
        .to_file("../csharp_mod/SlangGlue.cs")?
        .generate()
}
