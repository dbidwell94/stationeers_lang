use std::io::{BufWriter, Write};

use compiler::Compiler;
use parser::Parser;
use safer_ffi::ffi_export;
use tokenizer::Tokenizer;

#[ffi_export]
fn compile_from_string(
    input: &safer_ffi::string::String,
    output: &mut safer_ffi::string::String,
) -> i32 {
    todo!()
}
