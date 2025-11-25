use compiler::Compiler;
use parser::Parser;
use safer_ffi::ffi_export;
use std::io::BufWriter;
use tokenizer::Tokenizer;

#[ffi_export]
fn compile_from_string(
    input: &safer_ffi::string::String,
    output: &mut safer_ffi::string::String,
) -> bool {
    let mut writer = BufWriter::new(Vec::new());

    let tokenizer = Tokenizer::from(input.to_string());
    let parser = Parser::new(tokenizer);

    let compiler = Compiler::new(parser, &mut writer, None);

    let Ok(()) = compiler.compile() else {
        return false;
    };

    let Ok(buffer) = writer.into_inner() else {
        return false;
    };

    let Ok(output_string) = String::from_utf8(buffer) else {
        return false;
    };

    *output = output_string.into();

    return true;
}
