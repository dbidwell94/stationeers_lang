macro_rules! output {
    ($input:expr) => {
        String::from_utf8($input.into_inner()?)?
    };
}

#[macro_export]
macro_rules! compile {
    ($source:expr) => {{
        let mut writer = std::io::BufWriter::new(Vec::new());
        let compiler = crate::Compiler::new(
            parser::Parser::new(tokenizer::Tokenizer::from(String::from($source))),
            &mut writer,
            None,
        );
        compiler.compile()?;
        output!(writer)
    }};

    (debug $source:expr) => {{
        let mut writer = std::io::BufWriter::new(Vec::new());
        let compiler = crate::Compiler::new(
            parser::Parser::new(tokenizer::Tokenizer::from(String::from($source))),
            &mut writer,
            Some(crate::CompilerConfig { debug: true }),
        );
        compiler.compile()?;
        output!(writer)
    }};
}
mod declaration_function_invocation;
mod declaration_literal;
mod function_declaration;
