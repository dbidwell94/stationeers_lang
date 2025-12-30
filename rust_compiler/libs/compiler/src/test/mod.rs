#![allow(clippy::crate_in_macro_def)]

macro_rules! output {
    ($input:expr) => {
        String::from_utf8($input.into_inner()?)?
    };
}

#[cfg_attr(test, macro_export)]
macro_rules! compile {
    ($source:expr) => {{
        let mut writer = std::io::BufWriter::new(Vec::new());
        let compiler = ::Compiler::new(
            parser::Parser::new(tokenizer::Tokenizer::from(String::from($source))),
            None,
        );
        let res = compiler.compile();
        res.instructions.write(&mut writer)?;
        output!(writer)
    }};

    (result $source:expr) => {{
        let compiler = crate::Compiler::new(
            parser::Parser::new(tokenizer::Tokenizer::from($source)),
            Some(crate::CompilerConfig { debug: true }),
        );
        compiler.compile().errors
    }};

    (debug $source:expr) => {{
        let mut writer = std::io::BufWriter::new(Vec::new());
        let compiler = crate::Compiler::new(
            parser::Parser::new(tokenizer::Tokenizer::from($source)),
            Some(crate::CompilerConfig { debug: true }),
        );
        let res = compiler.compile();
        res.instructions.write(&mut writer)?;
        output!(writer)
    }};
}
mod binary_expression;
mod branching;
mod declaration_function_invocation;
mod declaration_literal;
mod device_access;
mod edge_cases;
mod error_handling;
mod function_declaration;
mod logic_expression;
mod loops;
mod math_syscall;
mod negation_priority;
mod scoping;
mod syscall;
mod tuple_literals;
