use super::v2::Compiler;
use crate::v2::CompilerConfig;
use indoc::indoc;
use parser::Parser;
use std::io::BufWriter;
use tokenizer::Tokenizer;

macro_rules! output {
    ($input:expr) => {
        String::from_utf8($input.into_inner()?)?
    };
}

macro_rules! compile {
    ($source:expr) => {{
        let mut writer = BufWriter::new(Vec::new());
        let compiler = Compiler::new(
            Parser::new(Tokenizer::from(String::from($source))),
            &mut writer,
            None,
        );
        compiler.compile()?;
        output!(writer)
    }};

    (debug $source:expr) => {{
        let mut writer = BufWriter::new(Vec::new());
        let compiler = Compiler::new(
            Parser::new(Tokenizer::from(String::from($source))),
            &mut writer,
            Some(CompilerConfig {
                debug: true,
                ..Default::default()
            }),
        );
        compiler.compile()?;
        output!(writer)
    }};
}

#[test]
fn test_function_declaration_with_register_params() -> anyhow::Result<()> {
    let compiled = compile!(debug r#"
        // This is a test function declaration with no body
        fn doSomething(arg1, arg2) {
        };
    "#);

    assert_eq!(
        compiled,
        indoc! {"
            j main
            doSomething:
            push ra
            push r4
            move r4 r0 #arg1
            push r5
            move r5 r1 #arg2
            pop r5
            pop r4
            pop ra
            j ra
        "}
    );

    Ok(())
}
