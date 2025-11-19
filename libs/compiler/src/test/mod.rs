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
fn test_function_declaration_with_spillover_params() -> anyhow::Result<()> {
    let compiled = compile!(debug r#"
        // we need more than 4 params to 'spill' into a stack var
        fn doSomething(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {};
    "#);

    assert_eq!(
        compiled,
        indoc! {"
            j main
            doSomething:
            pop r8 #arg9
            pop r9 #arg8
            pop r10 #arg7
            pop r11 #arg6
            pop r12 #arg5
            pop r13 #arg4
            pop r14 #arg3
            pop r15 #arg2
            push ra
            pop ra
            sub sp 1
            j ra
        "}
    );

    Ok(())
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
            pop r8 #arg2
            pop r9 #arg1
            push ra
            pop ra
            j ra
        "}
    );

    Ok(())
}
