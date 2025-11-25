use indoc::indoc;
use pretty_assertions::assert_eq;

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
            push ra
            sub r0 sp 1
            get ra db r0
            sub sp sp 3
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
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
        "}
    );

    Ok(())
}
