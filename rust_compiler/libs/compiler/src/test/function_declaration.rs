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
            pop r8
            pop r9
            pop r10
            pop r11
            pop r12
            pop r13
            pop r14
            push ra
            L1:
            sub r0 sp 1
            get ra db r0
            sub sp sp 3
            j ra
        "}
    );

    Ok(())
}

#[test]
fn test_early_return() -> anyhow::Result<()> {
    let compiled = compile!(debug r#"
        // This is a test function declaration with no body
        fn doSomething() {
            if (1 == 1) {
                return;
            }
            let i = 1 + 2;
            return;
        };
        doSomething();
    "#);

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            doSomething:
            push ra
            seq r1 1 1
            beqz r1 L2
            j L1
            L2:
            move r8 3
            j L1
            L1:
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            jal doSomething
            move r1 r15
            "
        }
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
            pop r8
            pop r9
            push ra
            L1:
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
        "}
    );

    Ok(())
}
