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
            beq r1 0 L2
            j L1
            L2:
            move r8 3 #i
            j L1
            L1:
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            jal doSomething
            move r1 r15 #__binary_temp_2
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
            pop r8 #arg2
            pop r9 #arg1
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
