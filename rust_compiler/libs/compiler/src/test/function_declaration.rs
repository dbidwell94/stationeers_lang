use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_function_declaration_with_spillover_params() -> anyhow::Result<()> {
    let compiled = compile!(debug r#"
        // we need more than 4 params to 'spill' into a stack var
        fn doSomething(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) {
            return arg1 + arg2 + arg3 + arg4 + arg5 + arg6 + arg7 + arg8 + arg9;
        };

        let item1 = 1;
        let returned = doSomething(item1, 2, 3, 4, 5, 6, 7, 8, 9);
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
            sub r0 sp 3
            get r1 db r0
            sub r0 sp 2
            get r2 db r0
            add r3 r1 r2
            add r4 r3 r14
            add r5 r4 r13
            add r6 r5 r12
            add r7 r6 r11
            add r1 r7 r10
            add r2 r1 r9
            add r3 r2 r8
            move r15 r3
            j __internal_L1
            __internal_L1:
            pop ra
            sub sp sp 2
            j ra
            main:
            move r8 1
            push r8
            push r8
            push 2
            push 3
            push 4
            push 5
            push 6
            push 7
            push 8
            push 9
            jal doSomething
            pop r8
            move r9 r15
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
            beqz r1 __internal_L2
            j __internal_L1
            __internal_L2:
            move r8 3
            j __internal_L1
            __internal_L1:
            pop ra
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
            __internal_L1:
            pop ra
            j ra
        "}
    );

    Ok(())
}
