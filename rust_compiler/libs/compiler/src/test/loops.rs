use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_infinite_loop() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 0;
        loop {
            a = a + 1;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end)
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            __internal_L1:
            add r1 r8 1
            move r8 r1
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_loop_break() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 0;
        loop {
            a = a + 1;
            if (a > 10) {
                break;
            }
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end), L3 (if end - implicit else label)
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            __internal_L1:
            add r1 r8 1
            move r8 r1
            sgt r2 r8 10
            beqz r2 __internal_L3
            j __internal_L2
            __internal_L3:
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_while_loop() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 0;
        while (a < 10) {
            a = a + 1;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end)
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            __internal_L1:
            slt r1 r8 10
            beqz r1 __internal_L2
            add r2 r8 1
            move r8 r2
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_loop_continue() -> anyhow::Result<()> {
    let result = compile! {
        check
        r#"
        let a = 0;
        loop {
            a = a + 1;
            if (a < 5) {
                continue;
            }
            break;
        }
        "#
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end), L3 (if end)
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            __internal_L1:
            add r1 r8 1
            move r8 r1
            slt r2 r8 5
            beqz r2 __internal_L3
            j __internal_L1
            __internal_L3:
            j __internal_L2
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_continue_with_stack_spill() -> anyhow::Result<()> {
    // Regression test for bug where continue statement doesn't clean up stack
    // when loop body has enough locals to spill to stack
    let result = compile! {
        check
        "
        loop {
            let t1 = 1; let t2 = 2; let t3 = 3; let t4 = 4;
            let t5 = 5; let t6 = 6; let t7 = 7; let t8 = 8;
            if (true) { continue; }
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end), L3 (if end)
    // The key: stack cleanup (sub sp sp 1) must happen BEFORE the continue jump
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            __internal_L1:
            move r8 1
            move r9 2
            move r10 3
            move r11 4
            move r12 5
            move r13 6
            move r14 7
            push 8
            beqz 1 __internal_L3
            sub sp sp 1
            j __internal_L1
            __internal_L3:
            sub sp sp 1
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_break_with_stack_spill() -> anyhow::Result<()> {
    // Regression test for bug where break statement doesn't clean up stack
    // when loop body has enough locals to spill to stack
    let result = compile! {
        check
        "
        loop {
            let t1 = 1; let t2 = 2; let t3 = 3; let t4 = 4;
            let t5 = 5; let t6 = 6; let t7 = 7; let t8 = 8;
            if (true) { break; }
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    // __internal_Labels: L1 (start), L2 (end), L3 (if end)
    // The key: stack cleanup (sub sp sp 1) must happen BEFORE the break jump
    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            __internal_L1:
            move r8 1
            move r9 2
            move r10 3
            move r11 4
            move r12 5
            move r13 6
            move r14 7
            push 8
            beqz 1 __internal_L3
            sub sp sp 1
            j __internal_L2
            __internal_L3:
            sub sp sp 1
            j __internal_L1
            __internal_L2:
            "
        }
    );

    Ok(())
}
