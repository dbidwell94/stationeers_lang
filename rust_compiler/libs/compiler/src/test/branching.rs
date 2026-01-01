use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_if_statement() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 10;
        if (a > 5) {
            a = 20;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 10
            sgt r1 r8 5
            beqz r1 __internal_L1
            move r8 20
            __internal_L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_if_else_statement() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 0;
        if (10 > 5) {
            a = 1;
        } else {
            a = 2;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            sgt r1 10 5
            beqz r1 __internal_L2
            move r8 1
            j __internal_L1
            __internal_L2:
            move r8 2
            __internal_L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_if_else_if_statement() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 0;
        if (a == 1) {
            a = 10;
        } else if (a == 2) {
            a = 20;
        } else {
            a = 30;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 0
            seq r1 r8 1
            beqz r1 __internal_L2
            move r8 10
            j __internal_L1
            __internal_L2:
            seq r2 r8 2
            beqz r2 __internal_L4
            move r8 20
            j __internal_L3
            __internal_L4:
            move r8 30
            __internal_L3:
            __internal_L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_spilled_variable_update_in_branch() -> anyhow::Result<()> {
    let result = compile! {
        check
        "
        let a = 1;
        let b = 2;
        let c = 3;
        let d = 4;
        let e = 5;
        let f = 6;
        let g = 7;
        let h = 8; // Spilled to stack (offset 0)

        if (a == 1) {
            h = 99;
        }
        "
    };

    assert!(
        result.errors.is_empty(),
        "Expected no errors, got: {:?}",
        result.errors
    );

    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 1
            move r9 2
            move r10 3
            move r11 4
            move r12 5
            move r13 6
            move r14 7
            push 8
            seq r1 r8 1
            beqz r1 __internal_L1
            sub r0 sp 1
            put db r0 99
            __internal_L1:
            sub sp sp 1
            "
        }
    );

    Ok(())
}
