use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_if_statement() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let a = 10;
        if (a > 5) {
            a = 20;
        }
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 10
            sgt r1 r8 5
            beqz r1 L1
            move r8 20
            L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_if_else_statement() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let a = 0;
        if (10 > 5) {
            a = 1;
        } else {
            a = 2;
        }
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            sgt r1 10 5
            beqz r1 L2
            move r8 1
            j L1
            L2:
            move r8 2
            L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_if_else_if_statement() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
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

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            seq r1 r8 1
            beqz r1 L2
            move r8 10
            j L1
            L2:
            seq r2 r8 2
            beqz r2 L4
            move r8 20
            j L3
            L4:
            move r8 30
            L3:
            L1:
            "
        }
    );

    Ok(())
}

#[test]
fn test_spilled_variable_update_in_branch() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
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

    assert_eq!(
        compiled,
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
            beqz r1 L1
            sub r0 sp 1
            put db r0 99
            L1:
            sub sp sp 1
            "
        }
    );

    Ok(())
}
