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
            move r8 10 #a
            sgt r1 r8 5
            beq r1 0 L1
            move r8 20 #a
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
            move r8 0 #a
            sgt r1 10 5
            beq r1 0 L2
            move r8 1 #a
            j L1
            L2:
            move r8 2 #a
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
            move r8 0 #a
            seq r1 r8 1
            beq r1 0 L2
            move r8 10 #a
            j L1
            L2:
            seq r2 r8 2
            beq r2 0 L4
            move r8 20 #a
            j L3
            L4:
            move r8 30 #a
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
            move r8 1 #a
            move r9 2 #b
            move r10 3 #c
            move r11 4 #d
            move r12 5 #e
            move r13 6 #f
            move r14 7 #g
            push 8 #h
            seq r1 r8 1
            beq r1 0 L1
            sub r0 sp 1
            put db r0 99 #h
            L1:
            "
        }
    );

    Ok(())
}
