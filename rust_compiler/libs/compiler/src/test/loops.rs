use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_infinite_loop() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let a = 0;
        loop {
            a = a + 1;
        }
        "
    };

    // Labels: L1 (start), L2 (end)
    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            L1:
            add r1 r8 1
            move r8 r1
            j L1
            L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_loop_break() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
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

    // Labels: L1 (start), L2 (end), L3 (if end - implicit else label)
    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            L1:
            add r1 r8 1
            move r8 r1
            sgt r2 r8 10
            beqz r2 L3
            j L2
            L3:
            j L1
            L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_while_loop() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let a = 0;
        while (a < 10) {
            a = a + 1;
        }
        "
    };

    // Labels: L1 (start), L2 (end)
    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            L1:
            slt r1 r8 10
            beqz r1 L2
            add r2 r8 1
            move r8 r2
            j L1
            L2:
            "
        }
    );

    Ok(())
}

#[test]
fn test_loop_continue() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
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

    // Labels: L1 (start), L2 (end), L3 (if end)
    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            L1:
            add r1 r8 1
            move r8 r1
            slt r2 r8 5
            beqz r2 L3
            j L1
            L3:
            j L2
            j L1
            L2:
            "
        }
    );

    Ok(())
}
