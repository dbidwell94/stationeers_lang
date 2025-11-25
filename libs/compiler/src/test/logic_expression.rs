use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_comparison_expressions() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let isGreater = 10 > 5;
        let isLess = 5 < 10;
        let isEqual = 5 == 5;
        let isNotEqual = 5 != 10;
        let isGreaterOrEqual = 10 >= 10;
        let isLessOrEqual = 5 <= 5;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sgt r1 10 5
            move r8 r1 #isGreater
            slt r2 5 10
            move r9 r2 #isLess
            seq r3 5 5
            move r10 r3 #isEqual
            sne r4 5 10
            move r11 r4 #isNotEqual
            sge r5 10 10
            move r12 r5 #isGreaterOrEqual
            sle r6 5 5
            move r13 r6 #isLessOrEqual
            "
        }
    );

    Ok(())
}

#[test]
fn test_logical_and_or_not() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let logic1 = 1 && 1;
        let logic2 = 1 || 0;
        let logic3 = !1;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            and r1 1 1
            move r8 r1 #logic1
            or r2 1 0
            move r9 r2 #logic2
            seq r3 1 0
            move r10 r3 #logic3
            "
        }
    );

    Ok(())
}

#[test]
fn test_complex_logic() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let logic = (10 > 5) && (5 < 10);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sgt r1 10 5
            slt r2 5 10
            and r3 r1 r2
            move r8 r3 #logic
            "
        }
    );

    Ok(())
}

#[test]
fn test_math_with_logic() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let logic = (1 + 2) > 1;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            
            "
        }
    );

    Ok(())
}
