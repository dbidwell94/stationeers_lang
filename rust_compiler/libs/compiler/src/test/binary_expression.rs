use crate::compile;
use anyhow::Result;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn simple_binary_expression() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = 1 + 2;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 3 #i
            "
        }
    );

    Ok(())
}

#[test]
fn nested_binary_expressions() -> Result<()> {
    let compiled = compile! {
        debug
        "
        fn calculateArgs(arg1, arg2, arg3) {
            return (arg1 + arg2) * arg3;
        };

        let returned = calculateArgs(10, 20, 30) + 100;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            calculateArgs:
            pop r8 #arg3
            pop r9 #arg2
            pop r10 #arg1
            push ra
            add r1 r10 r9
            mul r2 r1 r8
            move r15 r2
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            push 10
            push 20
            push 30
            jal calculateArgs
            move r1 r15 #__binary_temp_3
            add r2 r1 100
            move r8 r2 #returned
            "
        }
    );

    Ok(())
}

#[test]
fn stress_test_constant_folding() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let negationHell = (-1 + -2) * (-3 + (-4 * (-5 + -6)));
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 -123 #negationHell
            "
        }
    );

    Ok(())
}

#[test]
fn test_constant_folding_with_variables_mixed_in() -> Result<()> {
    let compiled = compile! {
        debug
        r#"
        device self = "db";
        let i = 1 - 3 * (1 + 123.4) * self.Setting + 245c;
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            l r1 db Setting
            mul r2 373.2 r1
            sub r3 1 r2
            add r4 r3 518.15
            move r8 r4 #i
            "
        }
    );

    Ok(())
}

#[test]
fn test_ternary_expression() -> Result<()> {
    let compiled = compile! {
        debug
        r#"
        let i = 1 > 2 ? 15 : 20;
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sgt r1 1 2
            select r2 r1 15 20
            move r8 r2 #i
            "
        }
    );

    Ok(())
}

#[test]
fn test_ternary_expression_assignment() -> Result<()> {
    let compiled = compile! {
        debug
        r#"
        let i = 0;
        i = 1 > 2 ? 15 : 20;
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0 #i
            sgt r1 1 2
            select r2 r1 15 20
            move r8 r2 #i
            "
        }
    );

    Ok(())
}
