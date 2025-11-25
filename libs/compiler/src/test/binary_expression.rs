use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn simple_binary_expression() -> anyhow::Result<()> {
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
            add r1 1 2
            move r8 r1 #i
            "
        }
    );

    Ok(())
}

#[test]
fn nested_binary_expressions() -> anyhow::Result<()> {
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
fn stress_test_negation_with_stack_spillover() -> anyhow::Result<()> {
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
            "
        }
    );

    Ok(())
}
