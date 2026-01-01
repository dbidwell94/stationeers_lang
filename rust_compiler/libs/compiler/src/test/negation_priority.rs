use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn simple_negation() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = -5;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 -5
            "
        }
    );

    Ok(())
}

#[test]
fn negation_of_variable() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 10;
            let y = -x;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 10
            sub r1 0 r8
            move r9 r1
            "
        }
    );

    Ok(())
}

#[test]
fn double_negation() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = -(-5);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 5
            "
        }
    );

    Ok(())
}

#[test]
fn negation_in_expression() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 10 + (-5);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 5
            "
        }
    );

    Ok(())
}

#[test]
fn negation_with_multiplication() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = -3 * 4;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 -12
            "
        }
    );

    Ok(())
}

#[test]
fn parentheses_priority() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = (2 + 3) * 4;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 20
            "
        }
    );

    Ok(())
}

#[test]
fn nested_parentheses() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = ((2 + 3) * (4 - 1));
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 15
            "
        }
    );

    Ok(())
}

#[test]
fn parentheses_with_variables() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let a = 5;
            let b = 10;
            let c = (a + b) * 2;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // Should calculate (5 + 10) * 2 = 30
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 5
            move r9 10
            add r1 r8 r9
            mul r2 r1 2
            move r10 r2
            "
        }
    );

    Ok(())
}

#[test]
fn priority_affects_result() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let with_priority = (2 + 3) * 4;
            let without_priority = 2 + 3 * 4;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // with_priority should be 20, without_priority should be 14
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            move r8 20
            move r9 14
            "
        }
    );

    Ok(())
}

#[test]
fn negation_of_expression() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = -(2 + 3);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // Should be -5 (constant folded)
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            sub r1 0 5
            move r8 r1
            "
        }
    );

    Ok(())
}

#[test]
fn complex_negation_and_priority() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = -((10 - 5) * 2);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // Should be -(5 * 2) = -10 (folded to constant)
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            sub r1 0 10
            move r8 r1
            "
        }
    );

    Ok(())
}

#[test]
fn negation_in_logical_expression() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = !(-5);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // -5 is truthy, so !(-5) should be 0
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            sub r1 0 5
            seq r2 r1 0
            move r8 r2
            "
        }
    );

    Ok(())
}

#[test]
fn parentheses_in_comparison() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = (10 + 5) > (3 * 4);
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // (10 + 5) = 15 > (3 * 4) = 12, so true (1)
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            sgt r1 15 12
            move r8 r1
            "
        }
    );

    Ok(())
}
