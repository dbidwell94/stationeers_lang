use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn block_scope() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 10;
            {
                let y = 20;
                let z = x + y;
            }
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
            move r9 20
            add r1 r8 r9
            move r10 r1
            "
        }
    );

    Ok(())
}

#[test]
fn variable_scope_isolation() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 10;
            {
                let x = 20;
                let y = x;
            }
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
            move r9 20
            move r10 r9
            "
        }
    );

    Ok(())
}

#[test]
fn function_parameter_scope() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            fn double(x) {
                return x * 2;
            };
            
            let result = double(5);
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
            double:
            pop r8
            push ra
            mul r1 r8 2
            move r15 r1
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            push 5
            jal double
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn nested_block_scopes() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 1;
            {
                let x = 2;
                {
                    let x = 3;
                    let y = x;
                }
                let z = x;
            }
            let w = x;
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
            move r8 1
            move r9 2
            move r10 3
            move r11 r10
            move r10 r9
            move r9 r8
            "
        }
    );

    Ok(())
}

#[test]
fn variable_shadowing_in_conditional() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 10;
            
            if (true) {
                let x = 20;
            }
            
            let y = x;
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
            beqz 1 __internal_L1
            move r9 20
            __internal_L1:
            move r9 r8
            "
        }
    );

    Ok(())
}

#[test]
fn variable_shadowing_in_loop() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 0;
            
            loop {
                let x = x + 1;
                if (x > 5) {
                    break;
                }
            }
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
            move r8 0
            __internal_L1:
            add r1 r8 1
            move r9 r1
            sgt r2 r9 5
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
fn const_scope() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            const PI = 3.14;
            
            {
                const PI = 2.71;
                let x = PI;
            }
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
            move r8 2.71
            "
        }
    );

    Ok(())
}

#[test]
fn device_in_scope() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
            
            {
                let value = d0.Temperature;
            }
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
            l r1 d0 Temperature
            move r8 r1
            "
        }
    );

    Ok(())
}

#[test]
fn function_scope_isolation() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            fn func1() {
                let x = 10;
                return x;
            };
            
            fn func2() {
                let x = 20;
                return x;
            };
            
            let a = func1();
            let b = func2();
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
            func1:
            push ra
            move r8 10
            move r15 r8
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            func2:
            push ra
            move r8 20
            move r15 r8
            j __internal_L2
            __internal_L2:
            pop ra
            j ra
            main:
            jal func1
            move r8 r15
            push r8
            jal func2
            pop r8
            move r9 r15
            "
        }
    );

    Ok(())
}

#[test]
fn tuple_unpacking_scope() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            fn pair() {
                return (1, 2);
            };
            
            {
                let (x, y) = pair();
                let z = x + y;
            }
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
            pair:
            push ra
            push 1
            push 2
            move r15 2
            j __internal_L1
            __internal_L1:
            pop ra
            sub sp sp 2
            j ra
            main:
            jal pair
            pop r9
            pop r8
            add r1 r8 r9
            move r10 r1
            "
        }
    );

    Ok(())
}

#[test]
fn shadowing_doesnt_affect_outer() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            let x = 5;
            let y = x;
            {
                let x = 10;
                let z = x;
            }
            let w = x + y;
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
            move r9 r8
            move r10 10
            move r11 r10
            add r1 r8 r9
            move r10 r1
            "
        }
    );

    Ok(())
}
