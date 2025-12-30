use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn zero_value_handling() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 0;
            let y = x + 0;
            let z = x * 100;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            add r1 r8 0
            move r9 r1
            mul r2 r8 100
            move r10 r2
            "
        }
    );

    Ok(())
}

#[test]
fn negative_number_handling() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = -100;
            let y = -x;
            let z = -(-50);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 -100
            move r9 50
            "
        }
    );

    Ok(())
}

#[test]
fn large_number_constants() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 999999999;
            let y = x + 1;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 999999999
            add r1 r8 1
            move r9 r1
            "
        }
    );

    Ok(())
}

#[test]
fn floating_point_precision() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let pi = 3.14159265;
            let e = 2.71828182;
            let sum = pi + e;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 3.14159265
            move r9 2.71828182
            add r1 r8 r9
            move r10 r1
            "
        }
    );

    Ok(())
}

#[test]
fn temperature_unit_conversion() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let celsius = 20c;
            let fahrenheit = 68f;
            let kelvin = 293.15k;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 293.15
            move r9 293.15
            move r10 293.15
            "
        }
    );

    Ok(())
}

#[test]
fn mixed_temperature_units() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let c = 0c;
            let f = 32f;
            let k = 273.15k;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 273.15
            move r9 273.15
            move r10 273.15
            "
        }
    );

    Ok(())
}

#[test]
fn boolean_constant_folding() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = true;
            let y = false;
            let z = true && true;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 1
            move r9 0
            and r1 1 1
            move r10 r1
            "
        }
    );

    Ok(())
}

#[test]
fn empty_block() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 5;
            {
            }
            let y = x;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 5
            move r9 r8
            "
        }
    );

    Ok(())
}

#[test]
fn multiple_statements_same_line() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 1; let y = 2; let z = 3;
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
            "
        }
    );

    Ok(())
}

#[test]
fn function_with_no_return() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            fn no_return() {
                let x = 5;
            };
            
            no_return();
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            no_return:
            push ra
            move r8 5
            __internal_L1:
            pop ra
            j ra
            main:
            jal no_return
            move r1 r15
            "
        }
    );

    Ok(())
}

#[test]
fn deeply_nested_expressions() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = ((((((((1 + 2) + 3) + 4) + 5) + 6) + 7) + 8) + 9);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 45
            "
        }
    );

    Ok(())
}

#[test]
fn constant_folding_with_operations() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 10 * 5 + 3 - 2;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 51
            "
        }
    );

    Ok(())
}

#[test]
fn constant_folding_with_division() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 100 / 2 / 5;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 10
            "
        }
    );

    Ok(())
}

#[test]
fn modulo_operation() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 17 % 5;
            let y = 10 % 3;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 2
            move r9 1
            "
        }
    );

    Ok(())
}

#[test]
fn exponentiation() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 2 ^ 8;
            let y = 3 ^ 3;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r0 3
            move r1 3
            "
        }
    );

    Ok(())
}

#[test]
fn comparison_with_zero() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 0 == 0;
            let y = 0 < 1;
            let z = 0 > -1;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            seq r1 0 0
            move r8 r1
            slt r2 0 1
            move r9 r2
            sgt r3 0 -1
            move r10 r3
            "
        }
    );

    Ok(())
}

#[test]
fn boolean_negation_edge_cases() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = !0;
            let y = !1;
            let z = !100;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            seq r1 0 0
            move r8 r1
            seq r2 1 0
            move r9 r2
            seq r3 100 0
            move r10 r3
            "
        }
    );

    Ok(())
}

#[test]
fn function_with_many_parameters() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            fn many_params(a, b, c, d, e, f, g, h) {
                return a + b + c + d + e + f + g + h;
            };
            
            let result = many_params(1, 2, 3, 4, 5, 6, 7, 8);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            many_params:
            pop r8
            pop r9
            pop r10
            pop r11
            pop r12
            pop r13
            pop r14
            push ra
            sub r0 sp 2
            get r1 db r0
            add r2 r1 r14
            add r3 r2 r13
            add r4 r3 r12
            add r5 r4 r11
            add r6 r5 r10
            add r7 r6 r9
            add r1 r7 r8
            move r15 r1
            j __internal_L1
            __internal_L1:
            pop ra
            sub sp sp 1
            j ra
            main:
            push 1
            push 2
            push 3
            push 4
            push 5
            push 6
            push 7
            push 8
            jal many_params
            move r8 r15
            "
        }
    );

    Ok(())
}
