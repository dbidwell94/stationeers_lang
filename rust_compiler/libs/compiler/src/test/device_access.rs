use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn device_declaration() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
        "
    };

    // Declaration only emits the jump label header
    assert_eq!(compiled.output, "j main\n");

    Ok(())
}

#[test]
fn device_property_read() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device ac = \"d0\";
            let temp = ac.Temperature;
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
fn device_property_write() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device ac = \"d0\";
            ac.On = 1;
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
            s d0 On 1
            "
        }
    );

    Ok(())
}

#[test]
fn multiple_device_declarations() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
            device d1 = \"d1\";
            device d2 = \"d2\";
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // Declarations only emit the header when unused
    assert_eq!(compiled.output, "j main\n");

    Ok(())
}

#[test]
fn device_with_variable_interaction() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device sensor = \"d0\";
            let reading = sensor.Temperature;
            let threshold = 373.15;
            let alert = reading > threshold;
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
            move r9 373.15
            sgt r2 r8 r9
            move r10 r2
            "
        }
    );

    Ok(())
}

#[test]
fn device_property_in_arithmetic() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
            let result = d0.Temperature + 100;
        "
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    // Verify that we load property, add 100, and move to result
    assert_eq!(
        compiled.output,
        indoc! {
            "
            j main
            main:
            l r1 d0 Temperature
            add r2 r1 100
            move r8 r2
            "
        }
    );

    Ok(())
}

#[test]
fn device_used_in_function() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
            
            fn check_power() {
                return d0.On;
            };
            
            let powered = check_power();
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
            check_power:
            push ra
            l r1 d0 On
            move r15 r1
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            jal check_power
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn device_in_conditional() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device d0 = \"d0\";
            
            if (d0.On) {
                let x = 1;
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
            l r1 d0 On
            beqz r1 __internal_L1
            move r8 1
            __internal_L1:
            "
        }
    );

    Ok(())
}

#[test]
fn device_property_with_underscore_name() -> anyhow::Result<()> {
    let compiled = compile! {
        check "
            device cool_device = \"d0\";
            let value = cool_device.SomeProperty;
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
            l r1 d0 SomeProperty
            move r8 r1
            "
        }
    );

    Ok(())
}
