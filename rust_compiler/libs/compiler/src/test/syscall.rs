use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_yield() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        "
        yield();
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
            yield
            "
        }
    );

    Ok(())
}

#[test]
fn test_sleep() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        "
        sleep(3);
        let sleepAmount = 15;
        sleep(sleepAmount);
        sleep(sleepAmount * 2);
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
            sleep 3
            move r8 15
            sleep r8
            mul r1 r8 2
            sleep r1
            "
        }
    );

    Ok(())
}

#[test]
fn test_set_on_device() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
            device airConditioner = "d0";
            let internalTemp = 20c;

            set(airConditioner, "On", internalTemp > 25c);
        "#
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
            move r8 293.15
            sgt r1 r8 298.15
            s d0 On r1
            "
        }
    );

    Ok(())
}

#[test]
fn test_set_on_device_batched() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        const doorHash = hash("Door");
        setBatched(doorHash, "Lock", true);
        "#
    };

    assert!(
        compiled.errors.is_empty(),
        "Expected no errors, got: {:?}",
        compiled.errors
    );

    assert_eq!(
        compiled.output,
        indoc! {
            r#"
            j main
            main:
            sb 718797587 Lock 1
            "#
        }
    );
    Ok(())
}

#[test]
fn test_set_on_device_batched_named() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        device dev = "d0";
        const devName = hash("test");

        sbn(dev, devName, "On", 12);
        "#
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
            sbn d0 -662733300 On 12
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_from_device() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        device airCon = "d0";

        let setting = load(airCon, "On");
        "#
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
            l r15 d0 On
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_from_slot() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        device airCon = "d0";

        let setting = ls(airCon, 0, "Occupied");
        "#
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
            ls r15 d0 0 Occupied
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_from_slot_binary_expression() -> anyhow::Result<()> {
    // Regression test: two ls() calls in a binary expression must not both
    // write to RETURN_REGISTER (r15) and then add r15+r15 (slot 0 + slot 0).
    // The LHS result must be spilled to a fresh register before the RHS ls()
    // overwrites r15.
    let compiled = compile! {
        check
        r#"
        device filtration = "d0";

        let filter = ls(filtration, 0, "Quantity") + ls(filtration, 1, "Quantity");
        "#
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
            ls r15 d0 0 Quantity
            move r1 r15
            ls r15 d0 1 Quantity
            add r2 r1 r15
            move r8 r2
            "
        }
    );

    Ok(())
}

#[test]
fn test_set_slot() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        device airCon = "d0";

        ss(airCon, 0, "Occupied", true);
        "#
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
            ss d0 0 Occupied 1
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_reagent() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        device thingy = "d0";

        let something = lr(thingy, "Contents", hash("Iron"));
        "#
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
            lr r15 d0 Contents -666742878
            move r8 r15
            "
        }
    );

    Ok(())
}
#[test]
fn test_clr() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        "
        device stackDevice = \"d0\";
        clr(stackDevice);
        let deviceRef = 5;
        clr(deviceRef);
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
            clr d0
            move r8 5
            clr r8
            "
        }
    );

    Ok(())
}
#[test]
fn test_rmap() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        "
        device printer = \"d0\";
        let reagentHash = 12345;
        let itemHash = rmap(printer, reagentHash);
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
            move r8 12345
            rmap r15 d0 r8
            move r9 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_batched_slot() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        let typeHash = 12345;
        let slotOccupied = lbs(typeHash, 0, "Occupied", "Average");
        "#
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
            move r8 12345
            lbs r15 r8 0 Occupied Average
            move r9 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_load_batched_named_slot() -> anyhow::Result<()> {
    let compiled = compile! {
        check
        r#"
        let typeHash = 12345;
        let nameHash = 67890;
        let slotOccupied = lbns(typeHash, nameHash, 0, "Occupied", "Average");
        "#
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
            move r8 12345
            move r9 67890
            lbns r15 r8 r9 0 Occupied Average
            move r10 r15
            "
        }
    );

    Ok(())
}
