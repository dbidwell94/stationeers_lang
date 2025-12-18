use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_yield() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        yield();
        "
    };

    assert_eq!(
        compiled,
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
        debug
        "
        sleep(3);
        let sleepAmount = 15;
        sleep(sleepAmount);
        sleep(sleepAmount * 2);
        "
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
            device airConditioner = "d0";
            let internalTemp = 20c;

            set(airConditioner, "On", internalTemp > 25c);
        "#
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
        const doorHash = hash("Door");
        setBatched(doorHash, "Lock", true);
        "#
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
        device dev = "d0";
        const devName = hash("test");

        sbn(dev, devName, "On", 12);
        "#
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
        device airCon = "d0";

        let setting = load(airCon, "On");
        "#
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
        device airCon = "d0";

        let setting = ls(airCon, 0, "Occupied");
        "#
    };

    assert_eq!(
        compiled,
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
fn test_set_slot() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        r#"
        device airCon = "d0";

        ss(airCon, 0, "Occupied", true);
        "#
    };

    assert_eq!(
        compiled,
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
        debug
        r#"
        device thingy = "d0";

        let something = lr(thingy, "Contents", hash("Iron"));
        "#
    };

    assert_eq!(
        compiled,
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
