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
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sleep 3
            move r8 15 #sleepAmount
            sleep r8
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

            let shouldToggleOn = internalTemp > 25c;
            setOnDevice(airConditioner, "On", shouldToggleOn);
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 293.15 #internalTemp
            sgt r1 r8 298.15
            move r9 r1 #shouldToggleOn
            s d0 On r9
            "
        }
    );

    Ok(())
}
