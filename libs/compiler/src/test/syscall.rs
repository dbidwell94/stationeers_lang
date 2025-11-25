use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_yield_syscall() -> anyhow::Result<()> {
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
fn test_sleep_syscall() -> anyhow::Result<()> {
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
