use anyhow::Result;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn test_acos() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = acos(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            acos r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_asin() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = asin(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            asin r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_atan() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = atan(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            atan r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_atan2() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = atan2(123, 456);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            atan2 r15 123 456
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_abs() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = abs(-123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            abs r15 -123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_ceil() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = ceil(123.90);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            ceil r15 123.90
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_cos() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = cos(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            cos r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_floor() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = floor(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            floor r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_log() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = log(123);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            log r15 123
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_max() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = max(123, 456);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            max r15 123 456
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_max_from_game() -> Result<()> {
    let compiled = compile! {
        debug
        r#"
        let item = 0;
        item = max(1 + 2, 2);
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            max r15 3 2
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_min() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = min(123, 456);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            min r15 123 456
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_rand() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = rand();
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            rand r15
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_sin() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = sin(3);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sin r15 3
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_sqrt() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = sqrt(3);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            sqrt r15 3
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_tan() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = tan(3);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            tan r15 3
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_trunc() -> Result<()> {
    let compiled = compile! {
        debug
        "
        let i = trunc(3.234);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            trunc r15 3.234
            move r8 r15
            "
        }
    );

    Ok(())
}
