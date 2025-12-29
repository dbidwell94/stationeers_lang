use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn variable_declaration_numeric_literal() -> anyhow::Result<()> {
    let compiled = crate::compile! {
        debug r#"
            let i = 20c;
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 293.15
            "
        }
    );

    Ok(())
}

#[test]
fn variable_declaration_numeric_literal_stack_spillover() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        r#"
        let a = 0;
        let b = 1;
        let c = 2;
        let d = 3;
        let e = 4;
        let f = 5;
        let g = 6;
        let h = 7;
        let i = 8;
        let j = 9;
    "#};

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 0
            move r9 1
            move r10 2
            move r11 3
            move r12 4
            move r13 5
            move r14 6
            push 7
            push 8
            push 9
            sub sp sp 3
            "
        }
    );

    Ok(())
}

#[test]
fn variable_declaration_negative() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let i = -1;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 -1
            "
        }
    );

    Ok(())
}

#[test]
fn test_boolean_declaration() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let t = true;
        let f = false;
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
            "
        }
    );

    Ok(())
}

#[test]
fn test_boolean_return() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn getTrue() {
            return true;
        };

        let val = getTrue();
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            getTrue:
            push ra
            move r15 1
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            jal getTrue
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn test_const_hash_expr() -> anyhow::Result<()> {
    let compiled = compile!(debug r#"
        const nameHash = hash("AccessCard");
        device self = "db";

        self.Setting = nameHash;
    "#);

    assert_eq!(
        compiled,
        indoc! {
        "
            j main
            main:
            s db Setting -732925934
        "
        }
    );
    Ok(())
}

#[test]
fn test_declaration_is_const() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        r#"
            const MAX = 100;

            let max = MAX;
        "#
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 100
            "
        }
    );

    Ok(())
}
