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
            move r8 293.15 #i
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
            move r8 0 #a
            move r9 1 #b
            move r10 2 #c
            move r11 3 #d
            move r12 4 #e
            move r13 5 #f
            move r14 6 #g
            push 7 #h
            push 8 #i
            push 9 #j
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
            move r8 -1 #i
            "
        }
    );

    Ok(())
}
