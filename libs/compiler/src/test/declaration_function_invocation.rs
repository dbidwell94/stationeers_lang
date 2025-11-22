use crate::compile;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn no_arguments() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething() {};
        let i = doSomething();
        "
    };

    let to_test = indoc! {
        "
        j main
        doSomething:
        push ra
        sub r0 sp 1
        get ra db r0
        sub sp sp 1
        j ra
        main:
        jal doSomething
        move r8 r15 #i
        "
    };

    assert_eq!(compiled, to_test);

    Ok(())
}

#[test]
fn let_var_args() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething(arg1) {};
        let arg1 = 123;
        let i = doSomething(arg1);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            doSomething:
            pop r8 #arg1
            push ra
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            move r8 123 #arg1
            push r8
            push r8
            jal doSomething
            sub r0 sp 1
            get r8 db r0
            sub sp sp 1
            move r9 r15 #i
            "
        }
    );

    Ok(())
}

#[test]
fn incorrect_args_count() -> anyhow::Result<()> {
    let compiled = compile! {
        result
        "
        fn doSomething(arg1, arg2){};
        let i = doSomething();
        "
    };

    assert!(matches!(
        compiled,
        Err(super::super::Error::AgrumentMismatch(_))
    ));

    Ok(())
}
