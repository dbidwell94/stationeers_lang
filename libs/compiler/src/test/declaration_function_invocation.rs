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

#[test]
fn inline_literal_args() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething(arg1, arg2) {};
        let thisVariableShouldStayInPlace = 123;
        let returnedValue = doSomething(12, 34);
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            doSomething:
            pop r8 #arg2
            pop r9 #arg1
            push ra
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            move r8 123 #thisVariableShouldStayInPlace
            push r8
            push 12
            push 34
            jal doSomething
            sub r0 sp 1
            get r8 db r0
            sub sp sp 1
            move r9 r15 #returnedValue
            "
        }
    );

    Ok(())
}

#[test]
fn mixed_args() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        let arg1 = 123;
        let returnValue = doSomething(arg1, 456);
        fn doSomething(arg1, arg2) {};
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            doSomething:
            pop r8 #arg2
            pop r9 #arg1
            push ra
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            move r8 123 #arg1
            push r8
            push r8
            push 456
            jal doSomething
            sub r0 sp 1
            get r8 db r0
            sub sp sp 1
            move r9 r15 #returnValue
            "
        }
    );

    Ok(())
}

#[test]
fn with_return_statement() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething(arg1) {
            return 456;
        };

        let returned = doSomething(123);
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
            move r15 456
            sub r0 sp 1
            get ra db r0
            sub sp sp 1
            j ra
            main:
            push 123
            jal doSomething
            move r8 r15 #returned
            "
        }
    );

    Ok(())
}
