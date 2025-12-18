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
        __internal_L1:
        pop ra
        j ra
        main:
        jal doSomething
        move r8 r15
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
        fn mul2(arg1) {
            return arg1 * 2;
        };
        loop {
            let arg1 = 123;
            let i = mul2(arg1);
            i = i ** 2;
        }
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            mul2:
            pop r8
            push ra
            mul r1 r8 2
            move r15 r1
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            __internal_L2:
            move r8 123
            push r8
            push r8
            jal mul2
            pop r8
            move r9 r15
            pow r1 r9 2
            move r9 r1
            j __internal_L2
            __internal_L3:
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
        compiled[0],
        super::super::Error::AgrumentMismatch(_, _)
    ));

    Ok(())
}

#[test]
fn inline_literal_args() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething(arg1, arg2) {
            return 5;
        };
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
            pop r8
            pop r9
            push ra
            move r15 5
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            move r8 123
            push r8
            push 12
            push 34
            jal doSomething
            pop r8
            move r9 r15
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
            pop r8
            pop r9
            push ra
            __internal_L1:
            pop ra
            j ra
            main:
            move r8 123
            push r8
            push r8
            push 456
            jal doSomething
            pop r8
            move r9 r15
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
            pop r8
            push ra
            move r15 456
            j __internal_L1
            __internal_L1:
            pop ra
            j ra
            main:
            push 123
            jal doSomething
            move r8 r15
            "
        }
    );

    Ok(())
}

#[test]
fn with_negative_return_literal() -> anyhow::Result<()> {
    let compiled = compile! {
        debug
        "
        fn doSomething() {
            return -1;
        };
        let i = doSomething();
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            doSomething:
            push ra
            move r15 -1
            __internal_L1:
            pop ra
            j ra
            main:
            jal doSomething
            move r8 r15
            "
        }
    );

    Ok(())
}
