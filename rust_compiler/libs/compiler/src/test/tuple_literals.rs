#[cfg(test)]
mod test {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_tuple_literal_declaration() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let (x, y) = (1, 2);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 1
                move r9 2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_declaration_with_underscore() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let (x, _) = (1, 2);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 1
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_assignment() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let x = 0;
            let y = 0;
            (x, y) = (5, 10);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 0
                move r9 0
                move r8 5
                move r9 10
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_with_variables() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let a = 42;
            let b = 99;
            let (x, y) = (a, b);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 42
                move r9 99
                move r10 r8
                move r11 r9
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_three_elements() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let (x, y, z) = (1, 2, 3);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 1
                move r9 2
                move r10 3
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_assignment_with_underscore() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            let i = 0;
            let x = 123;
            (i, _) = (456, 789);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                main:
                move r8 0
                move r9 123
                move r8 456
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_simple() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getPair() {
                return (10, 20);
            };
            let (x, y) = getPair();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getPair:
                move r15 sp
                push ra
                push 10
                push 20
                move r15 1
                sub r0 sp 3
                get ra db r0
                j ra
                main:
                jal getPair
                pop r9
                pop r8
                move sp r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_with_underscore() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getPair() {
                return (5, 15);
            };
            let (x, _) = getPair();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getPair:
                move r15 sp
                push ra
                push 5
                push 15
                move r15 1
                sub r0 sp 3
                get ra db r0
                j ra
                main:
                jal getPair
                pop r0
                pop r8
                move sp r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_three_elements() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getTriple() {
                return (1, 2, 3);
            };
            let (a, b, c) = getTriple();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getTriple:
                move r15 sp
                push ra
                push 1
                push 2
                push 3
                move r15 1
                sub r0 sp 4
                get ra db r0
                j ra
                main:
                jal getTriple
                pop r10
                pop r9
                pop r8
                move sp r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_assignment() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getPair() {
                return (42, 84);
            };
            let i = 1;
            let j = 2;
            (i, j) = getPair();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getPair:
                move r15 sp
                push ra
                push 42
                push 84
                move r15 1
                sub r0 sp 3
                get ra db r0
                j ra
                main:
                move r8 1
                move r9 2
                jal getPair
                pop r9
                pop r8
                move sp r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_mismatch() -> anyhow::Result<()> {
        let errors = compile!(
            result
            r#"
            fn doSomething() {
                return (1, 2, 3);
            };
            let (x, y) = doSomething();
            "#
        );

        // Should have exactly one error about tuple size mismatch
        assert_eq!(errors.len(), 1);

        // Check for the specific TupleSizeMismatch error
        match &errors[0] {
            crate::Error::TupleSizeMismatch(expected_size, actual_count, _) => {
                assert_eq!(*expected_size, 3);
                assert_eq!(*actual_count, 2);
            }
            e => panic!("Expected TupleSizeMismatch error, got: {:?}", e),
        }

        Ok(())
    }

    #[test]
    fn test_tuple_return_called_by_non_tuple_return() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn doSomething() {
                return (1, 2);
            };
            
            fn doSomethingElse() {
                let (x, y) = doSomething();
                return y;
            };
            
            let returnedValue = doSomethingElse();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                doSomething:
                move r15 sp
                push ra
                push 1
                push 2
                move r15 1
                sub r0 sp 3
                get ra db r0
                j ra
                doSomethingElse:
                push ra
                jal doSomething
                pop r9
                pop r8
                move sp r15
                move r15 r9
                j __internal_L2
                __internal_L2:
                pop ra
                j ra
                main:
                jal doSomethingElse
                move r8 r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_non_tuple_return_called_by_tuple_return() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getValue() {
                return 42;
            };
            
            fn getTuple() {
                let x = getValue();
                return (x, x);
            };
            
            let (a, b) = getTuple();
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getValue:
                push ra
                move r15 42
                j __internal_L1
                __internal_L1:
                pop ra
                j ra
                getTuple:
                move r15 sp
                push ra
                jal getValue
                move r8 r15
                push r8
                push r8
                move r15 1
                sub r0 sp 3
                get ra db r0
                j ra
                main:
                jal getTuple
                pop r9
                pop r8
                move sp r15
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_size_mismatch() -> anyhow::Result<()> {
        let errors = compile!(
            result
            r#"
            let (x, y) = (1, 2, 3);
            "#
        );

        // Should have exactly one error about tuple size mismatch
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            errors[0],
            crate::Error::TupleSizeMismatch(_, _, _)
        ));

        Ok(())
    }

    #[test]
    fn test_multiple_tuple_returns_in_function() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn getValue(x) {
                if (x) {
                    return (1, 2);
                } else {
                    return (3, 4);
                }
            };
            
            let (a, b) = getValue(1);
            "#
        );

        assert_eq!(
            compiled,
            indoc! {
                "
                j main
                getValue:
                pop r8
                move r15 sp
                push ra
                beqz r8 __internal_L3
                push 1
                push 2
                move r15 0
                sub r0 sp 3
                get ra db r0
                j ra
                sub sp sp 2
                j __internal_L2
                __internal_L3:
                push 3
                push 4
                move r15 0
                sub r0 sp 3
                get ra db r0
                j ra
                sub sp sp 2
                __internal_L2:
                main:
                push 1
                jal getValue
                pop r9
                pop r8
                move sp r15
                "
            },
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_with_expression() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn add(x, y) {
                return (x, y);
            };
            
            let (a, b) = add(5, 10);
            "#
        );

        // Should compile - we're just passing the parameter variables through
        assert!(compiled.contains("add:"));
        assert!(compiled.contains("jal "));

        Ok(())
    }

    #[test]
    fn test_nested_function_tuple_calls() -> anyhow::Result<()> {
        let compiled = compile!(
            debug
            r#"
            fn inner() {
                return (1, 2);
            };
            
            fn outer() {
                let (x, y) = inner();
                return (y, x);
            };
            
            let (a, b) = outer();
            "#
        );

        // Both functions return tuples
        assert!(compiled.contains("inner:"));
        assert!(compiled.contains("outer:"));

        Ok(())
    }
}
