#[cfg(test)]
mod test {
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_tuple_literal_declaration() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let (x, y) = (1, 2);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            let (x, _) = (1, 2);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            let x = 0;
            let y = 0;
            (x, y) = (5, 10);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            let a = 42;
            let b = 99;
            let (x, y) = (a, b);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            let (x, y, z) = (1, 2, 3);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            let i = 0;
            let x = 123;
            (i, _) = (456, 789);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
            check
            r#"
            fn getPair() {
                return (10, 20);
            };
            let (x, y) = getPair();
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getPair:
                push ra
                push 10
                push 20
                move r15 20
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                main:
                jal getPair
                pop r9
                pop r8
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_with_underscore() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn getPair() {
                return (5, 15);
            };
            let (x, _) = getPair();
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getPair:
                push ra
                push 5
                push 15
                move r15 15
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                main:
                jal getPair
                pop r0
                pop r8
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_three_elements() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn getTriple() {
                return (1, 2, 3);
            };
            let (a, b, c) = getTriple();
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getTriple:
                push ra
                push 1
                push 2
                push 3
                move r15 3
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 3
                j ra
                main:
                jal getTriple
                pop r10
                pop r9
                pop r8
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_assignment() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn getPair() {
                return (42, 84);
            };
            let i = 1;
            let j = 2;
            (i, j) = getPair();
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getPair:
                push ra
                push 42
                push 84
                move r15 84
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                main:
                move r8 1
                move r9 2
                jal getPair
                pop r9
                pop r8
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
            check
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

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                doSomething:
                push ra
                push 1
                push 2
                move r15 2
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                doSomethingElse:
                push ra
                jal doSomething
                pop r9
                pop r8
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
            check
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

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
                push ra
                jal getValue
                move r8 r15
                push r8
                push r8
                move r15 r8
                j __internal_L2
                __internal_L2:
                pop ra
                sub sp sp 2
                j ra
                main:
                jal getTuple
                pop r9
                pop r8
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
            check
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

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getValue:
                pop r8
                push ra
                beqz r8 __internal_L3
                push 1
                push 2
                move r15 2
                j __internal_L1
                j __internal_L2
                __internal_L3:
                push 3
                push 4
                move r15 4
                j __internal_L1
                __internal_L2:
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                main:
                push 1
                jal getValue
                pop r9
                pop r8
                "
            },
        );

        Ok(())
    }

    #[test]
    fn test_tuple_return_with_expression() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn add(x, y) {
                return (x, y);
            };
            
            let (a, b) = add(5, 10);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                add:
                pop r8
                pop r9
                push ra
                push r9
                push r8
                move r15 r8
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                main:
                push 5
                push 10
                jal add
                pop r9
                pop r8
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_nested_function_tuple_calls() -> anyhow::Result<()> {
        let compiled = compile!(
            check
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

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                inner:
                push ra
                push 1
                push 2
                move r15 2
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 2
                j ra
                outer:
                push ra
                jal inner
                pop r9
                pop r8
                push r9
                push r8
                move r15 r8
                j __internal_L2
                __internal_L2:
                pop ra
                sub sp sp 2
                j ra
                main:
                jal outer
                pop r9
                pop r8
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_with_constant_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let (a, b) = (1 + 2, 3 * 4);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                move r8 3
                move r9 12
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_with_variable_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let x = 5;
            let y = 10;
            let (a, b) = (x + 1, y * 2);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                move r8 5
                move r9 10
                add r1 r8 1
                move r10 r1
                mul r2 r9 2
                move r11 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_assignment_with_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let a = 0;
            let b = 0;
            let x = 5;
            (a, b) = (x + 1, x * 2);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                move r8 0
                move r9 0
                move r10 5
                add r1 r10 1
                move r8 r1
                mul r2 r10 2
                move r9 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_literal_with_function_calls() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn getValue() { return 42; };
            fn getOther() { return 99; };
            
            let (a, b) = (getValue(), getOther());
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
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
                getOther:
                push ra
                move r15 99
                j __internal_L2
                __internal_L2:
                pop ra
                j ra
                main:
                push r8
                jal getValue
                pop r8
                move r1 r15
                move r8 r1
                push r8
                push r9
                jal getOther
                pop r9
                pop r8
                move r2 r15
                move r9 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_logical_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let x = 1;
            let y = 0;
            let (a, b) = (x && y, x || y);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                move r8 1
                move r9 0
                and r1 r8 r9
                move r10 r1
                or r2 r8 r9
                move r11 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_comparison_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            let x = 5;
            let y = 10;
            let (a, b) = (x > y, x < y);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                move r8 5
                move r9 10
                sgt r1 r8 r9
                move r10 r1
                slt r2 r8 r9
                move r11 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_device_property_access() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            device sensor = "d0";
            device display = "d1";
            
            let (temp, pressure) = (sensor.Temperature, sensor.Pressure);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                main:
                l r1 d0 Temperature
                move r8 r1
                l r2 d0 Pressure
                move r9 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_device_property_and_function_call() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            device self = "db";
            
            fn getY() {
                return 42;
            }
            
            let (x, y) = (self.Setting, getY());
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getY:
                push ra
                move r15 42
                j __internal_L1
                __internal_L1:
                pop ra
                j ra
                main:
                l r1 db Setting
                move r8 r1
                push r8
                push r9
                jal getY
                pop r9
                pop r8
                move r2 r15
                move r9 r2
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_function_call_expressions() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn getValue() { return 10; }
            fn getOther() { return 20; }
            
            let (a, b) = (getValue() + 5, getOther() * 2);
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                getValue:
                push ra
                move r15 10
                j __internal_L1
                __internal_L1:
                pop ra
                j ra
                getOther:
                push ra
                move r15 20
                j __internal_L2
                __internal_L2:
                pop ra
                j ra
                main:
                push r8
                jal getValue
                pop r8
                move r1 r15
                add r2 r1 5
                move r8 r2
                push r8
                push r9
                jal getOther
                pop r9
                pop r8
                move r3 r15
                mul r4 r3 2
                move r9 r4
                "
            }
        );

        Ok(())
    }

    #[test]
    fn test_tuple_with_stack_spillover() -> anyhow::Result<()> {
        let compiled = compile!(
            check
            r#"
            fn get8() {
                return (1, 2, 3, 4, 5, 6, 7, 8);
            }
            
            let (a, b, c, d, e, f, g, h) = get8();
            let sum = a + h;
            "#
        );

        assert!(
            compiled.errors.is_empty(),
            "Expected no errors, got: {:?}",
            compiled.errors
        );

        assert_eq!(
            compiled.output,
            indoc! {
                "
                j main
                get8:
                push ra
                push 1
                push 2
                push 3
                push 4
                push 5
                push 6
                push 7
                push 8
                move r15 8
                j __internal_L1
                __internal_L1:
                pop ra
                sub sp sp 8
                j ra
                main:
                jal get8
                pop r0
                sub r0 sp 0
                put db r0 r0
                pop r14
                pop r13
                pop r12
                pop r11
                pop r10
                pop r9
                pop r8
                sub r0 sp 1
                get r1 db r0
                add r2 r8 r1
                push r2
                sub sp sp 2
                "
            }
        );

        Ok(())
    }
}
