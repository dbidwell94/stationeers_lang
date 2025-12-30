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

        // Basic structure check - should have the function label and main
        assert!(compiled.contains("getPair:"));
        assert!(compiled.contains("main:"));
        assert!(compiled.contains("jal getPair"));

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

        // Basic structure check
        assert!(compiled.contains("getPair:"));
        assert!(compiled.contains("main:"));
        assert!(compiled.contains("jal getPair"));

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

        // Basic structure check
        assert!(compiled.contains("getTriple:"));
        assert!(compiled.contains("main:"));
        assert!(compiled.contains("jal getTriple"));

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
            let i = 0;
            let j = 0;
            (i, j) = getPair();
            "#
        );

        // Basic structure check
        assert!(compiled.contains("getPair:"));
        assert!(compiled.contains("main:"));
        assert!(compiled.contains("jal getPair"));

        Ok(())
    }
}
