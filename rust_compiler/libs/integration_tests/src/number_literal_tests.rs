#[cfg(test)]
mod number_literal_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

    #[test]
    fn test_binary_literals() {
        let source = indoc! {"
            let binary = 0b1010_1100;
            let octal = 0o755;
            let hex_upper = 0xDEAD_BEEF;
            let hex_lower = 0xcafe;
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_number_literal_optimization() {
        let source = indoc! {"
            let decimal = 42_000;
            let negative_hex = -0xFF;
            let negative_binary = -0b1111_0000;
            let temp_c = 100c;
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
