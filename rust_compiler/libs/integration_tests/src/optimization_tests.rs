#[cfg(test)]
mod optimization_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

    #[test]
    fn test_constant_folding() {
        let source = "let x = 5 + 10;";
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_algebraic_simplification() {
        let source = "let x = 5; let y = x * 1;";
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_strength_reduction() {
        let source = "fn double(x) { return x * 2; }";
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_dead_code_elimination() {
        let source = indoc! {"
            fn compute(x) {
                let unused = 20;
                return x + 1;
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_peephole_comparison_fusion() {
        let source = indoc! {"
            fn compare(x, y) {
                if (x > y) {
                    let z = 1;
                }
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_select_optimization() {
        let source = indoc! {"
            fn ternary(cond) {
                let result = 0;
                if (cond) {
                    result = 10;
                } else {
                    result = 20;
                }
                return result;
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_complex_arithmetic() {
        let source = indoc! {"
            fn compute(a, b, c) {
                let x = a * 2;
                let y = b + 0;
                let z = c * 1;
                return x + y + z;
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    /// Regression test: two ls() syscalls on the LHS and RHS of a binary expression.
    /// The unoptimized output requires a spill `move` because both calls write to r15.
    /// The optimizer's register-forwarding pass should eliminate both intermediate moves,
    /// writing directly to the final destination registers.
    #[test]
    fn test_syscall_binary_expression_register_forwarding() {
        let source = indoc! {r#"
            device filtration = "d0";
            let filter = ls(filtration, 0, "Quantity") + ls(filtration, 1, "Quantity");
        "#};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
