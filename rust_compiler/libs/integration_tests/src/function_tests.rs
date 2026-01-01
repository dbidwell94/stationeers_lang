#[cfg(test)]
mod function_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

    #[test]
    fn test_simple_leaf_function() {
        let source = "fn test() { let x = 10; }";
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_function_with_call() {
        let source = indoc! {"
            fn add(a, b) { return a + b; }
            fn main() { let x = add(5, 10); }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_leaf_function_no_stack_frame() {
        let source = indoc! {"
            fn increment(x) { 
                x = x + 1;
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_nested_function_calls() {
        let source = indoc! {"
            fn add(a, b) { return a + b; }
            fn multiply(x, y) { return x * 2; }
            fn complex(a, b) {
                let sum = add(a, b);
                let doubled = multiply(sum, 2);
                return doubled;
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
