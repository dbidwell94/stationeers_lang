//! Integration tests for the Slang compiler with optimizer
//!
//! These tests compile Slang source code and verify both the compilation
//! and optimization passes work correctly together using snapshot testing.

#[cfg(test)]
mod tests {
    use compiler::Compiler;
    use indoc::indoc;
    use parser::Parser;
    use tokenizer::Tokenizer;

    /// Compile Slang source code and return both unoptimized and optimized output
    fn compile_with_and_without_optimization(source: &str) -> String {
        // Compile for unoptimized output
        let tokenizer = Tokenizer::from(source);
        let parser = Parser::new(tokenizer);
        let compiler = Compiler::new(parser, None);
        let result = compiler.compile();

        assert!(
            result.errors.is_empty(),
            "Compilation errors: {:?}",
            result.errors
        );

        // Get unoptimized output
        let mut unoptimized_writer = std::io::BufWriter::new(Vec::new());
        result
            .instructions
            .write(&mut unoptimized_writer)
            .expect("Failed to write unoptimized output");
        let unoptimized_bytes = unoptimized_writer
            .into_inner()
            .expect("Failed to get bytes");
        let unoptimized = String::from_utf8(unoptimized_bytes).expect("Invalid UTF-8");

        // Compile again for optimized output
        let tokenizer2 = Tokenizer::from(source);
        let parser2 = Parser::new(tokenizer2);
        let compiler2 = Compiler::new(parser2, None);
        let result2 = compiler2.compile();

        // Apply optimizations
        let optimized_instructions = optimizer::optimize(result2.instructions);

        // Get optimized output
        let mut optimized_writer = std::io::BufWriter::new(Vec::new());
        optimized_instructions
            .write(&mut optimized_writer)
            .expect("Failed to write optimized output");
        let optimized_bytes = optimized_writer.into_inner().expect("Failed to get bytes");
        let optimized = String::from_utf8(optimized_bytes).expect("Invalid UTF-8");

        // Combine both outputs with clear separators
        format!(
            "## Unoptimized Output\n\n{}\n## Optimized Output\n\n{}",
            unoptimized, optimized
        )
    }

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

    #[test]
    fn test_tuples() {
        let source = indoc! {r#"
            device self = "db";
            device day  = "d0";

            fn getSomethingElse(input) {
                return input + 1;
            }

            fn getSensorData() {
                return (
                    day.Vertical,
                    day.Horizontal,
                    getSomethingElse(3)
                );
            }

            loop {
                yield();
                
                let (vertical, horizontal, _) = getSensorData();

                (horizontal, _, _) = getSensorData();

                self.Setting = horizontal;
            }
        "#};

        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_larre_script() {
        let source = include_str!("./test_files/test_larre_script.stlg");
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_reagent_processing() {
        let source = include_str!("./test_files/reagent_processing.stlg");
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_setbatched_with_member_access() {
        let source = indoc! {r#"
            const SENSOR = 20088;
            const PANELS = hash("StructureSolarPanelDual");
            
            loop {
              setBatched(PANELS, "Horizontal", SENSOR.Horizontal);
              setBatched(PANELS, "Vertical", SENSOR.Vertical + 90);
              yield();
            }
        "#};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
