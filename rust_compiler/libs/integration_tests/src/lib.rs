//! Integration tests for the Slang compiler with optimizer
//!
//! These tests compile Slang source code and verify both the compilation
//! and optimization passes work correctly together using snapshot testing.

#[cfg(test)]
mod bitwise_tests;
#[cfg(test)]
mod common;
#[cfg(test)]
mod function_tests;
#[cfg(test)]
mod number_literal_tests;
#[cfg(test)]
mod optimization_tests;

#[cfg(test)]
mod integration_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

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
