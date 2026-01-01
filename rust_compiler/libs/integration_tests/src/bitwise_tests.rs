#[cfg(test)]
mod bitwise_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

    #[test]
    fn test_bitwise_operations() {
        let source = indoc! {"
            let a = 5;
            let b = 3;
            let and_result = a & b;
            let or_result = a | b;
            let xor_result = a ^ b;
            let not_result = ~a;
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_bitwise_shifts() {
        let source = indoc! {"
            let x = 8;
            let left_shift = x << 2;
            let arithmetic_shift = x >> 1;
            let logical_shift = x >>> 1;
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_bitwise_constant_folding() {
        let source = indoc! {"
            let packed = (1 << 16) & (255 << 8) & 2;
            let mask = 0xFF & 0x0F;
            let combined = (15 | 4096);
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_bitwise_with_variables() {
        let source = indoc! {"
            fn pack_bits(high, low) {
                let packed = (high << 8) | low;
                return packed;
            }
            fn extract_bits(value) {
                let high = (value >> 8) & 0xFF;
                let low = value & 0xFF;
                return (high, low);
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_complex_bit_manipulation() {
        let source = indoc! {"
            fn encode_flags(enabled, mode, priority) {
                let flag_byte = (enabled << 7) | (mode << 4) | priority;
                return flag_byte;
            }
            fn decode_flags(byte) {
                let enabled = (byte >> 7) & 1;
                let mode = (byte >> 4) & 0x7;
                let priority = byte & 0xF;
                return (enabled, mode, priority);
            }
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
