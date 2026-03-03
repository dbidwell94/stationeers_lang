#[cfg(test)]
mod device_indexing_tests {
    use crate::common::compile_with_and_without_optimization;
    use indoc::indoc;

    #[test]
    fn test_device_indexing_with_hash_and_binary_literals() {
        let source = indoc! {"
            device printer = \"d0\";
            
            let item_type = hash(\"ItemIronIngot\");
            let quality = 16;
            let quantity = 7;
            
            // Pack into a single value using bitwise operations
            // Format: (itemHash << 16) | (quality << 8) | quantity
            let packed = (item_type << 16) | (quality << 8) | quantity;
            
            // Write to device stack at address 255
            let addr = 255;
            printer[addr] = packed;
            
            // Read it back
            let result = printer[addr];
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_device_indexing_with_computed_index() {
        let source = indoc! {"
            device storage = \"d1\";
            
            let base_addr = 10;
            let offset = 5;
            let index = base_addr + offset;
            
            let value = 42;
            storage[index] = value;
            
            let retrieved = storage[index];
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_device_indexing_with_binary_literals() {
        let source = indoc! {"
            device mem = \"d0\";
            
            // Binary literals for bitwise operations
            let flags = 0b1010_0101;
            let mask = 0b1111_0000;
            let masked = flags & mask;
            
            // Write to device
            mem[0] = masked;
            
            // Read back
            let read_value = mem[0];
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_device_indexing_complex_expression() {
        let source = indoc! {"
            device db = \"d0\";
            
            let item = hash(\"ItemCopper\");
            let quality = 5;
            let quantity = 100;
            
            // Complex bitwise expression
            let packed = (item << 16) | ((quality & 0xFF) << 8) | (quantity & 0xFF);
            
            // Index with computed address
            let slot = 1;
            let address = slot * 256 + 100;
            db[address] = packed;
            
            // Read back with different computation
            let read_addr = (slot + 0) * 256 + 100;
            let stored_value = db[read_addr];
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }

    #[test]
    fn test_device_indexing_optimization_folds_constants() {
        let source = indoc! {"
            device cache = \"d0\";
            
            // This should optimize to a single constant
            let packed_constant = (hash(\"ItemSilver\") << 16) | (10 << 8) | 50;
            
            cache[255] = packed_constant;
            let result = cache[255];
        "};
        let output = compile_with_and_without_optimization(source);
        insta::assert_snapshot!(output);
    }
}
