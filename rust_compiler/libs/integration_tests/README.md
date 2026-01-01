# Integration Tests for Slang Compiler with Optimizer

This crate contains end-to-end integration tests for the Slang compiler that verify the complete compilation pipeline including all optimization passes.

## Snapshot Testing with Insta

These tests use [insta](https://insta.rs/) for snapshot testing, which captures the entire compiled output and stores it in snapshot files for comparison.

### Running Tests

```bash
# Run all integration tests
cargo test --package integration_tests

# Run a specific test
cargo test --package integration_tests test_simple_leaf_function
```

### Updating Snapshots

When you make changes to the compiler or optimizer that affect the output:

```bash
# Update all snapshots automatically
INSTA_UPDATE=always cargo test --package integration_tests

# Or use cargo-insta for interactive review (install first: cargo install cargo-insta)
cargo insta test --package integration_tests
cargo insta review --package integration_tests
```

### Understanding Snapshots

Snapshot files are stored in `src/snapshots/` and contain:

- The full IC10 assembly output from compiling Slang source code
- Metadata about which test generated them
- The expression that produced the output

Example snapshot structure:

```
---
source: libs/integration_tests/src/lib.rs
expression: output
---
j main
move r8 10
j ra
```

### What We Test

1. **Leaf Function Optimization** - Removal of unnecessary `push sp/ra` and `pop ra/sp`
2. **Function Calls** - Preservation of stack frame when calling functions
3. **Constant Folding** - Compile-time evaluation of constant expressions
4. **Algebraic Simplification** - Identity operations like `x * 1` → `x`
5. **Strength Reduction** - Converting expensive operations like `x * 2` → `x + x`
6. **Dead Code Elimination** - Removal of unused variables
7. **Peephole Comparison Fusion** - Combining comparison + branch instructions
8. **Select Optimization** - Converting if/else to single `select` instruction
9. **Complex Arithmetic** - Multiple optimizations working together
10. **Nested Function Calls** - Full program optimization

### Adding New Tests

To add a new integration test:

1. Add a new `#[test]` function in `src/lib.rs`
2. Call `compile_optimized()` with your Slang source code
3. Use `insta::assert_snapshot!(output)` to capture the output
4. Run with `INSTA_UPDATE=always` to create the initial snapshot
5. Review the snapshot file to ensure it looks correct

Example:

```rust
#[test]
fn test_my_optimization() {
    let source = "fn foo(x) { return x + 1; }";
    let output = compile_optimized(source);
    insta::assert_snapshot!(output);
}
```

### Benefits of Snapshot Testing

- **Full Output Verification**: Tests the entire compiled output, not just snippets
- **Easy to Review**: Visual diffs show exactly what changed in the output
- **Regression Detection**: Any change to output is immediately visible
- **Living Documentation**: Snapshots serve as examples of compiler output
- **Less Brittle**: No need to manually update expected strings when making intentional changes
