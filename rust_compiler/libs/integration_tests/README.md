# Integration Tests

## What is this?

The integration_tests crate runs end-to-end checks for the full Slang compilation pipeline.

Instead of testing one crate in isolation, it validates that tokenizer, parser, analysis, compiler, and optimizer all work together on real programs.

## What does it do?

This crate compiles Slang source through the full pipeline and snapshot-tests the resulting output.

Its test helper compiles the same source twice:

- Once without optimizer passes
- Once with optimizer passes

Then it stores and compares both outputs in a snapshot, so behavior changes are easy to spot.

**Example:**

```rust
let output = compile_with_and_without_optimization(source);
insta::assert_snapshot!(output);
```

Snapshot text typically contains sections like:

```text
## Unoptimized Output
...
## Optimized Output
...
```

## Why is this useful?

Unit tests can prove small pieces are correct, but integration tests prove the whole system behaves correctly when all stages are connected.

This crate helps by:

- Catching regressions across crate boundaries
- Verifying optimizer changes against real compiled output
- Making output changes easy to review with snapshot diffs
- Acting as living documentation of expected pipeline behavior

## Who should care?

- Compiler developers changing parsing, analysis, or codegen behavior
- Optimizer contributors validating pass interactions
- Contributors reviewing output regressions across the full pipeline

## How does it work?

At a high level, each integration test follows this flow:

- Parse source into AST
- Run static analysis
- Compile to IL/IC10 instructions
- Capture unoptimized output
- Run optimizer and capture optimized output
- Compare both outputs against stored snapshots

In short: unit tests answer "does this piece work?" and integration tests answer "does the full pipeline still work together?"

## Running tests

```bash
# Run all integration tests
cargo test --package integration_tests

# Run a specific test
cargo test --package integration_tests test_tuples
```

## Updating snapshots

```bash
# Update all snapshots automatically
INSTA_UPDATE=always cargo test --package integration_tests

# Interactive review (requires cargo-insta)
cargo insta test --package integration_tests
cargo insta review --package integration_tests
```

Snapshots are stored under src/snapshots/.
