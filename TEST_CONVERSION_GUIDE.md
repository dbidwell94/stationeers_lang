# Test Conversion Guide: `compile! { debug }` to `compile! { check }`

## Overview

The `compile! { check ... }` macro variant returns a `CompilationCheckResult` with both compilation errors and the compiled output. This allows tests to assert that no compilation errors occurred while also checking the output.

## Pattern

### Before (using `debug`):

```rust
#[test]
fn my_test() -> anyhow::Result<()> {
    let compiled = compile! {
        debug "
            let x = 42;
        "
    };

    assert_eq!(
        compiled,
        indoc! {
            "
            j main
            main:
            move r8 42
            "
        }
    );

    Ok(())
}
```

### After (using `check`):

```rust
#[test]
fn my_test() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let x = 42;
        "
    };

    assert!(result.errors.is_empty(), "Expected no errors, got: {:?}", result.errors);

    assert_eq!(
        result.output,
        indoc! {
            "
            j main
            main:
            move r8 42
            "
        }
    );

    Ok(())
}
```

## Key Changes

1. **Variable name**: Change `let compiled =` to `let result =`
2. **Macro variant**: Change `debug` to `check`
3. **Add error assertion**: Insert `assert!(result.errors.is_empty(), ...);` after the compile block
4. **Update assertion target**: Change `compiled` to `result.output` in the assert_eq!

## Multi-line format

When using multi-line source code in the macro:

```rust
let result = compile! {
    check
    "
    let x = 10;
    let y = 20;
    "
};

assert!(result.errors.is_empty(), "Expected no errors, got: {:?}", result.errors);

assert_eq!(
    result.output,
    indoc! {
        "
        j main
        main:
        move r8 10
        move r9 20
        "
    }
);
```

## Status

- ✅ `declaration_literal.rs` - Fully converted (7 tests)
- ⏳ Other files - Pending conversion

To convert a file, replace all occurrences following the pattern above.
