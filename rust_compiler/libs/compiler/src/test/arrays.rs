#[test]
fn arrayless_program_does_not_reserve_stack() -> anyhow::Result<()> {
    let result = compile! { check "let value = 7;" };
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(!result.output.contains("move sp 256"));
    Ok(())
}

#[test]
fn arrays_reserve_stack_and_support_indexing() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let values = [10, 20, 30];
            let index = 1;
            let value = values[index];
            values[0] = 99;
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.starts_with("move sp 256\nj main\n"));
    assert!(
        result
            .output
            .contains("put db 0 10\nput db 1 20\nput db 2 30")
    );
    assert!(result.output.contains("get r"));
    assert!(result.output.contains("put db 0 99"));
    assert!(result.output.contains("move r"));
    Ok(())
}

#[test]
fn arrays_can_be_passed_and_mutated_by_functions() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn mutate(values) {
                values[1] = 99;
            }
            let original = [1, 2, 3];
            mutate(original);
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.contains("put db r"));
    assert!(result.output.contains("push 0\njal mutate"));
    Ok(())
}

#[test]
fn array_literal_can_be_passed_directly_to_a_function() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn mutate(values) {
                values[0] = 9;
            }
            mutate([1, 2]);
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.contains("put db 0 1\nput db 1 2"));
    assert!(result.output.contains("push 0\njal mutate"));
    Ok(())
}

#[test]
fn arrays_cannot_be_aliased_by_assignment() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let values = [1, 2];
            let alias = values;
        "
    };
    assert!(!result.errors.is_empty(), "array aliasing must be rejected");
    Ok(())
}

#[test]
fn function_local_arrays_do_not_overlap_main_arrays() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn work() {
                let local = [7, 8];
            }
            let caller = [1, 2];
            work();
        "
    };
    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.contains("put db 0 7\nput db 1 8"));
    assert!(result.output.contains("put db 2 1\nput db 3 2"));
    Ok(())
}

#[test]
fn array_repeat_fill_and_uninitialized_forms_compile() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let filled = [|3| 0];
            let uninitialized = [|2|];
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.contains("put db 0 0\nput db 1 0\nput db 2 0"));
    assert!(!result.output.contains("put db 3"));
    Ok(())
}

#[test]
fn arrays_cannot_exceed_the_reserved_partition() -> anyhow::Result<()> {
    let result = compile! {
        check "let too_large = [|257|];"
    };
    assert!(
        !result.errors.is_empty(),
        "257 array slots exceed the v1 partition"
    );
    assert!(
        result.errors[0]
            .to_string()
            .contains("Array storage exceeded")
    );
    Ok(())
}

#[test]
fn array_size_must_be_compile_time_constant() -> anyhow::Result<()> {
    let result = compile! {
        check "
            const size = 3;
            let values = [|size + 1|];
        "
    };
    assert!(
        result.errors.is_empty(),
        "const-foldable size should pass: {:?}",
        result.errors
    );

    let result = compile! {
        check "
            let size = 3;
            let values = [|size|];
        "
    };
    assert!(
        !result.errors.is_empty(),
        "mutable variable size must be rejected"
    );

    let result = compile! {
        check "let values = [|2.5|];"
    };
    assert!(
        !result.errors.is_empty(),
        "fractional array size must be rejected"
    );

    Ok(())
}

#[test]
fn constant_array_indices_are_bounds_checked() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let values = [1, 2];
            let invalid = values[2];
        "
    };
    assert!(
        result
            .errors
            .iter()
            .any(|error| error.to_string().contains("outside the valid range")),
        "expected compile-time bounds error, got: {:?}",
        result.errors
    );

    let result = compile! {
        check "
            let values = [1, 2];
            let invalid = values[-1];
        "
    };
    assert!(
        !result.errors.is_empty(),
        "negative constant index must be rejected"
    );
    Ok(())
}

#[test]
fn arrays_forward_through_function_parameters() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn inner(values) {
                values[0] = 7;
            }
            fn outer(values) {
                inner(values);
            }
            let original = [1, 2, 3];
            outer(original);
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(!result.output.contains("div "));
    assert!(result.output.contains("push r8\njal inner"));
    Ok(())
}

#[test]
fn array_length_is_not_supported() -> anyhow::Result<()> {
    let result = compile! {
        check "
            let values = [1, 2, 3];
            let count = values.length;
        "
    };
    assert!(
        result
            .errors
            .iter()
            .any(|error| error.to_string().contains("Array length is not supported")),
        "expected an unsupported array length error, got: {:?}",
        result.errors
    );
    Ok(())
}

#[test]
fn function_array_parameter_receives_the_base_address() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn read_four(values) {
                let value = values[4];
            }
            let prefix = [|2|];
            let values = [|8|];
            let value = read_four(values);
        "
    };

    assert!(result.errors.is_empty(), "{:?}", result.errors);
    assert!(result.output.contains("add r1 r8 4"));
    assert!(result.output.contains("push 2\njal read_four"));
    assert!(!result.output.contains("div "));
    assert!(!result.output.contains("mod "));
    Ok(())
}

#[test]
fn function_parameter_cannot_be_both_array_and_scalar() -> anyhow::Result<()> {
    let result = compile! {
        check "
            fn inspect(value) {
                value[0] = 1;
            }
            let values = [1, 2];
            inspect(values);
            inspect(5);
        "
    };
    assert!(
        !result.errors.is_empty(),
        "mixed array/scalar calls must be rejected"
    );
    Ok(())
}
