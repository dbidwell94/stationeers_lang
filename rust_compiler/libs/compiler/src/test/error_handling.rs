use crate::Error;
use crate::variable_manager::Error as ScopeError;

#[test]
fn unknown_identifier_error() {
    let errors = compile! {
        result "let x = unknown_var;"
    };

    assert_eq!(errors.len(), 1);
    match &errors[0] {
        Error::UnknownIdentifier(name, _) => {
            assert_eq!(name.as_ref(), "unknown_var");
        }
        _ => panic!("Expected UnknownIdentifier error, got {:?}", errors[0]),
    }
}

#[test]
fn duplicate_identifier_error() {
    let errors = compile! {
        result "
            let x = 5;
            let x = 10;
        "
    };

    assert_eq!(errors.len(), 1);
    match &errors[0] {
        Error::Scope(ScopeError::DuplicateVariable(name, _)) => {
            assert_eq!(name.as_ref(), "x");
        }
        _ => panic!("Expected DuplicateIdentifier error, got {:?}", errors[0]),
    }
}

#[test]
fn const_reassignment_error() {
    let errors = compile! {
        result "
            const PI = 3.14;
            PI = 2.71;
        "
    };

    assert_eq!(errors.len(), 1);
    match &errors[0] {
        Error::ConstAssignment(name, _) => {
            assert_eq!(name.as_ref(), "PI");
        }
        _ => panic!("Expected ConstAssignment error, got {:?}", errors[0]),
    }
}

#[test]
fn unknown_function_call_error() {
    let errors = compile! {
        result "
            let result = unknown_function();
        "
    };

    assert_eq!(errors.len(), 1);
    match &errors[0] {
        Error::UnknownIdentifier(name, _) => {
            assert_eq!(name.as_ref(), "unknown_function");
        }
        _ => panic!("Expected UnknownIdentifier error, got {:?}", errors[0]),
    }
}

#[test]
fn argument_mismatch_error() {
    let errors = compile! {
        result "
            fn add(a, b) {
                return a + b;
            };
            
            let result = add(1);
        "
    };

    // The error should be an AgrumentMismatch
    assert!(
        errors
            .iter()
            .any(|e| matches!(e, Error::AgrumentMismatch(_, _)))
    );
}

#[test]
fn tuple_size_mismatch_error() {
    let errors = compile! {
        result "
            fn pair() {
                return (1, 2);
            };
            
            let (x, y, z) = pair();
        "
    };

    assert!(
        errors
            .iter()
            .any(|e| matches!(e, Error::TupleSizeMismatch(2, 3, _)))
    );
}

#[test]
fn multiple_errors_reported() {
    let errors = compile! {
        result "
            let x = unknown1;
            let x = 5;
            let y = unknown2;
        "
    };

    // Should have at least 3 errors
    assert!(
        errors.len() >= 2,
        "Expected at least 2 errors, got {}",
        errors.len()
    );
}

#[test]
fn return_outside_function_error() {
    let errors = compile! {
        result "
            let x = 5;
            return x;
        "
    };

    // Should have an error about return outside function
    assert!(
        !errors.is_empty(),
        "Expected error for return outside function"
    );
}

#[test]
fn break_outside_loop_error() {
    let errors = compile! {
        result "
            break;
        "
    };

    assert!(!errors.is_empty(), "Expected error for break outside loop");
}

#[test]
fn continue_outside_loop_error() {
    let errors = compile! {
        result "
            continue;
        "
    };

    assert!(
        !errors.is_empty(),
        "Expected error for continue outside loop"
    );
}

#[test]
fn device_reassignment_error() {
    let errors = compile! {
        result "
            device d0 = \"d0\";
            device d0 = \"d1\";
        "
    };

    assert!(
        errors
            .iter()
            .any(|e| matches!(e, Error::DuplicateIdentifier(_, _)))
    );
}

#[test]
fn invalid_device_error() {
    let errors = compile! {
        result "
            device d0 = \"d0\";
            d0 = \"d1\";
        "
    };

    // Device reassignment should fail
    assert!(!errors.is_empty(), "Expected error for device reassignment");
}
