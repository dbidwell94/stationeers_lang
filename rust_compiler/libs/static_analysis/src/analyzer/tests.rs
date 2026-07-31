use assert_matches::assert_matches;
use indoc::indoc;
use parser::{Parser, parse};
use tokenizer::Tokenizer;

use super::*;

#[test]
fn declaration_adds_symbol() -> anyhow::Result<()> {
    let parsed = parse!("let i = 0;")?.expect("there to be an expression");

    let mut analyzer = Analyzer::default();

    analyzer.analyze(&parsed);

    assert_eq!(analyzer.symbol_table.symbols.len(), 1);
    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            name: "i",
            is_read: false,
            is_written: false,
            kind: SymbolKind::Variable,
            ..
        }
    );

    Ok(())
}

#[test]
fn assignment_marks_written() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
       "
        let i = 0;
        i = 3;
       "
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();

    analyzer.analyze(&parsed);

    assert_eq!(analyzer.symbol_table.symbols.len(), 1);
    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            name: "i",
            is_written: true,
            is_read: false,
            ..
        }
    );

    Ok(())
}

#[test]
fn const_expressions_are_marked_as_literals() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        "
            const item = 34;
            let usedItem = item + 1;
        "
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert_eq!(analyzer.symbol_table.symbols.len(), 2);
    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            id: SymbolId(0),
            name: "item",
            kind: SymbolKind::Constant(Literal::Number(Number::Integer(34, Unit::None))),
            is_written: false,
            is_read: true,
            ..
        }
    );
    assert_matches!(
        analyzer.symbol_table.symbols[1],
        Symbol {
            id: SymbolId(1),
            name: "usedItem",
            kind: SymbolKind::Variable,
            is_written: false,
            is_read: false,
            ..
        }
    );

    Ok(())
}

#[test]
fn const_expression_with_allowed_syscalls_are_folded() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            const hashedItem = hash("thisIsAHash");
        "#
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert_eq!(analyzer.errors.len(), 0);
    assert_eq!(analyzer.symbol_table.symbols.len(), 1);

    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            name: "hashedItem",
            is_read: false,
            is_written: false,
            kind: SymbolKind::Constant(Literal::Number(Number::Integer(_, Unit::None))),
            ..
        }
    );

    Ok(())
}

#[test]
fn scoped_variables_are_valid() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            let i = 0;
            {
                let i = 23;
            }
        "#
    })?
    .expect("a valid expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert_eq!(analyzer.errors.len(), 0);
    assert_eq!(analyzer.symbol_table.symbols.len(), 2);

    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            scope_id: 1,
            name: "i",
            ..
        }
    );
    assert_matches!(
        analyzer.symbol_table.symbols[1],
        Symbol {
            scope_id: 2,
            name: "i",
            ..
        }
    );

    Ok(())
}

#[test]
fn using_variable_in_parent_scope_is_valid() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            let item = 0;
            {
                let item2 = item + 2;
            }
        "#
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert!(analyzer.errors.is_empty());
    assert_eq!(analyzer.symbol_table.symbols.len(), 2);

    assert_matches!(
        analyzer.symbol_table.symbols[0],
        Symbol {
            name: "item",
            is_read: true,
            is_written: false,
            ..
        }
    );

    assert_matches!(
        analyzer.symbol_table.symbols[1],
        Symbol {
            name: "item2",
            is_read: false,
            is_written: false,
            ..
        }
    );

    Ok(())
}

#[test]
fn function_invocations_record_parameter_kinds() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            device pin = "d0";
            device ref_id = 4660;

            fn ping(target, other) {
                return;
            };

            ping(pin, ref_id);
        "#
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert!(
        analyzer.errors.is_empty(),
        "Expected no errors, got: {:?}",
        analyzer.errors
    );

    let function_symbol = analyzer
        .symbol_table
        .symbols
        .iter()
        .find(|symbol| symbol.name == "ping")
        .expect("function symbol to exist");

    let metadata = analyzer
        .functions
        .get(&function_symbol.id)
        .expect("function metadata to exist");

    assert_eq!(
        metadata.parameter_kinds,
        vec![ParameterKind::DevicePin, ParameterKind::DeviceReference]
    );
    assert_eq!(metadata.call_sites.len(), 1);

    Ok(())
}

#[test]
fn function_invocations_reject_mixed_device_parameter_kinds() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            device pin = "d0";
            device ref_id = 4660;

            fn ping(target) {
                return;
            };

            ping(pin);
            ping(ref_id);
        "#
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert_eq!(analyzer.errors.len(), 1);
    assert_matches!(
        analyzer.errors[0],
        Error::ConflictingFunctionParameterType {
            ref function,
            parameter_index: 0,
            ref expected,
            ref actual,
            ..
        } if function == "ping" && expected == "device pin" && actual == "device reference"
    );

    Ok(())
}

#[test]
fn function_invocations_use_device_alias_declarations() -> anyhow::Result<()> {
    let parsed = parse!(indoc! {
        r#"
            device ref_id = 4660;
            let alias = ref_id;

            fn ping(target) {
                return;
            };

            ping(alias);
        "#
    })?
    .expect("an expression");

    let mut analyzer = Analyzer::default();
    analyzer.analyze(&parsed);

    assert!(
        analyzer.errors.is_empty(),
        "Expected no errors, got: {:?}",
        analyzer.errors
    );

    let function_symbol = analyzer
        .symbol_table
        .symbols
        .iter()
        .find(|symbol| symbol.name == "ping")
        .expect("function symbol to exist");

    let metadata = analyzer
        .functions
        .get(&function_symbol.id)
        .expect("function metadata to exist");

    assert_eq!(
        metadata.parameter_kinds,
        vec![ParameterKind::DeviceReference]
    );

    Ok(())
}
