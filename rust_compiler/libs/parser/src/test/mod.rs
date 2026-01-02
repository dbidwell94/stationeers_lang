#[macro_export]
macro_rules! parser {
    ($input:expr) => {
        Parser::new(Tokenizer::from($input))
    };
}

mod blocks;
use super::Parser;
use super::Tokenizer;
use anyhow::Result;
use pretty_assertions::assert_eq;

#[test]
fn test_unsupported_keywords() -> Result<()> {
    let mut parser = parser!("enum x;");
    assert!(parser.parse().is_err());

    let mut parser = parser!("if x {}");
    assert!(parser.parse().is_err());

    let mut parser = parser!("else {}");
    assert!(parser.parse().is_err());

    Ok(())
}

#[test]
fn test_declarations() -> Result<()> {
    let input = r#"
        let x = 5;
        // The below line should fail
        let y = 234
        "#;
    let tokenizer = Tokenizer::from(input);
    let mut parser = Parser::new(tokenizer);

    let expression = parser.parse()?.unwrap();

    assert_eq!("(let x = 5)", expression.to_string());

    assert!(parser.parse().is_err());

    Ok(())
}

#[test]
fn test_const_declaration() -> Result<()> {
    let input = r#"
        const item = 20c;
        const decimal = 200.15;
        const nameConst = "str_lit";
    "#;
    let tokenizer = Tokenizer::from(input);
    let mut parser = Parser::new(tokenizer);

    assert_eq!("(const item = 20c)", parser.parse()?.unwrap().to_string());

    assert_eq!(
        "(const decimal = 200.15)",
        parser.parse()?.unwrap().to_string()
    );

    assert_eq!(
        r#"(const nameConst = "str_lit")"#,
        parser.parse()?.unwrap().to_string()
    );

    assert_eq!(None, parser.parse()?);

    Ok(())
}

#[test]
fn test_function_expression() -> Result<()> {
    let input = r#"
            // This is a function. The parser is starting to get more complex
            fn add(x, y) {
                let z = x;
            }
        "#;

    let tokenizer = Tokenizer::from(input);
    let mut parser = Parser::new(tokenizer);

    let expression = parser.parse()?.unwrap();

    assert_eq!(
        "(fn add(x, y) { { (let z = x); } })",
        expression.to_string()
    );

    Ok(())
}

#[test]
fn test_function_invocation() -> Result<()> {
    let input = r#"
                add();
            "#;

    let tokenizer = Tokenizer::from(input);
    let mut parser = Parser::new(tokenizer);

    let expression = parser.parse()?.unwrap();

    assert_eq!("add()", expression.to_string());

    Ok(())
}

#[test]
fn test_priority_expression() -> Result<()> {
    let input = r#"
            let x = (4 + 3);
        "#;

    let tokenizer = Tokenizer::from(input);
    let mut parser = Parser::new(tokenizer);

    let expression = parser.parse()?.unwrap();

    assert_eq!("(let x = ((4 + 3)))", expression.to_string());

    Ok(())
}

#[test]
fn test_binary_expression() -> Result<()> {
    let expr = parser!("4 ** 2 + 5 ** 2;").parse()?.unwrap();
    assert_eq!("((4 ** 2) + (5 ** 2))", expr.to_string());

    let expr = parser!("2 ** 3 ** 4;").parse()?.unwrap();
    assert_eq!("(2 ** (3 ** 4))", expr.to_string());

    let expr = parser!("45 * 2 - 15 / 5 + 5 ** 2;").parse()?.unwrap();
    assert_eq!("(((45 * 2) - (15 / 5)) + (5 ** 2))", expr.to_string());

    let expr = parser!("(5 - 2) * 10;").parse()?.unwrap();
    assert_eq!("(((5 - 2)) * 10)", expr.to_string());

    Ok(())
}

#[test]
fn test_const_hash_expression() -> Result<()> {
    let expr = parser!(r#"const i = hash("item")"#).parse()?.unwrap();
    assert_eq!("(const i = hash(\"item\"))", expr.to_string());
    Ok(())
}

#[test]
fn test_const_hash() -> Result<()> {
    // This test explicitly validates the tokenizer rewind logic.
    // When parsing "const h = hash(...)", the parser:
    // 1. Consumes "const", identifier, "="
    // 2. Attempts to parse "hash(...)" as a literal - this fails
    // 3. Must rewind the tokenizer to before "hash"
    // 4. Then parse it as a syscall
    // If the rewind offset is wrong (e.g., positive instead of negative),
    // the tokenizer will be at the wrong position and parsing will fail.
    let expr = parser!(r#"const h = hash("ComponentComputer")"#)
        .parse()?
        .unwrap();
    assert_eq!(r#"(const h = hash("ComponentComputer"))"#, expr.to_string());
    Ok(())
}

#[test]
fn test_negative_literal_const() -> Result<()> {
    let expr = parser!(r#"const i = -123"#).parse()?.unwrap();

    assert_eq!("(const i = -123)", expr.to_string());

    Ok(())
}

#[test]
fn test_ternary_expression() -> Result<()> {
    let expr = parser!(r#"let i = x ? 1 : 2;"#).parse()?.unwrap();

    assert_eq!("(let i = (x ? 1 : 2))", expr.to_string());
    Ok(())
}

#[test]
fn test_complex_binary_with_ternary() -> Result<()> {
    let expr = parser!("let i = (x ? 1 : 3) * 2;").parse()?.unwrap();

    assert_eq!("(let i = (((x ? 1 : 3)) * 2))", expr.to_string());

    Ok(())
}

#[test]
fn test_operator_prescedence_with_ternary() -> Result<()> {
    let expr = parser!("let x = x ? 1 : 3 * 2;").parse()?.unwrap();

    assert_eq!("(let x = (x ? 1 : (3 * 2)))", expr.to_string());

    Ok(())
}

#[test]
fn test_nested_ternary_right_associativity() -> Result<()> {
    let expr = parser!("let i = a ? b : c ? d : e;").parse()?.unwrap();

    assert_eq!("(let i = (a ? b : (c ? d : e)))", expr.to_string());
    Ok(())
}

#[test]
fn test_tuple_declaration() -> Result<()> {
    let expr = parser!("let (x, _) = (1, 2);").parse()?.unwrap();

    assert_eq!("(let (x, _) = (1, 2))", expr.to_string());

    Ok(())
}
#[test]
fn test_tuple_assignment() -> Result<()> {
    let expr = parser!("(x, y) = (1, 2);").parse()?.unwrap();

    assert_eq!("((x, y) = (1, 2))", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_assignment_with_underscore() -> Result<()> {
    let expr = parser!("(x, _) = (1, 2);").parse()?.unwrap();

    assert_eq!("((x, _) = (1, 2))", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_declaration_with_function_call() -> Result<()> {
    let expr = parser!("let (x, y) = doSomething();").parse()?.unwrap();

    assert_eq!("(let (x, y) = doSomething())", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_declaration_with_function_call_with_underscore() -> Result<()> {
    let expr = parser!("let (x, _) = doSomething();").parse()?.unwrap();

    assert_eq!("(let (x, _) = doSomething())", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_assignment_with_function_call() -> Result<()> {
    let expr = parser!("(x, y) = doSomething();").parse()?.unwrap();

    assert_eq!("((x, y) = doSomething())", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_assignment_with_function_call_with_underscore() -> Result<()> {
    let expr = parser!("(x, _) = doSomething();").parse()?.unwrap();

    assert_eq!("((x, _) = doSomething())", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_declaration_with_complex_expressions() -> Result<()> {
    let expr = parser!("let (x, y) = (1 + 1, doSomething());")
        .parse()?
        .unwrap();

    assert_eq!("(let (x, y) = ((1 + 1), doSomething()))", expr.to_string());

    Ok(())
}

#[test]
fn test_tuple_assignment_with_complex_expressions() -> Result<()> {
    let expr = parser!("(x, y) = (doSomething(), 123 / someValue.Setting);")
        .parse()?
        .unwrap();

    assert_eq!(
        "((x, y) = (doSomething(), (123 / someValue.Setting)))",
        expr.to_string()
    );

    Ok(())
}

#[test]
fn test_tuple_declaration_all_complex_expressions() -> Result<()> {
    let expr = parser!("let (x, y) = (a + b, c * d);").parse()?.unwrap();

    assert_eq!("(let (x, y) = ((a + b), (c * d)))", expr.to_string());

    Ok(())
}
#[test]
fn test_eof_error_has_span() -> Result<()> {
    // Test that UnexpectedEOF errors capture the span of the last token
    let mut parser = parser!("let x = 5");
    let result = parser.parse();

    // Should have an error
    assert!(result.is_err());

    let err = result.unwrap_err();

    // Check that it's an UnexpectedEOF error
    match err {
        super::Error::UnexpectedEOF(Some(span)) => {
            // Verify the span points to somewhere in the code (not zero defaults)
            assert!(
                span.start_line > 0 || span.start_col > 0 || span.end_line > 0 || span.end_col > 0,
                "Span should not be all zeros: {:?}",
                span
            );
        }
        super::Error::UnexpectedEOF(None) => {
            eprintln!("ERROR: UnexpectedEOF captured None span instead of previous token span");
            eprintln!("This means unexpected_eof() is being called when current_token is None");
            panic!("UnexpectedEOF should have captured the previous token's span");
        }
        other => {
            panic!("Expected UnexpectedEOF error, got: {:?}", other);
        }
    }

    Ok(())
}
