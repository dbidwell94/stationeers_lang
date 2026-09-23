use super::Parser;
use super::Tokenizer;
use anyhow::Result;

#[test]
fn test_array_literal_with_values() -> Result<()> {
    let mut parser = parser!("let arr = [1, 2, 3, 4, 5];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let arr = [1, 2, 3, 4, 5])", expr.to_string());
    Ok(())
}

#[test]
fn test_array_literal_empty() -> Result<()> {
    let mut parser = parser!("let arr = [];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let arr = [])", expr.to_string());
    Ok(())
}

#[test]
fn test_array_repeat_with_fill() -> Result<()> {
    let mut parser = parser!("let arr = [|5| 0];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let arr = [|5| 0])", expr.to_string());
    Ok(())
}

#[test]
fn test_array_repeat_uninitialized() -> Result<()> {
    let mut parser = parser!("let arr = [|5|];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let arr = [|5|])", expr.to_string());
    Ok(())
}

#[test]
fn test_array_index_access() -> Result<()> {
    let mut parser = parser!("let x = arr[2];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let x = arr[2])", expr.to_string());
    Ok(())
}

#[test]
fn test_array_repeat_with_expression_size_and_fill() -> Result<()> {
    let mut parser = parser!("let arr = [|n + 1| x * 2];");
    let expr = parser.parse()?.expect("expected expression");
    assert_eq!("(let arr = [|(n + 1)| (x * 2)])", expr.to_string());
    Ok(())
}
