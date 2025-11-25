use tokenizer::Tokenizer;

use crate::Parser;

#[test]
fn test_block() -> anyhow::Result<()> {
    let mut parser = crate::parser!(
        r#"
        {
            let x = 5;
            let y = 10;
        }
        "#
    );

    let expression = parser.parse()?.unwrap();

    assert_eq!("{ (let x = 5); (let y = 10); }", expression.to_string());

    Ok(())
}
