use crate::fix_format;
use anyhow::Result;
use indoc::indoc;
use pretty_assertions::assert_eq;

#[test]
fn format_const_expression() -> Result<()> {
    let ugly = indoc! {
        "
                const       i = 123;
        "
    };

    let res = fix_format!(ugly);

    assert_eq!(
        res,
        indoc! {
            "
            const i = 123;
            "
        }
    );

    Ok(())
}
