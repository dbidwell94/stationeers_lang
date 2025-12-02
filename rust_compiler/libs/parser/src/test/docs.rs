use crate::Documentation;
use crate::sys_call;
use pretty_assertions::assert_eq;

#[test]
fn test_token_tree_docs() -> anyhow::Result<()> {
    let syscall = sys_call::System::Yield;

    assert_eq!(syscall.docs(), "");

    Ok(())
}
