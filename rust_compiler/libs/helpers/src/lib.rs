mod helper_funcs;
pub use helper_funcs::dedent;
mod macros;
mod syscall;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start_line: usize,
    pub end_line: usize,
    pub start_col: usize,
    pub end_col: usize,
}

impl From<Span> for lsp_types::Range {
    fn from(value: Span) -> Self {
        Self {
            start: lsp_types::Position {
                line: value.start_line as u32,
                character: value.start_col as u32,
            },
            end: lsp_types::Position {
                line: value.end_line as u32,
                character: value.end_col as u32,
            },
        }
    }
}

impl From<&Span> for lsp_types::Range {
    fn from(value: &Span) -> Self {
        Self {
            start: lsp_types::Position {
                line: value.start_line as u32,
                character: value.start_col as u32,
            },
            end: lsp_types::Position {
                line: value.end_line as u32,
                character: value.end_col as u32,
            },
        }
    }
}

/// This trait will allow the LSP to emit documentation for various tokens and expressions.
/// You can easily create documentation for large enums with the `documented!` macro.
pub trait Documentation {
    /// Retreive documentation for this specific item.
    fn docs(&self) -> String;

    fn get_all_documentation() -> Vec<(&'static str, String)>;
}

pub mod prelude {
    pub use super::helper_funcs::*;
    pub use super::{Documentation, documented, with_syscalls};
}
