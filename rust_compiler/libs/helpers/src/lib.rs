mod macros;

/// This trait will allow the LSP to emit documentation for various tokens and expressions.
/// You can easily create documentation for large enums with the `documented!` macro.
pub trait Documentation {
    /// Retreive documentation for this specific item.
    fn docs(&self) -> String;
}

pub mod prelude {
    pub use super::{Documentation, documented};
}
