use helpers::Span;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(
        "Error: Duplicate variable '{name}' at {current:?}. '{name}' was originally declared at {original:?}"
    )]
    DuplicateDeclaration {
        name: String,
        original: Span,
        current: Span,
    },

    #[error("Invalid return type")]
    InvalidReturnType { span: Span },

    #[error("{error}")]
    InvalidArgType { error: String, span: Span },

    #[error("Attempted to assign a value to an unknown variable")]
    MissingAsignee { span: Span },

    #[error("Attempted to access a variable that has not yet been defined")]
    InvalidVariable { name: String, span: Span },
}
