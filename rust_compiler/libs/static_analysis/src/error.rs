use helpers::Span;

use thiserror::Error;

#[derive(Error, Debug)]
pub struct AnalyzeErrors(pub Vec<Error>);

impl std::ops::Deref for AnalyzeErrors {
    type Target = Vec<Error>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::fmt::Display for AnalyzeErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.0 {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

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

    #[error("Error: Invalid symbol '{name}' at {span:?}. Symbol is not declared.")]
    MissingSymbol { name: String, span: Span },

    #[error(
        "Function '{function}' parameter {parameter_index} was inferred as '{expected}' but was later called with '{actual}'"
    )]
    ConflictingFunctionParameterType {
        function: String,
        parameter_index: usize,
        expected: String,
        actual: String,
        span: Span,
    },
}
