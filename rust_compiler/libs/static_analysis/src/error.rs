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

impl From<Error> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        let span = match &value {
            Error::DuplicateDeclaration { current, .. }
            | Error::InvalidReturnType { span: current }
            | Error::InvalidArgType { span: current, .. }
            | Error::MissingAsignee { span: current }
            | Error::InvalidVariable { span: current, .. }
            | Error::MissingSymbol { span: current, .. }
            | Error::ConflictingFunctionParameterType { span: current, .. } => *current,
        };

        lsp_types::Diagnostic {
            range: span.into(),
            severity: Some(lsp_types::DiagnosticSeverity::ERROR),
            message: value.to_string(),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub enum Error {
    DuplicateDeclaration {
        name: String,
        original: Span,
        current: Span,
    },

    InvalidReturnType {
        span: Span,
    },

    InvalidArgType {
        error: String,
        span: Span,
    },

    MissingAsignee {
        span: Span,
    },

    InvalidVariable {
        name: String,
        span: Span,
    },

    MissingSymbol {
        name: String,
        span: Span,
    },

    ConflictingFunctionParameterType {
        function: String,
        parameter_index: usize,
        expected: String,
        actual: String,
        span: Span,
    },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateDeclaration {
                name,
                original,
                current,
            } => write!(
                f,
                "Error: Duplicate variable '{name}' at line {}. '{name}' was originally declared at line {}",
                current.start_line, original.start_line
            ),
            Self::InvalidReturnType { .. } => write!(f, "Invalid return type"),
            Self::InvalidArgType { error, .. } => write!(f, "{error}"),
            Self::MissingAsignee { .. } => {
                write!(f, "Attempted to assign a value to an unknown variable")
            }
            Self::InvalidVariable { .. } => {
                write!(
                    f,
                    "Attempted to access a variable that has not yet been defined"
                )
            }
            Self::MissingSymbol { name, .. } => {
                write!(f, "Error: Invalid symbol '{name}'. Symbol is not declared.")
            }
            Self::ConflictingFunctionParameterType {
                function,
                parameter_index,
                expected,
                actual,
                ..
            } => write!(
                f,
                "Function '{function}' parameter {parameter_index} was inferred as '{expected}' but was later called with '{actual}'"
            ),
        }
    }
}

impl std::error::Error for Error {}
