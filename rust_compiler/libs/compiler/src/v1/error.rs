use crate::variable_manager;
use helpers::Span;
use std::borrow::Cow;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error("{0}")]
    Parse(parser::Error<'a>),

    #[error("{0}")]
    Scope(variable_manager::Error<'a>),

    #[error("IO Error: {0}")]
    IO(String),

    #[error("`{0}` has already been defined.")]
    DuplicateIdentifier(Cow<'a, str>, Span),

    #[error("`{0}` is not found in the current scope.")]
    UnknownIdentifier(Cow<'a, str>, Span),

    #[error("`{0}` is not valid.")]
    InvalidDevice(Cow<'a, str>, Span),

    #[error("Incorrent number of arguments passed into `{0}`")]
    AgrumentMismatch(Cow<'a, str>, Span),

    #[error("Attempted to re-assign a value to const variable `{0}`")]
    ConstAssignment(Cow<'a, str>, Span),

    #[error("Attempted to re-assign a value to a device const `{0}`")]
    DeviceAssignment(Cow<'a, str>, Span),

    #[error("Expected a {0}-tuple, but you're trying to destructure into {1} variables")]
    TupleSizeMismatch(usize, usize, Span),

    #[error("{0}")]
    OperationNotSupported(String, Span),

    #[error("{0}")]
    Unknown(String, Option<Span>),
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            Parse(e) => e.into(),
            IO(e) => Diagnostic {
                message: e.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            Scope(e) => e.into(),
            DuplicateIdentifier(_, span)
            | UnknownIdentifier(_, span)
            | InvalidDevice(_, span)
            | ConstAssignment(_, span)
            | DeviceAssignment(_, span)
            | AgrumentMismatch(_, span)
            | TupleSizeMismatch(_, _, span)
            | OperationNotSupported(_, span) => Diagnostic {
                range: span.into(),
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                ..Default::default()
            },
            Unknown(msg, span) => Diagnostic {
                message: msg.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                range: span.map(lsp_types::Range::from).unwrap_or_default(),
                ..Default::default()
            },
        }
    }
}

impl<'a> From<parser::Error<'a>> for Error<'a> {
    fn from(value: parser::Error<'a>) -> Self {
        Self::Parse(value)
    }
}

impl<'a> From<variable_manager::Error<'a>> for Error<'a> {
    fn from(value: variable_manager::Error<'a>) -> Self {
        Self::Scope(value)
    }
}

impl<'a> From<std::io::Error> for Error<'a> {
    fn from(err: std::io::Error) -> Self {
        Error::IO(err.to_string())
    }
}