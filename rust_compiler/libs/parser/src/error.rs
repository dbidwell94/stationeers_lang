use super::*;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error<'a> {
    #[error(transparent)]
    Tokenizer(#[from] tokenizer::Error),

    #[error("Unexpected token: {1}")]
    UnexpectedToken(Span, Token<'a>),

    #[error("Duplicate identifier: {1}")]
    DuplicateIdentifier(Span, Token<'a>),

    #[error("Invalid Syntax: {1}")]
    InvalidSyntax(Span, String),

    #[error("Unsupported Keyword: {1}")]
    UnsupportedKeyword(Span, Token<'a>),

    #[error("Expected semicolon")]
    MissingSemicolon(Span),

    #[error("Unexpected End of File")]
    UnexpectedEOF(Option<Span>),
}

impl<'a> From<Error<'a>> for lsp_types::Diagnostic {
    fn from(value: Error) -> Self {
        use Error::*;
        use lsp_types::*;
        match value {
            Tokenizer(e) => e.into(),
            UnexpectedToken(span, _)
            | DuplicateIdentifier(span, _)
            | InvalidSyntax(span, _)
            | UnsupportedKeyword(span, _)
            | MissingSemicolon(span) => Diagnostic {
                message: value.to_string(),
                severity: Some(DiagnosticSeverity::ERROR),
                range: span.into(),
                ..Default::default()
            },
            UnexpectedEOF(span) => {
                let range = span.map(|s| s.into()).unwrap_or_default();
                Diagnostic {
                    message: value.to_string(),
                    severity: Some(DiagnosticSeverity::ERROR),
                    range,
                    ..Default::default()
                }
            }
        }
    }
}