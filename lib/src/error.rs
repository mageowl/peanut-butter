use std::fmt::{Debug, Display};

use crate::span::Span;

pub struct ErrorSrc {
    pub label: Option<String>,
    pub span: Span,
}

impl Debug for ErrorSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for ErrorSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "[{}] {label}", self.span)
        } else {
            write!(f, "{}", self.span)
        }
    }
}

pub struct Error {
    pub stack: Vec<ErrorSrc>,
    pub message: String,
}

impl Error {
    pub fn new(span: Span, message: impl ToString) -> Self {
        Self {
            stack: vec![ErrorSrc { label: None, span }],
            message: message.to_string(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error: {}\n  -> {:?}", self.message, self.stack)
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Warn {
    pub source: Span,
    pub message: String,
}

impl Warn {
    pub fn new(span: Span, message: impl ToString) -> Self {
        Self {
            source: span,
            message: message.to_string(),
        }
    }
}

impl Display for Warn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "warning: {}\n  -> {:?}", self.message, self.source)
    }
}
