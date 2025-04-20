use crate::span::Span;

#[derive(Debug)]
pub struct Error {
    message: String,
    span: Span,
}

impl Error {
    pub fn new(message: impl ToString, span: Span) -> Self {
        Self {
            message: message.to_string(),
            span,
        }
    }

    pub fn display(&self, src: &str) {
        println!("error: {} at {:?}", self.message, self.span)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
