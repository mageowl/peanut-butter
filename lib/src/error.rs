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

#[derive(Debug)]
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
    pub fn print(mut self, file: &str) {
        let span = self.stack[0].span;
        let src = span.read_from(file);

        let ln_len = span.end.ln.ilog10() + 1;
        let offset = span.start.ln.max(2) - 1;

        for (i, line) in src.lines().enumerate() {
            let ln = offset + i;
            let ln_pad = (ln_len - ln.ilog10() - 1) as usize;

            const HIGHLIGHT: &str = "\x1b[31;1m";
            let mut line = line.to_owned();

            if ln == span.end.ln {
                line.insert_str(span.end.col - 1, "\x1b[0m")
            }
            if ln == span.start.ln {
                line.insert_str(span.start.col - 1, HIGHLIGHT)
            } else if ln >= span.start.ln && ln <= span.end.ln {
                line.insert_str(0, HIGHLIGHT);
            }

            line = line.replace("\t", "    ");

            println!(
                "\x1b[2m{pad}{ln} |\x1b[22m  {line}\x1b[0m",
                pad = " ".repeat(ln_pad)
            );
        }

        let indicies = self
            .message
            .match_indices("\n")
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        for i in indicies.into_iter().rev() {
            self.message.insert_str(i + 1, "       ");
        }

        println!("\n\x1b[31;1merror\x1b[0m: {msg}", msg = self.message);
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
