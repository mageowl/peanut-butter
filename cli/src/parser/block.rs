use super::{expression::Expression, parse_token, statement::Statement, Parse};
use crate::lexer::TokenStream;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
};

#[derive(Debug)]
pub struct Block {
    pub body: Vec<Chunk<Statement>>,
    pub tail: Option<Chunk<Box<Expression>>>,
}

impl Parse for Block {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let span = parse_token(source, Token::BraceOpen, "Expected a block.")?;
        let mut trailing_delimiter = true;
        let mut body = Vec::new();
        let span_end;

        loop {
            if let Some(Token::BraceClose) = source.peek_token() {
                let Some(Ok(Chunk {
                    span: brace_span, ..
                })) = source.next()
                else {
                    unreachable!()
                };
                span_end = brace_span;
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                body.push(Statement::parse(source)?);
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => Error::new(token.span, "Expected a semicolon."),
                    Some(Err(err)) => err,
                    None => Error::new(Span::char(source.pos()), "Expected a semicolon."),
                });
            }

            if let Some(Token::Semicolon) = source.peek_token() {
                trailing_delimiter = true;
                source.next();
            }
        }

        let tail = if trailing_delimiter {
            None
        } else {
            match body.pop() {
                Some(statement) => match statement.data {
                    Statement::Expression(expr) => Some(statement.span.with(Box::new(expr))),
                    _ => return Err(Error::new(statement.span, "Only expressions can be used as tail statments for a block. Try adding a semicolon to the end."))
                },
                None => None,
            }
        };

        Ok(Span {
            start: span.start,
            end: span_end.end,
        }
        .with(Self { body, tail }))
    }
}
