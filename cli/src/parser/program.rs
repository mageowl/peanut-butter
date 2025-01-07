use std::rc::Rc;

use super::{statement::Statement, Parse};
use crate::lexer::TokenStream;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
};

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Chunk<Statement>>,
}

impl Parse for Program {
    fn parse(source: &mut TokenStream<'_>) -> Result<Chunk<Program>> {
        let start = source.pos();
        let mut body = Vec::new();
        let mut trailing_delimiter = true;

        loop {
            if trailing_delimiter {
                trailing_delimiter = false;
                if source.peek().is_some() {
                    body.push(Statement::parse(source)?);
                } else {
                    break;
                }
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => Error::new(token.span, "Expected a semicolon."),
                    Some(Err(err)) => err,
                    None => Error::new(Span::char(source.pos()), "Expected a semicolon."),
                });
            }

            while let Some(Token::Semicolon) = source.peek_token() {
                trailing_delimiter = true;
                source.next();
            }
        }

        Ok(Span {
            start,
            end: source.pos(),
        }
        .with(Self { body }))
    }
}
