use std::rc::Rc;

use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
};

use super::{statement::Statement, Parse};
use crate::{lexer::TokenStream, type_check::scope::Scope};

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Chunk<Statement>>,
    pub scope: Option<Rc<Scope>>,
}

impl Parse for Program {
    fn parse(source: &mut TokenStream<'_>) -> Result<Chunk<Program>> {
        let start = source.pos();
        let mut body = Vec::new();
        let mut trailing_delimiter = true;

        loop {
            if trailing_delimiter {
                trailing_delimiter = false;
                if let Some(_) = source.peek() {
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
        .with(Self { body, scope: None }))
    }
}
