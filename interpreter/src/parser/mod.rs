use std::fmt::Debug;

use crate::lexer::TokenStream;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
};
use type_name::TypeName;

pub mod block;
pub mod expression;
pub mod program;
pub mod statement;
pub mod type_name;

pub trait Parse: Sized + Debug {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>>;
}

fn parse_ident(source: &mut TokenStream, error_message: impl ToString) -> Result<Chunk<String>> {
    match source.next() {
        Some(Ok(Chunk {
            data: Token::Ident(name),
            span,
        })) => Ok(span.with(name)),
        Some(Ok(Chunk { span, .. })) => return Err(Error::new(span, error_message)),
        Some(Err(err)) => return Err(err),
        None => return Err(Error::new(Span::char(source.pos()), error_message)),
    }
}

fn parse_token(
    source: &mut TokenStream,
    token: Token,
    error_message: impl ToString,
) -> Result<Span> {
    match source.next() {
        Some(Ok(Chunk { data, span })) if data == token => Ok(span),
        Some(Ok(Chunk { span, .. })) => Err(Error::new(span, error_message)),
        Some(Err(err)) => Err(err),
        None => Err(Error::new(Span::char(source.pos()), error_message)),
    }
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Chunk<String>,
    pub type_hint: Chunk<TypeName>,
}

impl Parse for Parameter {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let name = parse_ident(source, "Expected an argument name.")?;
        parse_token(
            source,
            Token::Colon,
            format!(
                "Expected an argument type. Try adding a colon: '{name}: /* type */'",
                name = name.data
            ),
        )?;

        let type_hint = TypeName::parse(source)?;

        Ok(Span {
            start: name.span.start,
            end: type_hint.span.end,
        }
        .with(Self { name, type_hint }))
    }
}
