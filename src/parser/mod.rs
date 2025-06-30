use crate::{
    error::{Error, Result},
    lexer::{TokenStream, token::Token},
    span::{Chunk, Span},
};

pub mod expression;
pub mod pattern;
pub mod statement;

pub trait Parse<'a>: Sized {
    type Context: Default;

    fn parse_context(source: &mut TokenStream<'a>, ctx: Self::Context) -> Result<Chunk<Self>>;
    #[inline]
    fn parse(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        Self::parse_context(source, Default::default())
    }

    fn parse_group(
        source: &mut TokenStream<'a>,
        delimiter: Token,
        through: Token,
    ) -> Result<Vec<Chunk<Self>>> {
        let mut vec = Vec::new();
        let mut trailing_delimiter = true;

        while source.peek_token() != Some(&through) {
            if trailing_delimiter {
                trailing_delimiter = false;
                vec.push(Self::parse(source)?);
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => {
                        Error::new(format!("Expected {}.", delimiter.name()), token.span)
                    }
                    Some(Err(e)) => e,
                    None => Error::new(
                        format!("Expected {}.", delimiter.name()),
                        Span::char(*source.pos()),
                    ),
                });
            }
            if source.peek_token() == Some(&delimiter) {
                trailing_delimiter = true;
                source.next();
            }
        }
        source.next(); // skip closing token

        Ok(vec)
    }
}

impl<'a> Parse<'a> for &'a str {
    type Context = ();
    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        match source.next() {
            Some(Ok(Chunk {
                span,
                data: Token::Ident(str),
            })) => Ok(Chunk::new(span, str)),
            Some(Ok(Chunk { span, .. })) => Err(Error::new("Expected an identifier.", span)),
            Some(Err(e)) => Err(e),
            None => Err(Error::new(
                "Expected an identifier.",
                Span::char(*source.pos()),
            )),
        }
    }
}

pub fn parse_token(source: &mut TokenStream, token: Token) -> Result<Span> {
    let next = source.next().unwrap_or_else(|| {
        Err(Error::new(
            format!("Expected {}. (EOF)", token.name()),
            Span::char(*source.pos()),
        ))
    })?;
    if next.data == token {
        Ok(next.span)
    } else {
        dbg!(next.span);
        Err(Error::new(format!("Expected {}.", token.name()), next.span))
    }
}
