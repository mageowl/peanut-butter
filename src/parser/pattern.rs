use crate::{
    error::Result,
    lexer::{TokenStream, token::Token},
    span::{Chunk, Span},
};

use super::{Parse, parse_token};

#[derive(Debug)]
pub struct MatchPattern<'a> {
    pub variant: Chunk<&'a str>,
    pub capture: Option<Chunk<MatchCapture<'a>>>,
}

#[derive(Debug)]
pub enum MatchCapture<'a> {
    Tuple(Vec<Chunk<&'a str>>),
}

impl<'a> Parse<'a> for MatchPattern<'a> {
    type Context = ();

    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let variant = <&str>::parse(source)?;
        let capture = match source.peek_token() {
            Some(Token::ParenOpen) => {
                let start = parse_token(source, Token::ParenOpen)?.start;
                let names = <&str>::parse_group(source, Token::Comma, Token::ParenClose)?;
                Some(Chunk::new(
                    Span {
                        start,
                        end: *source.pos(),
                    },
                    MatchCapture::Tuple(names),
                ))
            }
            _ => None,
        };

        return Ok(Chunk::new(
            Span {
                start: variant.span.start,
                end: *source.pos(),
            },
            Self { variant, capture },
        ));
    }
}
