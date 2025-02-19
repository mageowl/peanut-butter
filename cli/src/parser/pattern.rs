use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Pos, Span},
    token::Token,
    value::Key,
};

use crate::lexer::TokenStream;

use super::{parse_token, type_name::TypeName, Parse};

#[derive(Debug)]
pub enum Pattern {
    Identifier {
        name: Chunk<String>,
        type_hint: Option<Chunk<TypeName>>,
    },
    Table(HashMap<Chunk<Key>, Chunk<Pattern>>),
    Unit,
    Ignore {
        type_hint: Option<Chunk<TypeName>>,
    },
}

impl Pattern {
    pub fn type_hint(&mut self) -> Option<Chunk<TypeName>> {
        match self {
            Self::Identifier { type_hint, .. } => type_hint.take(),
            Self::Ignore { type_hint } => type_hint.take(),
            Self::Unit => Some(Span::char(Pos::new(0, 1)).with(TypeName::Unit)),
            Self::Table(map) => {
                let span = map
                    .iter()
                    .next()
                    .zip(map.iter().last())
                    .map(|(s, e)| Span {
                        start: s.0.span.start,
                        end: e.1.span.end,
                    })?;

                Some(
                    span.with(TypeName::Table(
                        map.into_iter()
                            .map(|(k, v)| Some((k.clone(), v.type_hint()?)))
                            .collect::<Option<_>>()?,
                    )),
                )
            }
        }
    }

    fn parse_table(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::BracketOpen, "Expected table pattern")?.start;
        if let Some(Token::BracketClose) = source.peek_token() {
            let Some(Ok(Chunk { span: end, .. })) = source.next() else {
                unreachable!()
            };
            return Ok(Span {
                start,
                end: end.end,
            }
            .with(Self::Unit));
        }

        let mut trailing_delimiter = true;
        let mut pairs = HashMap::new();
        let mut next_index = 0;

        loop {
            if let Some(Token::BracketClose) = source.peek_token() {
                source.next();
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                let is_key = source.peek_nth_token(2) == Some(&Token::Equals);
                match source.peek_token() {
                    Some(Token::Ident(_) | Token::String(_)) if is_key => {
                        let Some(Ok(Chunk {
                            span: key_span,
                            data: Token::Ident(key) | Token::String(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip equals
                        let value = Self::parse(source)?;

                        pairs.insert(key_span.with(Key::Named(key)), value);
                    }
                    Some(Token::Number(_)) if is_key => {
                        let Some(Ok(Chunk {
                            span: key_span,
                            data: Token::Number(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip equals
                        let value = Self::parse(source)?;

                        if key >= 0.0 && !key.is_nan() && key.floor() == key {
                            pairs.insert(key_span.with(Key::Index(key as usize)), value);
                        } else {
                            return Err(Error::new(key_span, "I can only use round numbers that are zero or more as keys in a table."));
                        }
                    }
                    Some(Token::KeywordWith) => {
                        source.next();

                        match Self::parse(source)? {
                            Chunk {
                                data: Self::Identifier { name, type_hint },
                                span,
                            } => {
                                pairs.insert(
                                    name.span.with(Key::Named(name.data.clone())),
                                    span.with(Self::Identifier { name, type_hint }),
                                );
                            }
                            Chunk { span, .. } => {
                                return Err(Error::new(
                                    span,
                                    "I can only unwrap named items from a table, not tables.",
                                ))
                            }
                        }
                    }
                    Some(_) => {
                        let key_span = Span::char(source.pos());
                        let key = next_index;
                        next_index += 1;

                        let value = Self::parse(source)?;
                        pairs.insert(key_span.with(Key::Index(key)), value);
                    }
                    None => {
                        source.next().transpose()?;
                        return Err(Error::new(
                            Span {
                                start,
                                end: source.pos(),
                            },
                            "Missing closing bracket for table.",
                        ));
                    }
                }
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => Error::new(token.span, "Expected a comma."),
                    Some(Err(err)) => err,
                    None => Error::new(Span::char(source.pos()), "Expected a comma."),
                });
            }

            if let Some(Token::Comma) = source.peek_token() {
                trailing_delimiter = true;
                source.next();
            }
        }

        Ok(Span {
            start,
            end: source.pos(),
        }
        .with(Self::Table(pairs)))
    }
}

impl Parse for Pattern {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        match source.peek_token() {
            Some(Token::Ident(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Ident(name),
                })) = source.next()
                else {
                    unreachable!();
                };
                let type_hint = if let Some(Token::Colon) = source.peek_token() {
                    source.next();
                    Some(TypeName::parse(source)?)
                } else {
                    None
                };

                Ok(Span {
                    start: span.start,
                    end: type_hint.as_ref().map_or_else(|| span.end, |t| t.span.end),
                }
                .with(if name == "_" {
                    Self::Ignore { type_hint }
                } else {
                    Self::Identifier {
                        name: span.with(name),
                        type_hint,
                    }
                }))
            }
            Some(Token::BracketOpen) => Self::parse_table(source),

            Some(_) => {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };
                Err(Error::new(span, "Expected a pattern or variable name."))
            }
            None => {
                // Make sure there wasn't a lexing error.
                source.next().transpose()?;
                Err(Error::new(
                    Span::char(source.pos()),
                    "Expected a pattern or variable name.",
                ))
            }
        }
    }
}
