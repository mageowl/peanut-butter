use hashbrown::HashMap;

use crate::lexer::TokenStream;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
    value::Key,
};

use super::{parse_ident, parse_token, Parse};

#[derive(Debug, Clone)]
pub enum TypeName {
    Named {
        name: Chunk<String>,
        generics: Vec<Chunk<TypeName>>,
    },
    Table(HashMap<Chunk<Key>, Chunk<TypeName>>),
    Reference(Chunk<Box<TypeName>>),
    Enum(HashMap<Chunk<String>, Chunk<TypeName>>),
    Unit,
    Function {
        parameters: Vec<Chunk<TypeName>>,
        return_ty: Chunk<Box<TypeName>>,
    },
}

impl TypeName {
    fn parse_named(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let name = parse_ident(source, "Expected a type, like string or list<number>.")?;

        let mut generics = Vec::new();

        if let Some(Token::Lt) = source.peek_token() {
            source.next();

            let mut trailing_delimiter = true;

            loop {
                if let Some(Token::Gt) = source.peek_token() {
                    source.next();
                    break;
                } else if trailing_delimiter {
                    trailing_delimiter = false;
                    generics.push(TypeName::parse(source)?);
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
        }

        Ok(Span {
            start: name.span.start,
            end: source.pos(),
        }
        .with(Self::Named { name, generics }))
    }

    fn parse_table(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let start = source.pos();
        let mut pairs = HashMap::new();
        let mut trailing_delimiter = true;
        let mut next_index = 0;

        source.next(); // Skip opening bracket

        loop {
            if let Some(Token::BracketClose) = source.peek_token() {
                source.next();
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                let is_key = source.peek_nth_token(2) == Some(&Token::Colon);
                match source.peek_token() {
                    Some(Token::BracketClose) => {
                        source.next();
                        break;
                    }
                    Some(Token::Ident(_)) if is_key => {
                        let Some(Ok(Chunk {
                            span: key_span,
                            data: Token::Ident(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip colon
                        let value = TypeName::parse(source)?;

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
                        source.next(); // Skip colon
                        let value = TypeName::parse(source)?;

                        if key >= 0.0 && !key.is_nan() && key.floor() == key {
                            pairs.insert(key_span.with(Key::Index(key as usize)), value);
                        } else {
                            return Err(Error::new(key_span, "I can only use round numbers that are zero or more as keys in a table."));
                        }
                    }
                    Some(_) => {
                        let key_span = Span::char(source.pos());
                        let key = next_index;
                        next_index += 1;

                        let value = TypeName::parse(source)?;
                        pairs.insert(key_span.with(Key::Index(key)), value);
                    }
                    None => {
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

    fn parse_reference(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let span = parse_token(source, Token::KeywordRef, "Expected reference type.")?;
        let ref_type = Self::parse(source)?;

        Ok(Span {
            start: span.start,
            end: ref_type.span.end,
        }
        .with(Self::Reference(ref_type.as_box())))
    }

    fn parse_fn(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let span = parse_token(source, Token::KeywordFunction, "Expected function type.")?;
        parse_token(
            source,
            Token::ParenOpen,
            "Expected an open parenethis for fn type arguments",
        )?;

        let mut args = Vec::new();
        let mut trailing_delimiter = true;
        let end_span;

        loop {
            if let Some(Token::ParenClose) = source.peek_token() {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };
                end_span = span;
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                args.push(Self::parse(source)?);
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => Error::new(token.span, "Expected a comma."),
                    Some(Err(err)) => err,
                    None => Error::new(
                        Span::char(source.pos()),
                        "Expected a closing parenthesis for fn type arguments.",
                    ),
                });
            }

            if let Some(Token::Comma) = source.peek_token() {
                trailing_delimiter = true;
                source.next();
            }
        }

        let return_ty = if let Some(Token::Arrow) = source.peek_token() {
            source.next();
            Self::parse(source)?
        } else {
            Chunk {
                span: end_span,
                data: Self::Unit,
            }
        };

        Ok(Span {
            start: span.start,
            end: return_ty.span.end,
        }
        .with(Self::Function {
            parameters: args,
            return_ty: return_ty.as_box(),
        }))
    }

    fn parse_enum(source: &mut TokenStream) -> Result<Chunk<Self>> {
        /*let span = parse_token(source, Token::KeywordEnum, "Expected enum type.")?;
        parse_token(
            source,
            Token::BraceOpen,
            "Expected open brace for enum variants.",
        )?;

        let mut map = HashMap::new();
        let mut trailing_delimiter = true;
        let end_span;

        loop {
            if let Some(Token::BraceClose) = source.peek_token() {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };
                end_span = span;
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;

                let name = parse_ident(source, "Expected a label for an enum variant")?;
                parse_token(
                    source,
                    Token::Colon,
                    "Expected an colon to separate the label from the type.",
                )?;
                map.insert(name, Self::parse(source)?);
            } else {
                return Err(match source.next() {
                    Some(Ok(token)) => Error::new(token.span, "Expected a comma."),
                    Some(Err(err)) => err,
                    None => Error::new(
                        Span::char(source.pos()),
                        "Expected a closing parenthesis for fn type arguments.",
                    ),
                });
            }

            if let Some(Token::Semicolon) = source.peek_token() {
                trailing_delimiter = true;
                source.next();
            }
        }

        Ok(Span {
            start: span.start,
            end: end_span.end,
        }
        .with(Self::Enum(map)))*/
        todo!()
    }
}

impl Parse for TypeName {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        match source.peek_token() {
            Some(Token::KeywordRef) => Self::parse_reference(source),
            Some(Token::BracketOpen) => Self::parse_table(source),
            Some(Token::KeywordFunction) => Self::parse_fn(source),
            Some(Token::KeywordEnum) => Self::parse_enum(source),
            _ => Self::parse_named(source),
        }
    }
}
