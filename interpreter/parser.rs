use std::{collections::HashMap, fmt::Debug, hash::Hash};

use crate::lexer::{Token, TokenStream};
use pbscript_lib::{
    error::{Error, Result},
    span::{Span, Spanned},
};

pub trait Parse: Sized + Debug {
    fn parse(source: &mut TokenStream) -> Result<Spanned<Self>>;
}

fn parse_ident(source: &mut TokenStream, error_message: impl ToString) -> Result<Spanned<String>> {
    match source.next() {
        Some(Ok(Spanned {
            data: Token::Ident(name),
            span,
        })) => Ok(span.with(name)),
        Some(Ok(Spanned { span, .. })) => return Err(Error::new(span, error_message)),
        Some(Err(err)) => return Err(err),
        None => return Err(Error::new(Span::char(source.pos()), error_message)),
    }
}

macro_rules! parse_token {
    ($source: ident, $token: ident, $error_message: expr) => {
        match $source.next() {
            Some(Ok(Spanned {
                data: Token::$token,
                ..
            })) => (),
            Some(Ok(Spanned { span, .. })) => return Err(Error::new(span, $error_message)),
            Some(Err(err)) => return Err(err),
            None => return Err(Error::new(Span::char($source.pos()), $error_message)),
        }
    };
}

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Spanned<Statement>>,
}

impl Parse for Program {
    fn parse(source: &mut TokenStream<'_>) -> Result<Spanned<Program>> {
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
        .with(Self { body }))
    }
}

#[derive(Debug)]
pub enum Statement {
    DefVariable {
        constant: bool,
        name: Spanned<String>,
        type_hint: Option<Spanned<Type>>,
        value: Option<Spanned<Expression>>,
    },
    DefFunction {
        name: Spanned<String>,
        args: Vec<Spanned<Arg>>,
        return_type: Option<Spanned<Type>>,
        body: Spanned<Expression>,
    },
    DefType {
        name: Spanned<String>,
        generics: Vec<Spanned<String>>,
        value: Spanned<Type>,
    },

    Expression(Expression),
}

impl Statement {
    fn parse_def_variable(source: &mut TokenStream) -> Result<Spanned<Self>> {
        let constant = source.peek_token() == Some(&Token::KeywordConst);

        let Some(Ok(Spanned { span, data: _ })) = source.next() else {
            unreachable!()
        };
        let name = parse_ident(source, "Expected a variable name. Valid identifiers must include just letters, underscores, and numbers.")?;

        let type_hint = if let Some(Token::Colon) = source.peek_token() {
            source.next();
            Some(Type::parse(source)?)
        } else {
            None
        };

        let value = match source.peek_token() {
                    Some(Token::Equals) => {
                        source.next();
                        Some(Expression::parse(source)?)
                    }
                    Some(Token::Semicolon) => None,
                    Some(_) => {
                        #[allow(clippy::unwrap_used)]
                        let Spanned { span, .. } = source.next().unwrap()?;
                        return Err(Error::new(span, "Expected an equal sign to denote an initial value, or a semicolon to leave it unassigned."));
                    }
                    None => return Err(Error::new(Span::char(source.pos()), "Expected an equal sign to denote an initial value, or a semicolon to leave it unassigned."))
                };

        Ok(Span {
            start: span.start,
            end: source.pos(),
        }
        .with(Self::DefVariable {
            constant,
            name,
            type_hint,
            value,
        }))
    }

    fn parse_def_function(source: &mut TokenStream) -> Result<Spanned<Self>> {
        let Some(Ok(Spanned { span, data: _ })) = source.next() else {
            unreachable!()
        };
        let name = parse_ident(
            source,
            "Expected a funtion name. Valid identifiers must include just letters, underscores, and numbers.",
        )?;

        parse_token!(
            source,
            ParenOpen,
            "Expected an opening parenthesis to start argument list."
        );

        let mut args = Vec::new();
        let mut trailing_delimiter = true;

        loop {
            if let Some(Token::ParenClose) = source.peek_token() {
                source.next();
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                args.push(Arg::parse(source)?);
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

        let return_type = if let Some(Token::Arrow) = source.peek_token() {
            source.next();
            Some(Type::parse(source)?)
        } else {
            None
        };

        parse_token!(
            source,
            Equals,
            "Expected an equals sign to seperate function signature and body."
        );

        let body = Expression::parse(source)?;

        Ok(Span {
            start: span.start,
            end: body.span.end,
        }
        .with(Self::DefFunction {
            name,
            args,
            return_type,
            body,
        }))
    }

    fn parse_def_type(source: &mut TokenStream) -> Result<Spanned<Self>> {
        let Some(Ok(Spanned { span, data: _ })) = source.next() else {
            unreachable!()
        };
        let name = parse_ident(
            source,
            "Expected a type name. Valid identifiers must include just letters, underscores, and numbers.",
        )?;

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
                    generics.push(parse_ident(source, "Expected a generic type name.")?);
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

        parse_token!(
            source,
            Equals,
            "Expected an equals sign to define type alias."
        );

        let value = Type::parse(source)?;

        Ok(Span {
            start: span.start,
            end: value.span.end,
        }
        .with(Self::DefType {
            name,
            generics,
            value,
        }))
    }
}

impl Parse for Statement {
    fn parse(source: &mut TokenStream) -> Result<Spanned<Self>> {
        match source.peek_token() {
            Some(Token::KeywordLet | Token::KeywordConst) => Self::parse_def_variable(source),
            Some(Token::KeywordFunction) => Self::parse_def_function(source),
            Some(Token::KeywordType) => Self::parse_def_type(source),
            Some(_) => {
                #[allow(clippy::unwrap_used)]
                let Spanned { span, .. } = source.next().unwrap()?;
                Err(Error::new(
                    span,
                    "Expected a statement, like a variable definition or function call.",
                ))
            }
            None => Err(Error::new(
                Span::char(source.pos()),
                "Expected a statement, like a variable definition or function call.",
            )),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    String(String),
    Number(f64),
    Boolean(bool),
    Table(HashMap<Spanned<Key>, Spanned<Expression>>),
    Lambda {
        args: Vec<Arg>,
        return_type: String,
        body: Box<Expression>,
    },

    FnCall {
        name: String,
        args: Vec<Expression>,
    },
    Variable(String),

    Block {
        body: Vec<Statement>,
        return_last: bool,
    },
    If {
        blocks: HashMap<Expression, Vec<Statement>>,
        else_body: Option<Vec<Statement>>,
    },
}

impl Expression {
    pub fn unit() -> Self {
        Self::Table(HashMap::new())
    }

    fn parse_table(source: &mut TokenStream) -> Result<Spanned<Self>> {
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
                let is_key = source.peek_nth_token(2) == Some(&Token::Equals);
                match source.peek_token() {
                    Some(Token::Ident(_) | Token::String(_)) if is_key => {
                        let Some(Ok(Spanned {
                            span: key_span,
                            data: Token::Ident(key) | Token::String(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip equals
                        let value = Expression::parse(source)?;

                        pairs.insert(key_span.with(Key::Named(key)), value);
                    }
                    Some(Token::Number(_)) if is_key => {
                        let Some(Ok(Spanned {
                            span: key_span,
                            data: Token::Number(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip equals
                        let value = Expression::parse(source)?;

                        if key >= 0.0 && !key.is_nan() && key.floor() == key {
                            pairs.insert(key_span.with(Key::Index(key as usize)), value);
                        } else {
                            return Err(Error::new(key_span, "I can only use round numbers that are zero or more as keys in a table."));
                        }
                    }
                    Some(Token::KeywordWith) => {
                        source.next(); // Skip with

                        match source.next() {
                            Some(Ok(Spanned {
                                span: name_span,
                                data: Token::Ident(name),
                            })) => {
                                pairs.insert(name_span.with(Key::Named(name.clone())), name_span.with(Expression::Variable(name)));
                            }
                            Some(Ok(Spanned { span, .. })) => {
                                return Err(Error::new(
                                    span,
                                    "I can only include variables in a table, not expressions. Try 'key = expression'.",
                                ))
                            }
                            Some(Err(err)) => return Err(err),
                            None => {
                                return Err(Error::new(
                                    Span::char(source.pos()),
                                    "Expected an indentifier to include in the table.",
                                ))
                            }
                        }
                    }
                    Some(_) => {
                        let key_span = Span::char(source.pos());
                        let key = next_index;
                        next_index += 1;

                        let value = Expression::parse(source)?;
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
}

impl Parse for Expression {
    fn parse(source: &mut TokenStream) -> Result<Spanned<Self>> {
        match source.peek_token() {
            Some(Token::String(_)) => {
                let Some(Ok(Spanned {
                    span,
                    data: Token::String(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                Ok(span.with(Self::String(data)))
            }
            Some(Token::Number(_)) => {
                let Some(Ok(Spanned {
                    span,
                    data: Token::Number(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                Ok(span.with(Self::Number(data)))
            }
            Some(Token::Boolean(_)) => {
                let Some(Ok(Spanned {
                    span,
                    data: Token::Boolean(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                Ok(span.with(Self::Boolean(data)))
            }

            // Table
            Some(Token::BracketOpen) => Self::parse_table(source),

            Some(_) => {
                #[allow(clippy::unwrap_used)]
                let Spanned { span, .. } = source.next().unwrap()?;
                Err(Error::new(
                    span,
                    "Expected an expression, like a number or function call.",
                ))
            }
            None => Err(Error::new(
                Span::char(source.pos()),
                "Expected an expression, like a number or function call.",
            )),
        }
    }
}

#[derive(Debug)]
pub struct Arg {
    pub name: Spanned<String>,
    pub type_hint: Spanned<Type>,
}

impl Parse for Arg {
    fn parse(source: &mut TokenStream) -> Result<Spanned<Self>> {
        let name = parse_ident(source, "Expected an argument name.")?;
        parse_token!(
            source,
            Colon,
            format!(
                "Expected an argument type. Try adding a colon: '{name}: /* type */'",
                name = name.data
            )
        );

        let type_hint = Type::parse(source)?;

        Ok(Span {
            start: name.span.start,
            end: type_hint.span.end,
        }
        .with(Self { name, type_hint }))
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Key {
    Named(String),
    Index(usize),
}

#[derive(Debug)]
pub enum Type {
    Named {
        name: Spanned<String>,
        generics: Vec<Spanned<Type>>,
    },
    Table(HashMap<Spanned<Key>, Spanned<Type>>),
}

impl Type {
    fn parse_named(source: &mut TokenStream) -> Result<Spanned<Self>> {
        let name = parse_ident(source, "Expected a type, like string or List<number>.")?;

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
                    generics.push(Type::parse(source)?);
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

    fn parse_table(source: &mut TokenStream) -> Result<Spanned<Self>> {
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
                        let Some(Ok(Spanned {
                            span: key_span,
                            data: Token::Ident(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip colon
                        let value = Type::parse(source)?;

                        pairs.insert(key_span.with(Key::Named(key)), value);
                    }
                    Some(Token::Number(_)) if is_key => {
                        let Some(Ok(Spanned {
                            span: key_span,
                            data: Token::Number(key),
                        })) = source.next()
                        else {
                            unreachable!()
                        };
                        source.next(); // Skip colon
                        let value = Type::parse(source)?;

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

                        let value = Type::parse(source)?;
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
}

impl Parse for Type {
    fn parse(source: &mut TokenStream) -> Result<Spanned<Self>> {
        match source.peek_token() {
            Some(Token::BracketOpen) => Self::parse_table(source),
            _ => Self::parse_named(source),
        }
    }
}
