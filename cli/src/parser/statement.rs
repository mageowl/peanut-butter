use crate::{
    lexer::TokenStream,
    parser::{parse_ident, parse_token},
};
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
};

use super::{block::Block, expression::Expression, type_name::TypeName, Parameter, Parse};

#[derive(Debug)]
pub enum Statement {
    DefVariable {
        mutable: bool,
        name: Chunk<String>,
        type_hint: Option<Chunk<TypeName>>,
        value: Option<Chunk<Expression>>,
    },
    DefFunction {
        mutable: bool,
        name: Chunk<String>,
        parameters: Vec<Chunk<Parameter>>,
        return_type: Option<Chunk<TypeName>>,
        body: Chunk<Expression>,
    },
    DefType {
        name: Chunk<String>,
        generics: Vec<Chunk<String>>,
        value: Chunk<TypeName>,
    },

    Assign {
        target: Chunk<Expression>,
        value: Chunk<Expression>,
        op: AssignmentOperator,
    },
    Expression(Expression),
    /*WhileLoop {
        condition: Chunk<Expression>,
        block: Chunk<Block>,
    },*/
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

impl Statement {
    fn parse_def_variable(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let Some(Ok(Chunk {
            span,
            data: keyword,
        })) = source.next()
        else {
            unreachable!()
        };
        let mutable = keyword == Token::KeywordMut;
        let name = parse_ident(source, "Expected a variable name. Valid identifiers must include just letters, underscores, and numbers.")?;

        let type_hint = if let Some(Token::Colon) = source.peek_token() {
            source.next();
            Some(TypeName::parse(source)?)
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
                    let Chunk { span, .. } = source.next().unwrap()?;
                    return Err(Error::new(span, "Expected an equal sign to denote an initial value, or a semicolon to leave it unassigned."));
                }
                None => return Err(Error::new(Span::char(source.pos()), "Expected an equal sign to denote an initial value, or a semicolon to leave it unassigned."))
            };

        Ok(Span {
            start: span.start,
            end: source.pos(),
        }
        .with(Self::DefVariable {
            mutable,
            name,
            type_hint,
            value,
        }))
    }

    fn parse_def_function(source: &mut TokenStream, mutable: bool) -> Result<Chunk<Self>> {
        let Some(Ok(Chunk { span, data: _ })) = source.next() else {
            unreachable!()
        };
        if mutable {
            source.next(); // Skip mut *and* fn keywords
        }

        let name = parse_ident(
            source,
            "Expected a funtion name. Valid identifiers must include just letters, underscores, and numbers.",
        )?;

        parse_token(
            source,
            Token::ParenOpen,
            "Expected an opening parenthesis to start argument list.",
        )?;

        let mut args = Vec::new();
        let mut trailing_delimiter = true;

        loop {
            if let Some(Token::ParenClose) = source.peek_token() {
                source.next();
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                args.push(Parameter::parse(source)?);
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
            Some(TypeName::parse(source)?)
        } else {
            None
        };

        parse_token(
            source,
            Token::Equals,
            "Expected an equals sign to seperate function signature and body.",
        )?;

        let body = Expression::parse(source)?;

        Ok(Span {
            start: span.start,
            end: body.span.end,
        }
        .with(Self::DefFunction {
            mutable: false,
            name,
            parameters: args,
            return_type,
            body,
        }))
    }

    fn parse_def_type(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let Some(Ok(Chunk { span, data: _ })) = source.next() else {
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

        parse_token(
            source,
            Token::Equals,
            "Expected an equals sign to define type alias.",
        )?;

        let value = TypeName::parse(source)?;

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

    fn parse_assignment(
        source: &mut TokenStream,
        target: Chunk<Expression>,
    ) -> Result<Chunk<Self>> {
        let op = match source.next().unwrap_or_else(|| {
            Err(Error::new(
                Span::char(source.pos()),
                "Expected assignment operator.",
            ))
        })? {
            Chunk {
                data: Token::Equals,
                ..
            } => AssignmentOperator::Assign,
            Chunk {
                data: Token::AddAssign,
                ..
            } => AssignmentOperator::AddAssign,
            Chunk {
                data: Token::SubAssign,
                ..
            } => AssignmentOperator::SubAssign,
            Chunk {
                data: Token::MulAssign,
                ..
            } => AssignmentOperator::MulAssign,
            Chunk {
                data: Token::DivAssign,
                ..
            } => AssignmentOperator::DivAssign,
            Chunk { span, .. } => return Err(Error::new(span, "Expected an assignment operator.")),
        };

        let value = Expression::parse(source)?;
        Ok(Span {
            start: target.span.start,
            end: value.span.end,
        }
        .with(Self::Assign { target, value, op }))
    }
}

impl Parse for Statement {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let ident_next = matches!(source.peek_nth_token(2), Some(Token::Ident(_)));
        match source.peek_token() {
            Some(Token::KeywordMut) => {
                if source.peek_nth_token(2) == Some(&Token::KeywordFunction) {
                    Self::parse_def_function(source, true)
                } else {
                    Self::parse_def_variable(source)
                }
            }
            Some(Token::KeywordLet) => Self::parse_def_variable(source),
            Some(Token::KeywordFunction) if ident_next => Self::parse_def_function(source, false),
            Some(Token::KeywordType) => Self::parse_def_type(source),
            Some(_) => {
                let expr = Expression::parse(source)?;
                if let Some(
                    Token::Equals
                    | Token::AddAssign
                    | Token::SubAssign
                    | Token::MulAssign
                    | Token::DivAssign,
                ) = source.peek_token()
                {
                    Self::parse_assignment(source, expr)
                } else {
                    Ok(expr.span.with(Self::Expression(expr.data)))
                }
            }
            None => Err(Error::new(
                Span::char(source.pos()),
                "Expected a statement, like a variable definition or function call.",
            )),
        }
    }
}
