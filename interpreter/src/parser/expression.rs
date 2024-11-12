use std::collections::HashMap;

use super::{parse_ident, parse_token};
use crate::lexer::TokenStream;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    token::Token,
    value::Key,
};

use super::{block::Block, type_hint::TypeName, Parameter, Parse};

#[derive(Debug)]
pub enum Expression {
    String(String),
    Number(f64),
    Boolean(bool),
    Table(HashMap<Chunk<Key>, Chunk<Expression>>),
    Lambda {
        parameters: Vec<Chunk<Parameter>>,
        return_type: Option<Chunk<TypeName>>,
        body: Chunk<Box<Expression>>,
    },

    Call {
        value: Chunk<Box<Expression>>,
        args: Vec<Chunk<Expression>>,
    },
    Variable(String),

    Block(Block),
    If {
        blocks: HashMap<Chunk<Expression>, Chunk<Block>>,
        else_block: Option<Chunk<Block>>,
    },

    Access(Chunk<Box<Expression>>, Chunk<String>),
    DynAccess(Chunk<Box<Expression>>, Chunk<Box<Expression>>),
    BinaryOp {
        a: Chunk<Box<Expression>>,
        b: Chunk<Box<Expression>>,
        op: Chunk<Operation>,
    },
    UnaryOp {
        a: Chunk<Box<Expression>>,
        op: Chunk<UnaryOperation>,
    },
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Operation {
    Exponentation,
    Multiplication,
    Division,
    Addition,
    Subtraction,
    Comparison(Comparison),
    BooleanAnd,
    BooleanOr,
    Access,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Comparison {
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum UnaryOperation {
    BooleanNot,
    Negation,
}

// Order for comparisons doesn't matter.
impl PartialOrd for Comparison {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}
impl Ord for Comparison {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl Expression {
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
                        let value = Expression::parse(source)?;

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
                            Some(Ok(Chunk {
                                span: name_span,
                                data: Token::Ident(name),
                            })) => {
                                pairs.insert(name_span.with(Key::Named(name.clone())), name_span.with(Expression::Variable(name)));
                            }
                            Some(Ok(Chunk { span, .. })) => {
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

    fn parse_lambda(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let Some(Ok(Chunk { span, data: _ })) = source.next() else {
            unreachable!()
        };

        if let Some(Ok(Chunk {
            span,
            data: Token::Ident(_),
        })) = source.peek()
        {
            return Err(Error::new(*span, "Lambda functions cannot have a name."));
        }

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

        let body = Expression::parse(source)?.as_box();

        Ok(Span {
            start: span.start,
            end: body.span.end,
        }
        .with(Self::Lambda {
            parameters: args,
            return_type,
            body,
        }))
    }

    fn parse_function_call(
        source: &mut TokenStream,
        value: Chunk<Box<Expression>>,
    ) -> Result<Chunk<Self>> {
        let mut args = Vec::new();
        let mut trailing_delimiter = true;
        loop {
            if let Some(Token::ParenClose) = source.peek_token() {
                source.next();
                break;
            } else if trailing_delimiter {
                trailing_delimiter = false;
                args.push(Expression::parse(source)?);
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
            start: value.span.start,
            end: source.pos(),
        }
        .with(Self::Call { value, args }))
    }

    fn parse_single(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let mut expr = match source.peek_token() {
            Some(Token::String(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::String(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                span.with(Self::String(data))
            }
            Some(Token::Number(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Number(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                span.with(Self::Number(data))
            }
            Some(Token::Boolean(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Boolean(data),
                })) = source.next()
                else {
                    unreachable!()
                };
                span.with(Self::Boolean(data))
            }
            Some(Token::BracketOpen) => Self::parse_table(source)?,
            Some(Token::KeywordFunction) => Self::parse_lambda(source)?,

            Some(Token::Ident(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Ident(name),
                })) = source.next()
                else {
                    unreachable!()
                };
                span.with(Self::Variable(name))
            }

            Some(Token::BraceOpen) => {
                let Chunk { span, data } = Block::parse(source)?;
                span.with(Self::Block(data))
            }

            Some(Token::Exclamation | Token::Sub) => {
                let Some(Ok(Chunk { span, data: op })) = source.next() else {
                    unreachable!()
                };
                let a = Self::parse_single(source)?;

                Span {
                    start: span.start,
                    end: a.span.end,
                }
                .with(Self::UnaryOp {
                    a: a.as_box(),
                    op: span.with(match op {
                        Token::Exclamation => UnaryOperation::BooleanNot,
                        Token::Sub => UnaryOperation::Negation,
                        _ => unreachable!(),
                    }),
                })
            }

            Some(_) => {
                #[allow(clippy::unwrap_used)]
                let Chunk { span, .. } = source.next().unwrap()?;
                return Err(Error::new(
                    span,
                    "Expected an expression, like a number or function call.",
                ));
            }
            None => {
                return Err(Error::new(
                    Span::char(source.pos()),
                    "Expected an expression, like a number or function call.",
                ))
            }
        };

        while let Some(Token::Dot | Token::BracketOpen | Token::ParenOpen) = source.peek_token() {
            #[allow(clippy::unwrap_used)]
            match source.next().unwrap()?.data {
                Token::ParenOpen => {
                    expr = Self::parse_function_call(source, expr.as_box())?;
                }
                Token::Dot => {
                    let property = parse_ident(source, "Expected a identifier.")?;

                    expr = Span {
                        start: expr.span.start,
                        end: property.span.end,
                    }
                    .with(Expression::Access(expr.as_box(), property));
                }
                Token::BracketOpen => {
                    let property = Expression::parse(source)?;
                    parse_token(
                        source,
                        Token::BracketClose,
                        "Missing a trailing bracket for getting a property of a table.",
                    )?;

                    expr = Span {
                        start: expr.span.start,
                        end: property.span.end,
                    }
                    .with(Expression::DynAccess(expr.as_box(), property.as_box()))
                }

                _ => unreachable!(),
            }
        }

        Ok(expr)
    }
}

impl Parse for Expression {
    fn parse(source: &mut TokenStream) -> Result<Chunk<Self>> {
        let mut expr = Self::parse_single(source)?;

        fn insert(
            lhs: Chunk<Box<Expression>>,
            op: Chunk<Operation>,
            rhs: Chunk<Box<Expression>>,
        ) -> Chunk<Expression> {
            let span = Span {
                start: lhs.span.start,
                end: rhs.span.end,
            };

            match *lhs.data {
                Expression::BinaryOp { a, b, op: b_op } if op.data <= b_op.data => {
                    span.with(Expression::BinaryOp {
                        a,
                        b: insert(b, op, rhs).as_box(),
                        op: b_op,
                    })
                }
                Expression::UnaryOp { a, op: b_op } if op.data == Operation::Access => {
                    span.with(Expression::UnaryOp {
                        a: insert(a, op, rhs).as_box(),
                        op: b_op,
                    })
                }
                expr => span.with(Expression::BinaryOp {
                    a: lhs.span.with(Box::new(expr)),
                    b: rhs,
                    op,
                }),
            }
        }

        while let Some(
            Token::Add
            | Token::Sub
            | Token::Mul
            | Token::Div
            | Token::Caret
            | Token::DoubleEqual
            | Token::NotEqual
            | Token::Gt
            | Token::GtEqual
            | Token::Lt
            | Token::LtEqual
            | Token::BooleanAnd
            | Token::BooleanOr,
        ) = source.peek_token()
        {
            #[allow(clippy::unwrap_used)]
            let op = source.next().unwrap()?;
            let op = Chunk {
                data: match op.data {
                    Token::Add => Operation::Addition,
                    Token::Sub => Operation::Subtraction,
                    Token::Mul => Operation::Multiplication,
                    Token::Div => Operation::Division,
                    Token::Caret => Operation::Exponentation,
                    Token::DoubleEqual => Operation::Comparison(Comparison::Equals),
                    Token::NotEqual => Operation::Comparison(Comparison::NotEquals),
                    Token::Lt => Operation::Comparison(Comparison::LessThan),
                    Token::LtEqual => Operation::Comparison(Comparison::LessThanEqual),
                    Token::Gt => Operation::Comparison(Comparison::GreaterThan),
                    Token::GtEqual => Operation::Comparison(Comparison::GreaterThanEqual),
                    Token::BooleanAnd => Operation::BooleanAnd,
                    Token::BooleanOr => Operation::BooleanOr,
                    _ => unreachable!(),
                },
                span: op.span,
            };

            let b_expr = Self::parse_single(source)?;
            expr = insert(expr.as_box(), op, b_expr.as_box());
        }

        Ok(expr)
    }
}