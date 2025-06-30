use crate::{
    error::{Error, Result},
    lexer::{TokenStream, token::Token},
    span::{Chunk, Pos, Span},
};

use super::{
    Parse,
    expression::{Expression, ExpressionContext},
    parse_token,
};

#[derive(Debug)]
pub enum Statement<'a> {
    DefVariable {
        name: Chunk<&'a str>,
        var_type: Option<Chunk<Expression<'a>>>,
        value: Option<Chunk<Expression<'a>>>,
    },
    DefConst(ConstantDefinition<'a>),
    DefFunction(FunctionDefinition<'a>),

    Assign {
        target: Chunk<Expression<'a>>,
        op: Chunk<AssignOperator>,
    },
    Expr(Expression<'a>),

    While {
        condition: Chunk<Expression<'a>>,
        body: Chunk<Block<'a>>,
    },
    For {
        item_name: Chunk<&'a str>,
        iterator: Chunk<Expression<'a>>,
        body: Chunk<Block<'a>>,
    },
    Loop(Chunk<Block<'a>>),

    Return(Option<Chunk<Expression<'a>>>),
    Break,
}
impl<'a> Statement<'a> {
    fn needs_semicolon(&self) -> bool {
        match self {
            Statement::DefVariable { .. } => true,
            Statement::DefConst { .. } => true,
            Statement::DefFunction { .. } => false,
            Statement::Assign { .. } => true,
            Statement::Expr(expression) => match expression {
                Expression::If { .. } => false,
                Expression::Block(_) => false,
                _ => true,
            },
            Statement::While { .. } => false,
            Statement::For { .. } => false,
            Statement::Loop(_) => false,
            Statement::Return(_) => true,
            Statement::Break => true,
        }
    }

    fn parse_loop(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordLoop)?.start; // skip loop keyword

        parse_token(source, Token::BraceOpen)?;
        let body = Block::parse(source)?;
        let end = parse_token(source, Token::BraceClose)?.end;

        Ok(Chunk::new(Span { start, end }, Self::Loop(body)))
    }

    fn parse_for(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordFor)?.start;

        let item_name = <&str>::parse(source)?;
        parse_token(source, Token::KeywordIn)?;
        let iterator = Expression::parse(source)?;

        parse_token(source, Token::BraceOpen)?;
        let body = Block::parse(source)?;
        let end = parse_token(source, Token::BraceClose)?.end;

        Ok(Chunk::new(
            Span { start, end },
            Self::For {
                item_name,
                iterator,
                body,
            },
        ))
    }

    fn parse_while(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordWhile)?.start;

        let condition = Expression::parse(source)?;

        parse_token(source, Token::BraceOpen)?;
        let body = Block::parse(source)?;
        let end = parse_token(source, Token::BraceClose)?.end;

        Ok(Chunk::new(
            Span { start, end },
            Self::While { condition, body },
        ))
    }

    fn parse_var(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordVar)?.start;

        let name = <&str>::parse(source)?;

        let var_type = if let Some(Token::Colon) = source.peek_token() {
            source.next();
            Some(Expression::parse(source)?)
        } else {
            None
        };

        let value = if let Some(Token::Equals) = source.peek_token() {
            source.next();
            Some(Expression::parse(source)?)
        } else {
            None
        };

        Ok(Chunk::new(
            Span {
                start,
                end: *source.pos(),
            },
            Self::DefVariable {
                name,
                var_type,
                value,
            },
        ))
    }
}
impl<'a> Parse<'a> for Statement<'a> {
    type Context = ();

    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let ident_next = matches!(source.peek_nth_token(1), Some(Token::Ident(_)));
        match source.peek_token() {
            Some(Token::KeywordVar) => Self::parse_var(source),
            Some(Token::KeywordConst) if ident_next => {
                Ok(ConstantDefinition::parse(source)?.map(|c| Self::DefConst(c)))
            }
            Some(Token::KeywordFn) => {
                Ok(FunctionDefinition::parse(source)?.map(|f| Self::DefFunction(f)))
            }
            Some(Token::KeywordWhile) => Self::parse_while(source),
            Some(Token::KeywordFor) => Self::parse_for(source),
            Some(Token::KeywordLoop) => Self::parse_loop(source),
            Some(Token::KeywordReturn) => {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };

                let expr = if let Some(&Token::Semicolon) = source.peek_token() {
                    None
                } else {
                    Some(Expression::parse(source)?)
                };
                Ok(Chunk::new(
                    Span {
                        start: span.start,
                        end: *expr.as_ref().map_or(&span.end, |expr| &expr.span.end),
                    },
                    Self::Return(expr),
                ))
            }
            Some(Token::KeywordBreak) => {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };

                Ok(Chunk::new(span, Self::Break))
            }

            Some(_) => {
                let expr = Expression::parse(source)?;
                let op = match source.peek_token() {
                    Some(Token::Equals) => AssignOperator::Set,
                    Some(Token::PlusEquals) => AssignOperator::Add,
                    Some(Token::MinusEquals) => AssignOperator::Subtract,
                    Some(Token::AsteriskEquals) => AssignOperator::Multiply,
                    Some(Token::SlashEquals) => AssignOperator::Divide,
                    _ => return Ok(expr.map(|e| Self::Expr(e))),
                };
                let Some(Ok(Chunk { span: op_span, .. })) = source.next() else {
                    unreachable!()
                };

                let value = Expression::parse(source)?;
                Ok(Chunk::new(
                    Span {
                        start: expr.span.start,
                        end: value.span.end,
                    },
                    Self::Assign {
                        target: expr,
                        op: Chunk::new(op_span, op),
                    },
                ))
            }

            None => Err(Error::new(
                "Expected a statement.",
                Span::char(*source.pos()),
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssignOperator {
    Set,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub body: Vec<Chunk<Statement<'a>>>,
    pub tail: Option<Chunk<Box<Expression<'a>>>>,
}

impl<'a> Parse<'a> for Block<'a> {
    type Context = ();
    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let mut body = Vec::new();
        let mut trailing_delimeter = true;
        while source.peek().is_some() && source.peek_token() != Some(&Token::BraceClose) {
            if trailing_delimeter {
                let statement = Statement::parse(source)?;
                if statement.data.needs_semicolon() {
                    if let Some(Token::Semicolon) = source.peek_token() {
                        source.next(); // skip semi colon
                    } else {
                        trailing_delimeter = false;
                    }
                }

                body.push(statement);
            } else {
                let span = match source.next() {
                    Some(Ok(t)) => t.span,
                    Some(Err(e)) => return Err(e),
                    None => Span::char(*source.pos()),
                };
                return Err(Error::new("Expected a semicolon.", span));
            }
        }

        let tail = if !trailing_delimeter {
            // if trailing delimeter is false, that means the loop happened at least once and added a statement.
            #[expect(clippy::unwrap_used)]
            let tail_statement = body.pop().unwrap();
            Some(match tail_statement.data {
                Statement::Expr(expr) => Chunk::new(tail_statement.span, Box::new(expr)),
                _ => return Err(Error::new("Expected an expression", tail_statement.span)),
            })
        } else {
            None
        };

        Ok(Chunk::new(
            Span {
                start: body.get(0).map_or(Pos::default(), |c| c.span.start),
                end: body.last().map_or(Pos::default(), |c| c.span.end),
            },
            Self { body, tail },
        ))
    }
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    name: Chunk<&'a str>,
    self_parameter: bool,
    parameters: Vec<Chunk<Parameter<'a>>>,
    return_type: Option<Chunk<Expression<'a>>>,
    body: Chunk<Block<'a>>,
}

impl<'a> Parse<'a> for FunctionDefinition<'a> {
    type Context = ();

    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordFn)?.start;

        let name = <&str>::parse(source)?;

        parse_token(source, Token::ParenOpen)?;
        let self_parameter = if let Some(Token::Ampersand) = source.peek_token() {
            source.next();
            parse_token(source, Token::KeywordSelf)?;
            true
        } else {
            false
        };
        let parameters = Parameter::parse_group(source, Token::Comma, Token::ParenClose)?;

        let return_type = if let Some(Token::Arrow) = source.peek_token() {
            source.next();
            Some(Expression::parse_context(
                source,
                ExpressionContext {
                    prevent_struct_initialization: true,
                },
            )?)
        } else {
            None
        };

        let body;
        let end;
        if let Some(Token::Equals) = source.peek_token() {
            source.next();
            let expr = Expression::parse(source)?;
            parse_token(source, Token::Semicolon)?;

            end = expr.span.end;
            body = Chunk::new(
                expr.span,
                Block {
                    body: vec![],
                    tail: Some(expr.as_box()),
                },
            );
        } else {
            parse_token(source, Token::BraceOpen)?;
            body = Block::parse(source)?;
            end = parse_token(source, Token::BraceClose)?.end;
        }

        Ok(Chunk::new(
            Span { start, end },
            Self {
                name,
                self_parameter,
                parameters,
                return_type,
                body,
            },
        ))
    }
}

#[derive(Debug)]
pub struct Parameter<'a>(Chunk<&'a str>, Chunk<Expression<'a>>);

impl<'a> Parse<'a> for Parameter<'a> {
    type Context = ();
    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let name = <&str>::parse(source)?;
        parse_token(source, Token::Colon)?;
        let ty = Expression::parse(source)?;
        Ok(Chunk::new(
            Span {
                start: name.span.start,
                end: name.span.end,
            },
            Self(name, ty),
        ))
    }
}

#[derive(Debug)]
pub struct ConstantDefinition<'a> {
    name: Chunk<&'a str>,
    var_type: Option<Chunk<Expression<'a>>>,
    value: Chunk<Expression<'a>>,
}

impl<'a> Parse<'a> for ConstantDefinition<'a> {
    type Context = ();
    fn parse_context(source: &mut TokenStream<'a>, _: ()) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordConst)?.start;

        let name = <&str>::parse(source)?;

        let var_type = if let Some(Token::Colon) = source.peek_token() {
            source.next();
            Some(Expression::parse(source)?)
        } else {
            None
        };

        parse_token(source, Token::Equals)?;
        let value = Expression::parse(source)?;

        Ok(Chunk::new(
            Span {
                start,
                end: value.span.end,
            },
            Self {
                name,
                var_type,
                value,
            },
        ))
    }
}
