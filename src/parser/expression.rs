use crate::{
    error::{Error, Result},
    lexer::{TokenStream, token::Token},
    span::{Chunk, Span},
};

use super::{
    Parse, parse_token,
    pattern::MatchPattern,
    statement::{Block, ConstantDefinition, FunctionDefinition},
};

pub struct ExpressionContext {
    pub prevent_struct_initialization: bool,
}

impl Default for ExpressionContext {
    fn default() -> Self {
        Self {
            prevent_struct_initialization: false,
        }
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    Null,
    String(&'a str),
    Int {
        slice: &'a str,
        negative: bool,
    },
    Float {
        slice: &'a str,
        negative: bool,
    },
    //Array() TODO
    Boolean(bool),

    Ident(&'a str),
    IdentSelf,
    Property(Chunk<Box<Self>>, Chunk<&'a str>),
    CallMethod {
        object: Chunk<Box<Self>>,
        name: Chunk<&'a str>,
        arguments: Vec<Chunk<Self>>,
    },
    Call {
        function: Chunk<Box<Self>>,
        arguments: Vec<Chunk<Self>>,
    },

    StructType {
        properties: Vec<(Chunk<&'a str>, Chunk<Self>)>,
        methods: Vec<Chunk<FunctionDefinition<'a>>>,
        constants: Vec<Chunk<ConstantDefinition<'a>>>,
    },
    EnumType {
        variants: Vec<(Chunk<&'a str>, Option<Chunk<Self>>)>,
        methods: Vec<Chunk<FunctionDefinition<'a>>>,
        constants: Vec<Chunk<ConstantDefinition<'a>>>,
    },

    StructInitializer {
        struct_type: Chunk<Box<Self>>,
        properties: Vec<(Chunk<&'a str>, Chunk<Self>)>,
    },
    Tuple(Vec<Chunk<Self>>),

    Block(Block<'a>),
    If {
        blocks: Vec<(Chunk<Expression<'a>>, Chunk<Block<'a>>)>,
        else_block: Option<Chunk<Block<'a>>>,
    },
    Match {
        value: Chunk<Box<Self>>,
        arms: Vec<(Chunk<MatchPattern<'a>>, Chunk<Box<Self>>)>,
        else_arm: Option<Chunk<Box<Self>>>,
    },

    BinaryOp {
        lhs: Chunk<Box<Self>>,
        rhs: Chunk<Box<Self>>,
        op: Chunk<Operation>,
    },
    UnaryOp {
        x: Chunk<Box<Self>>,
        op: Chunk<UnaryOperation>,
    },
    Group(Box<Self>),

    BuiltIn {
        name: Chunk<&'a str>,
        body: Vec<Chunk<Token<'a>>>,
    },
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Operation {
    Concat,
    Exponent,
    Multiply,
    Divide,
    Add,
    Subtract,
    Equals,
    NotEquals,
    Lt,
    LtEqual,
    Gt,
    GtEqual,
    And,
    Or,
}
#[derive(Debug, PartialEq, Eq)]
pub enum UnaryOperation {
    Not,
    Negative,
    Pointer,
    Dereference,
}

impl<'a> Expression<'a> {
    fn parse_if(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = *source.pos();
        let mut blocks = Vec::new();

        let mut else_block = None;

        while let Some(Token::KeywordIf) = source.peek_token() {
            source.next();
            let condition = Self::parse_context(
                source,
                ExpressionContext {
                    prevent_struct_initialization: true,
                },
            )?;
            parse_token(source, Token::BraceOpen)?;
            blocks.push((condition, Block::parse(source)?));
            parse_token(source, Token::BraceClose)?;

            if let Some(Token::KeywordElse) = source.peek_token() {
                source.next();
                if let Some(Token::KeywordIf) = source.peek_token() {
                    continue;
                } else {
                    parse_token(source, Token::BraceOpen)?;
                    else_block = Some(Block::parse(source)?);
                    parse_token(source, Token::BraceClose)?;
                }
            } else {
                break;
            }
        }

        Ok(Chunk::new(
            Span {
                start,
                end: *source.pos(),
            },
            Self::If { blocks, else_block },
        ))
    }

    fn parse_match(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordMatch)?.start;
        let value = Self::parse_context(
            source,
            ExpressionContext {
                prevent_struct_initialization: true,
            },
        )?
        .as_box();
        parse_token(source, Token::BraceOpen)?;

        let mut trailing_delimeter = true;
        let mut arms = Vec::new();
        let mut else_arm = None;

        loop {
            if let Some(Token::BraceClose) | None = source.peek_token() {
                break;
            }

            if trailing_delimeter {
                if let Some(Token::KeywordElse) = source.peek_token() {
                    if else_arm.is_some() {
                        return Err(Error::new(
                            "Duplicate else case in match statement.",
                            source.next().unwrap()?.span,
                        ));
                    }
                    source.next();
                    parse_token(source, Token::ThickArrow)?;
                    let block = Self::parse(source)?;
                    if !matches!(block.data, Self::Block(_))
                        || source
                            .peek_token()
                            .is_some_and(|t| matches!(t, Token::Comma))
                    {
                        parse_token(source, Token::Comma)?;
                    }
                    else_arm = Some(block.as_box());
                }

                let pattern = MatchPattern::parse(source)?;
                parse_token(source, Token::ThickArrow)?;
                let block = Self::parse(source)?;

                let comma_next = matches!(source.peek_token(), Some(Token::Comma));
                if comma_next {
                    source.next();
                } else if !matches!(block.data, Self::Block(_)) {
                    trailing_delimeter = false;
                }

                arms.push((pattern, block.as_box()));
            } else {
                let span = source.next().unwrap()?.span;
                return Err(Error::new("Expected a comma.", span));
            }
        }

        let end = parse_token(source, Token::BraceClose)?.end;
        Ok(Chunk::new(
            Span { start, end },
            Self::Match {
                value,
                arms,
                else_arm,
            },
        ))
    }

    fn parse_struct_type(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordStruct)?.start;
        parse_token(source, Token::BraceOpen)?;

        let mut properties = Vec::new();
        let mut methods = Vec::new();
        let mut constants = Vec::new();

        let mut trailing_delimeter = true;

        loop {
            match source.peek_token() {
                Some(Token::Ident(_)) => {
                    let name = <&str>::parse(source)?;
                    if !trailing_delimeter {
                        return Err(Error::new("Expected a comma.", name.span));
                    }

                    parse_token(source, Token::Colon)?;
                    let value = Self::parse(source)?;
                    if let Some(Token::Comma) = source.peek_token() {
                        source.next();
                    } else {
                        trailing_delimeter = false;
                    }
                    properties.push((name, value));
                }

                Some(Token::KeywordFn) => {
                    methods.push(FunctionDefinition::parse(source)?);
                }

                Some(Token::KeywordConst) => {
                    constants.push(ConstantDefinition::parse(source)?);
                    parse_token(source, Token::Semicolon)?;
                }

                None | Some(Token::BraceClose) => break,
                Some(_) => {
                    let Some(Ok(Chunk { span, data: _ })) = source.next() else {
                        unreachable!()
                    };
                    return Err(Error::new("Expected a struct member.", span));
                }
            }
        }

        let end = parse_token(source, Token::BraceClose)?.end;
        Ok(Chunk::new(
            Span { start, end },
            Self::StructType {
                properties,
                methods,
                constants,
            },
        ))
    }

    fn parse_struct_initializer(
        source: &mut TokenStream<'a>,
        struct_type: Chunk<Box<Self>>,
    ) -> Result<Chunk<Self>> {
        parse_token(source, Token::BraceOpen)?;

        let mut properties = Vec::new();
        let mut trailing_delimeter = true;

        loop {
            match source.peek_token() {
                Some(Token::Period) => {
                    let dot = parse_token(source, Token::Period)?;
                    if !trailing_delimeter {
                        return Err(Error::new("Expected a comma.", dot));
                    }
                    let name = <&str>::parse(source)?;

                    parse_token(source, Token::Equals)?;
                    let value = Self::parse(source)?;
                    if let Some(Token::Comma) = source.peek_token() {
                        source.next();
                    } else {
                        trailing_delimeter = false;
                    }
                    properties.push((name, value));
                }
                None | Some(Token::BraceClose) => break,
                Some(_) => {
                    let Some(Ok(Chunk { span, data: _ })) = source.next() else {
                        unreachable!()
                    };
                    return Err(Error::new("Expected a struct member.", span));
                }
            }
        }

        let end = parse_token(source, Token::BraceClose)?.end;
        Ok(Chunk::new(
            Span {
                start: struct_type.span.start,
                end,
            },
            Self::StructInitializer {
                struct_type,
                properties,
            },
        ))
    }

    fn parse_enum_type(source: &mut TokenStream<'a>) -> Result<Chunk<Self>> {
        let start = parse_token(source, Token::KeywordEnum)?.start;

        parse_token(source, Token::BraceOpen)?;

        let mut variants = Vec::new();
        let mut methods = Vec::new();
        let mut constants = Vec::new();

        let mut trailing_delimeter = true;

        loop {
            match source.peek_token() {
                Some(Token::Ident(_)) => {
                    let name = <&str>::parse(source)?;
                    if !trailing_delimeter {
                        return Err(Error::new("Expected a comma.", name.span));
                    }

                    // TODO: inline struct
                    if let Some(Token::ParenOpen) = source.peek_token() {
                        source.next();
                        let value = Self::parse(source)?;
                        variants.push((name, Some(value)));
                        parse_token(source, Token::ParenClose)?;
                    } else {
                        variants.push((name, None))
                    }

                    if let Some(Token::Comma) = source.peek_token() {
                        source.next();
                    } else {
                        trailing_delimeter = false;
                    }
                }

                Some(Token::KeywordFn) => {
                    methods.push(FunctionDefinition::parse(source)?);
                }

                Some(Token::KeywordConst) => {
                    constants.push(ConstantDefinition::parse(source)?);
                    parse_token(source, Token::Semicolon)?;
                }

                None | Some(Token::BraceClose) => break,
                Some(_) => {
                    let Some(Ok(Chunk { span, data: _ })) = source.next() else {
                        unreachable!()
                    };
                    return Err(Error::new("Expected a struct member.", span));
                }
            }
        }

        let end = parse_token(source, Token::BraceClose)?.end;
        Ok(Chunk::new(
            Span { start, end },
            Self::EnumType {
                variants,
                methods,
                constants,
            },
        ))
    }

    pub(super) fn parse_single(
        source: &mut TokenStream<'a>,
        ctx: &ExpressionContext,
    ) -> Result<Chunk<Self>> {
        let mut expr = match source.peek_token() {
            Some(Token::KeywordNull) => {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };
                Chunk::new(span, Self::Null)
            }
            Some(Token::String(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::String(slice),
                })) = source.next()
                else {
                    unreachable!()
                };
                Chunk::new(span, Self::String(slice))
            }
            Some(Token::Int { .. }) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Int { slice, negative },
                })) = source.next()
                else {
                    unreachable!()
                };
                Chunk::new(span, Self::Int { slice, negative })
            }
            Some(Token::Float { .. }) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Float { slice, negative },
                })) = source.next()
                else {
                    unreachable!()
                };
                Chunk::new(span, Self::Float { slice, negative })
            }
            Some(Token::Boolean(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Boolean(v),
                })) = source.next()
                else {
                    unreachable!()
                };
                Chunk::new(span, Self::Boolean(v))
            }

            Some(Token::Ident(_)) => {
                let Some(Ok(Chunk {
                    span,
                    data: Token::Ident(name),
                })) = source.next()
                else {
                    unreachable!()
                };
                Chunk::new(span, Self::Ident(name))
            }

            Some(Token::KeywordSelf) => {
                let Some(Ok(Chunk { span, .. })) = source.next() else {
                    unreachable!()
                };
                Chunk::new(span, Self::IdentSelf)
            }

            Some(Token::ParenOpen) => {
                let Some(Ok(Chunk {
                    span: Span { start, end: _ },
                    ..
                })) = source.next()
                else {
                    unreachable!()
                };
                let expr = Self::parse(source)?;
                if let Some(Token::Comma) = source.peek_token() {
                    source.next();
                    let mut tuple = vec![expr];
                    tuple.append(&mut Self::parse_group(
                        source,
                        Token::Comma,
                        Token::ParenClose,
                    )?);
                    Chunk::new(
                        Span {
                            start,
                            end: *source.pos(),
                        },
                        Self::Tuple(tuple),
                    )
                } else {
                    let end = parse_token(source, Token::ParenClose)?.end;
                    Chunk::new(Span { start, end }, Self::Group(Box::new(expr.data)))
                }
            }
            Some(Token::BraceOpen) => {
                let Some(Ok(Chunk {
                    span: Span { start, end: _ },
                    data: _,
                })) = source.next()
                else {
                    unreachable!()
                };
                let block = Block::parse(source)?.data;
                let end = parse_token(source, Token::BraceClose)?.end;
                Chunk::new(Span { start, end }, Self::Block(block))
            }
            Some(Token::KeywordConst) => {
                let Some(Ok(Chunk {
                    span: Span { start, end: _ },
                    data: _,
                })) = source.next()
                else {
                    unreachable!()
                };
                parse_token(source, Token::BraceOpen)?;
                let block = Block::parse(source)?.data;
                let end = parse_token(source, Token::BraceClose)?.end;
                Chunk::new(Span { start, end }, Self::Block(block))
            }

            Some(Token::KeywordIf) => Self::parse_if(source)?,
            Some(Token::KeywordMatch) => Self::parse_match(source)?,
            Some(Token::KeywordStruct) => Self::parse_struct_type(source)?,
            Some(Token::KeywordEnum) => Self::parse_enum_type(source)?,

            Some(Token::Bang | Token::Minus | Token::Ampersand | Token::Asterisk) => {
                let Some(Ok(op)) = source.next() else {
                    unreachable!();
                };
                let x = Self::parse_single(source, ctx)?.as_box();
                Chunk::new(
                    Span {
                        start: op.span.start,
                        end: x.span.end,
                    },
                    Self::UnaryOp {
                        x,
                        op: Chunk::new(
                            op.span,
                            match op.data {
                                Token::Bang => UnaryOperation::Not,
                                Token::Minus => UnaryOperation::Negative,
                                Token::Ampersand => UnaryOperation::Pointer,
                                Token::Asterisk => UnaryOperation::Dereference,
                                _ => unreachable!(),
                            },
                        ),
                    },
                )
            }

            Some(Token::AtSign) => {
                let Some(Ok(Chunk {
                    data: _,
                    span: Span { start, .. },
                })) = source.next()
                else {
                    unreachable!();
                };
                let name = <&str>::parse(source)?;

                parse_token(source, Token::ParenOpen)?;
                let mut level: u32 = 1;
                let mut body = Vec::new();
                let end = loop {
                    match source.next() {
                        Some(Ok(t)) => {
                            match &t.data {
                                Token::ParenOpen => level += 1,
                                Token::ParenClose => {
                                    level -= 1;
                                    if level == 0 {
                                        break t.span.end;
                                    }
                                }
                                _ => (),
                            }
                            body.push(t);
                        }
                        Some(Err(e)) => return Err(e),
                        None => {
                            return Err(Error::new(
                                "Expected a closing parenthesis.",
                                Span::char(*source.pos()),
                            ));
                        }
                    };
                };

                Chunk::new(Span { start, end }, Self::BuiltIn { name, body })
            }

            Some(_) => {
                let Some(Ok(Chunk { span, data: _ })) = source.next() else {
                    unreachable!();
                };

                return Err(Error::new("Expected an expression.", span));
            }
            None => {
                return Err(Error::new(
                    "Expected an expression.",
                    Span::char(*source.pos()),
                ));
            }
        };

        loop {
            match source.peek_token() {
                Some(Token::Period) => {
                    source.next();
                    let name = <&str>::parse(source)?;

                    if let Some(Token::ParenOpen) = source.peek_token() {
                        source.next();
                        let arguments = Self::parse_group(source, Token::Comma, Token::ParenClose)?;

                        expr = Chunk::new(
                            Span {
                                start: expr.span.start,
                                end: *source.pos(),
                            },
                            Self::CallMethod {
                                object: expr.as_box(),
                                name,
                                arguments,
                            },
                        )
                    } else {
                        expr = Chunk::new(
                            Span {
                                start: expr.span.start,
                                end: name.span.end,
                            },
                            Self::Property(expr.as_box(), name),
                        );
                    }
                }
                Some(Token::ParenOpen) => {
                    source.next();
                    let arguments = Self::parse_group(source, Token::Comma, Token::ParenClose)?;

                    expr = Chunk::new(
                        Span {
                            start: expr.span.start,
                            end: *source.pos(),
                        },
                        Self::Call {
                            function: expr.as_box(),
                            arguments,
                        },
                    )
                }
                Some(Token::BraceOpen)
                    if !ctx.prevent_struct_initialization
                        && !matches!(expr.data, Self::Block(_)) =>
                {
                    expr = Self::parse_struct_initializer(source, expr.as_box())?;
                }
                _ => {
                    if !matches!(
                        source.peek_token(),
                        Some(
                            Token::Comma
                                | Token::Semicolon
                                | Token::BracketClose
                                | Token::ParenClose
                                | Token::BraceClose
                                | Token::PlusEquals
                                | Token::Plus
                                | Token::MinusEquals
                                | Token::Minus
                                | Token::AsteriskEquals
                                | Token::Asterisk
                                | Token::SlashEquals
                                | Token::Slash
                        )
                    ) && (matches!(expr.data, Self::String(_))
                        || matches!(source.peek_token(), Some(Token::String(_))))
                    {
                        let rhs = Self::parse_single(source, ctx)?.as_box();
                        let op_span = if matches!(expr.data, Self::String(_)) {
                            expr.span
                        } else {
                            rhs.span
                        };
                        expr = Chunk::new(
                            Span {
                                start: expr.span.start,
                                end: rhs.span.end,
                            },
                            Self::BinaryOp {
                                lhs: expr.as_box(),
                                rhs,
                                op: Chunk::new(op_span, Operation::Concat),
                            },
                        )
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(expr)
    }

    fn insert(lhs: Chunk<Box<Self>>, op: Chunk<Operation>, rhs: Chunk<Box<Self>>) -> Chunk<Self> {
        let span = Span {
            start: lhs.span.start,
            end: rhs.span.end,
        };

        match *lhs.data {
            Expression::BinaryOp {
                lhs: b_lhs,
                rhs: b_rhs,
                op: b_op,
            } if op.data <= b_op.data => Chunk::new(
                span,
                Expression::BinaryOp {
                    lhs: b_lhs,
                    rhs: Self::insert(b_rhs, op, rhs).as_box(),
                    op: b_op,
                },
            ),
            _ => Chunk::new(span, Expression::BinaryOp { lhs, rhs, op }),
        }
    }
}

impl<'a> Parse<'a> for Expression<'a> {
    type Context = ExpressionContext;

    fn parse_context(source: &mut TokenStream<'a>, ctx: Self::Context) -> Result<Chunk<Self>> {
        let mut expr = Self::parse_single(source, &ctx)?;

        loop {
            let Some(Ok(op)) = source.peek() else {
                break;
            };
            let op = Chunk::new(
                op.span,
                match op.data {
                    Token::Plus => Operation::Add,
                    Token::Minus => Operation::Subtract,
                    Token::Asterisk => Operation::Multiply,
                    Token::Slash => Operation::Divide,
                    Token::DoubleEquals => Operation::Equals,
                    Token::NotEquals => Operation::NotEquals,
                    Token::Gt => Operation::Gt,
                    Token::GtEquals => Operation::GtEqual,
                    Token::Lt => Operation::Lt,
                    Token::LtEquals => Operation::LtEqual,
                    Token::DoubleAmpersand => Operation::And,
                    Token::DoublePipe => Operation::Or,
                    _ => break,
                },
            );
            source.next();

            let rhs = Self::parse_single(source, &ctx)?;
            expr = Self::insert(expr.as_box(), op, rhs.as_box());
        }

        Ok(expr)
    }
}
