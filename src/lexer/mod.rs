use std::{collections::VecDeque, iter::Peekable, str::CharIndices};

use token::Token;

use crate::{
    error::{Error, Result},
    span::{Chunk, Pos, Span},
};

pub mod token;

pub struct TokenStream<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    queue: VecDeque<Option<<Self as Iterator>::Item>>,
    pos: Pos,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            queue: VecDeque::new(),
            pos: Pos::default(),
        }
    }
}

impl<'a> TokenStream<'a> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        let mut c: (usize, char);
        loop {
            self.pos.col += 1;
            c = self.chars.next()?;
            if !c.1.is_whitespace() {
                break;
            } else if c.1 == '\n' {
                self.pos.col = 0;
                self.pos.ln += 1;
            }
        }
        Some(c)
    }

    fn next_number(&mut self) -> (bool, usize) {
        let mut idx_end = usize::MAX;
        let mut float = false;
        while let Some((i, c)) = self.chars.peek() {
            if !float && *c == '.' {
                float = true;
            } else if !c.is_ascii_digit() {
                idx_end = *i;
                break;
            }
            self.chars.next();
            self.pos.col += 1;
        }
        (float, idx_end)
    }

    fn force_next(&mut self) -> Option<<Self as Iterator>::Item> {
        let Some(char) = self.next_char() else {
            return None;
        };
        let start = self.pos.prev();

        macro_rules! ident_pattern {
            () => {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_'
            };
        }
        macro_rules! double_operator {
            ($token: ident, $pos: expr) => {{
                self.chars.next();
                Some(Ok(Chunk::new(
                    Span {
                        start: $pos.prev().prev(),
                        end: $pos,
                    },
                    Token::$token,
                )))
            }};
        }
        macro_rules! operator {
            ($token: ident, $pos: expr) => {
                Some(Ok(Chunk::new(Span::char($pos), Token::$token)))
            };
        }

        match char.1 {
            '/' if matches!(self.chars.peek(), Some((_, '/'))) => {
                loop {
                    if self.chars.next()?.1 == '\n' {
                        break;
                    }
                }
                self.pos.ln += 1;
                self.force_next()
            }
            '/' if matches!(self.chars.peek(), Some((_, '/'))) => {
                self.chars.next();
                loop {
                    let Some(char) = self.next_char() else {
                        return Some(Err(Error::new(
                            "Missing trailing '*/' to close block comment.",
                            Span {
                                start,
                                end: self.pos,
                            },
                        )));
                    };
                    match char.1 {
                        '*' if matches!(self.chars.peek(), Some((_, '/'))) => {
                            self.next_char();
                            break;
                        }
                        _ => (),
                    }
                }
                self.force_next()
            }

            '"' => {
                let Some((idx_start, _)) = self.chars.next() else {
                    return Some(Err(Error::new(
                        "Missing trailing quote '\"' to close string.",
                        Span {
                            start,
                            end: self.pos,
                        },
                    )));
                };
                self.pos.col += 1;
                for (i, c) in self.chars.by_ref() {
                    self.pos.col += 1;
                    if c == '"' {
                        return Some(Ok(Chunk::new(
                            Span {
                                start,
                                end: self.pos,
                            },
                            Token::String(&self.source[idx_start..i]),
                        )));
                    } else if c == '\n' {
                        self.pos.col = 0;
                        self.pos.ln += 1;
                    }
                }
                return Some(Err(Error::new(
                    "Missing trailing quote '\"' to close string.",
                    Span {
                        start,
                        end: self.pos,
                    },
                )));
            }

            c if c.is_ascii_digit() => {
                let (float, idx_end) = self.next_number();
                if float {
                    Some(Ok(Chunk::new(
                        Span {
                            start,
                            end: self.pos,
                        },
                        Token::Float {
                            slice: &self.source[char.0..idx_end],
                            negative: false,
                        },
                    )))
                } else {
                    Some(Ok(Chunk::new(
                        Span {
                            start,
                            end: self.pos,
                        },
                        Token::Int {
                            slice: &self.source[char.0..idx_end],
                            negative: false,
                        },
                    )))
                }
            }
            '-' if self.chars.peek().is_some_and(|(_, c)| c.is_ascii_digit()) => {
                let (float, idx_end) = self.next_number();
                if float {
                    Some(Ok(Chunk::new(
                        Span {
                            start,
                            end: self.pos,
                        },
                        Token::Float {
                            slice: &self.source[char.0..idx_end],
                            negative: true,
                        },
                    )))
                } else {
                    Some(Ok(Chunk::new(
                        Span {
                            start,
                            end: self.pos,
                        },
                        Token::Int {
                            slice: &self.source[char.0..idx_end],
                            negative: true,
                        },
                    )))
                }
            }

            ident_pattern!() => {
                let mut idx_end = usize::MAX;
                while let Some((i, c)) = self.chars.peek() {
                    if !matches!(c, ident_pattern!()) {
                        idx_end = *i;
                        break;
                    }
                    self.chars.next();
                    self.pos.col += 1;
                }
                Some(Ok(Chunk::new(
                    Span {
                        start,
                        end: self.pos,
                    },
                    match &self.source[char.0..idx_end] {
                        "var" => Token::KeywordVar,
                        "const" => Token::KeywordConst,
                        "fn" => Token::KeywordFn,
                        "self" => Token::KeywordSelf,
                        "match" => Token::KeywordMatch,
                        "if" => Token::KeywordIf,
                        "else" => Token::KeywordElse,
                        "while" => Token::KeywordWhile,
                        "for" => Token::KeywordFor,
                        "in" => Token::KeywordIn,
                        "loop" => Token::KeywordLoop,
                        "return" => Token::KeywordReturn,
                        "break" => Token::KeywordBreak,

                        "struct" => Token::KeywordStruct,
                        "enum" => Token::KeywordEnum,

                        "true" => Token::Boolean(true),
                        "false" => Token::Boolean(false),
                        "null" => Token::KeywordNull,

                        ident => Token::Ident(ident),
                    },
                )))
            }

            '<' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(LtEquals, self.pos)
            }
            '>' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(GtEquals, self.pos)
            }
            '=' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(DoubleEquals, self.pos)
            }
            '!' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(NotEquals, self.pos)
            }
            '+' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(PlusEquals, self.pos)
            }
            '-' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(MinusEquals, self.pos)
            }
            '*' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(AsteriskEquals, self.pos)
            }
            '/' if matches!(self.chars.peek(), Some((_, '='))) => {
                double_operator!(SlashEquals, self.pos)
            }
            '&' if matches!(self.chars.peek(), Some((_, '&'))) => {
                double_operator!(DoubleAmpersand, self.pos)
            }
            '|' if matches!(self.chars.peek(), Some((_, '|'))) => {
                double_operator!(DoublePipe, self.pos)
            }
            '-' if matches!(self.chars.peek(), Some((_, '>'))) => {
                double_operator!(Arrow, self.pos)
            }
            '=' if matches!(self.chars.peek(), Some((_, '>'))) => {
                double_operator!(ThickArrow, self.pos)
            }
            '.' if matches!(self.chars.peek(), Some((_, '.'))) => {
                double_operator!(DoublePeriod, self.pos)
            }

            ';' => operator!(Semicolon, self.pos),
            ':' => operator!(Colon, self.pos),
            ',' => operator!(Comma, self.pos),
            '.' => operator!(Period, self.pos),
            '=' => operator!(Equals, self.pos),
            '+' => operator!(Plus, self.pos),
            '-' => operator!(Minus, self.pos),
            '*' => operator!(Asterisk, self.pos),
            '/' => operator!(Slash, self.pos),
            '!' => operator!(Bang, self.pos),
            '?' => operator!(Question, self.pos),
            '&' => operator!(Ampersand, self.pos),
            '@' => operator!(AtSign, self.pos),
            '<' => operator!(Lt, self.pos),
            '>' => operator!(Gt, self.pos),
            '(' => operator!(ParenOpen, self.pos),
            ')' => operator!(ParenClose, self.pos),
            '[' => operator!(BracketOpen, self.pos),
            ']' => operator!(BracketClose, self.pos),
            '{' => operator!(BraceOpen, self.pos),
            '}' => operator!(BraceClose, self.pos),

            _ => Some(Err(Error::new(
                format!("Unexpected character '{char}'", char = char.1),
                Span {
                    start,
                    end: self.pos,
                },
            ))),
        }
    }

    pub fn peek_nth(&mut self, n: usize) -> Option<&<Self as Iterator>::Item> {
        while self.queue.len() < n + 1 {
            let item = self.force_next();
            self.queue.push_back(item);
        }
        self.queue.get(n)?.as_ref()
    }
    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.peek_nth(0)
    }
    pub fn peek_nth_token(&mut self, n: usize) -> Option<&Token<'a>> {
        self.peek_nth(n)
            .and_then(|r| r.as_ref().ok().map(|c| &c.data))
    }
    pub fn peek_token(&mut self) -> Option<&Token<'a>> {
        self.peek_nth_token(0)
    }

    pub fn pos(&self) -> &Pos {
        &self.pos
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Chunk<Token<'a>>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.queue.pop_front() {
            item
        } else {
            self.force_next()
        }
    }
}
