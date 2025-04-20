use std::{collections::VecDeque, f64, iter::Peekable, str::CharIndices};

use lib::{
    error::{Error, Result},
    span::{Chunk, Pos, Span},
    token::Token,
};

pub struct TokenStream<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    queue: VecDeque<Option<<Self as Iterator>::Item>>,

    done: bool,
    pos: Pos,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            queue: VecDeque::new(),

            done: source.is_empty(),
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
        if self.done {
            return None;
        }

        let Some(char) = self.next_char() else {
            self.done = true;
            return None;
        };
        let start = self.pos.prev();

        macro_rules! ident_pattern {
            () => {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_'
            };
        }
        macro_rules! double_operator {
            ($token: ident, $pos: expr) => {
                Some(Ok(Chunk::new(
                    Token::$token,
                    Span {
                        start: $pos.prev(),
                        end: $pos,
                    },
                )))
            };
        }
        macro_rules! operator {
            ($token: ident, $pos: expr) => {
                Some(Ok(Chunk::new(Token::$token, Span::char($pos))))
            };
        }

        match char.1 {
            '/' if matches!(self.chars.peek(), Some((_, '/'))) => {
                loop {
                    if self.chars.next()?.1 == '\n' {
                        break;
                    }
                }
                self.force_next()
            }
            '/' if matches!(self.chars.peek(), Some((_, '/'))) => {
                self.chars.next();
                loop {
                    let Some(char) = self.next_char() else {
                        self.done = true;
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
                for (i, c) in self.chars.by_ref() {
                    self.pos.col += 1;
                    if c == '"' {
                        return Some(Ok(Chunk::new(
                            Token::String(&self.source[idx_start..i]),
                            Span {
                                start,
                                end: self.pos,
                            },
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
                        Token::Float(self.source[char.0..idx_end].parse().unwrap_or_default()),
                        Span {
                            start,
                            end: self.pos,
                        },
                    )))
                } else {
                    Some(Ok(Chunk::new(
                        Token::Uint(self.source[char.0..idx_end].parse().unwrap_or_default()),
                        Span {
                            start,
                            end: self.pos,
                        },
                    )))
                }
            }
            '-' if self.chars.peek().is_some_and(|(_, c)| c.is_ascii_digit()) => {
                let (float, idx_end) = self.next_number();
                if float {
                    Some(Ok(Chunk::new(
                        Token::Float(
                            -self.source[char.0..idx_end]
                                .parse::<f64>()
                                .unwrap_or_default(),
                        ),
                        Span {
                            start,
                            end: self.pos,
                        },
                    )))
                } else {
                    Some(Ok(Chunk::new(
                        Token::Int(
                            -self.source[char.0..idx_end]
                                .parse::<i64>()
                                .unwrap_or_default(),
                        ),
                        Span {
                            start,
                            end: self.pos,
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
                    match &self.source[char.0..idx_end] {
                        "fn" => Token::KeywordFn,
                        "let" => Token::KeywordLet,
                        "mut" => Token::KeywordMut,
                        "type" => Token::KeywordType,
                        "if" => Token::KeywordIf,
                        "else" => Token::KeywordElse,
                        "while" => Token::KeywordWhile,
                        "for" => Token::KeywordFor,
                        "in" => Token::KeywordIn,
                        "match" => Token::KeywordMatch,

                        ident => Token::Ident(ident),
                    },
                    Span {
                        start,
                        end: self.pos,
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

            ';' => operator!(Semicolon, self.pos),
            ':' => operator!(Colon, self.pos),
            ',' => operator!(Comma, self.pos),
            '.' => operator!(Period, self.pos),
            '#' => operator!(Hash, self.pos),
            '|' => operator!(Pipe, self.pos),
            '=' => operator!(Equals, self.pos),
            '+' => operator!(Plus, self.pos),
            '-' => operator!(Minus, self.pos),
            '*' => operator!(Asterisk, self.pos),
            '/' => operator!(Slash, self.pos),
            '!' => operator!(Bang, self.pos),
            //'^' => operator!(Caret, self.pos),
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

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.queue.is_empty() {
            let item = self.force_next();
            self.queue.push_back(item);
        }
        self.queue.get(0)?.as_ref()
    }
    pub fn peek_token(&mut self) -> Option<&Token<'a>> {
        self.peek().and_then(|r| r.as_ref().ok().map(|c| &c.data))
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
