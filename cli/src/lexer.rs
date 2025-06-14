use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Pos, Span},
    token::Token,
};
use std::{
    collections::VecDeque,
    iter::Peekable,
    str::{Chars, Lines},
};

macro_rules! operator {
    ($token: ident, $span: expr) => {
        Some(Ok(Span::char($span).with(Token::$token)))
    };
}

macro_rules! double_operator {
    ($token: ident, $start: expr, $self: expr) => {{
        $self.next_char();
        Some(Ok(Span {
            start: $start,
            end: $self.pos,
        }
        .with(Token::$token)))
    }};
}

macro_rules! pat_ident {
    () => {
        'A'..='Z' | 'a'..='z' | '_' | '0'..='9'
    };
}

pub struct TokenStream<'a> {
    lines: Lines<'a>,
    current_line: Peekable<Chars<'a>>,
    done: bool,

    pos: Pos,
    next: VecDeque<Option<<Self as Iterator>::Item>>,
}

impl TokenStream<'_> {
    fn next_line(&mut self) -> bool {
        let Some(line) = self.lines.next() else {
            self.done = true;
            return false;
        };
        self.current_line = line.chars().peekable();
        self.pos.cr();
        true
    }

    fn next_char(&mut self) -> Option<char> {
        loop {
            if self.current_line.peek().is_some() {
                break;
            }

            if !self.next_line() {
                return None;
            }
        }

        self.pos.col += 1;
        self.current_line.next()
    }

    /// starts at 1
    pub fn peek_nth(&mut self, index: usize) -> Option<&<Self as Iterator>::Item> {
        while self.next.len() < index {
            let next = self.force_next();
            self.next.push_back(next);
        }

        #[allow(clippy::unwrap_used)]
        self.next[index - 1].as_ref()
    }
    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.peek_nth(1)
    }

    /// starts at 1
    pub fn peek_nth_token(&mut self, index: usize) -> Option<&Token> {
        self.peek_nth(index)
            .and_then(|r| r.as_ref().ok())
            .map(|s| &s.data)
    }
    pub fn peek_token(&mut self) -> Option<&Token> {
        self.peek_nth_token(1)
    }

    fn force_next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.done {
            return None;
        }

        let mut start = self.pos;
        let Some(char) = self.next_char() else {
            self.done = true;
            return None;
        };
        if self.pos.ln != start.ln {
            start = self.pos - Pos::new(0, 1);
        }

        match char {
            // Line comment
            '/' if self.current_line.peek() == Some(&'/') => {
                self.next_line();
                self.next()
            }
            // Block comment
            '/' if self.current_line.peek() == Some(&'*') => {
                loop {
                    let Some(char) = self.next_char() else {
                        self.done = true;
                        return Some(Err(Error::new(
                            Span {
                                start,
                                end: self.pos,
                            },
                            String::from("Missing trailing '*/' to close the block comment."),
                        )));
                    };

                    match char {
                        '*' if self.current_line.peek() == Some(&'/') => {
                            self.next_char();
                            break;
                        }
                        _ => (),
                    }
                }
                self.next()
            }

            '"' | '\'' => {
                let mut string = String::new();
                let term = char;
                let single = char == '\'';

                loop {
                    let Some(char) = self.next_char() else {
                        self.done = true;
                        return Some(Err(Error::new(
                            Span {
                                start,
                                end: self.pos,
                            },
                            format!("Missing trailing {type} quote ('{term}') to close string.", type = if single { "single" } else { "double" }),
                        )));
                    };

                    match char {
                        c if term == c => break,
                        '\\' => match self.next_char() {
                            None => {
                                continue;
                            }
                            Some(c) if term == c => string.push(c),
                            Some('n') => string.push('\n'),
                            Some('t') => string.push('\t'),
                            Some('r') => string.push('\r'),
                            Some('\\') => string.push('\\'),

                            Some(c) => {
                                return Some(Err(Error::new(
                                    Span::char(self.pos),
                                    format!("Unknown escape character {c}."),
                                )))
                            }
                        },
                        c => string.push(c),
                    }
                }
                Some(Ok(Span {
                    start,
                    end: self.pos,
                }
                .with(Token::String(string))))
            }

            '.' if self.current_line.peek().is_some_and(|c| c.is_ascii_digit()) => {
                let mut string = String::from(".");

                while self.current_line.peek().is_some_and(|c| c.is_ascii_digit()) {
                    #[allow(clippy::unwrap_used)]
                    string.push(self.next_char().unwrap());
                }

                Some(Ok(Span {
                    start,
                    end: self.pos,
                }
                .with(Token::Number(
                    string.parse().expect("Failed to construct number literal"),
                ))))
            }
            c if c.is_ascii_digit() => {
                let mut string = String::from(c);
                let mut decimal = false;

                while self.current_line.peek().is_some_and(|c| {
                    let c_is_decimal = *c == '.' && !decimal;
                    decimal |= c_is_decimal;
                    c.is_ascii_digit() || c_is_decimal
                }) {
                    #[allow(clippy::unwrap_used)]
                    string.push(self.next_char().unwrap());
                }

                Some(Ok(Span {
                    start,
                    end: self.pos,
                }
                .with(Token::Number(
                    string.parse().expect("Failed to construct number literal"),
                ))))
            }
            '-' if self.current_line.peek().is_some_and(|c| c.is_ascii_digit()) => {
                let mut string = String::from("-");
                let mut decimal = false;

                while self.current_line.peek().is_some_and(|c| {
                    let c_is_decimal = *c == 'n' && !decimal;
                    decimal = c_is_decimal;
                    c.is_ascii_digit() || c_is_decimal
                }) {
                    #[allow(clippy::unwrap_used)]
                    string.push(self.next_char().unwrap());
                }

                Some(Ok(Span {
                    start,
                    end: self.pos,
                }
                .with(Token::Number(
                    string.parse().expect("Failed to construct number literal"),
                ))))
            }

            pat_ident!() => {
                let mut string = String::from(char);

                while matches!(self.current_line.peek(), Some(pat_ident!())) {
                    #[allow(clippy::unwrap_used)]
                    string.push(self.next_char().unwrap());
                }

                let span = Span {
                    start,
                    end: self.pos,
                };
                match string.as_str() {
                    "use" => Some(Ok(span.with(Token::KeywordUse))),

                    "fn" => Some(Ok(span.with(Token::KeywordFunction))),
                    "mut" => Some(Ok(span.with(Token::KeywordMut))),
                    "let" => Some(Ok(span.with(Token::KeywordLet))),
                    "type" => Some(Ok(span.with(Token::KeywordType))),

                    "with" => Some(Ok(span.with(Token::KeywordWith))),
                    "ref" => Some(Ok(span.with(Token::KeywordRef))),

                    "if" => Some(Ok(span.with(Token::KeywordIf))),
                    "else" => Some(Ok(span.with(Token::KeywordElse))),
                    "while" => Some(Ok(span.with(Token::KeywordWhile))),
                    "for" => Some(Ok(span.with(Token::KeywordFor))),
                    "in" => Some(Ok(span.with(Token::KeywordIn))),
                    "match" => Some(Ok(span.with(Token::KeywordMatch))),

                    "true" => Some(Ok(span.with(Token::Boolean(true)))),
                    "false" => Some(Ok(span.with(Token::Boolean(false)))),

                    _ => Some(Ok(span.with(Token::Ident(string)))),
                }
            }

            // <=
            '<' if self.current_line.peek() == Some(&'=') => {
                double_operator!(LtEqual, start, self)
            }
            // >=
            '>' if self.current_line.peek() == Some(&'=') => {
                double_operator!(GtEqual, start, self)
            }
            // ==
            '=' if self.current_line.peek() == Some(&'=') => {
                double_operator!(DoubleEqual, start, self)
            }
            // !=
            '!' if self.current_line.peek() == Some(&'=') => {
                double_operator!(NotEqual, start, self)
            }
            // +=
            '+' if self.current_line.peek() == Some(&'=') => {
                double_operator!(AddAssign, start, self)
            }
            // -=
            '-' if self.current_line.peek() == Some(&'=') => {
                double_operator!(SubAssign, start, self)
            }
            // *=
            '*' if self.current_line.peek() == Some(&'=') => {
                double_operator!(MulAssign, start, self)
            }
            // /=
            '/' if self.current_line.peek() == Some(&'=') => {
                double_operator!(DivAssign, start, self)
            }
            // &&
            '&' if self.current_line.peek() == Some(&'&') => {
                double_operator!(BooleanAnd, start, self)
            }
            // ||
            '|' if self.current_line.peek() == Some(&'|') => {
                double_operator!(BooleanOr, start, self)
            }
            // ->
            '-' if self.current_line.peek() == Some(&'>') => {
                double_operator!(Arrow, start, self)
            }

            ';' => operator!(Semicolon, self.pos),
            ':' => operator!(Colon, self.pos),
            ',' => operator!(Comma, self.pos),
            '.' => operator!(Dot, self.pos),
            '#' => operator!(Hash, self.pos),
            '|' => operator!(Pipe, self.pos),
            '=' => operator!(Equals, self.pos),
            '+' => operator!(Add, self.pos),
            '-' => operator!(Sub, self.pos),
            '*' => operator!(Mul, self.pos),
            '/' => operator!(Div, self.pos),
            '!' => operator!(Exclamation, self.pos),
            '^' => operator!(Caret, self.pos),
            '<' => operator!(Lt, self.pos),
            '>' => operator!(Gt, self.pos),
            '(' => operator!(ParenOpen, self.pos),
            ')' => operator!(ParenClose, self.pos),
            '[' => operator!(BracketOpen, self.pos),
            ']' => operator!(BracketClose, self.pos),
            '{' => operator!(BraceOpen, self.pos),
            '}' => operator!(BraceClose, self.pos),

            ' ' | '\t' => self.next(),
            _ => Some(Err(Error::new(
                Span::char(self.pos),
                format!("I don't know how to handle this character: '{char}'."),
            ))),
        }
    }

    pub fn pos(&self) -> Pos {
        self.pos
    }
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(source: &'a str) -> Self {
        let mut lines = source.lines();
        let Some(line) = lines.next() else {
            return Self {
                lines,
                current_line: "".chars().peekable(),
                done: true,
                pos: Pos::new(1, 1),
                next: VecDeque::new(),
            };
        };
        let current_line = line.chars().peekable();

        TokenStream {
            lines,
            current_line,
            done: source.is_empty(),

            pos: Pos::new(1, 1),
            next: VecDeque::new(),
        }
    }
}

impl Iterator for TokenStream<'_> {
    type Item = Result<Chunk<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.pop_front() {
            next
        } else {
            self.force_next()
        }
    }
}
