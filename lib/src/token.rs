#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    /// A keyword or variable name: if, my_var
    Ident(String),

    /// Number literal: 2.0, 8.5, .62
    /// All numbers are stored as floats.
    Number(f64),
    /// String literal: "hello world", 'Owen L', "{\"method\":\"GET\"}"
    String(String),
    /// Boolean literal: true or false
    Boolean(bool),

    Semicolon,
    Colon,
    Comma,
    Dot,
    Hash,
    Pipe,
    Equals,
    Add,
    AddAssign,
    Sub,
    SubAssign,
    Mul,
    MulAssign,
    Div,
    DivAssign,
    Exclamation,
    Caret,
    Lt,
    Gt,
    LtEqual,
    GtEqual,
    DoubleEqual,
    NotEqual,
    BooleanAnd,
    BooleanOr,
    Arrow,

    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,

    KeywordFunction,
    KeywordMut,
    KeywordLet,
    KeywordType,
    KeywordWith,
    KeywordUse,
    KeywordIf,
    KeywordElse,
    KeywordRef,
    KeywordAs,
    KeywordWhile,
}
