use std::fmt::Debug;

#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct Pos {
    pub col: usize,
    pub ln: usize,
}

impl Pos {
    pub fn new(ln: usize, col: usize) -> Self {
        Self { ln, col }
    }
    pub fn prev(self) -> Self {
        let Self { ln, col } = self;
        Self {
            ln,
            col: col.saturating_sub(1),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn char(pos: Pos) -> Span {
        Self {
            start: pos.prev(),
            end: pos,
        }
    }
}

pub struct Chunk<T> {
    pub data: T,
    pub span: Span,
}

impl<T: Clone> Clone for Chunk<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            span: self.span,
        }
    }
}
impl<T: Copy> Copy for Chunk<T> {}
impl<T: Debug> Debug for Chunk<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

impl<T> Chunk<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }
    pub fn map<R>(self, f: impl FnOnce(T) -> R) -> Chunk<R> {
        Chunk {
            data: f(self.data),
            span: self.span,
        }
    }
}
