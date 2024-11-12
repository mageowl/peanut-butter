use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Add, AddAssign, Sub, SubAssign},
    slice::ChunkBy,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub ln: usize,
    pub col: usize,
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.ln.cmp(&other.ln) {
            Ordering::Equal => self.col.cmp(&other.col),
            ord => ord,
        }
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{ln}:{col}", ln = self.ln, col = self.col)
    }
}

impl Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Pos {
    pub fn new(ln: usize, col: usize) -> Self {
        Self { ln, col }
    }

    /// Move to the start of the next line
    pub fn cr(&mut self) {
        self.ln += 1;
        self.col = 1;
    }
}

impl Add for Pos {
    type Output = Self;

    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Pos {
    fn add_assign(&mut self, rhs: Self) {
        self.col += rhs.col;
        self.ln += rhs.ln;
    }
}

impl Sub for Pos {
    type Output = Self;

    fn sub(mut self, rhs: Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl SubAssign for Pos {
    fn sub_assign(&mut self, rhs: Self) {
        self.col -= rhs.col;
        self.ln -= rhs.ln;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn char(pos: Pos) -> Self {
        Self {
            start: pos - Pos::new(0, 1),
            end: pos,
        }
    }

    pub fn with<T>(self, data: T) -> Chunk<T> {
        Chunk { span: self, data }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{start}..{end}", start = self.start, end = self.end)
    }
}

#[derive(Debug)]
pub struct Chunk<T> {
    pub span: Span,
    pub data: T,
}

impl<T> Chunk<T> {
    pub fn new(span: Span, data: T) -> Self {
        Self { span, data }
    }

    pub fn as_box(self) -> Chunk<Box<T>> {
        Chunk {
            span: self.span,
            data: Box::new(self.data),
        }
    }
    pub fn as_ref(&self) -> Chunk<&T> {
        Chunk {
            span: self.span,
            data: &self.data,
        }
    }
}

impl<T: Display> Display for Chunk<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{span}] {data}", span = self.span, data = self.data)
    }
}

impl<T: Hash> Hash for Chunk<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Chunk<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data && self.span == other.span
    }
}

impl<T: Eq> Eq for Chunk<T> {}

impl<T: Clone> Clone for Chunk<T> {
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            data: self.data.clone(),
        }
    }
}

impl<T: Copy> Copy for Chunk<T> {}
