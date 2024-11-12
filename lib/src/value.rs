use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Key {
    Named(String),
    Index(usize),
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Table(HashMap<Key, Value>),
    Unit,
}

impl Value {
    pub fn is_unit(&self) -> bool {
        match self {
            Self::Table(map) => map.is_empty(),
            Self::Unit => true,
            _ => false,
        }
    }
}
