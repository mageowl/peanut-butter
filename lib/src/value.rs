use hashbrown::{Equivalent, HashMap};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Key {
    Named(String),
    Index(usize),
}

impl Equivalent<Key> for String {
    fn equivalent(&self, key: &Key) -> bool {
        match key {
            Key::Named(name) => name == self,
            Key::Index(_) => false,
        }
    }
}

impl Equivalent<Key> for usize {
    fn equivalent(&self, key: &Key) -> bool {
        match key {
            Key::Index(idx) => idx == self,
            Key::Named(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Table(HashMap<Key, Rc<RefCell<Value>>>),
    Reference(Rc<RefCell<Value>>),
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
