use core::panic;
use hashbrown::{Equivalent, HashMap};
use std::{
    cell::RefCell,
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    rc::Rc,
};

use crate::{
    error::Result,
    types::{GenericType, IntoType, Primitive, Type},
};

pub mod function;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Comparison {
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

// Order for comparisons doesn't matter.
impl PartialOrd for Comparison {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Comparison {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Key {
    Index(usize),
    Named(String),
}

impl Key {
    pub fn is_idx(&self) -> bool {
        matches!(self, Key::Index(_))
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Index(i) => write!(f, "{i}"),
            Self::Named(s) => write!(f, "{s}"),
        }
    }
}

impl Equivalent<Key> for str {
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

impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Index(x) => x.hash(state),
            Self::Named(x) => x.hash(state),
        }
    }
}

pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Table(HashMap<Key, Rc<RefCell<Value>>>),
    Function(Rc<dyn Call>),
    Reference(Rc<RefCell<Value>>),
    ImplicitRef(Rc<RefCell<Value>>),
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

    pub fn deref_implicit(self) -> Self {
        match self {
            Self::ImplicitRef(rc) => rc.borrow().clone().deref_implicit(),
            _ => self,
        }
    }

    pub fn fmt_table(
        map: &HashMap<Key, Rc<RefCell<Value>>>,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        f.write_str("[\n")?;
        let mut i = 0;
        while map.contains_key(&i) {
            if i > 0 {
                f.write_str(",\n")?;
            }
            write!(f, "{}", map[&i].borrow())?;

            i += 1;
        }

        for (k, v) in map.iter() {
            if let Some(key) = match k {
                Key::Named(k) => Some(k.to_string()),
                Key::Index(k) if k > &i => Some(k.to_string()),
                _ => None,
            } {
                if i > 0 {
                    f.write_str(",\n")?;
                }
                write!(f, "\t{key} = {}", v.borrow())?;

                i += 1;
            }
        }
        f.write_str("\n]")?;
        Ok(())
    }

    fn fmt_func(c: &Rc<dyn Call>, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("fn(")?;
        for param in c.parameters() {
            write!(f, "{param}")?
        }
        f.write_str(")")?;
        let return_ty = c.return_ty();
        match return_ty {
            Type::Unit => (),
            Type::Union(_) => write!(f, " -> ({return_ty})")?,
            _ => write!(f, " -> {return_ty}")?,
        }
        Ok(())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Table(a), Self::Table(b)) => a == b,
            (Self::Unit, a) | (a, Self::Unit) => a.is_unit(),
            (Self::Function(a), Self::Function(b)) => Rc::ptr_eq(a, b),
            (Self::Reference(a), Self::Reference(b)) => Rc::ptr_eq(a, b),

            (Self::ImplicitRef(rc), b) | (b, Self::ImplicitRef(rc)) => &*rc.borrow() == b,
            (_, _) => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(str) => write!(f, "\"{str}\""),
            Self::Number(n) => write!(f, "{n}"),
            Self::Boolean(b) => f.write_str(if *b { "true" } else { "false" }),
            Self::Table(map) => Self::fmt_table(map, f),
            Self::Function(c) => Self::fmt_func(c, f),
            Self::Reference(v) => write!(f, "ref {}", v.borrow()),
            Self::ImplicitRef(v) => write!(f, "&{}", v.borrow()),
            Self::Unit => write!(f, "[]"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::String(s) => Self::String(s.clone()),
            Self::Number(n) => Self::Number(*n),
            Self::Boolean(b) => Self::Boolean(*b),
            Self::Table(map) => Self::Table(
                map.iter()
                    .map(|(k, v)| (k.clone(), Rc::new(RefCell::new(v.borrow().clone()))))
                    .collect(),
            ),
            Self::Function(rc) => Self::Function(rc.clone()),
            Self::Reference(rc) => Self::Reference(rc.clone()),
            Self::ImplicitRef(rc) => Self::ImplicitRef(rc.clone()),
            Self::Unit => Self::Unit,
        }
    }
}

impl From<()> for Value {
    fn from(_val: ()) -> Self {
        Value::Unit
    }
}
impl From<Value> for () {
    fn from(_val: Value) -> Self {}
}
impl From<String> for Value {
    fn from(val: String) -> Self {
        Value::String(val)
    }
}
impl From<Value> for String {
    fn from(val: Value) -> Self {
        match val {
            Value::String(n) => n,
            _ => panic!("Expected a string to move across a language barrier."),
        }
    }
}
impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::Number(val)
    }
}
impl From<Value> for f64 {
    fn from(val: Value) -> Self {
        match val {
            Value::Number(n) => n,
            _ => panic!("Expected a number to move across a language barrier."),
        }
    }
}
impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Value::Boolean(val)
    }
}
impl From<Value> for bool {
    fn from(val: Value) -> Self {
        match val {
            Value::Boolean(n) => n,
            _ => panic!("Expected a boolean to move across a language barrier."),
        }
    }
}
impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(val: Vec<T>) -> Self {
        Value::Table(
            (0..val.len())
                .zip(val)
                .map(|(k, v)| (Key::Index(k), Rc::new(RefCell::new(v.into()))))
                .collect(),
        )
    }
}
impl<T: From<Value>> From<Value> for Vec<T> {
    fn from(val: Value) -> Self {
        match val {
            Value::Table(t) => {
                let mut i = 0;
                let mut vec = Vec::new();
                while t.contains_key(&i) {
                    vec.push(t[&i].borrow().clone().into());
                    i += 1;
                }

                vec
            }
            _ => panic!("Expected a table to move across a language barrier."),
        }
    }
}
impl<T: Into<Value>> From<HashMap<Key, T>> for Value {
    fn from(val: HashMap<Key, T>) -> Self {
        Value::Table(
            val.into_iter()
                .map(|(k, v)| (k, Rc::new(RefCell::new(v.into()))))
                .collect(),
        )
    }
}
impl<T: From<Value>> From<Value> for HashMap<Key, T> {
    fn from(val: Value) -> Self {
        match val {
            Value::Table(t) => {
                let mut hash_map = HashMap::new();
                for (k, v) in t {
                    hash_map.insert(k, v.borrow().clone().into());
                }

                hash_map
            }
            _ => panic!("Expected a table to move across a language barrier."),
        }
    }
}
impl<T: Into<Value>> From<Option<T>> for Value {
    fn from(val: Option<T>) -> Self {
        if let Some(val) = val {
            val.into()
        } else {
            Value::Unit
        }
    }
}
impl<T: From<Value> + IntoType> From<Value> for Option<T> {
    fn from(val: Value) -> Self {
        match val {
            Value::Unit => None,
            Value::Table(t) if t.is_empty() => None,
            _ => Some(val.into()),
        }
    }
}
impl From<Primitive> for Value {
    fn from(value: Primitive) -> Self {
        match value {
            Primitive::String(str) => Self::String(str),
            Primitive::Number(num) => Self::Number(num),
            Primitive::Boolean(bool) => Self::Boolean(bool),
        }
    }
}
impl From<Value> for Primitive {
    fn from(value: Value) -> Self {
        match value {
            Value::String(str) => Self::String(str),
            Value::Number(num) => Self::Number(num),
            Value::Boolean(bool) => Self::Boolean(bool),
            _ => panic!("Expected a primitive to move across a language barrier."),
        }
    }
}
impl<const ID: usize> From<Value> for GenericType<ID> {
    fn from(value: Value) -> Self {
        Self { value }
    }
}
impl<const ID: usize> From<GenericType<ID>> for Value {
    fn from(value: GenericType<ID>) -> Self {
        value.value
    }
}

pub trait Call {
    fn call(&self, args: Vec<Value>) -> Result<Value>;
    fn parameters(&self) -> &Vec<Type>;
    fn return_ty(&self) -> &Type;
}
