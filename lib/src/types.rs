use std::fmt::{Display, Formatter};

use hashbrown::HashMap;

use crate::value::{Key, Value};

pub mod partial;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    String,
    Number,
    Boolean,
    Table(HashMap<Key, Type>),
    Fn {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Ref(Box<Type>),
    Union(Vec<Type>),
    List(Box<Type>),
    Map(Box<Type>),
}

impl Type {
    pub fn simple_name(&self) -> &'static str {
        match self {
            Type::Unit => "unit",
            Type::String => "string",
            Type::Number => "number",
            Type::Boolean => "boolean",
            Type::Table(map) if map.is_empty() => "unit",
            Type::Table(_) => "table",
            Type::Fn { .. } => "function",
            Type::Ref(_) => "reference",
            Type::Union(_) => "union",
            Type::List(_) => "list",
            Type::Map(_) => "map",
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Type::Unit => true,
            Type::Table(map) => map.is_empty(),
            _ => false,
        }
    }
    pub fn is_union(&self) -> bool {
        matches!(self, Type::Union(_))
    }
    pub fn is_list(&self, item_type: &Type) -> bool {
        match self {
            Type::List(t1) => *item_type == **t1,
            Type::Table(map) => map
                .iter()
                .all(|(k, v)| item_type.matches(v) && matches!(k, Key::Index(_))),
            _ => false,
        }
    }
    pub fn is_map(&self, item_type: &Type) -> bool {
        match self {
            Type::Map(t1) => *item_type == **t1,
            Type::Table(map) => map.iter().all(|(_, v)| v == item_type),
            _ => false,
        }
    }

    /// Returns true if `self` is "less-than or equal to" `other`.
    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String, Self::String) => true,
            (Self::Number, Self::Number) => true,
            (Self::Boolean, Self::Boolean) => true,

            (Self::Unit, Self::Unit | Self::Table(_) | Self::List(_) | Self::Map(_)) => true,
            (Self::Table(m1), Self::Table(m2)) => m1
                .iter()
                .all(|(k, t)| m2.contains_key(k) && t.matches(&m2[k])),

            (
                Self::Fn {
                    parameters: p1,
                    return_type: r1,
                },
                Self::Fn {
                    parameters: p2,
                    return_type: r2,
                },
            ) => {
                p1.len() == p2.len()
                    && p1.iter().zip(p2).all(|(a, b)| a.matches(b))
                    && r1.matches(r2)
            }

            (Self::Ref(t1), Self::Ref(t2)) => t1.matches(t2),

            (Self::List(a), b) | (b, Self::List(a)) => b.is_list(a),
            (Self::Map(a), b) | (b, Self::Map(a)) => b.is_map(a),

            (Self::Union(a), Self::Union(b)) => b.iter().all(|b| a.iter().any(|a| a.matches(b))),
            (Self::Union(a), b) => a.iter().any(|a| a.matches(b)),

            _ => false,
        }
    }

    pub fn matches_val(&self, other: &Value) -> bool {
        match (self, other) {
            (_, Value::ImplicitRef(rc)) => self.matches_val(&rc.borrow()),

            (Self::String, Value::String(_)) => true,
            (Self::Number, Value::Number(_)) => true,
            (Self::Boolean, Value::Boolean(_)) => true,

            (a, Value::Unit) if a.is_unit() => true,
            (a, Value::Table(_)) if a.is_unit() => true,
            (Self::Table(a), Value::Table(b)) => a
                .iter()
                .all(|(k, t)| b.contains_key(k) && t.matches_val(&b[k].borrow())),
            (
                Self::Fn {
                    parameters: p1,
                    return_type: r1,
                },
                Value::Function(c),
            ) => {
                let p2 = c.parameters();
                if !(p1.len() == p2.len() && p1.iter().zip(p2).all(|(p1, p2)| p1.matches(p2))) {
                    return false;
                }

                let r2 = c.return_ty();
                r1.matches(r2)
            }

            (Self::Ref(t), Value::Reference(rc)) => t.matches_val(&rc.borrow()),

            (Self::List(a), Value::Table(b)) => b.iter().all(|(k, v)| {
                if matches!(k, Key::Index(_)) {
                    a.matches_val(&v.borrow())
                } else {
                    true
                }
            }),
            (Self::Map(a), Value::Table(b)) => b.iter().all(|(_, v)| a.matches_val(&v.borrow())),
            (Self::Union(a), b) => a.iter().any(|a| a.matches_val(b)),

            _ => false,
        }
    }

    fn fmt_table(map: &HashMap<Key, Type>, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("[\n")?;
        let mut i = 0;
        while map.contains_key(&i) {
            if i > 0 {
                f.write_str(",\n")?;
            }
            write!(f, "    {}", map[&i])?;

            i += 1;
        }

        let mut pairs = map.iter().collect::<Vec<_>>();
        pairs.sort_by(|(a, _), (b, _)| a.cmp(b));

        for (k, v) in pairs {
            if let Some(key) = match k {
                Key::Named(k) => Some(k.to_string()),
                Key::Index(k) if k > &i => Some(k.to_string()),
                _ => None,
            } {
                if i > 0 {
                    f.write_str(",\n")?;
                }
                write!(f, "    {key}: {}", v)?;

                i += 1;
            }
        }
        f.write_str("\n]")?;
        Ok(())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String, Self::String) => true,
            (Self::Number, Self::Number) => true,
            (Self::Boolean, Self::Boolean) => true,
            (a, b) if a.is_unit() && b.is_unit() => true,
            (Self::Table(m1), Self::Table(m2)) => m1 == m2,
            (
                Self::Fn {
                    parameters: p1,
                    return_type: r1,
                },
                Self::Fn {
                    parameters: p2,
                    return_type: r2,
                },
            ) => p1 == p2 && r1 == r2,
            (Self::Ref(t1), Self::Ref(t2)) => t1 == t2,
            (Self::List(a), b) | (b, Self::List(a)) => b.is_list(a),
            (Self::Map(a), b) | (b, Self::Map(a)) => b.is_map(a),

            _ => false,
        }
    }
}
impl Eq for Type {}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Type::Unit => f.write_str("[]"),
            Type::String => f.write_str("str"),
            Type::Number => f.write_str("num"),
            Type::Boolean => f.write_str("bool"),
            Type::Table(map) => Self::fmt_table(map, f),
            Type::Fn {
                parameters,
                return_type,
            } => {
                f.write_str("fn(")?;
                for param in parameters {
                    write!(f, "{param}")?
                }
                f.write_str(")")?;
                match **return_type {
                    Type::Unit => (),
                    Type::Union(_) => write!(f, " -> ({return_type})")?,
                    _ => write!(f, " -> {return_type}")?,
                }
                Ok(())
            }
            Type::Ref(ty) => write!(f, "ref {ty}"),
            Type::Union(vec) => {
                let mut iter = vec.iter();
                write!(f, "{}", iter.next().unwrap_or(&Type::Unit))?;
                for ty in iter {
                    write!(f, " | {}", ty)?;
                }
                Ok(())
            }
            Type::List(ty) => write!(f, "list<{ty}>"),
            Type::Map(ty) => write!(f, "map<{ty}>"),
        }
    }
}

pub trait IntoType {
    fn into_type() -> Type;
}

macro_rules! impl_into_type {
    ($rust_ty: ty => $pb_ty: expr $(, $generic: ident)*) => {
        impl<$($generic: IntoType),*> IntoType for $rust_ty {
            fn into_type() -> Type {
                $pb_ty
            }
        }
    };
}

impl_into_type!(() => Type::Unit);
impl_into_type!(String => Type::String);
impl_into_type!(f64 => Type::Number);
impl_into_type!(bool => Type::Boolean);
impl_into_type!(Vec<T> => Type::List(Box::new(T::into_type())), T);
impl_into_type!(HashMap<Key, T> => Type::Map(Box::new(T::into_type())), T);
impl_into_type!(HashMap<Key, Value> => Type::Unit);
impl_into_type!(Option<T> => Type::Union(vec![Type::Unit, T::into_type()]), T);

pub enum Primitive {
    Boolean(bool),
    Number(f64),
    String(String),
}
impl_into_type!(Primitive => Type::Union(vec![Type::String, Type::Number, Type::Boolean]));
