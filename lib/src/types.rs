use std::collections::HashMap;

use crate::value::Key;

#[derive(Debug, Clone)]
pub enum Type {
    Table(HashMap<Key, Type>),
    Fn {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    String,
    Number,
    Boolean,
    Unit,
}

impl Type {
    pub fn simple_name(&self) -> &'static str {
        match self {
            Type::Table(map) if map.is_empty() => "unit",
            Type::Table(_) => "table",
            Type::Fn { .. } => "function",
            Type::String => "string",
            Type::Number => "number",
            Type::Boolean => "boolean",
            Type::Unit => "unit",
        }
    }

    pub fn is_unit(&self) -> bool {
        match self {
            Type::Unit => true,
            Type::Table(map) => map.is_empty(),
            _ => false,
        }
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

            _ => false,
        }
    }
}
