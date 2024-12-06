use crate::module_tree::ItemPath;

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

    pub fn is_list(&self, item_type: &Type) -> bool {
        match self {
            Type::List(t1) => *item_type == **t1,
            Type::Table(map) => map
                .iter()
                .all(|(k, v)| v == item_type && matches!(k, Key::Index(_))),
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
            (Self::List(t1), Self::List(t2)) => t1 == t2,
            (Self::Map(t1), Self::List(t2)) => t1 == t2,

            (Self::List(a), b) | (b, Self::List(a)) => b.is_list(a),
            (Self::Map(a), b) | (b, Self::Map(a)) => b.is_map(a),

            _ => false,
        }
    }
}
