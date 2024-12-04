#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ItemPath {
    module: String,
    name: String,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    String,
    Number,
    Boolean,
    Fn {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Ref(Box<Type>),
    List(Box<Type>),
    Map(Box<Type>),
    Named(ItemPath),
}

impl Type {
    pub fn simple_name(&self) -> &str {
        match self {
            Type::Unit => "unit",
            Type::String => "string",
            Type::Number => "number",
            Type::Boolean => "boolean",
            Type::Fn { .. } => "function",
            Type::Ref(_) => "reference",
            Type::List(_) => "list",
            Type::Map(_) => "map",
            Type::Named(path) => &path.name,
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Type::Unit)
    }

    pub fn is_list(&self, item_type: &Type) -> bool {
        match self {
            Type::List(t1) => *item_type == **t1,
            _ => false,
        }
    }
    pub fn is_map(&self, item_type: &Type) -> bool {
        match self {
            Type::Map(t1) => *item_type == **t1,
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
            (Self::Named(p1), Self::Named(p2)) => p1 == p2,

            _ => false,
        }
    }
}
