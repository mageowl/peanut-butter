use hashbrown::HashMap;

use crate::value::Key;

use super::Type;

#[derive(Debug, Clone)]
pub enum PartialType {
    Unit,
    String,
    Number,
    Boolean,
    Table(HashMap<Key, PartialType>),
    Fn {
        parameters: Vec<PartialType>,
        return_type: Box<PartialType>,
    },
    Ref(Box<PartialType>),
    Union(Vec<PartialType>),
    List(Box<PartialType>),
    Map(Box<PartialType>),

    Generic(usize),
}

impl From<Type> for PartialType {
    fn from(value: Type) -> Self {
        match value {
            Type::Unit => Self::Unit,
            Type::String => Self::String,
            Type::Number => Self::Number,
            Type::Boolean => Self::Boolean,
            Type::Table(map) => {
                Self::Table(map.into_iter().map(|(k, v)| (k, Self::from(v))).collect())
            }
            Type::Fn {
                parameters,
                return_type,
            } => Self::Fn {
                parameters: parameters.into_iter().map(Into::into).collect(),
                return_type: Box::new((*return_type).into()),
            },
            Type::Ref(ty) => Self::Ref(Box::new((*ty).into())),
            Type::Union(vec) => Self::Union(vec.into_iter().map(Into::into).collect()),
            Type::List(ty) => Self::List(Box::new((*ty).into())),
            Type::Map(ty) => Self::Map(Box::new((*ty).into())),
        }
    }
}

impl PartialType {
    pub fn complete(self, generics: &Vec<Type>) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::String => Type::String,
            Self::Number => Type::Number,
            Self::Boolean => Type::Boolean,
            Self::Table(map) => Type::Table(
                map.into_iter()
                    .map(|(k, v)| (k, v.complete(generics)))
                    .collect(),
            ),
            Self::Ref(ty) => Type::Ref(Box::new(ty.complete(generics))),
            Self::Union(vec) => {
                Type::Union(vec.into_iter().map(|v| v.complete(generics)).collect())
            }
            Self::List(ty) => Type::List(Box::new(ty.complete(generics))),
            Self::Map(ty) => Type::Map(Box::new(ty.complete(generics))),
            Self::Fn {
                parameters,
                return_type,
            } => Type::Fn {
                parameters: parameters
                    .into_iter()
                    .map(|v| v.complete(generics))
                    .collect(),
                return_type: Box::new(return_type.complete(generics)),
            },
            Self::Generic(idx) => generics[idx].clone(),
        }
    }

    pub fn get_max_generics(&self) -> usize {
        match self {
            Self::Generic(n) => *n + 1,
            Self::Table(map) => map.values().map(Self::get_max_generics).max().unwrap_or(0),
            Self::Union(v) => v.iter().map(Self::get_max_generics).max().unwrap_or(0),
            Self::List(ty) => ty.get_max_generics(),
            Self::Map(ty) => ty.get_max_generics(),
            Self::Fn {
                parameters,
                return_type,
            } => parameters
                .iter()
                .map(Self::get_max_generics)
                .max()
                .unwrap_or(0)
                .max(return_type.get_max_generics()),
            _ => 0,
        }
    }
}

#[macro_export]
macro_rules! partial_ty {
    ({str}) => {
        $crate::types::partial::PartialType::String
    };
    (str) => {
        $crate::types::partial::PartialType::String
    };
    ({num}) => {
        $crate::types::partial::PartialType::Number
    };
    (num) => {
        $crate::types::partial::PartialType::Number
    };
    ({bool}) => {
        $crate::types::partial::PartialType::Boolean
    };
    (bool) => {
        $crate::types::partial::PartialType::Boolean
    };
    ({[]}) => {
        $crate::types::partial::PartialType::Unit
    };
    ([]) => {
        $crate::types::partial::PartialType::Unit
    };
    ({[$($idx: literal: $idx_ty: tt),*,$($key: ident: $ty: tt),*]}) => {
        $crate::types::partial::PartialType::Table(
            HashMap::from([
                $(($crate::value::Key::Index($idx), $crate::partial_ty!($idx_ty))),*,
                $(($crate::value::Key::Named(String::from(stringify!($key))), $crate::partial_ty!($ty))),*
            ])
        )
    };
    ([$($idx: literal: $idx_ty: tt),*,$($key: ident: $ty: tt),*]) => {
        $crate::types::partial::PartialType::Table(
            HashMap::from([
                $(($crate::value::Key::Index($idx), $crate::partial_ty!($idx_ty))),*,
                $(($crate::value::Key::Named(String::from(stringify!($key))), $crate::partial_ty!($ty))),*
            ])
        )
    };
    (fn($($param: tt),*) -> $return: tt) => {
        $crate::partial_ty!({fn($($param),*) -> $return})
    };
    ({fn($($param: tt),*) -> $return: tt}) => {
        $crate::types::partial::PartialType::Fn {
            parameters: vec![$($crate::partial_ty!($param)),*],
            return_type: Box::new($crate::partial_ty!($return)),
        }
    };
    (ref $ty: tt) => {
        $crate::partial_ty!((ref $ty))
    };
    ({ref $ty: tt}) => {
        $crate::types::partial::PartialType::Ref(Box::new($crate::partial_ty!($ty)))
    };
    (list $ty: tt) => {
        $crate::partial_ty!((list $ty))
    };
    ({list $ty: tt}) => {
        $crate::types::partial::PartialType::List(Box::new($crate::partial_ty!($ty)))
    };
    (map $ty: tt) => {
        $crate::partial_ty!((map $ty))
    };
    ({map $ty: tt}) => {
        $crate::types::partial::PartialType::Map(Box::new($crate::partial_ty!($ty)))
    };
    (generic($idx: literal)) => {
        $crate::partial_ty!((generic($idx)))
    };
    ({generic $idx: literal}) => {
        $crate::types::partial::PartialType::Generic($idx)
    };
    ({$init: tt | $($variant: tt)|*}) => {
        $crate::types::partial::PartialType::Union(vec![$crate::partial_ty!($init),$($crate::partial_ty!($variant)),*])
    }
}
