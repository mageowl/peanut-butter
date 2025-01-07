use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    types::Type,
};

use crate::parser::type_name::TypeName;

use super::Scope;

macro_rules! assert_generics {
    (type = $type:expr, name = $name:literal, $generics:ident) => {
        if !$generics.is_empty() {
            Err(Error::new(
                Span {
                    start: $generics[0].span.start,
                    #[allow(clippy::unwrap_used)]
                    end: $generics.last().unwrap().span.end,
                },
                format!("The primitive {} type doesn't need any generics.", $name),
            ))
        } else {
            Ok($type)
        }
    };
}

#[allow(clippy::only_used_in_recursion)]
pub fn compile_ty(type_name: Chunk<TypeName>, scope: &mut Scope) -> Result<Type> {
    match type_name.data {
        TypeName::Named { name, generics } => match name.data.as_str() {
            "str" => assert_generics!(type = Type::String, name = "string", generics),
            "num" => assert_generics!(type = Type::Number, name = "number", generics),
            "bool" => assert_generics!(type = Type::Boolean, name = "boolean", generics),
            _ => todo!(),
        },
        TypeName::Table(map) => Ok(Type::Table(
            map.into_iter()
                .map(|(k, v)| Ok((k.data, compile_ty(v, scope)?)))
                .collect::<Result<_>>()?,
        )),
        TypeName::Reference(chunk) => Ok(Type::Ref(Box::new(compile_ty(
            chunk.span.with(*chunk.data),
            scope,
        )?))),
        TypeName::Union(variants) => Ok(Type::Union(
            variants
                .into_iter()
                .map(|v| compile_ty(v, scope))
                .collect::<Result<_>>()?,
        )),
        TypeName::Unit => Ok(Type::Unit),
        TypeName::Function {
            parameters,
            return_ty,
        } => Ok(Type::Fn {
            parameters: parameters
                .into_iter()
                .map(|p| compile_ty(p, scope))
                .collect::<Result<_>>()?,
            return_type: Box::new(compile_ty(return_ty.span.with(*return_ty.data), scope)?),
        }),
    }
}
