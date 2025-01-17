use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    span::{Chunk, Span},
    types::{partial::PartialType, Type},
};

use crate::{parser::type_name::TypeName, prelude_map::VarMap};

use super::Scope;

macro_rules! assert_generics {
    (type = $type:expr, name = $name:literal, $generics:ident) => {
        assert_generics!(type = $type, error_msg = format!("The primitive {} type doesn't need any generics.", $name), $generics)
    };
    (type = $type:expr, error_msg = $error:expr, $generics:ident) => {
        if !$generics.is_empty() {
            Err(Error::new(
                Span {
                    start: $generics[0].span.start,
                    #[allow(clippy::unwrap_used)]
                    end: $generics.last().unwrap().span.end,
                },
                $error,
            ))
        } else {
            Ok($type)
        }
    };
}

pub fn compile_ty(type_name: Chunk<TypeName>, scope: &mut Scope) -> Result<Type> {
    match type_name.data {
        TypeName::Named { name, generics } => match name.data.as_str() {
            "str" => assert_generics!(type = Type::String, name = "string", generics),
            "num" => assert_generics!(type = Type::Number, name = "number", generics),
            "bool" => assert_generics!(type = Type::Boolean, name = "boolean", generics),

            name_data => {
                let generic_spans: Vec<_> = generics.iter().map(|g| g.span).collect();
                let generics: Vec<_> = generics
                    .into_iter()
                    .map(|g| compile_ty(g, scope))
                    .collect::<Result<_>>()?;
                let type_def = scope.get_type(name_data).ok_or_else(|| {
                    Error::new(name.span, format!("Type {name_data} does not exist."))
                })?;

                if type_def.generics != generics.len() {
                    return Err(Error::new(
                        *generic_spans.last().unwrap_or(&name.span),
                        format!(
                            "Type {name_data} expects {} generics, but got {} instead.",
                            type_def.generics,
                            generics.len()
                        ),
                    ));
                }

                Ok(type_def.build(generics))
            }
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
        TypeName::Unit => Ok(Type::Unit),
    }
}

pub fn compile_ty_generics(
    type_name: Chunk<TypeName>,
    scope: &mut Scope,
    input_generics: &Vec<String>,
) -> Result<PartialType> {
    match type_name.data {
        TypeName::Named { name, generics } => match name.data.as_str() {
            "str" => assert_generics!(type = PartialType::String, name = "string", generics),
            "num" => assert_generics!(type = PartialType::Number, name = "number", generics),
            "bool" => assert_generics!(type = PartialType::Boolean, name = "boolean", generics),

            name if input_generics.iter().any(|x| x == name) => {
                assert_generics!(
                    type = #[expect(clippy::unwrap_used)] PartialType::Generic(input_generics.iter().position(|g| g == name).unwrap()),
                    error_msg = "Generic parameters don't need generics themselves.",
                    generics
                )
            }
            name_data => {
                let generic_spans: Vec<_> = generics.iter().map(|g| g.span).collect();
                let generics: Vec<_> = generics
                    .into_iter()
                    .map(|g| compile_ty(g, scope))
                    .collect::<Result<_>>()?;
                let type_def = scope.get_type(name_data).ok_or_else(|| {
                    Error::new(name.span, format!("Type {name_data} does not exist."))
                })?;

                if type_def.generics != generics.len() {
                    return Err(Error::new(
                        *generic_spans.last().unwrap_or(&name.span),
                        format!(
                            "Type {name_data} expects {} generics, but got {} instead.",
                            type_def.generics,
                            generics.len()
                        ),
                    ));
                }

                Ok(type_def.build(generics).into())
            }
        },
        TypeName::Table(map) => Ok(PartialType::Table(
            map.into_iter()
                .map(|(k, v)| Ok((k.data, compile_ty_generics(v, scope, input_generics)?)))
                .collect::<Result<_>>()?,
        )),
        TypeName::Reference(chunk) => Ok(PartialType::Ref(Box::new(compile_ty_generics(
            chunk.span.with(*chunk.data),
            scope,
            input_generics,
        )?))),
        TypeName::Union(variants) => Ok(PartialType::Union(
            variants
                .into_iter()
                .map(|v| compile_ty_generics(v, scope, input_generics))
                .collect::<Result<_>>()?,
        )),
        TypeName::Function {
            parameters,
            return_ty,
        } => Ok(PartialType::Fn {
            parameters: parameters
                .into_iter()
                .map(|p| compile_ty_generics(p, scope, input_generics))
                .collect::<Result<_>>()?,
            return_type: Box::new(compile_ty_generics(
                return_ty.span.with(*return_ty.data),
                scope,
                input_generics,
            )?),
        }),
        TypeName::Unit => Ok(PartialType::Unit),
    }
}
