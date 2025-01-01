use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    instruction::Reporter,
    span::Chunk,
    types::Type,
    value::{Key, Value},
};

use crate::parser::expression::Expression;

use super::{statement::compile_fn, ty::compile_ty, Scope};

pub fn compile_expression(
    expression: Chunk<Expression>,
    scope: &mut Scope,
) -> Result<(Type, Reporter)> {
    match expression.data {
        Expression::String(s) => Ok((Type::String, Reporter::Const(Value::String(s)))),
        Expression::Number(n) => Ok((Type::Number, Reporter::Const(Value::Number(n)))),
        Expression::Boolean(b) => Ok((Type::Boolean, Reporter::Const(Value::Boolean(b)))),
        Expression::Table(hash_map) => {
            let (ty, val) = hash_map
                .into_iter()
                .map(|(k, v)| {
                    let (ty, val) = compile_expression(v, scope)?;
                    Ok(((k.data.clone(), ty), (k.data, val)))
                })
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .unzip::<_, _, HashMap<_, _>, HashMap<_, _>>();
            Ok((Type::Table(ty), Reporter::Table(val)))
        }
        Expression::Lambda {
            parameters,
            return_type,
            body,
        } => {
            let return_type =
                Box::new(return_type.map_or(Ok(Type::Unit), |rt| compile_ty(rt, scope))?);
            let body_span = body.span;
            let (expr_ty, rep) = compile_fn(body.span.with(*body.data), &parameters, scope)?;

            if !return_type.matches(&expr_ty) {
                return Err(
                    Error::new(
                        body_span,
                        format!(
                            "This expression does not match the function signature. Expected a return type of {}.", 
                            return_type.simple_name()
                        )
                    )
                );
            }

            let ty = Type::Fn {
                return_type,
                parameters: parameters
                    .into_iter()
                    .map(|p| compile_ty(p.data.type_hint, scope))
                    .collect::<Result<_>>()?,
            };

            Ok((ty, Reporter::Lambda(Box::new(rep))))
        }
        Expression::Variable(name) => {
            let Some((var, up)) = scope.get_var(&name) else {
                return Err(Error::new(
                    expression.span,
                    format!("Variable {name} does not exist."),
                ));
            };

            Ok((var.ty.clone(), Reporter::Get { up, idx: var.idx }))
        }
        Expression::Reference(target) => {
            if !is_mut(target.span.with(&target.data), scope) {
                return Err(Error::new(
                    target.span,
                    "Cannot make references to immutable variables or properties.",
                ));
            }

            match *target.data {
                Expression::Variable(name) => {
                    let Some((var, up)) = scope.get_var(&name) else {
                        return Err(Error::new(
                            expression.span,
                            format!("Variable {name} does not exist."),
                        ));
                    };

                    Ok((
                        Type::Ref(Box::new(var.ty.clone())),
                        Reporter::RefLocal { up, idx: var.idx },
                    ))
                }
                Expression::Access(target, key) => {
                    let (ty, rep) = compile_expression(target.span.with(*target.data), scope)?;

                    let prop_ty = get_key(&ty, &key)?.clone();

                    Ok((
                        Type::Ref(Box::new(prop_ty)),
                        Reporter::RefProp {
                            tbl: Box::new(rep),
                            key: key.data,
                        },
                    ))
                }
                Expression::DynAccess(_, _) => todo!(),

                // The parser filters out references to bad targets.
                _ => unreachable!(),
            }
        }
        Expression::Deref(expr) => {
            let (ty, rep) = compile_expression(expr.span.with(*expr.data), scope)?;
            match ty {
                Type::Ref(ty) => Ok((*ty, Reporter::Deref(Box::new(rep)))),
                _ => Err(Error::new(
                    expr.span,
                    format!("You cannot dereference a {}.", ty.simple_name()),
                )),
            }
        }
        Expression::Access(target, key) => {
            let (ty, rep) = compile_expression(target.span.with(*target.data), scope)?;

            Ok((
                get_key(&ty, &key)?.clone(),
                Reporter::Property {
                    tbl: Box::new(rep),
                    key: key.data,
                },
            ))
        }
        Expression::DynAccess(target, key) => todo!(),
        Expression::Call { value, args } => {
            let fn_span = value.span;
            let (ty, rep) = compile_expression(value.span.with(*value.data), scope)?;

            let (return_ty, parameters) = match ty {
                Type::Fn {
                    parameters,
                    return_type,
                } => (*return_type, parameters),
                _ => {
                    return Err(Error::new(
                        fn_span,
                        "This expression is not a function, so it can't be called.",
                    ))
                }
            };

            if parameters.len() != args.len() {
                return Err(Error::new(
                    args[parameters.len().min(args.len() - 1)].span,
                    format!(
                        "This function expects {} argument{}, but you are passing {} instead.",
                        parameters.len(),
                        if parameters.len() != 1 { "s" } else { "" },
                        args.len()
                    ),
                ));
            }

            let mut arg_reps = Vec::new();
            for (param, arg) in parameters.iter().zip(args.into_iter()) {
                let span = arg.span;
                let (ty, rep) = compile_expression(arg, scope)?;

                if !param.matches(&ty) {
                    return Err(Error::new(span, "This argument is the wrong type."));
                }
                arg_reps.push(rep);
            }

            Ok((return_ty, Reporter::Call(Box::new(rep), arg_reps)))
        }
        Expression::Block(block) => todo!(),
        Expression::If { blocks, else_block } => todo!(),
        Expression::BinaryOp { a, b, op } => todo!(),
        Expression::UnaryOp { a, op } => todo!(),
    }
}

pub fn is_mut(expression: Chunk<&Expression>, scope: &Scope) -> bool {
    match &expression.data {
        Expression::Variable(name) => scope.get_var(name).map_or(false, |v| v.0.mutable),
        Expression::Deref(_) => true,
        Expression::Access(target, _) | Expression::DynAccess(target, _) => {
            is_mut(target.span.with(&*target.data), scope)
        }
        _ => false,
    }
}

pub fn get_key<'a>(ty: &'a Type, key: &Chunk<Key>) -> Result<&'a Type> {
    match ty {
        Type::Table(map) => match map.get(&key.data) {
            Some(ty) => Ok(ty),
            None => Err(Error::new(
                key.span,
                format!("Property {key} doesn't exist on this table."),
            )),
        },
        _ => Err(Error::new(
            key.span,
            format!(
                "You can't access the properties of a {ty}.",
                ty = ty.simple_name()
            ),
        )),
    }
}
