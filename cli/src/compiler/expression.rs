use std::mem;

use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    instruction::{ArithmaticOperation, Instruction, InstructionSet, Reporter},
    span::{Chunk, Span},
    types::Type,
    value::{Comparison, Key, Value},
};

use crate::parser::{
    block::Block,
    expression::{Expression, Operation},
    pattern::Pattern,
};

use super::{
    statement::{compile_fn, compile_pattern, compile_statement},
    ty::compile_ty,
    Scope, VarMap,
};

pub fn compile_expression(
    expression: Chunk<Expression>,
    scope: &mut Scope,
) -> Result<(Type, Reporter)> {
    match expression.data {
        Expression::String(s) => Ok((Type::String, Reporter::Const(Value::String(s)))),
        Expression::Number(n) => Ok((Type::Number, Reporter::Const(Value::Number(n)))),
        Expression::Boolean(b) => Ok((Type::Boolean, Reporter::Const(Value::Boolean(b)))),
        Expression::Table(hash_map) => {
            if hash_map.is_empty() {
                return Ok((Type::Unit, Reporter::Const(Value::Unit)));
            }
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
            let return_ty = return_type.map_or(Ok(Type::Unit), |rt| compile_ty(rt, scope))?;
            let body_span = body.span;
            let (expr_ty, body, tail) = compile_fn(body, &parameters, scope)?;

            if !return_ty.matches(&expr_ty) {
                return Err(
                    Error::new(
                        body_span,
                        format!(
                            "This expression does not match the function signature. Expected a return type of {}.", 
                            return_ty.simple_name()
                        )
                    )
                );
            }

            let parameters = parameters
                .into_iter()
                .map(|p| compile_ty(p.data.type_hint, scope))
                .collect::<Result<Vec<_>>>()?;
            let ty = Type::Fn {
                return_type: Box::new(return_ty.clone()),
                parameters: parameters.clone(),
            };

            Ok((
                ty,
                Reporter::Lambda {
                    body,
                    tail: tail.map(Box::new),
                    parameters,
                    return_ty,
                },
            ))
        }
        Expression::Variable(name) => {
            let Some((var, mut up)) = scope.get_var(&name) else {
                return Err(Error::new(
                    expression.span,
                    format!("Variable {name} does not exist."),
                ));
            };
            let mut idx = var.idx;
            let ty = var.ty.clone();
            if let Some((a_idx, a_up)) = scope.aliases.as_ref().and_then(|m| m.get(&name)) {
                up = *a_up;
                idx = *a_idx;
            };

            Ok((ty, Reporter::Get { up, idx }))
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
        Expression::DynAccess(_, _) => todo!(),
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
                    return Err(Error::new(span, format!("This argument is the wrong type.\nExpected an argument of type: {param}\nGot an argument of type: {ty}")));
                }
                arg_reps.push(rep);
            }

            Ok((return_ty, Reporter::Call(Box::new(rep), arg_reps)))
        }
        Expression::Block(block) => {
            let (ty, instructions, tail) = compile_block(block, scope)?;
            Ok((ty, Reporter::Block(instructions, tail.map(Box::new))))
        }
        Expression::If { blocks, else_block } => {
            let mut types: Vec<Type> = Vec::new();
            let mut compiled_blocks = Vec::new();

            for (cond, block) in blocks {
                let cond_span = cond.span;

                let (ty, cond) = compile_expression(cond, scope)?;
                if !Type::Boolean.matches(&ty) {
                    return Err(Error::new(
                        cond_span,
                        format!("A condition must be a boolean.\nThis expression is of type: {ty}"),
                    ));
                }

                let (ty, instructions, tail) = compile_block(block.data, scope)?;
                if !types.contains(&ty) {
                    types.push(ty);
                }

                compiled_blocks.push((cond, instructions, tail));
            }

            let else_block = if let Some(Chunk { data: block, .. }) = else_block {
                let (ty, instructions, tail) = compile_block(block, scope)?;
                if !types.contains(&ty) {
                    types.push(ty);
                }

                Some((instructions, tail.map(Box::new)))
            } else {
                None
            };

            Ok((
                if types.len() > 1 {
                    Type::Union(types)
                } else {
                    types.remove(0)
                },
                Reporter::If {
                    blocks: compiled_blocks,
                    else_block,
                },
            ))
        }
        Expression::Match { value, cases } => {
            let value_span = value.span;
            let (value_ty, value) = compile_expression(value.span.with(*value.data), scope)?;
            let mut prev_cases = Vec::new();
            let mut return_ty: Vec<Type> = Vec::new();
            let mut compiled_cases = Vec::new();

            let idx = scope.instructions.allocation;
            scope
                .instructions
                .push(Instruction::Set { up: 0, idx, value });
            scope.instructions.allocation += 1;

            for (mut pat, arm) in cases {
                let pat_ty = pat.type_hint();
                let pat_ty = if matches!(&pat.data, Pattern::Ignore { .. }) && pat_ty.is_none() {
                    value_ty.clone()
                } else {
                    compile_ty(
                        pat_ty.ok_or_else(|| {
                            Error::new(pat.span, "This pattern does not have a fully defined type.")
                        })?,
                        scope,
                    )?
                };
                if !value_ty.matches(&pat_ty) {
                    return Err(Error::new(
                        pat.span,
                        format!(
                            "This pattern will never match the expression type.\nExpression type: {}\nPattern type: {}",
                            value_ty,
                            pat_ty
                        )
                    ));
                }

                let mut flat = pat_ty.clone().flat();
                if flat
                    .iter()
                    .all(|b| prev_cases.iter().any(|a: &Type| a.matches(b)))
                {
                    return Err(Error::new(
                        pat.span,
                        "This pattern is redundant because a previous case covers it.",
                    ));
                }
                prev_cases.append(&mut flat);

                let mut scope = Scope {
                    variables: HashMap::new(),
                    aliases: None,
                    types: HashMap::new(),
                    instructions: InstructionSet::default(),
                    parent: Some(scope),
                };

                if let Pattern::Identifier { name, type_hint: _ } = &pat.data {
                    scope.aliases = Some(HashMap::from([(name.data.clone(), (idx, 1))]));
                }

                compile_pattern(
                    pat_ty.clone(),
                    pat.span,
                    pat,
                    Some(Reporter::Get { up: 1, idx }),
                    false,
                    &mut scope,
                    true,
                )?;

                for statement in arm.data.body {
                    compile_statement(statement, &mut scope)?;
                }

                let (arm_ty, tail) = arm
                    .data
                    .tail
                    .map(|tail| compile_expression(tail.span.with(*tail.data), &mut scope))
                    .transpose()?
                    .unzip();
                let arm_ty = arm_ty.unwrap_or(Type::Unit);
                if !return_ty.contains(&arm_ty) {
                    return_ty.push(arm_ty);
                }

                compiled_cases.push((pat_ty, scope.instructions, tail.map(Box::new)));
            }

            let cases_ty = Type::Union(prev_cases);
            let value_ty = Type::Union(value_ty.flat());
            if !cases_ty.matches(&value_ty) {
                return Err(Error::new(
                    value_span,
                    format!("The match statement does not cover all the cases of the value.\nValue type: {value_ty}\nCases: {cases_ty}"),
                ));
            }

            Ok((
                if return_ty.len() == 1 {
                    return_ty.remove(0)
                } else {
                    Type::Union(return_ty)
                },
                Reporter::Match {
                    target: idx,
                    cases: compiled_cases,
                },
            ))
        }
        Expression::BinaryOp { a, b, op } => match op.data {
            Operation::Concatination => {
                let (a_span, b_span) = (a.span, b.span);
                let (a_ty, a) = compile_expression(a.span.with(*a.data), scope)?;
                let (b_ty, b) = compile_expression(b.span.with(*b.data), scope)?;

                if !Type::String.matches(&a_ty) {
                    return Err(Error::new(
                        a_span,
                        "This expression is not a string. Expected a string for concatination.",
                    ));
                }
                if !Type::String.matches(&b_ty) {
                    return Err(Error::new(
                        b_span,
                        "This expression is not a string. Expected a string for concatination.",
                    ));
                }

                Ok((
                    Type::String,
                    Reporter::Concat {
                        a: Box::new(a),
                        b: Box::new(b),
                    },
                ))
            }
            Operation::Exponentation
            | Operation::Multiplication
            | Operation::Division
            | Operation::Addition
            | Operation::Subtraction => {
                let (a_span, b_span) = (a.span, b.span);
                let (a_ty, a) = compile_expression(a.span.with(*a.data), scope)?;
                let (b_ty, b) = compile_expression(b.span.with(*b.data), scope)?;

                if !Type::Number.matches(&a_ty) {
                    return Err(Error::new(
                        a_span,
                        "This expression is not a number. Expected a number for arthimetic.",
                    ));
                }
                if !Type::Number.matches(&b_ty) {
                    return Err(Error::new(
                        b_span,
                        "This expression is not a number. Expected a number for arthimetic.",
                    ));
                }

                Ok((
                    Type::Number,
                    Reporter::Arithmetic {
                        a: Box::new(a),
                        b: Box::new(b),
                        op: match op.data {
                            Operation::Exponentation => ArithmaticOperation::Exponentation,
                            Operation::Multiplication => ArithmaticOperation::Multiplication,
                            Operation::Division => ArithmaticOperation::Division,
                            Operation::Addition => ArithmaticOperation::Addition,
                            Operation::Subtraction => ArithmaticOperation::Subtraction,
                            _ => unreachable!(),
                        },
                    },
                ))
            }
            Operation::Comparison(comp @ (Comparison::Equals | Comparison::NotEquals)) => {
                let (a_span, b_span) = (a.span, b.span);
                let (a_ty, a) = compile_expression(a.span.with(*a.data), scope)?;
                let (b_ty, b) = compile_expression(b.span.with(*b.data), scope)?;

                if a_ty != b_ty {
                    return Err(Error::new(
                        Span {
                            start: a_span.start,
                            end: b_span.end,
                        },
                        "These expressions are not of the same type, so they can never be equal.",
                    ));
                }

                Ok((
                    Type::Boolean,
                    if comp == Comparison::NotEquals {
                        Reporter::BooleanNot(Box::new(Reporter::Equality {
                            a: Box::new(a),
                            b: Box::new(b),
                        }))
                    } else {
                        Reporter::Equality {
                            a: Box::new(a),
                            b: Box::new(b),
                        }
                    },
                ))
            }
            Operation::Comparison(mut comp) => {
                let (a_span, b_span) = (a.span, b.span);
                let (a_ty, mut a) = compile_expression(a.span.with(*a.data), scope)?;
                let (b_ty, mut b) = compile_expression(b.span.with(*b.data), scope)?;

                if !Type::Number.matches(&a_ty) {
                    return Err(Error::new(
                        a_span,
                        "Only numbers can be compared with an inequality.",
                    ));
                }
                if !Type::Number.matches(&b_ty) {
                    return Err(Error::new(
                        b_span,
                        "Only numbers can be compared with an inequality.",
                    ));
                }

                if comp == Comparison::LessThanEqual || comp == Comparison::GreaterThanEqual {
                    mem::swap(&mut a, &mut b);
                    comp = match comp {
                        Comparison::LessThanEqual => Comparison::GreaterThan,
                        Comparison::GreaterThanEqual => Comparison::LessThan,
                        _ => unreachable!(),
                    }
                }

                Ok((
                    Type::Boolean,
                    Reporter::Inequality {
                        a: Box::new(a),
                        b: Box::new(b),
                        op: comp,
                    },
                ))
            }
            Operation::BooleanAnd | Operation::BooleanOr => {
                let (a_span, b_span) = (a.span, b.span);
                let (a_ty, a) = compile_expression(a.span.with(*a.data), scope)?;
                let (b_ty, b) = compile_expression(b.span.with(*b.data), scope)?;

                if !Type::Boolean.matches(&a_ty) {
                    return Err(Error::new(
                        a_span,
                        "Boolean operations like && and || can only be used on booleans.",
                    ));
                }
                if !Type::Boolean.matches(&b_ty) {
                    return Err(Error::new(
                        b_span,
                        "Boolean operations like && and || can only be used on booleans.",
                    ));
                }

                Ok((
                    Type::Boolean,
                    Reporter::BooleanOp {
                        a: Box::new(a),
                        b: Box::new(b),
                        and: op.data == Operation::BooleanAnd,
                    },
                ))
            }
            Operation::Access => {
                unreachable!("BinaryOp is not used for access operators (only sorting)")
            }
        },
        Expression::UnaryOp { a, op } => todo!(),
    }
}

fn compile_block(
    block: Block,
    scope: &mut Scope,
) -> Result<(Type, InstructionSet, Option<Reporter>)> {
    let mut scope = Scope {
        variables: HashMap::new(),
        aliases: None,
        types: HashMap::new(),
        instructions: InstructionSet::default(),
        parent: Some(scope),
    };

    for statement in block.body {
        compile_statement(statement, &mut scope)?;
    }

    let (ty, rep) = block
        .tail
        .map(|tail| compile_expression(tail.span.with(*tail.data), &mut scope))
        .transpose()?
        .unzip();

    Ok((ty.unwrap_or(Type::Unit), scope.instructions, rep))
}

pub fn is_mut(expression: Chunk<&Expression>, scope: &Scope) -> bool {
    match &expression.data {
        Expression::Variable(name) => scope.get_var(name).is_some_and(|v| v.0.mutable),
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
                format!("Property `{}` doesn't exist on this table.", key.data),
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
