use super::{
    expression::{compile_expression, get_key, is_mut},
    ty::{compile_ty, compile_ty_generics},
    Scope, TypeDef, VarMap, Variable,
};
use crate::parser::{
    block::Block,
    expression::Expression,
    pattern::Pattern,
    statement::{AssignmentOperator, Statement},
    Parameter,
};
use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    instruction::{ArithmaticOperation, Instruction, InstructionSet, Reporter},
    span::{Chunk, Span},
    types::Type,
    value::Key,
};

pub fn compile_statement(statement: Chunk<Statement>, scope: &mut Scope) -> Result<()> {
    match statement.data {
        Statement::DefVariable {
            mutable,
            mut pattern,
            value,
        } => {
            let (ty, rep, value_span) = match value {
                Some(v) => {
                    let span = v.span;
                    let (ty, rep) = compile_expression(v, scope)?;
                    (Some(ty), Some(rep), Some(span))
                }
                None => (None, None, None),
            };

            let (type_hint, type_span) = if let Some(hint) = pattern.type_hint() {
                let hint_span = hint.span;
                let hint_ty = compile_ty(hint, scope)?;
                if let (Some(ty), Some(value_span)) = (ty, value_span) {
                    if !hint_ty.matches(&ty) {
                        return Err(match ty {
                            Type::Union(v) if v.iter().any(|v| v.matches(&hint_ty)) => {
                                Error::new(
                                    value_span,
                                    String::from("This pattern does not always match the type of the expression.
Consider using a match statement:
    let my_value = match /* expression */ {
        /* pattern */ = my_value;
        _ = /* otherwise */;
    }")
                                )
                            }
                            _ => Error::new(
                                value_span,
                                format!(
                                    "This expression does not match the type of the variable.\nExpected type: {}\nFound type: {}",
                                    hint_ty,
                                    ty
                                )
                            ),
                        });
                    }
                }
                (hint_ty, hint_span)
            } else if let (Some(ty), Some(value_span)) = (ty, value_span) {
                (ty, value_span)
            } else {
                return Err(Error::new(
                    pattern.span,
                    "Variables need either a type or an initial value.",
                ));
            };

            compile_pattern(type_hint, type_span, pattern, rep, mutable, scope, true)
        }
        Statement::DefFunction {
            mutable,
            name,
            parameters,
            return_type,
            body,
        } => {
            let idx = scope.variables.len();
            let return_ty =
                Box::new(return_type.map_or(Ok(Type::Unit), |rt| compile_ty(rt, scope))?);
            let body_span = body.span;
            let (expr_ty, instructions, tail) = compile_fn(body, &parameters, scope)?;

            if !return_ty.matches(&expr_ty) {
                return Err(
                    Error::new(
                        body_span,
                        format!(
                            "This expression does not match the function signature.\nExpected a return type of {return_ty}.", 
                        )
                    )
                );
            }
            let parameters = parameters
                .into_iter()
                .map(|p| compile_ty(p.data.type_hint, scope))
                .collect::<Result<Vec<_>>>()?;

            let ty = Type::Fn {
                return_type: return_ty.clone(),
                parameters: parameters.clone(),
            };

            scope.variables.insert(
                name.data,
                Variable {
                    ty,
                    mutable,
                    initialized: true,
                    idx,
                },
            );
            scope.instructions.allocation += 1;
            scope.instructions.push(Instruction::Set {
                up: 0,
                idx,
                value: Reporter::Lambda {
                    body: instructions,
                    tail: tail.map(Box::new),
                    parameters,
                    return_ty: *return_ty,
                },
            });

            Ok(())
        }
        Statement::DefType {
            name,
            generics,
            value,
        } => {
            let generics_count = generics.len();
            let partial = compile_ty_generics(
                value,
                scope,
                &generics.into_iter().map(|s| s.data).collect(),
            )?;
            scope.types.insert(
                name.data,
                TypeDef {
                    partial,
                    generics: generics_count,
                },
            );
            Ok(())
        }
        Statement::Assign { target, value, op } => match target.data {
            Expression::Variable(name) => {
                let (up, var_ty, idx) = {
                    let Some((var, up)) = scope.get_var(&name) else {
                        return Err(Error::new(
                            target.span,
                            format!("Variable {name} does not exist."),
                        ));
                    };
                    if !var.mutable {
                        return Err(Error::new(
                            target.span,
                            format!("Variable {name} is not mutable"),
                        ));
                    }
                    (up, var.ty.clone(), var.idx)
                };

                let value_span = value.span;
                let (ty, mut rep) = compile_expression(value, scope)?;

                if op != AssignmentOperator::Assign {
                    if ty
                        != (match op {
                            AssignmentOperator::AddAssign => Type::Number,
                            _ => Type::Number,
                        })
                    {
                        return Err(Error::new(
                            value_span,
                            "Only numbers can be used for arithmatic.",
                        ));
                    }

                    rep = Reporter::Arithmetic {
                        a: Box::new(Reporter::Get { up, idx }),
                        b: Box::new(rep),
                        op: match op {
                            AssignmentOperator::AddAssign => ArithmaticOperation::Addition,
                            AssignmentOperator::SubAssign => ArithmaticOperation::Subtraction,
                            AssignmentOperator::MulAssign => ArithmaticOperation::Multiplication,
                            AssignmentOperator::DivAssign => ArithmaticOperation::Division,
                            _ => unreachable!(),
                        },
                    }
                }

                if !var_ty.matches(&ty) {
                    return Err(Error::new(
                        value_span,
                        format!("This expression does not match the type of the variable {name}."),
                    ));
                }

                scope.instructions.push(Instruction::Set {
                    up,
                    idx,
                    value: rep,
                });

                Ok(())
            }
            Expression::Access(target, key) => {
                let mutable = is_mut(target.span.with(&target.data), scope);
                let (tbl_ty, tbl) = compile_expression(target.span.with(*target.data), scope)?;
                let prop_ty = get_key(&tbl_ty, &key)?;

                if !mutable {
                    return Err(Error::new(
                        target.span,
                        "You cannot modify the properties of an immutable table.",
                    ));
                }

                let value_span = value.span;
                let (ty, mut rep) = compile_expression(value, scope)?;

                if op != AssignmentOperator::Assign {
                    if ty
                        != (match op {
                            AssignmentOperator::AddAssign => Type::Number,
                            _ => Type::Number,
                        })
                    {
                        return Err(Error::new(
                            value_span,
                            "Only numbers can be used for arithmatic.",
                        ));
                    }

                    rep = Reporter::Arithmetic {
                        a: Box::new(Reporter::Property {
                            tbl: Box::new(tbl.clone()),
                            key: key.data.clone(),
                        }),
                        b: Box::new(rep),
                        op: match op {
                            AssignmentOperator::AddAssign => ArithmaticOperation::Addition,
                            AssignmentOperator::SubAssign => ArithmaticOperation::Subtraction,
                            AssignmentOperator::MulAssign => ArithmaticOperation::Multiplication,
                            AssignmentOperator::DivAssign => ArithmaticOperation::Division,
                            _ => unreachable!(),
                        },
                    }
                }

                if !prop_ty.matches(&ty) {
                    return Err(Error::new(
                        value_span,
                        "This expression does not match the type of the property.",
                    ));
                }

                scope.instructions.push(Instruction::SetProp {
                    tbl,
                    key: key.data,
                    value: rep,
                });

                Ok(())
            }
            Expression::Deref(target) => {
                let (ref_ty, reference) =
                    compile_expression(target.span.with(*target.data), scope)?;
                let ref_ty = match ref_ty {
                    Type::Ref(ty) => *ty,
                    _ => {
                        return Err(Error::new(
                            target.span,
                            format!("You cannot dereference a {}.", ref_ty.simple_name()),
                        ))
                    }
                };

                let value_span = value.span;
                let (ty, mut rep) = compile_expression(value, scope)?;

                if op != AssignmentOperator::Assign {
                    if ty
                        != (match op {
                            AssignmentOperator::AddAssign => Type::Number,
                            _ => Type::Number,
                        })
                    {
                        return Err(Error::new(
                            value_span,
                            "Only numbers can be used for arithmatic.",
                        ));
                    }

                    rep = Reporter::Arithmetic {
                        a: Box::new(Reporter::Deref(Box::new(reference.clone()))),
                        b: Box::new(rep),
                        op: match op {
                            AssignmentOperator::AddAssign => ArithmaticOperation::Addition,
                            AssignmentOperator::SubAssign => ArithmaticOperation::Subtraction,
                            AssignmentOperator::MulAssign => ArithmaticOperation::Multiplication,
                            AssignmentOperator::DivAssign => ArithmaticOperation::Division,
                            _ => unreachable!(),
                        },
                    }
                }

                if !ref_ty.matches(&ty) {
                    return Err(Error::new(
                        value_span,
                        "This expression does not match the type of the property.",
                    ));
                }

                scope.instructions.push(Instruction::SetRef {
                    reference,
                    value: rep,
                });

                Ok(())
            }
            _ => Err(Error::new(
                target.span,
                "This expression is not mutable, but you are trying to assign a value to it.",
            )),
        },
        Statement::WhileLoop { condition, body } => {
            let cond_span = condition.span;
            let (ty, condition) = compile_expression(condition, scope)?;
            if !Type::Boolean.matches(&ty) {
                return Err(Error::new(
                    cond_span,
                    format!("A condition must be a boolean.\nThis expression is of type: {ty}"),
                ));
            }

            let mut body_scope = Scope {
                variables: HashMap::new(),
                aliases: None,
                types: HashMap::new(),
                instructions: InstructionSet::default(),
                parent: Some(scope),
            };
            for statement in body {
                compile_statement(statement, &mut body_scope)?;
            }

            scope.instructions.push(Instruction::While {
                condition,
                body: body_scope.instructions,
            });
            Ok(())
        }
        Statement::ForLoop {
            pattern,
            iter,
            body,
        } => {
            let iter_span = iter.span;
            let (iter_ty, iter) = compile_expression(iter, scope)?;

            let item_ty = match iter_ty {
                Type::Fn {
                    ref parameters,
                    ref return_type,
                } if parameters.is_empty() => {
                    macro_rules! incorrect_type {
                        () => {
                            return Err(Error::new(
                                iter_span,
                                format!(
                                    "This value is not an iterator.\
                                    Expected type: fn() -> (T | [])\
                                    Found: {iter_ty}"
                                ),
                            ));
                        };
                    }
                    let Type::Union(variants) = &**return_type else {
                        incorrect_type!();
                    };

                    if variants.len() != 2 || !variants.contains(&Type::Unit) {
                        incorrect_type!();
                    }

                    // Check that both 2 variants, and at least one is unit
                    #[allow(clippy::unwrap_used)]
                    variants
                        .iter()
                        .find(|ty| ty != &&Type::Unit)
                        .unwrap()
                        .clone()
                }
                Type::List(ty) => *ty,
                Type::Map(ty) => Type::Table(HashMap::from([
                    (Key::Index(0), Type::Union(vec![Type::Number, Type::String])),
                    (Key::Index(1), *ty),
                ])),
                _ => {
                    return Err(Error::new(
                        iter_span,
                        "This value is not an iterator.\
                        Only maps and lists can be implicitly converted into an iterator.",
                    ))
                }
            };

            let iter_idx = scope.instructions.allocation;
            scope.instructions.allocation += 1;
            scope.instructions.push(Instruction::Set {
                up: 0,
                idx: iter_idx,
                value: iter,
            });

            let idx = scope.instructions.allocation;
            scope.instructions.allocation += 1;
            // TODO: Implicit into_iter call
            scope.instructions.push(Instruction::Set {
                up: 0,
                idx,
                value: Reporter::Call(
                    Box::new(Reporter::Get {
                        up: 0,
                        idx: iter_idx,
                    }),
                    Vec::new(),
                ),
            });

            let mut body_scope = Scope {
                variables: HashMap::new(),
                aliases: None,
                types: HashMap::new(),
                instructions: InstructionSet::default(),
                parent: Some(scope),
            };
            compile_pattern(
                item_ty.clone(),
                iter_span,
                pattern,
                Some(Reporter::Get { up: 1, idx }),
                false,
                &mut body_scope,
                // Its already a variable, so we dont need to optimize reporter calls.
                false,
            )?;

            for statement in body {
                compile_statement(statement, &mut body_scope)?;
            }
            body_scope.instructions.push(Instruction::Set {
                up: 1,
                idx,
                value: Reporter::Call(
                    Box::new(Reporter::Get {
                        up: 1,
                        idx: iter_idx,
                    }),
                    Vec::new(),
                ),
            });

            scope.instructions.push(Instruction::While {
                condition: Reporter::Matches {
                    target: Box::new(Reporter::Get { up: 0, idx }),
                    pattern: item_ty,
                },
                body: body_scope.instructions,
            });
            Ok(())
        }
        Statement::Expression(expression) => {
            let (_, rep) = compile_expression(statement.span.with(expression), scope)?;
            scope.instructions.push(Instruction::Void(rep));
            Ok(())
        }
    }
}

pub fn compile_pattern(
    ty: Type,
    type_span: Span,
    pat: Chunk<Pattern>,
    mut reporter: Option<Reporter>,
    mutable: bool,
    scope: &mut Scope,
    top_level: bool,
) -> Result<()> {
    match pat.data {
        Pattern::Identifier { name, .. } => {
            let idx = scope.instructions.allocation;

            scope.variables.insert(
                name.data,
                Variable {
                    ty,
                    mutable,
                    initialized: reporter.is_some(),
                    idx,
                },
            );

            scope.instructions.allocation += 1;
            if let Some(reporter) = reporter {
                scope.instructions.push(Instruction::Set {
                    up: 0,
                    idx,
                    value: reporter,
                });
            }
        }
        Pattern::Table(map) => {
            if top_level {
                let idx = scope.instructions.allocation;

                scope.instructions.allocation += 1;
                if let Some(value) = reporter {
                    scope
                        .instructions
                        .push(Instruction::Set { up: 0, idx, value });
                    reporter = Some(Reporter::Get { up: 0, idx });
                }
            }

            let Type::Table(tbl) = ty else {
                return Err(Error::new(
                    type_span,
                    "You cannot destructure a non-table type.",
                ));
            };
            for (key, pat) in map {
                let ty = tbl
                        .get(&key.data)
                        .ok_or_else(|| {
                            Error::new(
                                key.span,
                                if tbl.iter().any(|k| k.0.is_idx()) {
                                    "This key doesn't exist on the destructured table.\nDid you mean to use an index? `0 = my_var`"
                                } else {
                                    "This key doesn't exist on the destructured table."
                                }
                            )
                        })?
                        .clone();
                compile_pattern(
                    ty,
                    type_span,
                    pat,
                    reporter.clone().map(|r| Reporter::Property {
                        tbl: Box::new(r),
                        key: key.data,
                    }),
                    mutable,
                    scope,
                    false,
                )?;
            }
        }
        Pattern::Unit => (),
        Pattern::Ignore { .. } => (),
    };
    Ok(())
}

pub fn compile_fn(
    body: Chunk<Block>,
    parameters: &[Chunk<Parameter>],
    parent: &mut Scope,
) -> Result<(Type, InstructionSet, Option<Reporter>)> {
    let mut scope = Scope {
        variables: parameters
            .iter()
            .enumerate()
            .map(|(idx, p)| {
                Ok((
                    p.name.data.clone(),
                    Variable {
                        ty: compile_ty(p.type_hint.clone(), parent)?,
                        idx,
                        mutable: false,
                        initialized: true,
                    },
                ))
            })
            .collect::<Result<_>>()?,
        aliases: None,
        types: HashMap::new(),
        instructions: InstructionSet::default(),

        parent: Some(parent),
    };

    for statement in body.data.body {
        compile_statement(statement, &mut scope)?;
    }

    if let Some(tail) = body.data.tail {
        let (ty, tail) = compile_expression(tail.span.with(*tail.data), &mut scope)?;
        Ok((ty, scope.instructions, Some(tail)))
    } else {
        Ok((Type::Unit, scope.instructions, None))
    }
}
