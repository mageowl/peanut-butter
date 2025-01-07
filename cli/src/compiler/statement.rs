use super::{
    expression::{compile_expression, get_key, is_mut},
    ty::compile_ty,
    Scope, VarMap, Variable,
};
use crate::parser::{
    expression::Expression,
    statement::{AssignmentOperator, Statement},
    Parameter,
};
use pbscript_lib::{
    error::{Error, Result},
    instruction::{ArithmaticOperation, Instruction, InstructionSet, Reporter},
    span::Chunk,
    types::Type,
};

pub fn compile_statement(statement: Chunk<Statement>, scope: &mut Scope) -> Result<()> {
    match statement.data {
        Statement::DefVariable {
            mutable,
            name,
            type_hint,
            value,
        } => {
            let idx = scope.variables.len();
            let (ty, rep, value_span) = match value {
                Some(v) => {
                    let span = v.span;
                    let (ty, rep) = compile_expression(v, scope)?;
                    (Some(ty), Some(rep), Some(span))
                }
                None => (None, None, None),
            };

            let type_hint = if let Some(hint) = type_hint {
                let hint_ty = compile_ty(hint, scope)?;
                if let (Some(ty), Some(value_span)) = (ty, value_span) {
                    if !hint_ty.matches(&ty) {
                        return Err(Error::new(
                            value_span,
                            "This expression does not match the type of the variable.",
                        ));
                    }
                }
                hint_ty
            } else if let Some(ty) = ty {
                ty
            } else {
                return Err(Error::new(
                    name.span,
                    "Variables need either a type or an initial value.",
                ));
            };

            scope.variables.insert(
                name.data,
                Variable {
                    ty: type_hint,
                    mutable,
                    initialized: rep.is_some(),
                    idx,
                },
            );
            scope.instructions.allocation += 1;

            if let Some(value) = rep {
                scope
                    .instructions
                    .push(Instruction::Set { up: 0, idx, value });
            }
            Ok(())
        }
        Statement::DefFunction {
            mutable,
            name,
            parameters,
            return_type,
            body,
        } => {
            let idx = scope.variables.len();
            let return_type =
                Box::new(return_type.map_or(Ok(Type::Unit), |rt| compile_ty(rt, scope))?);
            let body_span = body.span;
            let (expr_ty, rep) = compile_fn(body, &parameters, scope)?;

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
                value: Reporter::Lambda(Box::new(rep)),
            });

            Ok(())
        }
        Statement::DefType { .. } => todo!(),
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
            _ => Err(Error::new(
                target.span,
                "This expression is not mutable, but you are trying to assign a value to it.",
            )),
        },
        Statement::Expression(expression) => {
            let (_, rep) = compile_expression(statement.span.with(expression), scope)?;
            scope.instructions.push(Instruction::Void(rep));
            Ok(())
        }
    }
}

pub fn compile_fn(
    body: Chunk<Expression>,
    parameters: &[Chunk<Parameter>],
    parent: &mut Scope,
) -> Result<(Type, Reporter)> {
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
                        ..Default::default()
                    },
                ))
            })
            .collect::<Result<_>>()?,
        instructions: InstructionSet::default(),

        parent: Some(parent),
    };

    compile_expression(body, &mut scope)
}
