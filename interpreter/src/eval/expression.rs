use std::{cell::RefCell, rc::Rc};

use super::{Evaluate, EvaluateChunk};
use crate::parser::{
    block::Block,
    expression::{Expression, Operation, UnaryOperation},
};
use hashbrown::HashMap;
use pbscript_lib::{error::Result, module_tree::Scope, span::Chunk, value::Value};

impl Evaluate for Expression {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value> {
        match chunk.data {
            Expression::String(text) => Ok(Value::String(text.clone())),
            Expression::Number(value) => Ok(Value::Number(*value)),
            Expression::Boolean(value) => Ok(Value::Boolean(*value)),
            Expression::Table(map) => Ok(Value::Table(
                map.into_iter()
                    .map(|(k, v)| {
                        Ok((
                            k.data.clone(),
                            Rc::new(RefCell::new(v.as_ref().eval(scope.clone())?)),
                        ))
                    })
                    .collect::<Result<HashMap<_, _>>>()?,
            )),
            Expression::Lambda { value, .. } => Ok(value.clone().expect("Didn't lock lambda")),

            Expression::Variable(name) => Ok(scope
                .get_var(name)
                .unwrap_or_else(|| panic!("failed to get var {name}. {:?}", scope))
                .get()),
            Expression::Reference(variable) => match &*variable.data {
                Expression::Variable(name) => Ok(Value::Reference(
                    scope
                        .get_var(name)
                        .unwrap_or_else(|| panic!("failed to get var {name}. {:?}", scope))
                        .get_ref()
                        .expect("type check"),
                )),
                Expression::Access(tbl, prop) => {
                    let Value::Table(tbl) = tbl.span.with(&*tbl.data).eval(scope.clone())? else {
                        unreachable!("not a table");
                    };
                    Ok(Value::Reference(tbl[&prop.data].clone()))
                }
                _ => unreachable!(),
            },
            Expression::Deref(reference) => {
                let Value::Reference(rc) = reference.span.with(&*reference.data).eval(scope)?
                else {
                    unreachable!()
                };
                let borrow = rc.borrow();
                Ok(borrow.clone())
            }
            Expression::Access(tbl, prop) => {
                let Value::Table(tbl) = tbl.span.with(&*tbl.data).eval(scope.clone())? else {
                    unreachable!("not a table");
                };
                let borrow = tbl[&prop.data].borrow();
                Ok(borrow.clone())
            }
            Expression::DynAccess(_, _) => todo!(),
            Expression::Call { value, args } => {
                let Value::Function(function) =
                    value.span.with(&*value.data).eval(scope.clone())?
                else {
                    unreachable!("not a function")
                };
                function.call(
                    args.iter()
                        .map(|a| a.as_ref().eval(scope.clone()))
                        .collect::<Result<Vec<_>>>()?,
                )
            }

            Expression::Block(block) => chunk.span.with(block).eval(scope),
            Expression::If { blocks, else_block } => {
                for (condition, block) in blocks {
                    let Value::Boolean(condition) = condition.as_ref().eval(scope.clone())? else {
                        unreachable!();
                    };
                    if condition {
                        return block.as_ref().eval(scope);
                    }
                }

                if let Some(block) = else_block {
                    block.as_ref().eval(scope)
                } else {
                    Ok(Value::Unit)
                }
            }

            Expression::BinaryOp { a, b, op } => {
                let a = a.span.with(a.data.as_ref()).eval(scope.clone())?;
                let b = b.span.with(b.data.as_ref()).eval(scope)?;
                match op.data {
                    Operation::Comparison(comparison) => todo!(),
                    Operation::Concatination => {
                        let Value::String(a) = a else {
                            panic!("Type checking failed.")
                        };
                        let Value::String(b) = b else {
                            panic!("Type checking failed.")
                        };

                        Ok(Value::String(a + &b))
                    }
                    op @ (Operation::Exponentation
                    | Operation::Multiplication
                    | Operation::Division
                    | Operation::Addition
                    | Operation::Subtraction) => {
                        let Value::Number(a) = a else { unreachable!() };
                        let Value::Number(b) = b else { unreachable!() };

                        Ok(Value::Number(match op {
                            Operation::Exponentation => {
                                if b.round() == b {
                                    a.powi(b as i32)
                                } else {
                                    a.powf(b)
                                }
                            }

                            Operation::Multiplication => a * b,
                            Operation::Division => a / b,
                            Operation::Subtraction => a - b,
                            _ => unreachable!(),
                        }))
                    }
                    Operation::BooleanAnd => todo!(),
                    Operation::BooleanOr => todo!(),
                    Operation::Access => todo!(),
                }
            }
            Expression::UnaryOp {
                a,
                op:
                    Chunk {
                        data: UnaryOperation::BooleanNot,
                        ..
                    },
            } => {
                let Value::Boolean(a) = a.span.with(&*a.data).eval(scope)? else {
                    unreachable!()
                };
                Ok(Value::Boolean(!a))
            }
            Expression::UnaryOp {
                a,
                op:
                    Chunk {
                        data: UnaryOperation::Negation,
                        ..
                    },
            } => {
                let Value::Number(a) = a.span.with(&*a.data).eval(scope)? else {
                    unreachable!()
                };
                Ok(Value::Number(-a))
            }
        }
    }
}

impl Evaluate for Block {
    fn eval(chunk: Chunk<&Self>, _scope: Rc<Scope>) -> Result<Value> {
        let scope = chunk.data.scope.as_ref().expect("unlocked");
        for item in &chunk.data.body {
            item.as_ref().eval(scope.clone())?;
        }
        match &chunk.data.tail {
            None => Ok(Value::Unit),
            Some(tail) => tail.span.with(&*tail.data).eval(scope.clone()),
        }
    }
}
