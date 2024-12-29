use std::{cell::RefCell, rc::Rc};

use super::{Evaluate, EvaluateChunk};
use crate::parser::{
    expression::Expression,
    statement::{AssignmentOperator, Statement},
};
use pbscript_lib::{error::Result, module_tree::Scope, span::Chunk, value::Value};

impl Evaluate for Statement {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value> {
        match chunk.data {
            Statement::DefVariable { name, value, .. } => {
                if let Some(expr) = value {
                    scope.variables[&name.data]
                        .value
                        .replace(expr.as_ref().eval(scope.clone())?);
                }
            }
            Statement::DefFunction { .. } => {
                // already initialized by type checker
            }
            Statement::Assign { target, value, op } => {
                let var = match &target.data {
                    Expression::Access(tbl, prop) => {
                        let Value::Table(tbl) = tbl.span.with(&*tbl.data).eval(scope.clone())?
                        else {
                            unreachable!("not a table");
                        };
                        tbl[&prop.data].clone()
                    }
                    Expression::Variable(name) => scope
                        .get_var(name)
                        .expect("type check")
                        .get_ref()
                        .expect("mutable"),
                    Expression::Deref(expr) => {
                        let Value::Reference(r) =
                            expr.span.with(expr.data.as_ref()).eval(scope.clone())?
                        else {
                            unreachable!("type check")
                        };
                        r
                    }
                    _ => unreachable!(),
                };
                match op {
                    AssignmentOperator::Assign => {
                        *var.borrow_mut() = value.as_ref().eval(scope)?;
                    }
                    op => {
                        let Value::Number(current) = var.replace(Value::Unit) else {
                            unreachable!("type check")
                        };
                        let Value::Number(value) = value.as_ref().eval(scope)? else {
                            unreachable!("type check")
                        };

                        *var.borrow_mut() = Value::Number(match op {
                            AssignmentOperator::AddAssign => current + value,
                            AssignmentOperator::SubAssign => current - value,
                            AssignmentOperator::MulAssign => current * value,
                            AssignmentOperator::DivAssign => current / value,
                            _ => unreachable!(),
                        })
                    }
                }
            }
            Statement::Expression(expr) => {
                chunk.span.with(expr).eval(scope)?;
            }
            _ => todo!(),
        }

        Ok(Value::Unit)
    }
}
