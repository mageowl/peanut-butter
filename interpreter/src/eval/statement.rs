use std::{cell::RefCell, rc::Rc};

use super::{Evaluate, EvaluateChunk};
use crate::parser::statement::Statement;
use pbscript_lib::{error::Result, module_tree::Scope, span::Chunk, value::Value};

impl Evaluate for Statement {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value> {
        match chunk.data {
            Statement::DefVariable { name, value, .. } => {
                if let Some(expr) = value {
                    scope.variables[&name.data]
                        .value
                        .replace(Some(expr.eval(scope.clone())?));
                }
            }
            Statement::DefFunction {
                name,
                parameters,
                body,
                ..
            } => {
                todo!()
            }
            _ => todo!(),
        }

        Ok(Value::Unit)
    }
}
