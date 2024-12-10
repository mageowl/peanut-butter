use std::rc::Rc;

use crate::parser::program::Program;
use pbscript_lib::{error::Result, module_tree::Scope, span::Chunk, value::Value};

mod expression;
mod statement;

pub trait Evaluate: Sized {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value>;
}

impl Evaluate for Program {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value> {
        for expr in &chunk.data.body {
            expr.eval(scope.clone())?;
        }
        Ok(Value::Unit)
    }
}

pub trait EvaluateChunk {
    fn eval(&self, scope: Rc<Scope>) -> Result<Value>;
}

impl<T: Evaluate> EvaluateChunk for Chunk<T> {
    fn eval(&self, scope: Rc<Scope>) -> Result<Value> {
        T::eval(self.as_ref(), scope)
    }
}
