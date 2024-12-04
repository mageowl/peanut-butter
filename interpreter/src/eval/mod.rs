use crate::parser::program::Program;
use pbscript_lib::{error::Result, span::Chunk, value::Value};

pub trait Evaluate: Sized {
    fn eval(chunk: Chunk<Self>) -> Result<Value>;
}

impl Evaluate for Program {
    fn eval(chunk: Chunk<Self>) -> Result<Value> {
        todo!();
    }
}

pub trait EvaluateChunk {
    fn eval(self) -> Result<Value>;
}

impl<T: Evaluate> EvaluateChunk for Chunk<T> {
    fn eval(self) -> Result<Value> {
        T::eval(self)
    }
}
