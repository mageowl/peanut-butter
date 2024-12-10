use std::rc::Rc;

use super::Evaluate;
use crate::parser::expression::Expression;
use pbscript_lib::{error::Result, module_tree::Scope, span::Chunk, value::Value};

impl Evaluate for Expression {
    fn eval(chunk: Chunk<&Self>, scope: Rc<Scope>) -> Result<Value> {
        todo!()
    }
}
