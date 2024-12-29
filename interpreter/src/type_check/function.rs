use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;
use pbscript_lib::{
    error::Result,
    module_tree::{Scope, Variable},
    span::Chunk,
    types::Type,
    value::{Call, Value},
};

use crate::{eval::EvaluateChunk, parser::expression::Expression};

#[derive(Debug)]
pub struct Function {
    pub body: Chunk<Expression>,
    pub parameters: Vec<String>,
    pub name: String,
    pub(super) call_scope: Rc<Scope>,
}

impl Call for Function {
    fn call(&self, args: Vec<Value>) -> Result<Value> {
        for (arg, p) in args.into_iter().zip(self.parameters.iter()) {
            self.call_scope
                .variables
                .get(p)
                .expect("incorrectly setup")
                .value
                .replace(arg);
        }

        self.body.as_ref().eval(self.call_scope.clone())
    }
}
