use std::rc::Rc;

use pbscript_lib::{
    error::Result,
    module_tree::Scope,
    span::Chunk,
    value::{Call, Value},
};

use crate::{eval::EvaluateChunk, parser::expression::Expression};

#[derive(Debug)]
pub struct Function {
    pub body: Chunk<Expression>,
    pub parameters: Vec<String>,
    pub(super) call_scope: Rc<Scope>,
    pub signature: String,
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

    fn to_str(&self) -> &str {
        &self.signature
    }
}
