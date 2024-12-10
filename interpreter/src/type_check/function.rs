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
    pub parent: Rc<Scope>,
    pub name: String,
}

impl Call for Function {
    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let call_scope = Rc::new(Scope {
            variables: HashMap::from_iter(self.parameters.iter().map(Clone::clone).zip(
                args.into_iter().map(|n| {
                    Variable {
                        value: Rc::new(RefCell::new(Some(n))),
                        mutable: false,
                        value_type: Type::Unit, // type checking already happened
                        initialized: true,
                    }
                }),
            )),
            parent: Some(self.parent.clone()),
        });
        self.body.eval(call_scope)
    }
}
