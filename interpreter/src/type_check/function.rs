use std::rc::Rc;

use hashbrown::HashMap;
use pbscript_lib::{
    error::Result,
    module_tree::{Constant, Scope},
    span::Chunk,
    types::Type,
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
        let arg_map = HashMap::from_iter(self.parameters.iter().map(Clone::clone).zip(
            args.into_iter().map(|n| {
                Constant {
                    value: n,
                    value_type: Type::Unit, // type checking already happened
                }
            }),
        ));
        dbg!(&arg_map);
        let call_scope = Rc::new(Scope {
            variables: HashMap::new(),
            imported_constants: arg_map,
            parent: Some(self.parent.clone()),
        });

        self.body.as_ref().eval(call_scope)
    }

    fn to_str(&self) -> &str {
        &self.signature
    }

    fn to_str(&self) -> &str {
        &self.signature
    }

    fn to_str(&self) -> &str {
        &self.signature
    }
}
