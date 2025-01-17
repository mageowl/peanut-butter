use std::{cell::RefCell, rc::Rc};

use pbscript_lib::{
    error::Result,
    instruction::{InstructionSet, Reporter},
    types::Type,
    value::{Call, Value},
};

use super::{evaluate_reporter, evaluate_state, State};

pub struct PbFunction {
    pub body: InstructionSet,
    pub tail: Option<Reporter>,
    pub return_ty: Type,
    pub parameters: Vec<Type>,
    pub parent: Rc<State>,
}

impl Call for PbFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let state = Rc::new(State {
            stack: args.into_iter().map(|a| Rc::new(RefCell::new(a))).collect(),
            parent: Some(self.parent.clone()),
        });

        evaluate_state(self.body.clone(), state.clone())?;

        if let Some(tail) = &self.tail {
            evaluate_reporter(tail.clone(), state)
        } else {
            Ok(Value::Unit)
        }
    }

    fn return_ty(&self) -> &Type {
        &self.return_ty
    }

    fn parameters(&self) -> &Vec<Type> {
        &self.parameters
    }
}
