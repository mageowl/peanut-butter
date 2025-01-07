use std::{cell::RefCell, rc::Rc};

use pbscript_lib::{
    error::Result,
    instruction::Reporter,
    value::{Call, Value},
};

use super::{evaluate_reporter, State};

pub struct PbFunction {
    pub reporter: Reporter,
    pub signature: String,
    pub parent: Rc<State>,
}

impl Call for PbFunction {
    fn call(&self, args: Vec<Value>) -> Result<Value> {
        let state = State {
            stack: args.into_iter().map(|a| Rc::new(RefCell::new(a))).collect(),
            parent: Some(self.parent.clone()),
        };

        evaluate_reporter(self.reporter.clone(), Rc::new(state))
    }
}
