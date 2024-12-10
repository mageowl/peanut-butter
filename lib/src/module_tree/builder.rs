use crate::value::{function::FFIWrapper, Call};
use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;

use super::{ExternalModule, Variable};
use crate::{
    types::Type,
    value::{function::FFIFunction, Value},
};

pub struct ModuleBuilder {
    pub variables: HashMap<String, Variable>,
    pub submodules: HashMap<String, ExternalModule>,
}

impl ModuleBuilder {
    pub fn build(self) -> ExternalModule {
        ExternalModule {
            variables: self.variables,
            submodules: self.submodules,
        }
    }

    pub fn function<T: FFIFunction<M> + 'static, M: 'static>(mut self, name: &str, func: T) -> Self
    where
        FFIWrapper<T, M>: Call,
    {
        if !self.variables.contains_key(name) {
            self.variables.insert(
                name.to_string(),
                Variable {
                    value_type: Type::Fn {
                        parameters: T::parameters(),
                        return_type: Box::new(T::return_ty()),
                    },
                    value: Rc::new(RefCell::new(Some(Value::Function(Rc::new(
                        func.into_wrapper(),
                    ))))),
                    initialized: true,
                    mutable: false,
                },
            );
            self
        } else {
            panic!("A function of name {name} has already been declared in this module.");
        }
    }
}
