use crate::value::{function::FFIWrapper, Call};
use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;

use super::{Constant, ExternalModule, Variable};
use crate::{
    types::Type,
    value::{function::FFIFunction, Value},
};

pub struct ModuleBuilder {
    pub constants: HashMap<String, Constant>,
    pub variables: HashMap<String, Variable>,
    pub submodules: HashMap<String, ExternalModule>,
}

impl ModuleBuilder {
    pub fn build(self) -> ExternalModule {
        ExternalModule {
            constants: self.constants,
            variables: self.variables,
            submodules: self.submodules,
        }
    }

    pub fn function<T: FFIFunction<M> + 'static, M: 'static>(mut self, name: &str, func: T) -> Self
    where
        FFIWrapper<T, M>: Call,
    {
        if !self.constants.contains_key(name) {
            self.constants.insert(
                name.to_string(),
                Constant {
                    value_type: Type::Fn {
                        parameters: T::parameters(),
                        return_type: Box::new(T::return_ty()),
                    },
                    value: Value::Function(Rc::new(func.into_wrapper())),
                },
            );
            self
        } else {
            panic!("A constant of name {name} has already been declared in this module.");
        }
    }
}
