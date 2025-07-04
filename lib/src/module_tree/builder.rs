use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;

use super::{ExternalConstant, ExternalModule, ExternalTypeAlias, ExternalVariable};
use crate::{
    types::Type,
    value::{
        function::{FFIFunction, FFIWrapper},
        Call, Value,
    },
};

pub struct ModuleBuilder {
    pub constants: HashMap<String, ExternalConstant>,
    pub variables: HashMap<String, ExternalVariable>,
    pub submodules: HashMap<String, ExternalModule>,
    pub types: HashMap<String, ExternalTypeAlias>,
}

impl ExternalModule {
    pub fn builder() -> ModuleBuilder {
        ModuleBuilder {
            constants: HashMap::new(),
            variables: HashMap::new(),
            submodules: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

impl ModuleBuilder {
    pub fn build(self) -> ExternalModule {
        ExternalModule {
            constants: self.constants,
            variables: self.variables,
            submodules: self.submodules,
            types: self.types,
        }
    }

    pub fn add_function<T: FFIFunction<M> + 'static, M: 'static>(
        mut self,
        name: &str,
        func: T,
    ) -> Self
    where
        FFIWrapper<T, M>: Call,
    {
        if !self.constants.contains_key(name) {
            self.constants.insert(
                name.to_string(),
                ExternalConstant {
                    value_type: Type::Fn {
                        parameters: T::parameters(),
                        return_type: Box::new(T::return_ty()),
                    },
                    value: Rc::new(RefCell::new(Value::Function(Rc::new(func.into_wrapper())))),
                },
            );
            self
        } else {
            panic!("A constant of name {name} has already been declared in this module.");
        }
    }
    pub fn add_type(mut self, name: &str, value: Type) -> Self {
        if !self.types.contains_key(name) {
            self.types.insert(
                name.to_string(),
                ExternalTypeAlias {
                    generics: value.get_max_generics(),
                    value,
                },
            );
            self
        } else {
            panic!("A type of name {name} has already been declared in this module.");
        }
    }
}
