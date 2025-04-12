use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;
use pbscript_lib::{
    error::Result,
    module_tree::{builder::ModuleBuilder, ExternalModule},
    types::{GenericType, IntoType, Type},
    value::{function::PbFunction, Key, Value},
};

pub fn build() -> ExternalModule {
    ExternalModule::builder()
        .add_type("Iter", PbIter::<GenericType<0>>::into_type())
        .add_function("collect", collect)
        .build()
}

pub(super) fn add_to_prelude(builder: ModuleBuilder) -> ModuleBuilder {
    builder
        .add_type("Iter", PbIter::<GenericType<0>>::into_type())
        .add_function("collect", collect)
}

pub struct PbIter<T: IntoType> {
    next: PbFunction<(), Option<T>>,
}
impl<T: IntoType + From<Value>> IntoType for PbIter<T> {
    fn into_type() -> Type {
        Type::Table(HashMap::from([(
            Key::Named(String::from("next")),
            PbFunction::<(), Option<T>>::into_type(),
        )]))
    }
}
impl<T: IntoType + From<Value>> From<Value> for PbIter<T> {
    fn from(value: Value) -> Self {
        match value {
            Value::Table(map) => Self {
                next: map
                    .get("next")
                    .expect("a function to move across a language barrier")
                    .borrow()
                    .clone()
                    .into(),
            },
            _ => panic!("Expected an iterator to pass across a language barrier."),
        }
    }
}

fn collect(iter: PbIter<GenericType<0>>) -> Result<Vec<GenericType<0>>> {
    let mut vec = Vec::new();
    while let Some(next) = iter.next.call()? {
        vec.push(next);
    }
    Ok(vec)
}
