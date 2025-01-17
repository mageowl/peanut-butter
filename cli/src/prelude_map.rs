use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;
use pbscript_lib::module_tree::{ExternalModule, ExternalTypeAlias};

use crate::{
    compiler::{TypeDef, Variable},
    interpreter::State,
};

pub trait VarMap {
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)>;
    fn get_type(&self, name: &str) -> Option<&TypeDef>;
}

pub struct PreludeMap {
    variables: HashMap<String, Variable>,
    types: HashMap<String, TypeDef>,
}

impl PreludeMap {
    pub fn create_state(self, module: &ExternalModule) -> State {
        let mut stack = self
            .variables
            .into_iter()
            .map(|(name, var)| {
                (
                    module
                        .constants
                        .get(&name)
                        .map(|v| v.value.clone())
                        .or_else(|| module.variables.get(&name).map(|v| v.value.clone()))
                        .expect("failed to build prelude state"),
                    var.idx,
                )
            })
            .collect::<Vec<_>>();
        stack.sort_by(|(_, a), (_, b)| a.cmp(b));
        State {
            stack: stack.into_iter().map(|(v, _)| v).collect(),
            parent: None,
        }
    }
}

impl From<&ExternalModule> for PreludeMap {
    fn from(module: &ExternalModule) -> Self {
        let mut variables = HashMap::new();
        let mut idx = 0;

        for (name, var) in &module.variables {
            variables.insert(
                name.clone(),
                Variable {
                    ty: var.value_type.clone(),
                    mutable: true,
                    initialized: var.initialized,
                    idx,
                },
            );
            idx += 1;
        }
        for (name, con) in &module.constants {
            variables.insert(
                name.clone(),
                Variable {
                    ty: con.value_type.clone(),
                    mutable: false,
                    initialized: true,
                    idx,
                },
            );
            idx += 1;
        }

        Self {
            variables,
            types: module
                .types
                .iter()
                .map(|(k, ExternalTypeAlias { partial, generics })| {
                    (
                        k.clone(),
                        TypeDef {
                            partial: partial.clone(),
                            generics: *generics,
                        },
                    )
                })
                .collect(),
        }
    }
}

impl VarMap for PreludeMap {
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)> {
        self.variables.get(name).map(|v| (v, 0))
    }

    fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }
}
