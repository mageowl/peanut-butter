use std::{cell::RefCell, rc::Rc};

use hashbrown::HashMap;
use pbscript_lib::{module_tree::ExternalModule, value::Value};

use crate::{compiler::Variable, interpreter::State};

pub trait VarMap {
    #[expect(private_interfaces)]
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)>;
}

pub struct PreludeMap {
    variables: HashMap<String, Variable>,
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

        Self { variables }
    }
}

impl VarMap for PreludeMap {
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)> {
        self.variables.get(name).map(|v| (v, 0))
    }
}
