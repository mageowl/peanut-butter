use std::hash::Hash;

use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    instruction::InstructionSet,
    module_tree::ExternalModule,
    types::Type,
};
use statement::compile_statement;

use crate::parser::program::Program;

mod expression;
mod statement;
mod ty;

struct Variable {
    ty: Type,
    mutable: bool,
    initialized: bool,

    idx: usize,
}

impl Default for Variable {
    fn default() -> Self {
        Self {
            ty: Type::Unit,
            mutable: false,
            initialized: true,
            idx: usize::MAX,
        }
    }
}

pub trait VarMap {
    #[expect(private_interfaces)]
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)>;
}

pub struct PreludeMap {
    variables: HashMap<String, Variable>,
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

struct Scope<'a> {
    variables: HashMap<String, Variable>,
    instructions: InstructionSet,

    parent: Option<&'a dyn VarMap>,
}
impl VarMap for Scope<'_> {
    fn get_var(&self, name: &str) -> Option<(&Variable, usize)> {
        if let Some(var) = self.variables.get(name) {
            Some((var, 0))
        } else if let Some((var, up)) = self.parent.as_ref().and_then(|p| p.get_var(name)) {
            Some((var, up + 1))
        } else {
            None
        }
    }
}

pub fn compile(tree: Program, parent: Option<&dyn VarMap>) -> Result<InstructionSet> {
    let mut scope = Scope {
        variables: HashMap::new(),
        instructions: InstructionSet::default(),
        parent,
    };

    for statement in tree.body {
        compile_statement(statement, &mut scope)?;
    }

    Ok(scope.instructions)
}
