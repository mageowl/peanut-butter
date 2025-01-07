use hashbrown::HashMap;
use pbscript_lib::{error::Result, instruction::InstructionSet, types::Type};
use statement::compile_statement;

use crate::{parser::program::Program, prelude_map::VarMap};

mod expression;
mod statement;
mod ty;

pub struct Variable {
    pub ty: Type,
    pub mutable: bool,
    pub initialized: bool,

    pub idx: usize,
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
