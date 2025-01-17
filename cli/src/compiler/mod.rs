use hashbrown::HashMap;
use pbscript_lib::{
    error::Result,
    instruction::InstructionSet,
    types::{partial::PartialType, Type},
};
use statement::compile_statement;

use crate::{parser::program::Program, prelude_map::VarMap};

mod expression;
mod statement;
mod ty;

#[derive(Debug)]
pub struct Variable {
    pub ty: Type,
    pub mutable: bool,
    pub initialized: bool,

    pub idx: usize,
}

pub struct TypeDef {
    pub partial: PartialType,
    pub generics: usize,
}
impl TypeDef {
    pub fn build(&self, generics: Vec<Type>) -> Type {
        self.partial.clone().complete(&generics)
    }
}

struct Scope<'a> {
    variables: HashMap<String, Variable>,
    aliases: Option<HashMap<String, (usize, usize)>>,
    types: HashMap<String, TypeDef>,

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
    fn get_type(&self, name: &str) -> Option<&TypeDef> {
        if let Some(type_def) = self.types.get(name) {
            Some(type_def)
        } else {
            self.parent.and_then(|p| p.get_type(name))
        }
    }
}

pub fn compile(tree: Program, parent: Option<&dyn VarMap>) -> Result<InstructionSet> {
    let mut scope = Scope {
        variables: HashMap::new(),
        aliases: None,
        types: HashMap::new(),
        instructions: InstructionSet::default(),
        parent,
    };

    for statement in tree.body {
        compile_statement(statement, &mut scope)?;
    }

    Ok(scope.instructions)
}
