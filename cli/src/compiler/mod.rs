use hashbrown::HashMap;
use pbscript_lib::{error::Result, instruction::InstructionSet, types::Type};
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
    pub value: Type,
    pub generics: usize,
}
impl TypeDef {
    pub fn build(&self, generics: Vec<Type>) -> Type {
        self.value.clone().complete(&generics)
    }
}

#[derive(Default)]
struct Scope<'a> {
    variables: HashMap<String, Variable>,
    aliases: Option<HashMap<String, (usize, usize)>>,
    types: HashMap<String, TypeDef>,
    generics: HashMap<String, usize>,

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
    fn get_generic(&self, name: &str) -> Option<usize> {
        self.generics
            .get(name)
            .copied()
            .or_else(|| self.parent.map_or(None, |p| p.get_generic(name)))
    }
    fn get_generic_count(&self) -> usize {
        self.generics.len() + self.parent.map_or(0, |p| p.get_generic_count())
    }
}

pub fn compile(tree: Program, parent: Option<&dyn VarMap>) -> Result<InstructionSet> {
    let mut scope = Scope {
        parent,
        ..Default::default()
    };

    for statement in tree.body {
        compile_statement(statement, &mut scope)?;
    }

    Ok(scope.instructions)
}
