use hashbrown::HashMap;
use pbscript_lib::{
    error::{Error, Result},
    instruction::InstructionSet,
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

struct Scope<'a> {
    variables: HashMap<String, Variable>,
    instructions: InstructionSet,

    parent: Option<&'a mut Scope<'a>>,
}

pub fn compile(tree: Program) -> Result<InstructionSet> {
    let mut scope = Scope {
        variables: HashMap::new(),
        instructions: InstructionSet::default(),
        parent: None,
    };

    for statement in tree.body {
        compile_statement(statement, &mut scope)?
    }

    Ok(scope.instructions)
}
