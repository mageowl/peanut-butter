#![deny(clippy::unwrap_used)]

use std::{env::args, fs, path::PathBuf};

use eval::{Evaluate, EvaluateChunk};
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::{
    error::Result,
    module_tree::{LocalModule, ModuleTree},
};
use type_check::{Ancestry, UnlockedScope};

mod eval;
mod lexer;
mod parser;
mod prelude;
mod type_check;

pub fn interpret(code: &str, mut module: LocalModule, mut module_tree: ModuleTree) -> Result<()> {
    let mut token_stream = TokenStream::from(code);
    let mut program = Program::parse(&mut token_stream)?;

    let prelude = prelude::build();
    let ancestry = Ancestry::Module {
        tree: &mut module_tree,
        module: &mut module,
        prelude: &prelude,
    };
    let scope = UnlockedScope::new(&mut program.data, &ancestry)?;
    scope.lock(&mut program.data);

    program.eval(
        program
            .scope
            .as_ref()
            .expect("failed to lock scope")
            .clone(),
    )?;
    Ok(())
}

fn main() {
    let path = PathBuf::from(args().nth(1).expect("expected a file path to run."));

    let file = fs::read_to_string(&path).expect("failed to open file");

    let module_tree = ModuleTree::new();
    let module = LocalModule::new();

    if let Err(error) = interpret(&file, module, module_tree) {
        println!("{error}")
    }
}
