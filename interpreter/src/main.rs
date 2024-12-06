#![deny(clippy::unwrap_used)]

use std::{env::args, fs, path::PathBuf};

use eval::{Evaluate, EvaluateChunk};
use hashbrown::HashMap;
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::{
    error::Result,
    module_tree::{Module, ModuleTree},
};
use type_check::{Ancestry, UnlockedScope};

pub mod eval;
pub mod lexer;
pub mod parser;
pub mod type_check;

pub fn interpret(code: &str, mut module: Module, mut module_tree: ModuleTree) -> Result<()> {
    let mut token_stream = TokenStream::from(code);
    let mut program = Program::parse(&mut token_stream)?;

    let ancestry = Ancestry::Module {
        tree: &mut module_tree,
        module: &mut module,
    };
    let scope = UnlockedScope::new(&mut program.data, &ancestry)?;
    scope.lock(&mut program.data);

    program.eval()?;
    Ok(())
}

fn main() {
    let path = PathBuf::from(args().nth(1).expect("expected a file path to run."));

    let file = fs::read_to_string(&path).expect("failed to open file");

    let module_tree = ModuleTree::new();
    let module = Module::new(vec![match path.file_stem() {
        Some(os) => os.to_string_lossy().to_string(),
        None => String::from("main"),
    }]);

    if let Err(error) = interpret(&file, module, module_tree) {
        println!("{error}")
    }
}
