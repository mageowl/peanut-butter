#![deny(clippy::unwrap_used)]

use std::{env::args, fs, path::PathBuf};

use compiler::compile;
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::{error::Result, module_tree::ModuleTree};

mod compiler;
mod lexer;
mod parser;
mod prelude;

pub fn interpret(code: &str, mut _module_tree: ModuleTree) -> Result<()> {
    let mut token_stream = TokenStream::from(code);
    let program = Program::parse(&mut token_stream)?;

    let _prelude = prelude::build();
    let instructions = compile(program.data);
    todo!()
}

fn main() {
    let path = PathBuf::from(args().nth(1).expect("expected a file path to run."));

    let file = fs::read_to_string(&path).expect("failed to open file");

    let module_tree = ModuleTree::new();
    if let Err(error) = interpret(&file, module_tree) {
        println!("{error}")
    }
}
