#![deny(clippy::unwrap_used)]

use std::{env::args, fs};

use eval::EvaluateChunk;
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::error::{Result, Warn};
use type_check::TypeChecker;

pub mod eval;
pub mod lexer;
pub mod parser;
pub mod type_check;

pub fn interpret(code: &str) -> Result<()> {
    let mut warnings: Vec<Warn> = Vec::new();

    let mut token_stream = TokenStream::from(code);
    let mut program = Program::parse(&mut token_stream)?;
    let mut type_checker = TypeChecker::new(&mut warnings, None);
    type_checker.scope(&mut program)?;

    Ok(())
}

fn main() {
    let path = args().skip(1).next().unwrap();
    let file = fs::read_to_string(path).expect("failed to open file");

    if let Err(error) = interpret(&file) {
        println!("{error}")
    }
}
