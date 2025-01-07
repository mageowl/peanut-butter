#![deny(clippy::unwrap_used)]

use std::{env::args, fs, path::PathBuf, rc::Rc};

use compiler::compile;
use interpreter::evaluate;
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::{error::Result, module_tree::ModuleTree};
use prelude_map::PreludeMap;
use std_library::prelude;

mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod prelude_map;
mod std_library;

pub fn interpret(code: &str, mut _module_tree: ModuleTree) -> Result<()> {
    let mut token_stream = TokenStream::from(code);
    let program = Program::parse(&mut token_stream)?;

    let prelude = prelude::build();
    let prelude_map = PreludeMap::from(&prelude);
    let instructions = compile(program.data, Some(&prelude_map))?;
    evaluate(
        instructions,
        Some(Rc::new(prelude_map.create_state(&prelude))),
    )?;
    Ok(())
}

fn main() {
    let path = PathBuf::from(args().nth(1).expect("expected a file path to run."));

    let file = fs::read_to_string(&path).expect("failed to open file");

    let module_tree = ModuleTree::new();
    if let Err(mut error) = interpret(&file, module_tree) {
        let span = error.stack[0].span;
        let src = span.read_from(&file);

        let ln_len = span.end.ln.ilog10() + 1;
        println!(
            "\x1b[2m{pad}|\x1b[22m",
            pad = " ".repeat(ln_len as usize + 1)
        );

        for (i, line) in src.lines().enumerate() {
            let ln = span.start.ln + i;
            let ln_pad = (ln_len - ln.ilog10() - 1) as usize;

            const HIGHLIGHT: &str = "\x1b[31;1m";
            let mut line = line.replace("\t", "    ");

            if ln == span.end.ln {
                line.insert_str(span.end.col - 1, "\x1b[0m")
            }
            if ln == span.start.ln {
                line.insert_str(span.start.col - 1, HIGHLIGHT)
            } else if ln >= span.start.ln && ln <= span.end.ln {
                line.insert_str(0, HIGHLIGHT);
            }

            println!(
                "\x1b[2m{pad}{ln} |\x1b[22m  {line}\x1b[0m",
                pad = " ".repeat(ln_pad)
            );
        }

        println!(
            "\x1b[2m{pad}|\x1b[22m",
            pad = " ".repeat(ln_len as usize + 1)
        );

        let indicies = error
            .message
            .match_indices("\n")
            .map(|(i, _)| i)
            .collect::<Vec<_>>();
        for i in indicies.into_iter().rev() {
            error.message.insert_str(i + 1, "       ");
        }

        println!("\n\x1b[31;1merror\x1b[0m: {msg}", msg = error.message);
    }
}
