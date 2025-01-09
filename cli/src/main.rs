#![deny(clippy::unwrap_used)]

use std::{fs, path::PathBuf, process::ExitCode, rc::Rc};

use clap::{
    builder::{
        styling::{AnsiColor, Effects},
        Styles,
    },
    Parser, Subcommand,
};
use compiler::compile;
use interpreter::evaluate;
use lexer::TokenStream;
use parser::{program::Program, Parse};
use pbscript_lib::{error::Error, module_tree::ModuleTree};
use prelude_map::PreludeMap;
use std_library::prelude;

mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod prelude_map;
mod std_library;

pub fn interpret(code: &str, mut _module_tree: ModuleTree) -> Result<(), Error> {
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

const STYLE: Styles = Styles::styled()
    .header(AnsiColor::Green.on_default().effects(Effects::BOLD))
    .usage(AnsiColor::Green.on_default().effects(Effects::BOLD))
    .literal(AnsiColor::Cyan.on_default().effects(Effects::BOLD))
    .placeholder(AnsiColor::Cyan.on_default())
    .error(AnsiColor::Red.on_default().effects(Effects::BOLD))
    .valid(AnsiColor::Cyan.on_default().effects(Effects::BOLD))
    .invalid(AnsiColor::Yellow.on_default().effects(Effects::BOLD));

#[derive(Parser)]
#[command(name = "pbscript")]
#[command(about = "A statically typed interpreted language.", long_about = None, styles(STYLE))]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    #[command(arg_required_else_help = true)]
    /// Run code from a .pb file.
    Run {
        /// Path to a file to run.
        file: PathBuf,
        /// Additional arguments that are passed to the program.
        args: Vec<String>,
    },
    /// Open an interactive command line
    Repl,
    #[command(arg_required_else_help = true)]
    Check { file: PathBuf },
    #[command(hide = true)]
    CompileDebug { file: PathBuf },
}

fn main() -> ExitCode {
    let Cli { command } = Cli::parse();
    match command {
        Command::Run { file, args: _ } => {
            let file = match read_file(file) {
                Ok(file) => file,
                Err(code) => return code,
            };
            let module_tree = ModuleTree::new();
            if let Err(error) = interpret(&file, module_tree) {
                error.print(&file);
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Command::CompileDebug { file } => {
            let file = match read_file(file) {
                Ok(file) => file,
                Err(code) => return code,
            };
            let mut token_stream = TokenStream::from(file.as_str());
            let program = Program::parse(&mut token_stream).expect("error failed");
            let prelude = prelude::build();
            let prelude_map = PreludeMap::from(&prelude);
            let instructions = compile(program.data, Some(&prelude_map)).expect("error failed");
            dbg!(instructions);
            ExitCode::SUCCESS
        }
        _ => todo!(),
    }
}

fn read_file(file: PathBuf) -> Result<String, ExitCode> {
    match fs::read_to_string(&file) {
        Ok(file) => Ok(file),
        Err(err) => {
            println!(
                "\n\x1b[31;1merror\x1b[0m: Failed to open `{}`. {err}",
                file.to_string_lossy()
            );
            Err(ExitCode::FAILURE)
        }
    }
}
