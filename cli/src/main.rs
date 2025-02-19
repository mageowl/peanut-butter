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
use pbscript_lib::{
    error::Error,
    module_tree::{ExternalModule, ModuleTree},
};
use prelude_map::PreludeMap;
use std_library::prelude;

mod compiler;
mod interpreter;
mod lexer;
mod parser;
mod prelude_map;
mod std_library;

pub fn interpret(
    code: &str,
    mut _module_tree: ModuleTree,
    prelude: &ExternalModule,
) -> Result<(), Error> {
    let mut token_stream = TokenStream::from(code);
    let program = Program::parse(&mut token_stream)?;

    let prelude_map = PreludeMap::from(prelude);
    let instructions = compile(program.data, Some(&prelude_map))?;

    evaluate(
        instructions,
        Some(Rc::new(prelude_map.create_state(prelude))),
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
    Debug {
        file: PathBuf,
        #[arg(short = 'c', long = "compile")]
        compile: bool,
        #[arg(short = 'p', long = "parse")]
        parse: bool,
        #[arg(short = 'l', long = "lexer")]
        lexical: bool,
    },
}

fn main() -> ExitCode {
    let Cli { command } = Cli::parse();
    match command {
        Command::Run { file, args: _ } => {
            let file = match read_file(file) {
                Ok(file) => file,
                Err(code) => return code,
            };

            let prelude = prelude();
            let module_tree = ModuleTree::new();
            if let Err(error) = interpret(&file, module_tree, &prelude) {
                error.print(&file);
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        Command::Debug {
            file,
            compile: flag_compile,
            parse: flag_parse,
            lexical: flag_lexical,
        } => {
            let file = match read_file(file) {
                Ok(file) => file,
                Err(code) => return code,
            };

            let mut token_stream = TokenStream::from(file.as_str());
            if flag_lexical {
                dbg!(token_stream.collect::<Vec<_>>());
                return ExitCode::SUCCESS;
            }

            let program = match Program::parse(&mut token_stream) {
                Ok(p) => p,
                Err(e) => {
                    e.print(&file);
                    return ExitCode::FAILURE;
                }
            };
            if flag_parse {
                dbg!(program);
                return ExitCode::SUCCESS;
            }

            let prelude = prelude();
            let prelude_map = PreludeMap::from(&prelude);
            let instructions = match compile(program.data, Some(&prelude_map)) {
                Ok(t) => t,
                Err(e) => {
                    e.print(&file);
                    return ExitCode::FAILURE;
                }
            };
            if flag_compile {
                dbg!(instructions);
                return ExitCode::SUCCESS;
            }

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
                "\n\x1b[31;1merror:\x1b[0m Failed to open `{}`. {err}",
                file.to_string_lossy()
            );
            Err(ExitCode::FAILURE)
        }
    }
}
