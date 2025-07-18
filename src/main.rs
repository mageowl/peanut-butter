use std::{fs, process};

use compiler::Compiler;
use error::Error;
use lexer::TokenStream;
use parser::{Parse, statement::Block};
use vm::VirtualMachine;

pub mod compiler;
mod error;
mod lexer;
mod parser;
mod span;
pub mod vm;

#[cfg(feature = "16bit")]
#[expect(non_camel_case_types)]
mod arch {
    pub type uarch = u16;
    pub type iarch = i16;
    pub const ARCH_BYTES: usize = 2;
}
#[cfg(not(feature = "16bit"))]
#[expect(non_camel_case_types)]
mod arch {
    pub type uarch = u64;
    pub type iarch = i64;
    pub const ARCH_BYTES: usize = 8;
}

fn main() {
    let file = fs::read_to_string("test/goober.pb").unwrap();
    let mut tokens = TokenStream::from(file.as_str());
    let ast = handle_error(Block::parse(&mut tokens), &file);

    let mut compiler = Compiler::new();
    handle_error(compiler.compile_program(ast), &file);
    let mut vm: VirtualMachine = compiler.create_vm();
    vm.run();
}

fn handle_error<T>(res: Result<T, Error>, src: &str) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            e.display(src);
            process::exit(1)
        }
    }
}
