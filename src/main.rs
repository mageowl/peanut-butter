use std::fs;

use compiler::Compiler;
use lexer::TokenStream;
use parser::{Parse, statement::Block};
use vm::{VirtualMachine, bytecode::OpCode};

pub mod compiler;
mod error;
mod lexer;
mod parser;
mod span;
pub mod vm;

#[cfg(feature = "16bit")]
#[expect(non_camel_case_types)]
pub type uarch = u16;
#[cfg(not(feature = "16bit"))]
#[expect(non_camel_case_types)]
pub type uarch = u64;

fn main() {
    let file = fs::read_to_string("test/all.pb").unwrap();
    let mut tokens = TokenStream::from(file.as_str());
    let ast = Block::parse(&mut tokens);

    // match ast {
    //     Ok(ast) => {
    //         dbg!(ast);
    //     }
    //     Err(e) => e.display(&file),
    // }

    // let constants: [u8; 2] = [1, 2];
    // #[rustfmt::skip]
    // let instructions = vec![
    //                          // index, size
    //     OpCode::Constant as u8, 0x00,  0x01, 0x00,
    //     OpCode::Constant as u8, 0x01,  0x01, 0x00,
    //     OpCode::U8Add as u8,
    //     OpCode::Pop as u8,
    //     OpCode::ReturnVoid as u8,
    // ];

    // let mut vm: VirtualMachine<256, 2> = VirtualMachine::new(instructions, constants);
    // vm.finish();
}
