use std::fs;

use lexer::TokenStream;
use parser::{Parse, statement::Block};
use vm::{VirtualMachine, bytecode::OpCode};

mod error;
mod lexer;
mod parser;
mod span;
pub mod vm;

fn main() {
    // let file = fs::read_to_string("test/all.pb").unwrap();
    // let mut tokens = TokenStream::from(file.as_str());
    // let ast = Block::parse(&mut tokens);

    // match ast {
    //     Ok(ast) => {
    //         dbg!(ast);
    //     }
    //     Err(e) => e.display(&file),
    // }

    let constants: [u8; 2] = [1, 2];
    #[rustfmt::skip]
    let instructions = vec![
                                // index    // size
        OpCode::Constant as u8, 0x00, 0x00, 0x01, 0x00,
        OpCode::Constant as u8, 0x01, 0x00, 0x01, 0x00,
        OpCode::U8Add as u8,
        OpCode::Pop as u8,
        OpCode::ReturnVoid as u8,
    ];

    let mut vm: VirtualMachine<256, 2> = VirtualMachine::new(instructions, constants);
    vm.finish();
}
