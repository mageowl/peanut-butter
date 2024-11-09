use std::{env::args, fs};

use lexer::TokenStream;
use parser::{Parse, Program};

mod lexer;
mod parser;

fn main() {
    let path = args().skip(1).next().unwrap();
    let file = fs::read_to_string(path).expect("failed to open file");
    let mut token_stream = TokenStream::from(file.as_str());

    println!("{:#?}", Program::parse(&mut token_stream));
    //dbg!(token_stream.collect::<Vec<_>>());
}
