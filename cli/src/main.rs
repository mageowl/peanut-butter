use std::{env::args, fs};

mod lexer;
mod parser;

fn main() {
    let mut args = args();
    args.next();
    let file = fs::read_to_string(args.next().unwrap()).unwrap();
    let tokens = lexer::TokenStream::from(file.as_str());
    dbg!(tokens.collect::<Vec<_>>());
}
