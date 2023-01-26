use crate::{lex::lex, parse::parse};

mod ast;
mod lex;
mod parse;

fn main() {
    let tokens = lex(" fn f");

    let ast = parse(tokens);

    println!("{:?}", ast);
}
