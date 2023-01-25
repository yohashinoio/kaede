use crate::lex::lex;

mod lex;
mod parse;

fn main() {
    let tokens = lex(" fn f(yoha, io) { 4810 }");

    println!("{:?}", tokens);
}
