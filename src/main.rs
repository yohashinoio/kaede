use crate::lex::lex;

mod lex;

fn main() {
    let tokens = lex(" fn f(yoha, io) { 4810 }");

    println!("{:?}", tokens);
}
