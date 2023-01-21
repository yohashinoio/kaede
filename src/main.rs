use crate::lex::lex;

mod lex;

fn main() {
    let tokens = lex(" 1 23 4810 ");

    println!("{:?}", tokens);
}
