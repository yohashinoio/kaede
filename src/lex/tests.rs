use crate::lex::token::TokenKind::*;

use super::*;

#[test]
fn number() {
    assert_eq!(lex("4810"), vec![Integer(4810), Eof]);

    assert_eq!(lex(" 4810  "), vec![Integer(4810), Eof]);

    assert_eq!(lex("\t\r4810\n"), vec![Integer(4810), Eof]);
}

#[test]
fn multi_numbers() {
    assert_eq!(
        lex("48 10  5\r8\t"),
        vec![Integer(48), Integer(10), Integer(5), Integer(8), Eof]
    );
}
