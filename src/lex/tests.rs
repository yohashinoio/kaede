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

#[test]
fn identifier() {
    assert_eq!(
        lex("yoha io"),
        vec![Ident(String::from("yoha")), Ident(String::from("io")), Eof]
    );
}

#[test]
fn punct() {
    assert_eq!(lex("() ,"), vec![OpenParen, CloseParen, Comma, Eof]);
}
