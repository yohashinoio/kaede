use crate::TokenKind::*;

use super::*;

#[test]
fn number() {
    assert_eq!(lex("4810").collect::<Vec<_>>(), vec![Integer(4810)]);

    assert_eq!(lex(" 4810  ").collect::<Vec<_>>(), vec![Integer(4810)]);

    assert_eq!(lex("\t\r4810\n").collect::<Vec<_>>(), vec![Integer(4810)]);
}

#[test]
fn multi_numbers() {
    assert_eq!(
        lex("48 10  5\r8\t").collect::<Vec<_>>(),
        vec![Integer(48), Integer(10), Integer(5), Integer(8)]
    );
}

#[test]
fn identifier() {
    assert_eq!(
        lex("yoha io").collect::<Vec<_>>(),
        vec![Ident("yoha".to_string()), Ident("io".to_string())]
    );
}

#[test]
fn punct() {
    assert_eq!(
        lex("() , {}").collect::<Vec<_>>(),
        vec![OpenParen, CloseParen, Comma, OpenBrace, CloseBrace]
    );
}
