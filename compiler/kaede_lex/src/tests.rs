use kaede_location::{Location, Span};

use crate::TokenKind::*;

use super::*;

fn without_span(it: impl Iterator<Item = Token>) -> Vec<TokenKind> {
    it.map(|t| t.kind).collect()
}

#[test]
fn number() {
    assert_eq!(without_span(lex("4810")), vec![Integer(4810)]);

    assert_eq!(without_span(lex(" 4810  ")), vec![Integer(4810)]);

    assert_eq!(without_span(lex("\t\r4810\n")), vec![Integer(4810)]);
}

#[test]
fn multi_numbers() {
    assert_eq!(
        without_span(lex("48 10  5\r8\t")),
        vec![Integer(48), Integer(10), Integer(5), Integer(8)]
    );
}

#[test]
fn identifier() {
    assert_eq!(
        without_span(lex("yoha io")),
        vec![Ident("yoha".to_string()), Ident("io".to_string())]
    );
}

#[test]
fn punct() {
    assert_eq!(
        without_span(lex("() , {}")),
        vec![OpenParen, CloseParen, Comma, OpenBrace, CloseBrace]
    );
}

#[test]
fn span() {
    let mut r = lex("48\n + 10");

    r.next();

    let t = r.next().unwrap();

    assert_eq!(t.kind, TokenKind::Add);
    assert_eq!(
        t.span,
        Span::new(
            Location { line: 2, column: 2 },
            Location { line: 2, column: 3 }
        )
    );
}

#[test]
fn return_() {
    assert_eq!(without_span(lex(" return\n")), vec![Return]);

    assert_ne!(without_span(lex("ret urn")), vec![Return]);
}
