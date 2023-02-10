use kaede_location::{Location, Span};

use crate::TokenKind::*;

use super::*;

fn without_span(it: impl Iterator<Item = Token>) -> Vec<TokenKind> {
    it.map(|t| t.kind).collect()
}

#[test]
fn number() {
    assert_eq!(without_span(lex("4810")), vec![Integer(4810), Semi]);

    assert_eq!(without_span(lex(" 4810  ")), vec![Integer(4810), Semi]);

    assert_eq!(without_span(lex("\t\r4810")), vec![Integer(4810), Semi]);
}

#[test]
fn multi_numbers() {
    assert_eq!(
        without_span(lex("48 10  5\r8\t")),
        vec![Integer(48), Integer(10), Integer(5), Integer(8), Semi]
    );
}

#[test]
fn identifier() {
    assert_eq!(
        without_span(lex("yoha io")),
        vec![Ident("yoha".to_string()), Ident("io".to_string()), Semi]
    );
}

#[test]
fn punct() {
    assert_eq!(
        without_span(lex("() , {}")),
        vec![OpenParen, CloseParen, Comma, OpenBrace, CloseBrace, Semi]
    );
}

#[test]
fn span() {
    let mut r = lex("\n48 + 10;");

    r.next();

    let t = r.next().unwrap();

    assert_eq!(t.kind, TokenKind::Add);
    assert_eq!(
        t.span,
        Span::new(
            Location { line: 2, column: 4 },
            Location { line: 2, column: 5 }
        )
    );
}

#[test]
fn return_() {
    assert_eq!(without_span(lex(" return")), vec![Return, Semi]);

    assert_ne!(without_span(lex("ret urn")), vec![Return, Semi]);
}

#[test]
fn auto_insert_semi() {
    assert_eq!(
        without_span(lex("48 +\n 10\n")),
        vec![Integer(48), Add, Integer(10), Semi]
    );

    assert_eq!(without_span(lex("return")), vec![Return, Semi]);
}

#[test]
fn auto_inserted_semi_span() {
    let mut r = lex("return");

    r.next();

    let t = r.next().unwrap();

    assert_eq!(t.kind, TokenKind::Semi);
    assert_eq!(
        t.span,
        Span::new(
            Location { line: 1, column: 7 },
            Location { line: 1, column: 8 }
        )
    );
}
