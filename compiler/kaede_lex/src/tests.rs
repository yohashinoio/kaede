use kaede_location::{Location, Span};

use crate::TokenKind::*;

use super::*;

fn without_span(it: impl Iterator<Item = Token>) -> Vec<TokenKind> {
    it.map(|t| t.kind).collect()
}

fn lex_test(program: &str, expect: Vec<TokenKind>) {
    assert_eq!(without_span(lex(program)), expect);
}

#[test]
fn number() {
    lex_test("4810", vec![Integer(4810.to_string()), Semi]);
}

#[test]
fn skip_whitespaces() {
    lex_test("  \n  ", vec![]);

    lex_test("\t\r", vec![]);
}

#[test]
fn multi_numbers() {
    lex_test(
        "48 10 5 8",
        vec![
            Integer(48.to_string()),
            Integer(10.to_string()),
            Integer(5.to_string()),
            Integer(8.to_string()),
            Semi,
        ],
    );
}

#[test]
fn identifier() {
    lex_test(
        "yoha io",
        vec![Ident("yoha".to_string()), Ident("io".to_string()), Semi],
    );
}

#[test]
fn punct() {
    lex_test(
        "() , {}",
        vec![OpenParen, CloseParen, Comma, OpenBrace, CloseBrace, Semi],
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
fn auto_insert_semi() {
    lex_test(
        "48 +\n 10\n",
        vec![Integer(48.to_string()), Add, Integer(10.to_string()), Semi],
    );

    lex_test("return", vec![Return, Semi]);
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
