use crate::{ast::ast::Top, lex::token::TokenKind};

use super::*;

#[test]
fn function() {
    assert_eq!(
        parse(vec![TokenKind::Function, TokenKind::Ident(String::from("f"))].into_iter()),
        vec![Top::Function {
            name: String::from("f")
        }]
    );
}
