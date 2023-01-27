use std::iter::Peekable;

use crate::lex::token::TokenKind;

pub struct Parser<T: Iterator<Item = TokenKind>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self { tokens }
    }

    pub fn first(&mut self) -> &TokenKind {
        self.tokens.peek().unwrap_or(&TokenKind::Eof)
    }

    pub fn bump(&mut self) -> Option<TokenKind> {
        self.tokens.next()
    }
}
