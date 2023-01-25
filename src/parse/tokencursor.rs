use std::iter::Peekable;

use crate::lex::token::TokenKind;

pub struct TokenCursor<T: Iterator<Item = TokenKind>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self { tokens: tokens }
    }

    // Returns the current cursor target.
    pub fn point(&mut self) -> &TokenKind {
        self.tokens.peek().unwrap_or(&TokenKind::Eof)
    }

    pub fn bump(&mut self) -> Option<TokenKind> {
        self.tokens.next()
    }
}
