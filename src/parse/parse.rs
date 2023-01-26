use crate::{
    ast::ast::{Function, Top, TranslationUnit},
    lex::token::TokenKind,
};

use super::tokencursor::TokenCursor;

pub fn parse<T>(tokens: T) -> TranslationUnit
where
    T: Iterator<Item = TokenKind>,
{
    let mut cursor = TokenCursor::new(tokens.peekable());

    let mut result = Vec::new();

    loop {
        match cursor.parse_top() {
            Some(top) => result.push(top),
            None => return result,
        }
    }
}

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
    fn parse_top(&mut self) -> Option<Top> {
        let token = match self.bump() {
            Some(x) => x,
            None => return None,
        };

        match token {
            TokenKind::Function => Some(Top::Function(self.parse_fn()?)),

            t => unreachable!("{:?}", t),
        }
    }

    fn parse_fn(&mut self) -> Option<Function> {
        let name = self.parse_ident()?;

        self.consume(TokenKind::OpenParen)?;
        self.consume(TokenKind::CloseParen)?;

        self.consume(TokenKind::OpenBrace)?;
        self.consume(TokenKind::CloseBrace)?;

        Some(Function { name: name })
    }

    fn parse_ident(&mut self) -> Option<String> {
        let is_ident = match self.first() {
            TokenKind::Ident(_) => true,
            _ => false,
        };

        if is_ident {
            if let TokenKind::Ident(ident) = self.bump().unwrap() {
                return Some(ident);
            }
        }

        None
    }

    fn consume(&mut self, tok: TokenKind) -> Option<()> {
        if self.first() == &tok {
            self.bump();
            return Some(());
        }

        return None;
    }
}
