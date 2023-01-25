use crate::{
    ast::ast::{Top, TranslationUnit},
    lex::{token::TokenKind, LexResult},
};

use super::tokencursor::TokenCursor;

pub fn parse(lex_result: LexResult) -> TranslationUnit {
    let mut cursor = TokenCursor::new(lex_result.into_iter().peekable());

    let mut result = Vec::new();

    loop {
        match cursor.parse_top() {
            Some(top) => result.push(top),
            None => return result,
        }
    }
}

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
    pub fn parse_top(&mut self) -> Option<Top> {
        let token = match self.bump() {
            Some(x) => x,
            None => return None,
        };

        match token {
            TokenKind::Function => Some(Top::Function {
                name: self.parse_ident()?,
            }),

            TokenKind::Eof => return None,

            t => unreachable!("{:?}", t),
        }
    }

    fn parse_ident(&mut self) -> Option<String> {
        let is_ident = match self.point() {
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
}
