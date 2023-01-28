use crate::{ast::ast::Top, lex::token::TokenKind};

use super::{error::ParseResult, parser::Parser};

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    /// None if end of tokens
    pub fn top(&mut self) -> ParseResult<Option<Top>> {
        let token = match self.bump() {
            Some(x) => x,
            None => return Ok(None),
        };

        match token {
            TokenKind::Function => Ok(Some(self.func()?)),

            t => unreachable!("{:?}", t),
        }
    }

    fn func(&mut self) -> ParseResult<Top> {
        let name = self.ident()?;

        self.consume(TokenKind::OpenParen)?;
        self.consume(TokenKind::CloseParen)?;

        self.consume(TokenKind::OpenBrace)?;

        let expr = self.expr()?;

        self.consume(TokenKind::CloseBrace)?;

        Ok(Top::Function { name, body: expr })
    }
}
