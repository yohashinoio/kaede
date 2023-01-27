use crate::{
    ast::ast::{Function, Top},
    lex::token::TokenKind,
};

use super::tokencursor::TokenCursor;

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
    /// None if end of tokens
    pub fn parse_top(&mut self) -> anyhow::Result<Option<Top>> {
        let token = match self.bump() {
            Some(x) => x,
            None => return Ok(None),
        };

        match token {
            TokenKind::Function => Ok(Some(Top::Function(self.parse_fn()?))),

            t => unreachable!("{:?}", t),
        }
    }

    fn parse_fn(&mut self) -> anyhow::Result<Function> {
        let name = self.parse_ident()?;

        self.consume(TokenKind::OpenParen)?;
        self.consume(TokenKind::CloseParen)?;

        self.consume(TokenKind::OpenBrace)?;

        let expr = self.parse_expr()?;

        self.consume(TokenKind::CloseBrace)?;

        Ok(Function {
            name: name,
            body: expr,
        })
    }
}
