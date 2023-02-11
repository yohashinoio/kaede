use kaede_ast::Top;
use kaede_lex::token::{Token, TokenKind};

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// None if end of tokens
    pub fn top(&mut self) -> ParseResult<Option<Top>> {
        let token = match self.bump() {
            Some(x) => x,
            None => return Ok(None),
        };

        match token.kind {
            TokenKind::Function => Ok(Some(self.func()?)),

            t => unreachable!("{:?}", t),
        }
    }

    fn func(&mut self) -> ParseResult<Top> {
        let name = self.ident()?;

        self.consume(&TokenKind::OpenParen)?;
        self.consume(&TokenKind::CloseParen)?;

        let body = self.stmt_list()?;

        self.consume_semi()?;

        Ok(Top::Function { name, body: body })
    }
}
