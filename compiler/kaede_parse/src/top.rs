use kaede_ast::top::{Func, Top, TopEnum};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::{spanned, Location, Span};

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// None if end of tokens
    pub fn top(&mut self) -> ParseResult<Option<Top>> {
        let token = match self.bump() {
            Some(x) => x,
            None => return Ok(None),
        };

        match token.kind {
            TokenKind::Function => Ok(Some(self.func(token.span.start)?)),

            t => unreachable!("{:?}", t),
        }
    }

    fn func(&mut self, start: Location) -> ParseResult<Top> {
        let name = self.ident()?;

        self.consume(&TokenKind::OpenParen)?;
        self.consume(&TokenKind::CloseParen)?;

        let return_ty = {
            if self.check(&TokenKind::OpenBrace) {
                None
            } else {
                Some(self.ty()?)
            }
        };

        let body = self.stmt_list()?;

        let finish_span = self.consume_semi_s()?;

        Ok(spanned(
            TopEnum::Func(Func {
                name: name.val,
                body,
                return_ty,
            }),
            Span::new(start, finish_span.finish),
        ))
    }
}
