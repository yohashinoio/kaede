use kaede_ast::top::{Func, Params, Top, TopEnum};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::{Location, Span, Spanned};

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

        let params = self.func_params()?;

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

        Ok(Spanned::new(
            TopEnum::Func(Func {
                name: name.val,
                params,
                body,
                return_ty,
            }),
            Span::new(start, finish_span.finish),
        ))
    }

    fn func_params(&mut self) -> ParseResult<Params> {
        let mut result = Params::new();

        loop {
            if self.check(&TokenKind::CloseParen) {
                break;
            }

            result.push((self.ident()?.val, self.ty()?));

            if self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(result)
    }
}
