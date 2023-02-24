use kaede_ast::top::{Fn, Params, Top, TopKind};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::{Location, Span};

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

        let params = self.fn_params()?;

        self.consume(&TokenKind::CloseParen)?;

        let return_ty = {
            if self.check(&TokenKind::OpenBrace) {
                None
            } else {
                Some(self.ty()?)
            }
        };

        let body = self.block()?;

        let finish = self.consume_semi()?.finish;

        Ok(Top {
            kind: TopKind::Fn(Fn {
                name: name.name,
                params,
                body,
                return_ty,
            }),
            span: Span::new(start, finish),
        })
    }

    fn fn_params(&mut self) -> ParseResult<Params> {
        let mut params = Params::new();

        if self.check(&TokenKind::CloseParen) {
            return Ok(params);
        }

        loop {
            params.push((self.ident()?.name, self.ty()?));

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }
}
