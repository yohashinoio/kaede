use kaede_ast::top::{Access, Fn, Params, Struct, StructField, TopLevel, TopLevelKind};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::Span;

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn top(&mut self) -> ParseResult<TopLevel> {
        let token = self.first();

        let top = match token.kind {
            TokenKind::Fn => {
                // Because the semicolon is consumed before the value is returned, ? is required
                self.func()?
            }

            TokenKind::Struct => {
                // Because the semicolon is consumed before the value is returned, ? is required
                self.struct_()?
            }

            _ => unreachable!("{:?}", token.kind),
        };

        self.consume_semi()?;

        Ok(top)
    }

    fn func(&mut self) -> ParseResult<TopLevel> {
        let start = self.consume(&TokenKind::Fn).unwrap().start;

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

        let span = Span::new(start, body.span.finish);

        Ok(TopLevel {
            kind: TopLevelKind::Fn(Fn {
                name,
                params,
                body,
                return_ty,
                span,
            }),
            span,
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

    fn struct_(&mut self) -> ParseResult<TopLevel> {
        let start = self.consume(&TokenKind::Struct).unwrap().start;

        let name = self.ident()?;

        self.consume(&TokenKind::OpenBrace)?;

        let fields = self.struct_fields()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = Span::new(start, finish);

        Ok(TopLevel {
            kind: TopLevelKind::Struct(Struct { name, fields, span }),
            span,
        })
    }

    fn struct_fields(&mut self) -> ParseResult<Vec<StructField>> {
        let mut fields = Vec::new();

        let mut offset = 0;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            let ty = self.ty()?;

            self.consume_semi()?;

            fields.push(StructField {
                name,
                ty,
                access: Access::Public,
                offset,
            });

            offset += 1;
        }

        Ok(fields)
    }
}
