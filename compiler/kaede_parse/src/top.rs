use kaede_ast::top::{Fn, Import, Params, Struct, StructField, TopLevel, TopLevelKind, Visibility};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::Span;

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn top_level(&mut self) -> ParseResult<TopLevel> {
        let vis = self.consume_b(&TokenKind::Pub).into();

        let token = self.first();

        let (span, kind) = match token.kind {
            TokenKind::Import => {
                let kind = self.import()?;
                (kind.span, TopLevelKind::Import(kind))
            }

            TokenKind::Fn => {
                let kind = self.func()?;
                (kind.span, TopLevelKind::Fn(kind))
            }

            TokenKind::Struct => {
                let kind = self.struct_()?;
                (kind.span, TopLevelKind::Struct(kind))
            }

            _ => unreachable!("{:?}", token.kind),
        };

        self.consume_semi()?;

        Ok(TopLevel { kind, vis, span })
    }

    fn import(&mut self) -> ParseResult<Import> {
        let start = self.consume(&TokenKind::Import).unwrap().start;

        let modpath = self.ident()?;

        let span = Span::new(start, modpath.span.finish);

        Ok(Import { modpath, span })
    }

    fn func(&mut self) -> ParseResult<Fn> {
        let start = self.consume(&TokenKind::Fn).unwrap().start;

        let name = self.ident()?;

        self.consume(&TokenKind::OpenParen)?;

        let params = self.fn_params()?;

        self.consume(&TokenKind::CloseParen)?;

        let return_ty = if self.consume_b(&TokenKind::Arrow) {
            Some(self.ty()?)
        } else {
            None
        };

        let body = self.block()?;

        let span = Span::new(start, body.span.finish);

        Ok(Fn {
            name,
            params,
            body,
            return_ty,
            span,
        })
    }

    fn fn_params(&mut self) -> ParseResult<Params> {
        let mut params = Params::new();

        if self.check(&TokenKind::CloseParen) {
            return Ok(params);
        }

        loop {
            let mutability = self.consume_b(&TokenKind::Mut).into();

            let name = self.ident()?;

            self.consume(&TokenKind::Colon)?;

            let ty = self.ty()?;

            params.push((name, mutability, ty));

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    fn struct_(&mut self) -> ParseResult<Struct> {
        let start = self.consume(&TokenKind::Struct).unwrap().start;

        let name = self.ident()?;

        self.consume(&TokenKind::OpenBrace)?;

        let fields = self.struct_fields()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = Span::new(start, finish);

        Ok(Struct { name, fields, span })
    }

    fn struct_fields(&mut self) -> ParseResult<Vec<StructField>> {
        let mut fields = Vec::new();

        let mut offset = 0;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            self.consume(&TokenKind::Colon)?;

            let ty = self.ty()?;

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }

            fields.push(StructField {
                name,
                ty,
                vis: Visibility::Public,
                offset,
            });

            offset += 1;
        }

        Ok(fields)
    }
}
