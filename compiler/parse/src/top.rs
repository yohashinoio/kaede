use std::{collections::VecDeque, rc::Rc};

use kaede_ast::top::{
    Enum, EnumVariant, Fn, GenericParams, Impl, Import, Param, Params, Struct, StructField,
    TopLevel, TopLevelKind, Visibility,
};
use kaede_lex::token::TokenKind;
use kaede_span::Span;

use crate::{error::ParseResult, Parser};

impl Parser {
    pub fn top_level(&mut self) -> ParseResult<TopLevel> {
        let vis = self.consume_b(&TokenKind::Pub).into();

        let token = self.first();

        let (span, kind) = match token.kind {
            TokenKind::Import => {
                let kind = self.import()?;
                (kind.span, TopLevelKind::Import(kind))
            }

            TokenKind::Fn => {
                let kind = self.function()?;
                (kind.span, TopLevelKind::Fn(kind))
            }

            TokenKind::Struct => {
                let kind = self.struct_()?;
                (kind.span, TopLevelKind::Struct(kind))
            }

            TokenKind::Impl => {
                let kind = self.impl_()?;
                (kind.span, TopLevelKind::Impl(kind))
            }

            TokenKind::Enum => {
                let kind = self.enum_()?;
                (kind.span, TopLevelKind::Enum(kind))
            }

            _ => unreachable!("{:?}", token.kind),
        };

        self.consume_semi()?;

        Ok(TopLevel { kind, vis, span })
    }

    fn generic_params(&mut self) -> ParseResult<GenericParams> {
        let start = self.consume(&TokenKind::Lt)?.start;

        let mut names = Vec::new();

        loop {
            names.push(self.ident()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::Gt)?.finish;

        Ok(GenericParams {
            names,
            span: Span::new(start, finish),
        })
    }

    fn impl_(&mut self) -> ParseResult<Impl> {
        let start = self.consume(&TokenKind::Impl).unwrap().start;

        let name = self.ident()?;

        self.consume(&TokenKind::OpenBrace)?;

        let mut items = Vec::new();

        loop {
            if let Ok(span) = self.consume(&TokenKind::CloseBrace) {
                return Ok(Impl {
                    name,
                    items,
                    span: Span::new(start, span.finish),
                });
            }

            items.push(self.top_level()?);
        }
    }

    fn import(&mut self) -> ParseResult<Import> {
        let start = self.consume(&TokenKind::Import).unwrap().start;

        let module_path = self.ident()?;

        let span = Span::new(start, module_path.span().finish);

        Ok(Import { module_path, span })
    }

    fn function(&mut self) -> ParseResult<Fn> {
        let start = self.consume(&TokenKind::Fn).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            Some(self.generic_params()?)
        } else {
            None
        };

        let params_start = self.consume(&TokenKind::OpenParen)?.start;

        let mutability = self.consume_b(&TokenKind::Mut).into();
        let has_this = self.consume_b(&TokenKind::This);
        let first_param = if has_this {
            let _ = self.consume(&TokenKind::Comma);
            None
        } else if self.check(&TokenKind::CloseParen) {
            None
        } else {
            let mut param = self.fn_param()?;
            let _ = self.consume(&TokenKind::Comma);
            param.mutability = mutability;
            Some(param)
        };

        let params = {
            let mut params = self.fn_params()?;
            if let Some(first_param) = first_param {
                params.push_front(first_param);
            }
            params
        };

        let params_finish = self.consume(&TokenKind::CloseParen)?.finish;

        let params = Params(params, Span::new(params_start, params_finish));

        let return_ty = if self.consume_b(&TokenKind::Colon) {
            Some(self.ty()?)
        } else {
            None
        };

        let body = self.block()?;

        let span = Span::new(start, body.span.finish);

        Ok(Fn {
            this: if has_this { Some(mutability) } else { None },
            name,
            generic_params,
            params,
            body,
            return_ty,
            span,
        })
    }

    fn fn_params(&mut self) -> ParseResult<VecDeque<Param>> {
        let mut params = VecDeque::new();

        if self.check(&TokenKind::CloseParen) {
            return Ok(params);
        }

        loop {
            params.push_back(self.fn_param()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    fn fn_param(&mut self) -> ParseResult<Param> {
        let mutability = self.consume_b(&TokenKind::Mut).into();

        let name = self.ident()?;

        self.consume(&TokenKind::Colon)?;

        let ty = self.ty()?;

        Ok(Param {
            name,
            mutability,
            ty,
        })
    }

    fn enum_(&mut self) -> ParseResult<Enum> {
        let start = self.consume(&TokenKind::Enum).unwrap().start;

        let name = self.ident()?;

        self.consume(&TokenKind::OpenBrace)?;

        let variants = self.enum_variants()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        Ok(Enum {
            name,
            variants,
            span: Span::new(start, finish),
        })
    }

    fn enum_variants(&mut self) -> ParseResult<Vec<EnumVariant>> {
        let mut variants = Vec::new();

        let mut offset = 0;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            let ty = if self.consume_b(&TokenKind::OpenParen) {
                let ty = self.ty()?;
                self.consume(&TokenKind::CloseParen)?;
                Some(ty)
            } else {
                None
            };

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }

            variants.push(EnumVariant {
                name,
                ty,
                vis: Visibility::Public,
                offset,
            });

            offset += 1;
        }

        Ok(variants)
    }

    fn struct_(&mut self) -> ParseResult<Struct> {
        let start = self.consume(&TokenKind::Struct).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            Some(self.generic_params()?)
        } else {
            None
        };

        self.consume(&TokenKind::OpenBrace)?;

        let fields = self.struct_fields()?;

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = Span::new(start, finish);

        Ok(Struct {
            name,
            generic_params,
            fields,
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

            self.consume(&TokenKind::Colon)?;

            let ty = Rc::new(self.ty()?);

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
