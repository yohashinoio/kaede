use std::{collections::VecDeque, rc::Rc};

use kaede_ast::top::{
    Enum, EnumVariant, Extern, Fn, FnDecl, GenericParams, Impl, Import, Param, Params, Struct,
    StructField, TopLevel, TopLevelKind, Visibility,
};
use kaede_lex::token::TokenKind;
use kaede_span::Location;

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
                let kind = self.func()?;
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

            TokenKind::Extern => {
                let kind = self.extern_()?;
                (kind.span, TopLevelKind::Extern(kind))
            }

            _ => unreachable!("{:?}", token.kind),
        };

        self.consume_semi()?;

        self.generic_param_names.clear();

        Ok(TopLevel { kind, vis, span })
    }

    fn extern_(&mut self) -> ParseResult<Extern> {
        let start = self.consume(&TokenKind::Extern).unwrap().start;

        let lang_linkage = self.string_literal_internal();

        let fn_decl = self.fn_decl()?;

        Ok(Extern {
            span: self.new_span(start, fn_decl.span.finish),
            lang_linkage,
            fn_decl,
        })
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

        names.iter().for_each(|name| {
            self.generic_param_names.push(name.symbol());
        });

        Ok(GenericParams {
            names,
            span: self.new_span(start, finish),
        })
    }

    fn impl_(&mut self) -> ParseResult<Impl> {
        let start = self.consume(&TokenKind::Impl).unwrap().start;

        let (ty, _) = self.ty()?;

        self.consume(&TokenKind::OpenBrace)?;

        let mut items = Vec::new();

        loop {
            if let Ok(span) = self.consume(&TokenKind::CloseBrace) {
                return Ok(Impl {
                    ty,
                    items,
                    span: self.new_span(start, span.finish),
                });
            }

            items.push(self.top_level()?);
        }
    }

    fn import(&mut self) -> ParseResult<Import> {
        let start = self.consume(&TokenKind::Import).unwrap().start;

        let module_path = self.ident()?;

        if !self.imported_modules.contains(&module_path.symbol()) {
            self.imported_modules.push(module_path.symbol());
        }

        let span = self.new_span(start, module_path.span().finish);

        Ok(Import { module_path, span })
    }

    fn fn_decl(&mut self) -> ParseResult<FnDecl> {
        let start = self.consume(&TokenKind::Fn).unwrap().start;

        let name = self.ident()?;

        let generic_params = if self.check(&TokenKind::Lt) {
            Some(self.generic_params()?)
        } else {
            None
        };

        let params_start = self.consume(&TokenKind::OpenParen)?.start;

        let mutability = self.consume_b(&TokenKind::Mut).into();
        let has_self = self.consume_b(&TokenKind::Self_);
        let first_param = if has_self {
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
            let mut params = self.fn_params(params_start)?;
            if let Some(first_param) = first_param {
                params.v.push_front(first_param);
            }
            params
        };

        let (return_ty, finish) = if let Ok(span) = self.consume(&TokenKind::Colon) {
            (Some(self.ty()?.0), span.finish)
        } else {
            (None, params.span.finish)
        };

        Ok(FnDecl {
            self_: if has_self { Some(mutability) } else { None },
            name,
            generic_params,
            params,
            return_ty,
            span: self.new_span(start, finish),
        })
    }

    fn func(&mut self) -> ParseResult<Fn> {
        let decl = self.fn_decl()?;

        let body = Rc::new(self.block()?);

        let span = self.new_span(decl.span.start, body.span.finish);

        Ok(Fn { decl, body, span })
    }

    fn fn_params(&mut self, span_start: Location) -> ParseResult<Params> {
        let mut params = VecDeque::new();

        if let Ok(span) = self.consume(&TokenKind::CloseParen) {
            return Ok(Params {
                v: params,
                span: self.new_span(span_start, span.finish),
                is_var_args: false,
            });
        }

        loop {
            if self.var_arg().is_some() {
                let finish = self.consume(&TokenKind::CloseParen)?.finish;
                break Ok(Params {
                    v: params,
                    span: self.new_span(span_start, finish),
                    is_var_args: true,
                });
            }

            params.push_back(self.fn_param()?);

            if !self.consume_b(&TokenKind::Comma) {
                let finish = self.consume(&TokenKind::CloseParen)?.finish;
                break Ok(Params {
                    v: params,
                    span: self.new_span(span_start, finish),
                    is_var_args: false,
                });
            }
        }
    }

    fn fn_param(&mut self) -> ParseResult<Param> {
        let mutability = self.consume_b(&TokenKind::Mut).into();

        let name = self.ident()?;

        self.consume(&TokenKind::Colon)?;

        let (ty, _) = self.ty()?;

        Ok(Param {
            name,
            mutability,
            ty: Rc::new(ty),
        })
    }

    fn var_arg(&mut self) -> Option<()> {
        if self.consume_b(&TokenKind::DotDotDot) {
            Some(())
        } else {
            None
        }
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
            span: self.new_span(start, finish),
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
                let (ty, _) = self.ty()?;
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

        let span = self.new_span(start, finish);

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

            let (ty, _) = self.ty()?;

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }

            fields.push(StructField {
                name,
                ty: Rc::new(ty),
                vis: Visibility::Public,
                offset,
            });

            offset += 1;
        }

        Ok(fields)
    }
}
