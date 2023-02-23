use kaede_ast::expr::Expr;
use kaede_ast::stmt::{Let, Return, Stmt, StmtEnum, StmtList};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::{Span, Spanned};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn stmt_list(&mut self) -> ParseResult<StmtList> {
        let mut result = StmtList::new();

        self.consume(&TokenKind::OpenBrace)?;

        loop {
            if self.consume_b(&TokenKind::CloseBrace) {
                return Ok(result);
            } else if self.check(&TokenKind::Eoi) {
                return Err(ParseError::ExpectedError {
                    expected: TokenKind::CloseBrace.to_string(),
                    but: self.first().kind.clone(),
                    span: self.first().span.clone(),
                });
            }

            result.push(self.stmt()?);
        }
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        let result;

        if self.check(&TokenKind::Return) {
            let r = self.return_()?;
            result = Spanned::new(StmtEnum::Return(r.val), r.span);
        } else if self.check(&TokenKind::Let) {
            let l = self.let_()?;
            result = Spanned::new(StmtEnum::Let(l.val), l.span);
        } else {
            match self.expr() {
                Ok(e) => result = self.expr_stmt(e)?,

                Err(_) => {
                    return Err(ParseError::ExpectedError {
                        expected: "statement".to_string(),
                        but: self.first().kind.clone(),
                        span: self.first().span.clone(),
                    })
                }
            }
        }

        self.consume_semi()?;

        Ok(result)
    }

    fn expr_stmt(&mut self, e: Expr) -> ParseResult<Stmt> {
        let span = e.span.clone();

        Ok(Spanned::new(StmtEnum::Expr(e), span))
    }

    fn return_(&mut self) -> ParseResult<Spanned<Return>> {
        let span = self.consume_s(&TokenKind::Return).unwrap();

        if self.consume_semi_b() {
            return Ok(Spanned::new(Return(None), span));
        }

        let expr = self.expr()?;

        self.consume_semi()?;

        Ok(Spanned::new(
            Return(Some(Spanned::new(expr.val, expr.span.clone()))),
            Span::new(span.start, expr.span.finish),
        ))
    }

    fn let_(&mut self) -> ParseResult<Spanned<Let>> {
        let span = self.consume_s(&TokenKind::Let).unwrap();

        let mutability = if self.consume_b(&TokenKind::Mut) {
            Mutability::Mut
        } else {
            Mutability::Not
        };

        let ident = self.ident()?;

        if self.consume_b(&TokenKind::Eq) {
            let init = self.expr()?;

            let finish_loc = init.span.finish.clone();

            return Ok(Spanned::new(
                Let {
                    name: ident.val,
                    init: Some(init),
                    ty: Ty::new(make_fundamental_type(FundamentalTypeKind::I32), mutability),
                },
                Span::new(span.start, finish_loc),
            ));
        }

        Ok(Spanned::new(
            Let {
                name: ident.val,
                init: None,
                ty: Ty::new(make_fundamental_type(FundamentalTypeKind::I32), mutability),
            },
            Span::new(span.start, ident.span.finish),
        ))
    }
}
