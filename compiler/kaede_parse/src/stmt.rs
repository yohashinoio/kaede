use kaede_ast::{
    expr::Expr,
    stmt::{Let, Return, Stmt, StmtKind, StmtList},
};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::Span;
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
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                });
            }

            result.push(self.stmt()?);
        }
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        let result;

        if self.check(&TokenKind::Return) {
            let r = self.return_()?;
            result = Stmt {
                span: r.span,
                kind: StmtKind::Return(r),
            };
        } else if self.check(&TokenKind::Let) {
            let l = self.let_()?;
            result = Stmt {
                span: l.span,
                kind: StmtKind::Let(l),
            }
        } else {
            match self.expr() {
                Ok(e) => result = self.expr_stmt(e),

                Err(_) => {
                    return Err(ParseError::ExpectedError {
                        expected: "statement".to_string(),
                        but: self.first().kind.to_string(),
                        span: self.first().span,
                    })
                }
            }
        }

        self.consume_semi()?;

        Ok(result)
    }

    fn expr_stmt(&mut self, e: Expr) -> Stmt {
        Stmt {
            span: e.span,
            kind: StmtKind::Expr(e),
        }
    }

    fn return_(&mut self) -> ParseResult<Return> {
        let span = self.consume(&TokenKind::Return).unwrap();

        if self.consume_semi_b() {
            return Ok(Return { val: None, span });
        }

        let expr = self.expr()?;

        self.consume_semi()?;

        Ok(Return {
            span: Span::new(span.start, expr.span.finish),
            val: Some(expr),
        })
    }

    fn let_(&mut self) -> ParseResult<Let> {
        let start = self.consume(&TokenKind::Let).unwrap().start;

        let mutability = if self.consume_b(&TokenKind::Mut) {
            Mutability::Mut
        } else {
            Mutability::Not
        };

        let ident = self.ident()?;

        if self.consume_b(&TokenKind::Eq) {
            let init = self.expr()?;

            let finish = init.span.finish;

            return Ok(Let {
                name: ident.name,
                init: Some(init),
                ty: Ty::new(make_fundamental_type(FundamentalTypeKind::I32), mutability),
                span: Span::new(start, finish),
            });
        }

        Ok(Let {
            name: ident.name,
            init: None,
            ty: Ty::new(make_fundamental_type(FundamentalTypeKind::I32), mutability),
            span: Span::new(start, ident.span.finish),
        })
    }
}
