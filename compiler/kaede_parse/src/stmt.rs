use kaede_ast::{
    expr::Expr,
    stmt::{Block, Else, If, Let, Return, Stmt, StmtKind},
};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::Span;
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn block(&mut self) -> ParseResult<Block> {
        let mut body = Vec::new();

        let start = self.consume(&TokenKind::OpenBrace)?.start;

        loop {
            if let Ok(span) = self.consume(&TokenKind::CloseBrace) {
                return Ok(Block {
                    body,
                    span: Span::new(start, span.finish),
                });
            } else if self.check(&TokenKind::Eoi) {
                return Err(ParseError::ExpectedError {
                    expected: TokenKind::CloseBrace.to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                });
            }

            body.push(self.stmt()?);
            self.consume_semi()?;
        }
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        if self.check(&TokenKind::Return) {
            let r = self.return_()?;
            Ok(Stmt {
                span: r.span,
                kind: StmtKind::Return(r),
            })
        } else if self.check(&TokenKind::Let) {
            let l = self.let_()?;
            Ok(Stmt {
                span: l.span,
                kind: StmtKind::Let(l),
            })
        } else if self.check(&TokenKind::If) {
            let i = self.if_()?;
            Ok(Stmt {
                span: i.span,
                kind: StmtKind::If(i),
            })
        } else {
            // Expression statement
            match self.expr() {
                Ok(e) => {
                    let expr_stmt = self.expr_stmt(e);
                    Ok(expr_stmt)
                }

                Err(_) => Err(ParseError::ExpectedError {
                    expected: "statement".to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                }),
            }
        }
    }

    fn expr_stmt(&mut self, e: Expr) -> Stmt {
        Stmt {
            span: e.span,
            kind: StmtKind::Expr(e),
        }
    }

    fn if_(&mut self) -> ParseResult<If> {
        let start = self.consume(&TokenKind::If).unwrap().start;

        let cond = self.expr()?;

        let then = self.block()?;

        let else_ = self.else_()?.map(Box::new);

        let finish = match else_.as_ref() {
            Some(else_) => match else_.as_ref() {
                Else::Block(block) => block.span.finish,
                Else::If(if_) => if_.span.finish,
            },

            None => then.span.finish,
        };

        Ok(If {
            cond,
            then,
            else_,
            span: Span::new(start, finish),
        })
    }

    /// `None` if there is no else
    fn else_(&mut self) -> ParseResult<Option<Else>> {
        if !self.consume_b(&TokenKind::Else) {
            return Ok(None);
        }

        if self.check(&TokenKind::If) {
            return Ok(Some(Else::If(self.if_()?)));
        }

        Ok(Some(Else::Block(self.block()?)))
    }

    fn return_(&mut self) -> ParseResult<Return> {
        let span = self.consume(&TokenKind::Return).unwrap();

        if self.consume_semi_b() {
            return Ok(Return { val: None, span });
        }

        let expr = self.expr()?;

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
