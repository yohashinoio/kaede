use kaede_ast::{
    expr::Expr,
    stmt::{Assign, AssignKind, Block, Break, Else, If, Let, Loop, Return, Stmt, StmtKind},
};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::Span;
use kaede_type::{Mutability, Ty, TyKind};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Consume a semicolon immediately following each statement
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
            // Consume a semicolon immediately following each statement
            self.consume_semi()?;
        }
    }

    /// Semicolons are **not** consumed
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
        } else if self.check(&TokenKind::Loop) {
            let l = self.loop_()?;
            Ok(Stmt {
                span: l.span,
                kind: StmtKind::Loop(l),
            })
        } else if self.check(&TokenKind::Break) {
            let b = self.break_()?;
            Ok(Stmt {
                span: b.span,
                kind: StmtKind::Break(b),
            })
        } else {
            let expr = self.expr()?;

            // Assignment statement
            if let Some(kind) = self.assign_ops() {
                let rhs = self.expr()?;

                let span = Span::new(expr.span.start, rhs.span.finish);

                return Ok(Stmt {
                    kind: StmtKind::Assign(Assign {
                        lhs: expr,
                        kind,
                        rhs,
                        span,
                    }),
                    span,
                });
            }

            // Expression statement
            let expr_stmt = self.expr_stmt(expr);
            Ok(expr_stmt)
        }
    }

    fn expr_stmt(&mut self, e: Expr) -> Stmt {
        Stmt {
            span: e.span,
            kind: StmtKind::Expr(e),
        }
    }

    /// Assignment operators
    fn assign_ops(&mut self) -> Option<AssignKind> {
        if self.consume_b(&TokenKind::Assign) {
            return Some(AssignKind::Simple);
        }

        None
    }

    fn break_(&mut self) -> ParseResult<Break> {
        Ok(Break {
            span: self.consume(&TokenKind::Break).unwrap(),
        })
    }

    fn loop_(&mut self) -> ParseResult<Loop> {
        let start = self.consume(&TokenKind::Loop).unwrap().start;

        let body = self.block()?;

        Ok(Loop {
            span: Span::new(start, body.span.finish),
            body,
        })
    }

    /// Needed to **avoid confusion** between struct literals and block statements
    ///
    /// if x {}
    ///
    /// In such code as above,
    /// `then block` of `if statement` is not parsed as an initializer of a struct literals
    fn cond_expr(&mut self) -> ParseResult<Expr> {
        self.in_cond_expr = true;
        let cond = self.expr();
        self.in_cond_expr = false;
        cond
    }

    fn if_(&mut self) -> ParseResult<If> {
        let start = self.consume(&TokenKind::If).unwrap().start;

        let cond = self.cond_expr()?;

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

        if self.check_semi() {
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

        let name = self.ident()?;

        let ty = match self.ty() {
            Ok(ty) => ty,
            Err(_) => Ty::new(TyKind::Unknown, mutability),
        };

        if self.consume_b(&TokenKind::Assign) {
            let init = self.expr()?;

            let finish = init.span.finish;

            return Ok(Let {
                name,
                init: Some(init),
                ty,
                span: Span::new(start, finish),
            });
        }

        if ty.kind.is_unknown() {
            // Error if both type and initializer are missing
            self.consume(&TokenKind::Assign)?;
        }

        Ok(Let {
            span: Span::new(start, name.span.finish),
            name,
            init: None,
            ty,
        })
    }
}
