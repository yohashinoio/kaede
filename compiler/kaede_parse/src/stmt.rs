use std::rc::Rc;

use kaede_ast::{
    expr::Expr,
    stmt::{
        Assign, AssignKind, Block, Break, Else, If, Let, LetKind, Loop, NormalLet, Return, Stmt,
        StmtKind, TupleUnpack,
    },
};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::{Location, Span};
use kaede_type::{Ty, TyKind};

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
        if self.consume_b(&TokenKind::Eq) {
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

        if self.check(&TokenKind::OpenParen) {
            // Tuple unpacking
            return self.tuple_unpacking(&start);
        }

        let mutability = self.consume_b(&TokenKind::Mut).into();

        let name = self.ident()?;

        if self.consume_b(&TokenKind::Eq) {
            let init = self.expr()?;

            let finish = init.span.finish;

            let span = Span::new(start, finish);

            return Ok(Let {
                kind: LetKind::NormalLet(NormalLet {
                    name,
                    mutability,
                    init: Some(init.into()),
                    ty: Ty::new(TyKind::Inferred.into(), mutability).into(),
                    span,
                }),
                span,
            });
        }

        let ty = self.ty()?;

        let init = if self.consume_b(&TokenKind::Eq) {
            Some(Rc::new(self.expr()?))
        } else {
            None
        };

        let span = match &init {
            Some(e) => e.span,
            None => Span::new(start, name.span.finish),
        };

        Ok(Let {
            kind: LetKind::NormalLet(NormalLet {
                span,
                mutability,
                name,
                init,
                ty: ty.into(),
            }),
            span,
        })
    }

    // Expect that the Let token has already been consumed!
    fn tuple_unpacking(&mut self, start: &Location) -> ParseResult<Let> {
        self.consume(&TokenKind::OpenParen)?;

        let mut names = Vec::new();

        loop {
            let mutability = self.consume_b(&TokenKind::Mut).into();

            let ident = self.ident()?;

            if ident.as_str() == "_" {
                // Ignore field
                names.push(None);
            } else {
                names.push(Some((ident, mutability)));
            }

            if self.consume_b(&TokenKind::CloseParen) {
                break;
            }

            self.consume(&TokenKind::Comma)?;
        }

        self.consume(&TokenKind::Eq)?;

        let init = self.expr()?;

        let span = Span::new(*start, init.span.finish);

        Ok(Let {
            kind: LetKind::TupleUnpack(TupleUnpack {
                names,
                init: init.into(),
                span,
            }),
            span,
        })
    }
}
