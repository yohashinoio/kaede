use kaede_ast::expr::{Args, BinOp, BinOpKind, Expr, ExprKind, FnCall, Ident, Int, IntKind};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::Span;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.add()
    }

    /// Same precedence of multiplication
    fn add(&mut self) -> ParseResult<Expr> {
        let mut node = self.mul()?;

        loop {
            if let Ok(span) = self.consume(&TokenKind::Add) {
                node = Expr {
                    kind: ExprKind::BinOp(BinOp::new(
                        Box::new(node),
                        BinOpKind::Add,
                        Box::new(self.mul()?),
                    )),
                    span,
                };
            } else if let Ok(span) = self.consume(&TokenKind::Sub) {
                node = Expr {
                    kind: ExprKind::BinOp(BinOp::new(
                        Box::new(node),
                        BinOpKind::Sub,
                        Box::new(self.mul()?),
                    )),
                    span,
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Same precedence of multiplication
    fn mul(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if let Ok(span) = self.consume(&TokenKind::Mul) {
                node = Expr {
                    kind: ExprKind::BinOp(BinOp::new(
                        Box::new(node),
                        BinOpKind::Mul,
                        Box::new(self.unary()?),
                    )),
                    span,
                };
            } else if let Ok(span) = self.consume(&TokenKind::Div) {
                node = Expr {
                    kind: ExprKind::BinOp(BinOp::new(
                        Box::new(node),
                        BinOpKind::Div,
                        Box::new(self.unary()?),
                    )),
                    span,
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Unary operators
    fn unary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::Add) {
            return self.primary();
        }

        if let Ok(span) = self.consume(&TokenKind::Sub) {
            // Subtracting a number from 0 inverts the sign.
            let zero = Box::new(Expr {
                kind: ExprKind::Int(Int {
                    kind: IntKind::I32(0),
                    span,
                }),
                span,
            });

            let primary = self.primary()?;

            return Ok(Expr {
                span: Span::new(span.start, primary.span.finish),
                kind: ExprKind::BinOp(BinOp::new(zero, BinOpKind::Sub, Box::new(primary))),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::OpenParen) {
            // '(' expr ')'
            let node = self.expr()?;
            self.consume(&TokenKind::CloseParen)?;
            return Ok(node);
        }

        if let Ok(ident) = self.ident() {
            // Function call
            if self.check(&TokenKind::OpenParen) {
                return self.fn_call(ident);
            }

            // Identifier
            return Ok(Expr {
                span: ident.span,
                kind: ExprKind::Ident(ident),
            });
        }

        let int = self.integer()?;
        Ok(Expr {
            span: int.span,
            kind: ExprKind::Int(int),
        })
    }

    fn fn_call(&mut self, name: Ident) -> ParseResult<Expr> {
        self.consume(&TokenKind::OpenParen)?;

        let args = self.fn_call_args()?;

        let start = name.span.start;
        let finish = self.consume(&TokenKind::CloseParen)?.finish;
        let span = Span::new(start, finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall { name, args, span }),
            span,
        })
    }

    fn fn_call_args(&mut self) -> ParseResult<Args> {
        let mut args = Args::new();

        if self.check(&TokenKind::CloseParen) {
            // No arguments.
            return Ok(args);
        }

        loop {
            args.push(self.expr()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    pub fn integer(&mut self) -> ParseResult<Int> {
        let token = self.bump().unwrap();

        match token.kind {
            TokenKind::Int(int_s) => {
                // Try to convert to i32.
                match int_s.parse() {
                    Ok(n) => Ok(Int {
                        kind: IntKind::I32(n),
                        span: token.span,
                    }),
                    Err(_) => Err(ParseError::OutOfRangeForI32(token.span)),
                }
            }

            _ => Err(ParseError::ExpectedError {
                expected: "integer".to_string(),
                but: token.kind.to_string(),
                span: token.span,
            }),
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        let span = self.first().span;

        if matches!(self.first().kind, TokenKind::Ident(_)) {
            if let TokenKind::Ident(ident) = self.bump().unwrap().kind {
                return Ok(Ident { name: ident, span });
            }
        }

        Err(ParseError::ExpectedError {
            expected: "identifier".to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        })
    }
}
