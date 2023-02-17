use kaede_ast::{BinOpKind, Expr, FuncCall, Int};
use kaede_lex::token::{Token, TokenKind};

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
            if self.consume_b(&TokenKind::Add) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Add, Box::new(self.mul()?));
            } else if self.consume_b(&TokenKind::Sub) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Sub, Box::new(self.mul()?));
            } else {
                return Ok(node);
            }
        }
    }

    /// Same precedence of multiplication
    fn mul(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if self.consume_b(&TokenKind::Mul) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Mul, Box::new(self.unary()?));
            } else if self.consume_b(&TokenKind::Div) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Div, Box::new(self.unary()?));
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

        if self.consume_b(&TokenKind::Sub) {
            return Ok(Expr::BinOp(
                Box::new(Expr::Int(Int::I32(0))),
                BinOpKind::Sub,
                Box::new(self.primary()?),
            ));
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
            if self.consume_b(&TokenKind::OpenParen) {
                self.consume(&TokenKind::CloseParen)?;

                return Ok(Expr::FuncCall(FuncCall { name: ident }));
            }

            return Ok(Expr::Ident(ident));
        }

        Ok(Expr::Int(self.integer()?))
    }

    pub fn integer(&mut self) -> ParseResult<Int> {
        let token = self.bump().unwrap();

        match token.kind {
            TokenKind::Integer(int_s) => {
                // Try to convert to i32.
                match int_s.parse::<i32>() {
                    Ok(n) => Ok(Int::I32(n)),
                    Err(_) => Err(ParseError::OutOfRangeForI32(token.span)),
                }
            }

            _ => Err(ParseError::ExpectedError {
                expected: "integer".to_string(),
                but: token.kind,
                span: token.span,
            }),
        }
    }

    pub fn ident(&mut self) -> ParseResult<String> {
        if matches!(self.first().kind, TokenKind::Ident(_)) {
            if let TokenKind::Ident(ident) = self.bump().unwrap().kind {
                return Ok(ident);
            }
        }

        Err(ParseError::ExpectedError {
            expected: "identifier".to_string(),
            but: self.first().kind.clone(),
            span: self.first().span.clone(),
        })
    }
}
