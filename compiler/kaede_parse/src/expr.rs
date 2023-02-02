use kaede_ast::{BinOpKind, Expr};
use kaede_lex::token::TokenKind;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.add()
    }

    /// Same precedence of multiplication
    fn add(&mut self) -> ParseResult<Expr> {
        let mut node = self.mul()?;

        loop {
            if self.consume_b(TokenKind::Add) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Add, Box::new(self.mul()?));
            } else if self.consume_b(TokenKind::Sub) {
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
            if self.consume_b(TokenKind::Mul) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Mul, Box::new(self.unary()?));
            } else if self.consume_b(TokenKind::Div) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Div, Box::new(self.unary()?));
            } else {
                return Ok(node);
            }
        }
    }

    /// Unary operators
    fn unary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(TokenKind::Add) {
            return self.primary();
        }

        if self.consume_b(TokenKind::Sub) {
            return Ok(Expr::BinOp(
                Box::new(Expr::Integer(0)),
                BinOpKind::Sub,
                Box::new(self.primary()?),
            ));
        }

        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(TokenKind::OpenParen) {
            // '(' expr ')'
            let node = self.expr()?;
            self.consume(TokenKind::CloseParen)?;
            return Ok(node);
        }

        Ok(Expr::Integer(self.integer()?))
    }

    pub fn integer(&mut self) -> ParseResult<u64> {
        match self.bump().unwrap() {
            TokenKind::Integer(int) => Ok(int),

            _ => Err(ParseError::ExpectedError {
                expected: TokenKind::Integer(0),
                but: self.first().clone(),
            }),
        }
    }

    pub fn ident(&mut self) -> ParseResult<String> {
        let is_ident = matches!(self.first(), TokenKind::Ident(_));

        if is_ident {
            if let TokenKind::Ident(ident) = self.bump().unwrap() {
                return Ok(ident);
            }
        }

        Err(ParseError::ExpectedError {
            expected: TokenKind::Ident("".to_string()),
            but: self.first().clone(),
        })
    }
}
