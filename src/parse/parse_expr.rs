use crate::{
    ast::ast::{BinOpKind, Expr},
    lex::token::TokenKind,
};

use super::{error::ParseError, parser::Parser};

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    pub fn expr(&mut self) -> anyhow::Result<Expr> {
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

    fn mul(&mut self) -> anyhow::Result<Expr> {
        let mut node = self.primary()?;

        loop {
            if self.consume_b(TokenKind::Mul) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Mul, Box::new(self.primary()?));
            } else if self.consume_b(TokenKind::Div) {
                node = Expr::BinOp(Box::new(node), BinOpKind::Div, Box::new(self.primary()?));
            } else {
                return Ok(node);
            }
        }
    }

    fn primary(&mut self) -> anyhow::Result<Expr> {
        if self.consume_b(TokenKind::OpenParen) {
            // '(' expr ')'
            let node = self.expr()?;
            self.consume(TokenKind::CloseParen)?;
            return Ok(node);
        }

        Ok(Expr::Integer(self.integer()?))
    }

    pub fn integer(&mut self) -> anyhow::Result<u64> {
        match self.bump().unwrap() {
            TokenKind::Integer(int) => Ok(int),

            _ => Err(ParseError::ExpectedError {
                expected: TokenKind::Integer(0),
                but: self.first().clone(),
            }
            .into()),
        }
    }

    pub fn ident(&mut self) -> anyhow::Result<String> {
        let is_ident = matches!(self.first(), TokenKind::Ident(_));

        if is_ident {
            if let TokenKind::Ident(ident) = self.bump().unwrap() {
                return Ok(ident);
            }
        }

        Err(ParseError::ExpectedError {
            expected: TokenKind::Ident("".to_string()),
            but: self.first().clone(),
        }
        .into())
    }
}
