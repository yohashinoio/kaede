use kaede_ast::expr::{
    new_binop, new_func_call, new_i32, new_ident, BinOpKind, Expr, ExprEnum, Int,
};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::{spanned, Spanned};

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
            if let Some(s) = self.consume_s(&TokenKind::Add) {
                node = new_binop(Box::new(node), BinOpKind::Add, Box::new(self.mul()?), s);
            } else if let Some(s) = self.consume_s(&TokenKind::Sub) {
                node = new_binop(Box::new(node), BinOpKind::Sub, Box::new(self.mul()?), s);
            } else {
                return Ok(node);
            }
        }
    }

    /// Same precedence of multiplication
    fn mul(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if let Some(s) = self.consume_s(&TokenKind::Mul) {
                node = new_binop(Box::new(node), BinOpKind::Mul, Box::new(self.unary()?), s);
            } else if let Some(s) = self.consume_s(&TokenKind::Div) {
                node = new_binop(Box::new(node), BinOpKind::Div, Box::new(self.unary()?), s);
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

        if let Some(s) = self.consume_s(&TokenKind::Sub) {
            return Ok(new_binop(
                Box::new(new_i32(0, s.clone())),
                BinOpKind::Sub,
                Box::new(self.primary()?),
                s,
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

                return Ok(new_func_call(ident.val, ident.span));
            }

            return Ok(new_ident(ident.val, ident.span));
        }

        let int = self.integer()?;
        Ok(spanned(ExprEnum::Int(int.val), int.span))
    }

    pub fn integer(&mut self) -> ParseResult<Spanned<Int>> {
        let token = self.bump().unwrap();

        match token.kind {
            TokenKind::Int(int_s) => {
                // Try to convert to i32.
                match int_s.parse() {
                    Ok(n) => Ok(spanned(Int::I32(n), token.span)),
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

    pub fn ident(&mut self) -> ParseResult<Spanned<String>> {
        let start_span = self.first().span.clone();

        if matches!(self.first().kind, TokenKind::Ident(_)) {
            if let TokenKind::Ident(ident) = self.bump().unwrap().kind {
                return Ok(spanned(ident, start_span));
            }
        }

        Err(ParseError::ExpectedError {
            expected: "identifier".to_string(),
            but: self.first().kind.clone(),
            span: self.first().span.clone(),
        })
    }
}
