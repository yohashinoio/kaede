use crate::{ast::ast::Expr, lex::token::TokenKind};

use super::{error::ParseError, tokencursor::TokenCursor};

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
    pub fn parse_expr(&mut self) -> anyhow::Result<Expr> {
        let number = self.parse_number()?;

        Ok(Expr::Integer(number))
    }

    pub fn parse_number(&mut self) -> anyhow::Result<u64> {
        let is_number = match self.first() {
            TokenKind::Integer(_) => true,
            _ => false,
        };

        if is_number {
            if let TokenKind::Integer(num) = self.bump().unwrap() {
                return Ok(num);
            }
        }

        Err(ParseError::ExpectedError {
            expected: TokenKind::Integer(0),
            but: self.first().clone(),
        }
        .into())
    }

    pub fn parse_ident(&mut self) -> anyhow::Result<String> {
        let is_ident = match self.first() {
            TokenKind::Ident(_) => true,
            _ => false,
        };

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
