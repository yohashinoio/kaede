use kaede_ast::{Expr, Return, Stmt};
use kaede_lex::token::{Token, TokenKind};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        if self.consume_b(TokenKind::Return) {
            return Ok(Stmt::Return(self.return_()?));
        }

        match self.expr() {
            Ok(e) => self.expr_stmt(e),

            Err(_) => Err(ParseError::ExpectedError {
                expected: "statement".to_string(),
                but: self.first().kind.clone(),
                span: self.first().span.clone(),
            }),
        }
    }

    fn expr_stmt(&mut self, e: Expr) -> ParseResult<Stmt> {
        self.consume(TokenKind::Semi)?;

        Ok(Stmt::Expr(e))
    }

    fn return_(&mut self) -> ParseResult<Return> {
        if self.consume_b(TokenKind::Semi) {
            return Ok(Return(None));
        }

        let val = self.expr()?;

        self.consume(TokenKind::Semi)?;

        Ok(Return(Some(val)))
    }
}
