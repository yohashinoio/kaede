use kaede_ast::{Expr, Let, Return, Stmt, StmtList};
use kaede_lex::token::{Token, TokenKind};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn stmt_list(&mut self) -> ParseResult<StmtList> {
        let mut result = StmtList::new();

        self.consume(&TokenKind::OpenBrace)?;

        loop {
            if self.consume_b(&TokenKind::CloseBrace) {
                return Ok(result);
            } else if self.check(&TokenKind::Eoi) {
                return Err(ParseError::ExpectedError {
                    expected: TokenKind::CloseBrace.to_string(),
                    but: self.first().kind.clone(),
                    span: self.first().span.clone(),
                });
            }

            result.push(self.stmt()?);
        }
    }

    pub fn stmt(&mut self) -> ParseResult<Stmt> {
        let result;

        if self.consume_b(&TokenKind::Return) {
            result = Stmt::Return(self.return_()?);
        } else if self.consume_b(&TokenKind::Let) {
            result = Stmt::Let(self.let_()?);
        } else {
            match self.expr() {
                Ok(e) => result = self.expr_stmt(e)?,

                Err(_) => {
                    return Err(ParseError::ExpectedError {
                        expected: "statement".to_string(),
                        but: self.first().kind.clone(),
                        span: self.first().span.clone(),
                    })
                }
            }
        }

        self.consume_semi()?;

        Ok(result)
    }

    fn expr_stmt(&mut self, e: Expr) -> ParseResult<Stmt> {
        Ok(Stmt::Expr(e))
    }

    fn return_(&mut self) -> ParseResult<Return> {
        if self.consume_semi_b() {
            return Ok(Return(None));
        }

        let val = self.expr()?;

        self.consume_semi()?;

        Ok(Return(Some(val)))
    }

    fn let_(&mut self) -> ParseResult<Let> {
        Ok(Let {
            name: self.ident()?,
        })
    }
}
