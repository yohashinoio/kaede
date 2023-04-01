mod error;
mod expr;
mod stmt;
mod top;
mod ty;

pub fn parse(tokens: impl Iterator<Item = Token>) -> ParseResult<CompileUnit> {
    let mut parser = Parser::new(tokens.peekable());

    parser.run()
}

use std::iter::Peekable;

use error::{ParseError, ParseResult};
use kaede_ast::{expr::Expr, CompileUnit};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::Span;

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,

    end_token: Option<Token>,

    pub in_cond_expr: bool,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self {
            tokens,
            end_token: None,
            in_cond_expr: false,
        }
    }

    /// The argument specifies the condition under which the parsing is terminated
    ///
    /// When using "self", use the argument self
    ///
    /// Example:
    /// |self_| self_.xxx()
    pub fn compile_unit(
        &mut self,
        mut end_predicate: impl FnMut(&mut Self) -> bool,
    ) -> ParseResult<CompileUnit> {
        let mut compile_unit = CompileUnit {
            top_levels: Vec::new(),
        };

        while end_predicate(self) {
            let top = self.top_level()?;

            compile_unit.top_levels.push(top);
        }

        Ok(compile_unit)
    }

    pub fn run(&mut self) -> ParseResult<CompileUnit> {
        self.compile_unit(|self_| !self_.is_eof())
    }

    /// Needed to **avoid confusion** between struct literals and block statements
    ///
    /// if x {}
    ///
    /// In such code as above,
    /// `then block` of `if statement` is not parsed as an initializer of a struct literals
    pub fn cond_expr(&mut self) -> ParseResult<Expr> {
        self.in_cond_expr = true;
        let cond = self.expr();
        self.in_cond_expr = false;
        cond
    }

    fn is_eof(&mut self) -> bool {
        self.end_token.is_some() || self.first().kind == TokenKind::Eoi
    }

    pub fn first(&mut self) -> &Token {
        self.tokens
            .peek()
            .unwrap_or_else(|| self.end_token.as_ref().unwrap())
    }

    /// Advance to next token
    pub fn bump(&mut self) -> Option<Token> {
        self.tokens.next().map(|t| match t.kind {
            TokenKind::Eoi => {
                self.end_token = Some(t.clone());
                t
            }

            _ => t,
        })
    }

    /// Check without consuming tokens
    pub fn check(&mut self, tok: &TokenKind) -> bool {
        &self.first().kind == tok
    }

    pub fn consume(&mut self, tok: &TokenKind) -> ParseResult<Span> {
        let span = self.first().span;

        if &self.first().kind == tok {
            self.bump().unwrap();
            return Ok(span);
        }

        Err(ParseError::ExpectedError {
            expected: tok.to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        })
    }

    /// Return boolean
    pub fn consume_b(&mut self, tok: &TokenKind) -> bool {
        if &self.first().kind == tok {
            self.bump().unwrap();
            return true;
        }

        false
    }

    /// Consume a semicolon
    /// ')' or '}', then success (Following Golang's rules)
    pub fn consume_semi(&mut self) -> ParseResult<Span> {
        if let Ok(span) = self.consume(&TokenKind::Semi) {
            return Ok(span);
        }

        if self.check(&TokenKind::CloseParen) || self.check(&TokenKind::CloseBrace) {
            return Ok(self.first().span);
        }

        Err(ParseError::ExpectedError {
            expected: TokenKind::Semi.to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        })
    }

    /// Consume a semicolon
    /// ')' or '}', then success (Following Golang's rules)
    /// Return boolean
    pub fn consume_semi_b(&mut self) -> bool {
        if self.consume_b(&TokenKind::Semi)
            || (self.check(&TokenKind::CloseParen) || self.check(&TokenKind::CloseBrace))
        {
            return true;
        }

        false
    }

    /// Check a semicolon (Not consumed)
    /// ')' or '}', then success (Following Golang's rules)
    pub fn check_semi(&mut self) -> bool {
        if self.check(&TokenKind::Semi)
            || (self.check(&TokenKind::CloseParen) || self.check(&TokenKind::CloseBrace))
        {
            return true;
        }

        false
    }
}
