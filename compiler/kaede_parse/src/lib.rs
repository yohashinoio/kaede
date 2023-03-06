mod error;
mod expr;
mod stmt;
mod top;
mod ty;

pub fn parse(tokens: impl Iterator<Item = Token>) -> ParseResult<TranslationUnit> {
    let mut parser = Parser::new(tokens.peekable());

    parser.parse()
}

use std::iter::Peekable;

use error::{ParseError, ParseResult};
use kaede_ast::TranslationUnit;
use kaede_lex::token::{Token, TokenKind};
use kaede_location::Span;

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

    pub fn parse(&mut self) -> ParseResult<TranslationUnit> {
        let mut unit = TranslationUnit::new();

        while !self.is_eof() {
            let top = self.top()?;

            unit.push(top);
        }

        Ok(unit)
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
