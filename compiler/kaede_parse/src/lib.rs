mod error;
mod expr;
mod stmt;
mod top;

#[cfg(test)]
mod tests;

pub fn parse(tokens: impl Iterator<Item = Token>) -> ParseResult<TranslationUnit> {
    let mut parser = Parser::new(tokens.peekable());

    let mut result = Vec::new();

    loop {
        let top = parser.top()?;

        match top {
            Some(x) => result.push(x),
            None => break,
        }
    }

    Ok(result)
}

use std::iter::Peekable;

use error::{ParseError, ParseResult};
use kaede_ast::TranslationUnit;
use kaede_lex::token::{Token, TokenKind};

pub struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self { tokens }
    }

    pub fn first(&mut self) -> &Token {
        self.tokens.peek().unwrap()
    }

    pub fn bump(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    pub fn consume(&mut self, tok: TokenKind) -> ParseResult<()> {
        if self.first().kind == tok {
            self.bump();
            return Ok(());
        }

        Err(ParseError::ExpectedError {
            expected: tok.to_string(),
            but: self.first().kind.clone(),
            span: self.first().span.clone(),
        })
    }

    /// _b because it returns a bool type.
    pub fn consume_b(&mut self, tok: TokenKind) -> bool {
        if self.first().kind == tok {
            self.bump();
            return true;
        }

        false
    }
}
