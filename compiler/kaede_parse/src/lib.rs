mod error;
mod expr;
mod top;

pub fn parse<T>(tokens: T) -> ParseResult<TranslationUnit>
where
    T: Iterator<Item = TokenKind>,
{
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
use kaede_lex::token::TokenKind;

pub struct Parser<T: Iterator<Item = TokenKind>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self { tokens }
    }

    pub fn first(&mut self) -> &TokenKind {
        self.tokens.peek().unwrap_or(&TokenKind::Eof)
    }

    pub fn bump(&mut self) -> Option<TokenKind> {
        self.tokens.next()
    }

    pub fn consume(&mut self, tok: TokenKind) -> ParseResult<()> {
        if self.first() == &tok {
            self.bump();
            return Ok(());
        }

        Err(ParseError::ExpectedError {
            expected: tok,
            but: self.first().clone(),
        })
    }

    /// _b because it returns a bool type.
    pub fn consume_b(&mut self, tok: TokenKind) -> bool {
        if self.first() == &tok {
            self.bump();
            return true;
        }

        false
    }
}
