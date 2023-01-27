use crate::{ast::ast::TranslationUnit, lex::token::TokenKind};

use super::{error::ParseError, parser::Parser};

pub fn parse<T>(tokens: T) -> anyhow::Result<TranslationUnit>
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

impl<T: Iterator<Item = TokenKind>> Parser<T> {
    pub fn consume(&mut self, tok: TokenKind) -> anyhow::Result<()> {
        if self.first() == &tok {
            self.bump();
            return Ok(());
        }

        Err(ParseError::ExpectedError {
            expected: tok,
            but: self.first().clone(),
        }
        .into())
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
