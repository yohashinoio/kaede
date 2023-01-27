use crate::{ast::ast::TranslationUnit, lex::token::TokenKind};

use super::{error::ParseError, tokencursor::TokenCursor};

pub fn parse<T>(tokens: T) -> anyhow::Result<TranslationUnit>
where
    T: Iterator<Item = TokenKind>,
{
    let mut cursor = TokenCursor::new(tokens.peekable());

    let mut result = Vec::new();

    loop {
        let top = cursor.parse_top()?;

        match top {
            Some(x) => result.push(x),
            None => break,
        }
    }

    Ok(result)
}

impl<T: Iterator<Item = TokenKind>> TokenCursor<T> {
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
}
