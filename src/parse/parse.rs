use crate::{
    ast::ast::{Function, Top, TranslationUnit},
    lex::token::TokenKind,
};

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
    // None if end of tokens
    fn parse_top(&mut self) -> anyhow::Result<Option<Top>> {
        let token = match self.bump() {
            Some(x) => x,
            None => return Ok(None),
        };

        match token {
            TokenKind::Function => Ok(Some(Top::Function(self.parse_fn()?))),

            t => unreachable!("{:?}", t),
        }
    }

    fn parse_fn(&mut self) -> anyhow::Result<Function> {
        let name = self.parse_ident()?;

        self.consume(TokenKind::OpenParen)?;
        self.consume(TokenKind::CloseParen)?;

        self.consume(TokenKind::OpenBrace)?;
        self.consume(TokenKind::CloseBrace)?;

        Ok(Function { name: name })
    }

    fn parse_ident(&mut self) -> anyhow::Result<String> {
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

    fn consume(&mut self, tok: TokenKind) -> anyhow::Result<()> {
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
