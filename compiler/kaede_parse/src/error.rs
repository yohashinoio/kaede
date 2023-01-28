use kaede_lex::token::TokenKind;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected {} but {} found.", .expected, .but)]
    ExpectedError { expected: TokenKind, but: TokenKind },
}

pub type ParseResult<T> = Result<T, ParseError>;
