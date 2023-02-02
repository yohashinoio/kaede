use kaede_lex::token::TokenKind;
use kaede_location::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{}:{} Expected {} but {} found.", span.start.line, span.start.column,  .expected, .but)]
    ExpectedError {
        expected: TokenKind,
        but: TokenKind,
        span: Span,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;
