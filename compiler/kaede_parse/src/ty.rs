use kaede_lex::token::Token;
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Type.
    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        let ty = self.ident()?;

        match ty.val.as_str() {
            "i32" => Ok(Ty::new(make_fundamental_type(I32), Mutability::Not)),

            _ => Err(ParseError::ExpectedError {
                expected: ty.val,
                but: self.first().kind.clone(),
                span: ty.span,
            }),
        }
    }
}
