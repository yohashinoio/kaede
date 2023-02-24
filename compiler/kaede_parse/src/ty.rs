use kaede_lex::token::Token;
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Type.
    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        let ty = self.ident()?;

        match ty.name.as_str() {
            "i32" => Ok(make_fundamental_type(I32, Mutability::Not)),
            "bool" => Ok(make_fundamental_type(Bool, Mutability::Not)),

            _ => unimplemented!(),
        }
    }
}
