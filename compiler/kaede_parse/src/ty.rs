use kaede_lex::token::{Token, TokenKind};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind};

use crate::{error::ParseResult, Parser};

impl<T: Iterator<Item = Token>> Parser<T> {
    /// Type
    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        let is_ref_ty = self.consume_b(&TokenKind::And);
        let ref_mutability = self.consume_b(&TokenKind::Mut).into();

        let ty = self.ident()?;

        let ty = match ty.as_str() {
            "i32" => make_fundamental_type(I32, Mutability::Not),
            "bool" => make_fundamental_type(Bool, Mutability::Not),

            _ => unimplemented!(),
        };

        if is_ref_ty {
            Ok(Ty {
                kind: TyKind::Reference((ty.into(), ref_mutability)).into(),
                mutability: Mutability::Not,
            })
        } else {
            Ok(ty)
        }
    }
}
