use kaede_lex::token::{Token, TokenKind};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        if self.check(&TokenKind::OpenBracket) {
            // Array type
            return self.array_ty();
        }

        if self.check(&TokenKind::And) {
            // Reference type
            return self.reference_ty();
        }

        Ok(match self.ident()?.as_str() {
            "i32" => make_fundamental_type(I32, Mutability::Not),
            "bool" => make_fundamental_type(Bool, Mutability::Not),

            _ => unimplemented!(),
        })
    }

    fn reference_ty(&mut self) -> ParseResult<Ty> {
        // &mut i32
        // ^
        self.consume(&TokenKind::And)?;

        // &mut i32
        //  ^~~
        let ref_mutability = self.consume_b(&TokenKind::Mut).into();

        // &mut i32
        //      ^~~
        let refee_ty = self.ty()?;

        Ok(Ty {
            kind: TyKind::Reference((refee_ty.into(), ref_mutability)).into(),
            mutability: Mutability::Not,
        })
    }

    fn array_size(&mut self) -> ParseResult<u32> {
        let token = self.bump().unwrap();

        if let TokenKind::Int(s) = &token.kind {
            match s.parse() {
                Ok(n) => Ok(n),
                Err(_) => Err(ParseError::OutOfRangeForU32(token.span)),
            }
        } else {
            Err(ParseError::ExpectedError {
                expected: "array size".to_string(),
                but: token.kind.to_string(),
                span: token.span,
            })
        }
    }

    fn array_ty(&mut self) -> ParseResult<Ty> {
        // [i32; 58]
        // ^
        self.consume(&TokenKind::OpenBracket)?;

        // [i32; 58]
        //  ^~~
        let element_ty = self.ty()?;

        // [i32; 58]
        //     ^
        self.consume(&TokenKind::Semi)?;

        // [i32; 58]
        //       ^~
        let size = self.array_size()?;

        // [i32; 58]
        //         ^
        self.consume(&TokenKind::CloseBracket)?;

        Ok(Ty {
            kind: TyKind::Array((element_ty.into(), size)).into(),
            mutability: Mutability::Not,
        })
    }
}
