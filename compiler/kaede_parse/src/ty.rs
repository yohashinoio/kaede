use kaede_lex::token::{Token, TokenKind};
use kaede_type::{
    make_fundamental_type, FundamentalTypeKind, Mutability, RefrenceType, Ty, TyKind,
    UserDefinedType,
};

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

fn wrap_in_reference(refee_ty: Ty) -> Ty {
    Ty {
        kind: TyKind::Reference(RefrenceType {
            refee_ty: refee_ty.into(),
        })
        .into(),
        mutability: Mutability::Not,
    }
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn ty(&mut self) -> ParseResult<Ty> {
        use FundamentalTypeKind::*;

        if self.check(&TokenKind::OpenBracket) {
            // Array type
            return self.array_ty();
        }

        if self.check(&TokenKind::OpenParen) {
            // Tuple type
            return self.tuple_ty();
        }

        let ty_name = match self.ident() {
            Ok(t) => t,
            Err(_) => {
                return Err(ParseError::ExpectedError {
                    expected: "type".to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                })
            }
        };

        Ok(match ty_name.as_str() {
            "i32" => make_fundamental_type(I32, Mutability::Not),
            "bool" => make_fundamental_type(Bool, Mutability::Not),

            // User defined type
            _ => wrap_in_reference(
                Ty {
                    kind: TyKind::UserDefined(UserDefinedType { name: ty_name.name }).into(),
                    mutability: Mutability::Not,
                },
            ),
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

        Ok(wrap_in_reference(Ty {
            kind: TyKind::Array((element_ty.into(), size)).into(),
            mutability: Mutability::Not,
        }))
    }

    fn tuple_ty(&mut self) -> ParseResult<Ty> {
        // (i32, bool)
        // ^
        self.consume(&TokenKind::OpenParen)?;

        // (i32, bool)
        //  ^~~~~~~~~
        let mut field_types = Vec::new();

        loop {
            field_types.push(self.ty()?.into());

            if self.consume_b(&TokenKind::CloseParen) {
                return Ok(wrap_in_reference(Ty {
                    kind: TyKind::Tuple(field_types).into(),
                    mutability: Mutability::Not,
                }));
            }

            self.consume(&TokenKind::Comma)?;
        }
    }
}
