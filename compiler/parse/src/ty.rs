use std::rc::Rc;

use kaede_lex::token::TokenKind;
use kaede_span::Span;
use kaede_type::{
    make_fundamental_type, FundamentalTypeKind, GenericArgs, Mutability, RefrenceType, Ty, TyKind,
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

impl Parser {
    fn generic_args(&mut self) -> ParseResult<GenericArgs> {
        let start = self.consume(&TokenKind::Lt)?.start;

        let mut types = Vec::new();

        loop {
            types.push(Rc::new(self.ty()?.0));

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::Gt)?.finish;

        Ok(GenericArgs {
            types,
            span: Span::new(start, finish),
        })
    }

    pub fn ty(&mut self) -> ParseResult<(Ty, Span)> {
        use FundamentalTypeKind::*;

        if self.check(&TokenKind::Asterisk) {
            // Pointer type
            return self.pointer_ty();
        }

        if self.check(&TokenKind::OpenBracket) {
            // Array type
            return self.array_ty();
        }

        if self.check(&TokenKind::OpenParen) {
            // Tuple type
            return self.tuple_ty();
        }

        let type_ident = match self.ident() {
            Ok(t) => t,
            Err(_) => {
                return Err(ParseError::ExpectedError {
                    expected: "type".to_string(),
                    but: self.first().kind.to_string(),
                    span: self.first().span,
                })
            }
        };

        Ok((
            match type_ident.as_str() {
                "i8" => make_fundamental_type(I8, Mutability::Not),
                "u8" => make_fundamental_type(U8, Mutability::Not),
                "i32" => make_fundamental_type(I32, Mutability::Not),
                "u32" => make_fundamental_type(U32, Mutability::Not),
                "i64" => make_fundamental_type(I64, Mutability::Not),
                "u64" => make_fundamental_type(U64, Mutability::Not),
                "bool" => make_fundamental_type(Bool, Mutability::Not),

                "str" => wrap_in_reference(Ty {
                    kind: TyKind::Str.into(),
                    mutability: Mutability::Not,
                }),

                // User defined type
                _ => {
                    let generic_args = if self.check(&TokenKind::Lt) {
                        Some(self.generic_args()?)
                    } else {
                        None
                    };

                    wrap_in_reference(Ty {
                        kind: TyKind::UserDefined(UserDefinedType::new(type_ident, generic_args))
                            .into(),
                        mutability: Mutability::Not,
                    })
                }
            },
            type_ident.span(),
        ))
    }

    fn pointer_ty(&mut self) -> ParseResult<(Ty, Span)> {
        // *i32
        // ^
        let start = self.consume(&TokenKind::Asterisk)?.start;

        // *i32
        //  ^~~
        let (ty, span) = self.ty()?;

        Ok((
            Ty {
                kind: TyKind::Pointer(Rc::new(ty)).into(),
                mutability: Mutability::Not,
            },
            Span::new(start, span.finish),
        ))
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

    fn array_ty(&mut self) -> ParseResult<(Ty, Span)> {
        // [i32; 58]
        // ^
        let start = self.consume(&TokenKind::OpenBracket)?.start;

        // [i32; 58]
        //  ^~~
        let (element_ty, _) = self.ty()?;

        // [i32; 58]
        //     ^
        self.consume(&TokenKind::Semi)?;

        // [i32; 58]
        //       ^~
        let size = self.array_size()?;

        // [i32; 58]
        //         ^
        let finish = self.consume(&TokenKind::CloseBracket)?.finish;

        Ok((
            wrap_in_reference(Ty {
                kind: TyKind::Array((element_ty.into(), size)).into(),
                mutability: Mutability::Not,
            }),
            Span::new(start, finish),
        ))
    }

    fn tuple_ty(&mut self) -> ParseResult<(Ty, Span)> {
        // (i32, bool)
        // ^
        let start = self.consume(&TokenKind::OpenParen)?.start;

        // (i32, bool)
        //  ^~~~~~~~~
        let mut field_types = Vec::new();

        loop {
            field_types.push(self.ty()?.0.into());

            if let Ok(span) = self.consume(&TokenKind::CloseParen) {
                return Ok((
                    wrap_in_reference(Ty {
                        kind: TyKind::Tuple(field_types).into(),
                        mutability: Mutability::Not,
                    }),
                    Span::new(start, span.finish),
                ));
            }

            self.consume(&TokenKind::Comma)?;
        }
    }
}
