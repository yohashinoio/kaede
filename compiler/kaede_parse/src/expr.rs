use kaede_ast::expr::{
    Args, Binary, BinaryKind, Expr, ExprKind, FnCall, Ident, Int, IntKind, StructInstantiation,
};
use kaede_lex::token::{Token, TokenKind};
use kaede_location::Span;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.add()
    }

    /// Same precedence of addition
    fn add(&mut self) -> ParseResult<Expr> {
        let mut node = self.mul()?;

        loop {
            if let Ok(span) = self.consume(&TokenKind::Add) {
                node = Expr {
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Add,
                        Box::new(self.mul()?),
                    )),
                    span,
                };
            } else if let Ok(span) = self.consume(&TokenKind::Sub) {
                node = Expr {
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Sub,
                        Box::new(self.mul()?),
                    )),
                    span,
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Same precedence of multiplication
    fn mul(&mut self) -> ParseResult<Expr> {
        let mut node = self.equal()?;

        loop {
            if let Ok(span) = self.consume(&TokenKind::Mul) {
                node = Expr {
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Mul,
                        Box::new(self.equal()?),
                    )),
                    span,
                };
            } else if let Ok(span) = self.consume(&TokenKind::Div) {
                node = Expr {
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Div,
                        Box::new(self.equal()?),
                    )),
                    span,
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Same precedence of equal
    fn equal(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if let Ok(span) = self.consume(&TokenKind::Eq) {
                node = Expr {
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Eq,
                        Box::new(self.unary()?),
                    )),
                    span,
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Unary operators
    fn unary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::Add) {
            return self.primary();
        }

        if let Ok(span) = self.consume(&TokenKind::Sub) {
            // Subtracting a number from 0 inverts the sign
            let zero = Box::new(Expr {
                kind: ExprKind::Int(Int {
                    kind: IntKind::I32(0),
                    span,
                }),
                span,
            });

            let primary = self.primary()?;

            return Ok(Expr {
                span: Span::new(span.start, primary.span.finish),
                kind: ExprKind::Binary(Binary::new(zero, BinaryKind::Sub, Box::new(primary))),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::OpenParen) {
            // '(' expr ')'
            let node = self.expr()?;
            self.consume(&TokenKind::CloseParen)?;
            return Ok(node);
        }

        if let Ok(ident) = self.ident() {
            // Function call
            if self.first().kind == TokenKind::OpenParen {
                return self.fn_call(ident);
            }

            // Struct instantiation
            if self.first().kind == TokenKind::OpenBrace {
                // Check if this brace is from a block statement
                // if x {}
                // Such codes must not be interpreted as struct literals

                // If parsing an expression for a condition now, skip
                if !self.in_cond_expr {
                    return self.struct_instantiation(ident);
                }
            }

            // Identifier
            return Ok(Expr {
                span: ident.span,
                kind: ExprKind::Ident(ident),
            });
        }

        // String literals
        if let Some(lit) = self.string_literal() {
            return Ok(lit);
        }

        // Boolean literals
        if let Some(lit) = self.boolean_literal() {
            return Ok(lit);
        }

        // Integers
        let int = self.integer()?;
        Ok(Expr {
            span: int.span,
            kind: ExprKind::Int(int),
        })
    }

    fn boolean_literal(&mut self) -> Option<Expr> {
        if let Ok(span) = self.consume(&TokenKind::True) {
            return Some(Expr {
                kind: ExprKind::True,
                span,
            });
        }

        if let Ok(span) = self.consume(&TokenKind::False) {
            return Some(Expr {
                kind: ExprKind::False,
                span,
            });
        }

        None
    }

    fn struct_instantiation(&mut self, struct_name: Ident) -> ParseResult<Expr> {
        let mut inits = Vec::new();

        self.consume(&TokenKind::OpenBrace).unwrap();

        if let Ok(finish) = self.consume(&TokenKind::CloseBrace) {
            return Ok(Expr {
                span: Span::new(struct_name.span.start, finish.finish),
                kind: ExprKind::StructInstantiation(StructInstantiation {
                    struct_name,
                    values: inits,
                }),
            });
        }

        loop {
            inits.push((self.ident()?, self.expr()?));

            if !self.consume_b(&TokenKind::Comma) {
                let finish = self.consume(&TokenKind::CloseBrace)?.finish;

                return Ok(Expr {
                    span: Span::new(struct_name.span.start, finish),
                    kind: ExprKind::StructInstantiation(StructInstantiation {
                        struct_name,
                        values: inits,
                    }),
                });
            }
        }
    }

    fn string_literal(&mut self) -> Option<Expr> {
        if matches!(self.first().kind, TokenKind::StringLiteral(_)) {
            let token = self.bump().unwrap();

            Some(Expr {
                span: token.span,
                kind: ExprKind::StirngLiteral(match token.kind {
                    TokenKind::StringLiteral(s) => s,
                    _ => unreachable!(),
                }),
            })
        } else {
            None
        }
    }

    fn fn_call(&mut self, name: Ident) -> ParseResult<Expr> {
        self.consume(&TokenKind::OpenParen)?;

        let args = self.fn_call_args()?;

        let start = name.span.start;
        let finish = self.consume(&TokenKind::CloseParen)?.finish;
        let span = Span::new(start, finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall { name, args, span }),
            span,
        })
    }

    /// Works with zero arguments
    fn fn_call_args(&mut self) -> ParseResult<Args> {
        let mut args = Args::new();

        if self.check(&TokenKind::CloseParen) {
            // No arguments
            return Ok(args);
        }

        loop {
            args.push(self.expr()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    pub fn integer(&mut self) -> ParseResult<Int> {
        let token = self.bump().unwrap();

        match token.kind {
            TokenKind::Int(int_s) => {
                // Try to convert to i32
                match int_s.parse() {
                    Ok(n) => Ok(Int {
                        kind: IntKind::I32(n),
                        span: token.span,
                    }),
                    Err(_) => Err(ParseError::OutOfRangeForI32(token.span)),
                }
            }

            _ => Err(ParseError::ExpectedError {
                expected: "integer".to_string(),
                but: token.kind.to_string(),
                span: token.span,
            }),
        }
    }

    pub fn ident(&mut self) -> ParseResult<Ident> {
        if matches!(self.first().kind, TokenKind::Ident(_)) {
            if let TokenKind::Ident(ident) = self.bump().unwrap().kind {
                return Ok(Ident {
                    name: ident,
                    span: self.first().span,
                });
            }
        }

        Err(ParseError::ExpectedError {
            expected: "identifier".to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        })
    }
}
