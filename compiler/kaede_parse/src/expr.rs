use kaede_ast::expr::{
    Args, ArrayLiteral, Binary, BinaryKind, Borrow, Deref, Expr, ExprKind, FnCall, Ident, Int,
    IntKind, LogicalNot, StructLiteral,
};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::Span;

use crate::{
    error::{ParseError, ParseResult},
    Parser,
};

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn expr(&mut self) -> ParseResult<Expr> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> ParseResult<Expr> {
        let mut node = self.logical_and()?;

        loop {
            if self.consume_b(&TokenKind::LogicalOr) {
                let right = self.logical_and()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::LogicalOr,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn logical_and(&mut self) -> ParseResult<Expr> {
        let mut node = self.eq_or_ne()?;

        loop {
            if self.consume_b(&TokenKind::LogicalAnd) {
                let right = self.eq_or_ne()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::LogicalAnd,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn eq_or_ne(&mut self) -> ParseResult<Expr> {
        let mut node = self.lt_gt_le_ge()?;

        loop {
            if self.consume_b(&TokenKind::DoubleEq) {
                let right = self.lt_gt_le_ge()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Eq,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Ne) {
                let right = self.lt_gt_le_ge()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Ne,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn lt_gt_le_ge(&mut self) -> ParseResult<Expr> {
        let mut node = self.add_or_sub()?;

        loop {
            if self.consume_b(&TokenKind::Lt) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Lt,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Le) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Le,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Gt) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Gt,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Ge) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Ge,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn add_or_sub(&mut self) -> ParseResult<Expr> {
        let mut node = self.mul_or_div_or_rem()?;

        loop {
            if self.consume_b(&TokenKind::Plus) {
                let right = self.mul_or_div_or_rem()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Add,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Minus) {
                let right = self.mul_or_div_or_rem()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Sub,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn mul_or_div_or_rem(&mut self) -> ParseResult<Expr> {
        let mut node = self.unary()?;

        loop {
            if self.consume_b(&TokenKind::Asterisk) {
                let right = self.unary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Mul,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Slash) {
                let right = self.unary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Div,
                        Box::new(right),
                    )),
                };
            } else if self.consume_b(&TokenKind::Percent) {
                let right = self.unary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Rem,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        if self.consume_b(&TokenKind::Plus) {
            return self.access();
        }

        if let Ok(span) = self.consume(&TokenKind::Minus) {
            // Subtracting a number from 0 inverts the sign
            let zero = Box::new(Expr {
                kind: ExprKind::Int(Int {
                    kind: IntKind::I32(0),
                    span,
                }),
                span,
            });

            let e = self.access()?;

            return Ok(Expr {
                span: Span::new(span.start, e.span.finish),
                kind: ExprKind::Binary(Binary::new(zero, BinaryKind::Sub, Box::new(e))),
            });
        }

        // Logical not
        if let Ok(span) = self.consume(&TokenKind::LogicalNot) {
            let operand = self.access()?;
            let span = Span::new(span.start, operand.span.finish);

            return Ok(Expr {
                kind: ExprKind::LogicalNot(LogicalNot {
                    operand: Box::new(operand),
                    span,
                }),
                span,
            });
        }

        // Borrow
        if self.check(&TokenKind::And) {
            return self.borrow();
        }

        // Dereference
        if self.check(&TokenKind::Asterisk) {
            return self.deref();
        }

        self.access()
    }

    fn deref(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Asterisk).unwrap().start;

        let operand = Box::new(self.access()?);

        let span = Span::new(start, operand.span.finish);

        Ok(Expr {
            kind: ExprKind::Deref(Deref { span, operand }),
            span,
        })
    }

    fn borrow(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::And).unwrap().start;

        let mutability = self.consume_b(&TokenKind::Mut).into();

        let operand = Box::new(self.access()?);

        let span = Span::new(start, operand.span.finish);

        Ok(Expr {
            kind: ExprKind::Borrow(Borrow {
                span,
                operand,
                mutability,
            }),
            span,
        })
    }

    /// Field access or module item access
    fn access(&mut self) -> ParseResult<Expr> {
        let mut node = self.primary()?;

        loop {
            if self.consume_b(&TokenKind::Dot) {
                let right = self.primary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        Box::new(node),
                        BinaryKind::Access,
                        Box::new(right),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
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

            // Struct literal
            if self.first().kind == TokenKind::OpenBrace {
                // Check if this brace is from a block statement
                // if x {}
                // Such codes must not be interpreted as struct literals

                // If parsing an expression for a condition now, skip
                if !self.in_cond_expr {
                    return self.struct_literal(ident);
                }
            }

            // Identifier
            return Ok(Expr {
                span: ident.span,
                kind: ExprKind::Ident(ident),
            });
        }

        if self.check(&TokenKind::OpenBracket) {
            // Array literal
            return self.array_literal();
        }

        // String literal
        if let Some(lit) = self.string_literal() {
            return Ok(lit);
        }

        // Boolean literal
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

    fn array_literal(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::OpenBracket).unwrap().start;

        // Elements
        let mut elems = Vec::new();

        loop {
            elems.push(self.expr()?);

            if let Ok(span) = self.consume(&TokenKind::CloseBracket) {
                let span = Span::new(start, span.finish);
                return Ok(Expr {
                    kind: ExprKind::ArrayLiteral(ArrayLiteral { elems, span }),
                    span,
                });
            }

            self.consume(&TokenKind::Comma)?;
        }
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

    fn struct_literal(&mut self, struct_name: Ident) -> ParseResult<Expr> {
        let mut inits = Vec::new();

        self.consume(&TokenKind::OpenBrace).unwrap();

        if let Ok(finish) = self.consume(&TokenKind::CloseBrace) {
            return Ok(Expr {
                span: Span::new(struct_name.span.start, finish.finish),
                kind: ExprKind::StructLiteral(StructLiteral {
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
                    kind: ExprKind::StructLiteral(StructLiteral {
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
                kind: ExprKind::StringLiteral(match token.kind {
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
