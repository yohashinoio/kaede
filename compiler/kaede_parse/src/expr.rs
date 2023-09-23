use std::{collections::VecDeque, rc::Rc};

use kaede_ast::expr::{
    Args, ArrayLiteral, Binary, BinaryKind, Break, Else, Expr, ExprKind, FnCall, Ident, If,
    Indexing, Int, IntKind, LogicalNot, Loop, Match, MatchArm, MatchArms, Return, StructLiteral,
    TupleLiteral,
};
use kaede_lex::token::{Token, TokenKind};
use kaede_span::{Location, Span};

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
                        node.into(),
                        BinaryKind::LogicalOr,
                        right.into(),
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
                        node.into(),
                        BinaryKind::LogicalAnd,
                        right.into(),
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
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Eq, right.into())),
                };
            } else if self.consume_b(&TokenKind::Ne) {
                let right = self.lt_gt_le_ge()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Ne, right.into())),
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
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Lt, right.into())),
                };
            } else if self.consume_b(&TokenKind::Le) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Le, right.into())),
                };
            } else if self.consume_b(&TokenKind::Gt) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Gt, right.into())),
                };
            } else if self.consume_b(&TokenKind::Ge) {
                let right = self.add_or_sub()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Ge, right.into())),
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
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Add, right.into())),
                };
            } else if self.consume_b(&TokenKind::Minus) {
                let right = self.mul_or_div_or_rem()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Sub, right.into())),
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
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Mul, right.into())),
                };
            } else if self.consume_b(&TokenKind::Slash) {
                let right = self.unary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Div, right.into())),
                };
            } else if self.consume_b(&TokenKind::Percent) {
                let right = self.unary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(node.into(), BinaryKind::Rem, right.into())),
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
            let zero = Expr {
                kind: ExprKind::Int(Int {
                    kind: IntKind::I32(0),
                    span,
                }),
                span,
            }
            .into();

            let e = self.access()?;

            return Ok(Expr {
                span: Span::new(span.start, e.span.finish),
                kind: ExprKind::Binary(Binary::new(zero, BinaryKind::Sub, e.into())),
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

        self.access()
    }

    /// Field access or module item access or tuple indexing
    fn access(&mut self) -> ParseResult<Expr> {
        let mut node = self.indexing()?;

        loop {
            if self.consume_b(&TokenKind::Dot) {
                let right = self.indexing()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::Access,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Array subscripting
    fn indexing(&mut self) -> ParseResult<Expr> {
        let mut node = self.scope_resolution()?;

        loop {
            if self.consume_b(&TokenKind::OpenBracket) {
                let index = self.expr()?;
                let finish = self.consume(&TokenKind::CloseBracket)?.finish;
                let span = Span::new(node.span.start, finish);
                node = Expr {
                    span,
                    kind: ExprKind::Indexing(Indexing {
                        operand: node.into(),
                        index: index.into(),
                        span,
                    }),
                };
            } else {
                return Ok(node);
            }
        }
    }

    /// Scope resolution
    fn scope_resolution(&mut self) -> ParseResult<Expr> {
        let mut node = self.primary()?;

        loop {
            if self.consume_b(&TokenKind::DoubleColon) {
                let right = self.primary()?;
                node = Expr {
                    span: Span::new(node.span.start, right.span.finish),
                    kind: ExprKind::Binary(Binary::new(
                        node.into(),
                        BinaryKind::ScopeResolution,
                        right.into(),
                    )),
                };
            } else {
                return Ok(node);
            }
        }
    }

    fn primary(&mut self) -> ParseResult<Expr> {
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

        if self.check(&TokenKind::Break) {
            return self.break_();
        }

        if self.check(&TokenKind::Loop) {
            return self.loop_();
        }

        if self.check(&TokenKind::If) {
            let node = self.if_()?;
            return Ok(Expr {
                span: node.span,
                kind: ExprKind::If(node),
            });
        }

        if self.check(&TokenKind::Match) {
            return self.match_();
        }

        if self.check(&TokenKind::Return) {
            return self.return_();
        }

        // Array literal
        if self.check(&TokenKind::OpenBracket) {
            return self.array_literal();
        }

        if let Some(lit) = self.string_literal() {
            return Ok(lit);
        }

        if let Some(lit) = self.boolean_literal() {
            return Ok(lit);
        }

        // Integer
        if matches!(self.first().kind, TokenKind::Int(_)) {
            let int = self.integer()?;
            return Ok(Expr {
                span: int.span,
                kind: ExprKind::Int(int),
            });
        }

        if self.consume_b(&TokenKind::OpenParen) {
            let node = self.expr()?;

            if let Ok(span) = self.consume(&TokenKind::Comma) {
                // Tuple literal
                return self.tuple_literal(span.start, node);
            }

            // '(' expr ')'
            self.consume(&TokenKind::CloseParen)?;
            return Ok(node);
        }

        Err(ParseError::ExpectedError {
            expected: "expression".to_string(),
            but: self.first().kind.to_string(),
            span: self.first().span,
        })
    }

    fn match_(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Match)?.start;

        let value = self.cond_expr()?;

        self.consume(&TokenKind::OpenBrace)?;

        let mut arms = Vec::new();

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let pattern = self.expr()?;

            self.consume(&TokenKind::Eq)?;
            self.consume(&TokenKind::Gt)?;

            let code = self.expr()?;

            arms.push(MatchArm {
                pattern,
                code: Rc::new(code),
            });

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        let span = Span::new(start, finish);

        Ok(Expr {
            span,
            kind: ExprKind::Match(Match {
                target: value.into(),
                arms: MatchArms::new(arms),
                span,
            }),
        })
    }

    fn comma_separated_elements(&mut self, end: &TokenKind) -> ParseResult<Vec<Expr>> {
        let mut elems = Vec::new();

        loop {
            elems.push(self.expr()?);

            if self.check(end) {
                return Ok(elems);
            }

            self.consume(&TokenKind::Comma)?;
        }
    }

    fn comma_separated_elements_deque(&mut self, end: &TokenKind) -> ParseResult<VecDeque<Expr>> {
        let mut elems = VecDeque::new();

        loop {
            elems.push_back(self.expr()?);

            if self.check(end) {
                return Ok(elems);
            }

            self.consume(&TokenKind::Comma)?;
        }
    }

    /// (xxx, 58, true)
    /// ^~~~~
    /// Expect that this part has already been analyzed
    fn tuple_literal(&mut self, start: Location, first_elem: Expr) -> ParseResult<Expr> {
        let mut elements = self.comma_separated_elements_deque(&TokenKind::CloseParen)?;

        elements.push_front(first_elem);

        let finish = self.consume(&TokenKind::CloseParen).unwrap().finish;

        let span = Span::new(start, finish);

        Ok(Expr {
            kind: ExprKind::TupleLiteral(TupleLiteral { elements, span }),
            span,
        })
    }

    fn array_literal(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::OpenBracket).unwrap().start;

        let elements = self.comma_separated_elements(&TokenKind::CloseBracket)?;

        let finish = self.consume(&TokenKind::CloseBracket).unwrap().finish;

        let span = Span::new(start, finish);

        Ok(Expr {
            kind: ExprKind::ArrayLiteral(ArrayLiteral { elements, span }),
            span,
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

    fn struct_literal(&mut self, struct_name: Ident) -> ParseResult<Expr> {
        let mut inits = Vec::new();

        self.consume(&TokenKind::OpenBrace)?;

        loop {
            if self.check(&TokenKind::CloseBrace) {
                break;
            }

            let name = self.ident()?;

            self.consume(&TokenKind::Colon)?;

            let init = self.expr()?;

            inits.push((name, init));

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseBrace)?.finish;

        Ok(Expr {
            span: Span::new(struct_name.span.start, finish),
            kind: ExprKind::StructLiteral(StructLiteral {
                struct_name,
                values: inits,
            }),
        })
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
        let args = self.fn_call_args()?;

        let start = name.span.start;
        let span = Span::new(start, args.1.finish);

        Ok(Expr {
            kind: ExprKind::FnCall(FnCall { name, args, span }),
            span,
        })
    }

    /// Works with zero arguments
    fn fn_call_args(&mut self) -> ParseResult<Args> {
        let start = self.consume(&TokenKind::OpenParen)?.start;

        let mut args = VecDeque::new();

        if let Ok(span) = self.consume(&TokenKind::CloseParen) {
            // No arguments
            return Ok(Args(args, Span::new(start, span.finish)));
        }

        loop {
            args.push_back(self.expr()?);

            if !self.consume_b(&TokenKind::Comma) {
                break;
            }
        }

        let finish = self.consume(&TokenKind::CloseParen)?.finish;
        Ok(Args(args, Span::new(start, finish)))
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

    fn break_(&mut self) -> ParseResult<Expr> {
        let span = self.consume(&TokenKind::Break).unwrap();

        Ok(Expr {
            kind: ExprKind::Break(Break { span }),
            span,
        })
    }

    fn loop_(&mut self) -> ParseResult<Expr> {
        let start = self.consume(&TokenKind::Loop).unwrap().start;

        let body = self.block()?;

        let span = Span::new(start, body.span.finish);

        Ok(Expr {
            kind: ExprKind::Loop(Loop { span, body }),
            span,
        })
    }

    fn if_(&mut self) -> ParseResult<If> {
        let start = self.consume(&TokenKind::If).unwrap().start;

        let cond = self.cond_expr()?;

        let then = Rc::new(self.block()?);

        let else_ = self.else_()?.map(Box::new);

        let finish = match else_.as_ref() {
            Some(else_) => match else_.as_ref() {
                Else::Block(block) => block.span.finish,
                Else::If(if_) => if_.span.finish,
            },

            None => then.span.finish,
        };

        Ok(If {
            cond: cond.into(),
            then,
            else_,
            span: Span::new(start, finish),
        })
    }

    /// `None` if there is no else
    fn else_(&mut self) -> ParseResult<Option<Else>> {
        if !self.consume_b(&TokenKind::Else) {
            return Ok(None);
        }

        if self.check(&TokenKind::If) {
            return Ok(Some(Else::If(self.if_()?)));
        }

        Ok(Some(Else::Block(Rc::new(self.block()?))))
    }

    fn return_(&mut self) -> ParseResult<Expr> {
        let span = self.consume(&TokenKind::Return).unwrap();

        if self.check_semi() {
            return Ok(Expr {
                kind: ExprKind::Return(Return { val: None, span }),
                span,
            });
        }

        let expr = self.expr()?;

        let span = Span::new(span.start, expr.span.finish);

        Ok(Expr {
            kind: ExprKind::Return(Return {
                span,
                val: Some(expr.into()),
            }),
            span,
        })
    }
}
