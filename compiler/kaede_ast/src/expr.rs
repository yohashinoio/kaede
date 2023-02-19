use inkwell::{context::Context, values::IntValue};
use kaede_location::{spanned, Span, Spanned};

#[derive(Debug, PartialEq, Eq)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub name: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Int {
    I32(i32),
}

impl Int {
    pub fn as_llvm_int<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        use Int::*;

        match self {
            I32(n) => context.i32_type().const_int(*n as u64, false),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub op: BinOpKind,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprEnum {
    Int(Int),
    BinOp(BinOp),
    Ident(String),
    FuncCall(FuncCall),
}

pub fn new_i32(n: i32, span: Span) -> Expr {
    spanned(ExprEnum::Int(Int::I32(n)), span)
}

pub fn new_ident(name: String, span: Span) -> Expr {
    spanned(ExprEnum::Ident(name), span)
}

pub fn new_func_call(name: String, span: Span) -> Expr {
    spanned(ExprEnum::FuncCall(FuncCall { name }), span)
}

pub fn new_binop(lhs: Box<Expr>, op: BinOpKind, rhs: Box<Expr>, span: Span) -> Expr {
    spanned(ExprEnum::BinOp(BinOp { lhs, op, rhs }), span)
}

pub type Expr = Spanned<ExprEnum>;
