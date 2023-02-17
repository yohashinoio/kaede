use inkwell::{context::Context, values::IntValue};

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
pub enum Expr {
    Int(Int),
    BinOp(BinOp),
    Ident(String),
    FuncCall(FuncCall),
}

impl Expr {
    pub fn new_i32(n: i32) -> Self {
        Expr::Int(Int::I32(n))
    }

    pub fn new_binop(lhs: Box<Expr>, op: BinOpKind, rhs: Box<Expr>) -> Self {
        Expr::BinOp(BinOp { lhs, op, rhs })
    }
}
