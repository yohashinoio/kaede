use inkwell::{context::Context, values::IntValue};
use kaede_type::TypeEnum;

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
pub enum Expr {
    Int(Int),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    Ident(String),
    FuncCall(FuncCall),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Return(pub Option<Expr>);

#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    pub name: String,
    pub init: Option<Expr>,
    pub ty: TypeEnum,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    Return(Return),
    Let(Let),
}

pub type StmtList = Vec<Stmt>;

#[derive(Debug, PartialEq, Eq)]
pub enum Top {
    Function { name: String, body: StmtList },
}

pub type TranslationUnit = Vec<Top>;
