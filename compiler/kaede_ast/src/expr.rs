use std::{collections::VecDeque, rc::Rc};

use inkwell::{context::Context, values::IntValue};
use kaede_span::Span;
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

#[derive(Debug, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

// Struct literal
#[derive(Debug, PartialEq, Eq)]
pub struct StructLiteral {
    pub struct_name: Ident,
    pub values: Vec<(Ident, Expr)>,
}

pub type Args = Vec<Expr>;

#[derive(Debug, PartialEq, Eq)]
pub struct FnCall {
    pub name: Ident,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Int {
    pub kind: IntKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum IntKind {
    I32(i32),
    U64(u64),
}

impl Int {
    pub fn as_u64(&self) -> u64 {
        use IntKind::*;

        match self.kind {
            I32(n) => n as u64,

            U64(n) => n,
        }
    }

    pub fn as_llvm_int<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        use IntKind::*;

        match self.kind {
            I32(n) => context.i32_type().const_int(n as u64, true),
            U64(n) => context.i64_type().const_int(n, false),
        }
    }

    pub fn get_type(&self) -> Ty {
        match self.kind {
            IntKind::I32(_) => make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not),
            IntKind::U64(_) => make_fundamental_type(FundamentalTypeKind::U64, Mutability::Not),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum BinaryKind {
    // Addition
    Add,
    // Subtraction
    Sub,
    // Multiplication
    Mul,
    // Division
    Div,
    /// Integer remainder
    Rem,

    /// Equal to
    Eq,
    /// Not equal to
    Ne,

    /// Field access or module item access
    Access,

    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,

    LogicalOr,
    LogicalAnd,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Binary {
    pub lhs: Rc<Expr>,
    pub kind: BinaryKind,
    pub rhs: Rc<Expr>,
}

impl Binary {
    pub fn new(lhs: Rc<Expr>, op: BinaryKind, rhs: Rc<Expr>) -> Self {
        Self { lhs, kind: op, rhs }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Borrow {
    pub mutability: Mutability,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Deref {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleLiteral {
    pub elements: VecDeque<Expr>,
    pub span: Span,
}

/// Sometimes called `Array subscripting`
#[derive(Debug, PartialEq, Eq)]
pub struct Index {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Int(Int),
    StringLiteral(String),
    Binary(Binary),
    Ident(Ident),
    FnCall(FnCall),
    StructLiteral(StructLiteral),
    True,
    False,
    Borrow(Borrow),
    Deref(Deref),
    LogicalNot(LogicalNot),
    ArrayLiteral(ArrayLiteral),
    Indexing(Index),
    TupleLiteral(TupleLiteral),
}
