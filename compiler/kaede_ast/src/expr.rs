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
}

impl IntKind {
    pub fn as_llvm_int<'ctx>(&self, context: &'ctx Context) -> IntValue<'ctx> {
        use IntKind::*;

        match self {
            I32(n) => context.i32_type().const_int(*n as u64, false),
        }
    }

    pub fn get_type(&self) -> Ty {
        match self {
            IntKind::I32(_) => make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum BinaryKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    FieldAccess,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub kind: BinaryKind,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(lhs: Box<Expr>, op: BinaryKind, rhs: Box<Expr>) -> Self {
        Self { lhs, kind: op, rhs }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Int(Int),
    StirngLiteral(String),
    Binary(Binary),
    Ident(Ident),
    FnCall(FnCall),
    StructLiteral(StructLiteral),
    True,
    False,
}
