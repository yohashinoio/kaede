use std::{collections::VecDeque, rc::Rc, slice::Iter};

use inkwell::{context::Context, values::IntValue};
use kaede_span::Span;
use kaede_symbol::Symbol;
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty};

use crate::stmt::Block;

// Do not derive PartialEq or Eq!
// Unintended behavior is likely to be caused by comparisons involving span!
#[derive(Debug, Clone, Copy)]
pub struct Ident {
    name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn from_symbol_and_span(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    pub fn symbol(self) -> Symbol {
        self.name
    }
}

#[derive(Debug)]
pub struct StructLiteral {
    pub struct_name: Ident,
    pub values: Vec<(Ident, Expr)>,
}

#[derive(Debug)]
pub struct Args(pub VecDeque<Expr>, pub Span);

#[derive(Debug)]
pub struct FnCall {
    pub name: Ident,
    pub args: Args,
    pub span: Span,
}

#[derive(Debug)]
pub struct Int {
    pub kind: IntKind,
    pub span: Span,
}

#[derive(Debug)]
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

    ScopeResolution,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LogicalNot {
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct TupleLiteral {
    pub elements: VecDeque<Expr>,
    pub span: Span,
}

/// Sometimes called `Array subscripting`
#[derive(Debug)]
pub struct Indexing {
    pub operand: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Loop {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug)]
pub enum Else {
    If(If),
    Block(Rc<Block>),
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Rc<Block>,
    pub else_: Option<Box<Else>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Return {
    pub val: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Box<Expr>,
    pub code: Rc<Expr>,
}

impl MatchArm {
    pub fn is_wildcard(&self) -> bool {
        match &self.pattern.kind {
            ExprKind::Ident(ident) => ident.as_str() == "_",
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct MatchArmList {
    arms: Vec<MatchArm>,
    pub wildcard: Option<MatchArm>,
}

impl MatchArmList {
    pub fn new(arms: Vec<MatchArm>, wildcard: Option<MatchArm>) -> Self {
        Self { arms, wildcard }
    }

    pub fn non_wildcard_iter(&self) -> Iter<MatchArm> {
        self.arms.iter()
    }

    pub fn at(&self, idx: usize) -> &MatchArm {
        &self.arms[idx]
    }

    pub fn len(&self) -> usize {
        self.arms.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug)]
pub struct Match {
    pub target: Box<Expr>,
    pub arms: MatchArmList,
    pub span: Span,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Int(Int),
    StringLiteral(String),
    Binary(Binary),
    Ident(Ident),
    FnCall(FnCall),
    StructLiteral(StructLiteral),
    True,
    False,
    LogicalNot(LogicalNot),
    ArrayLiteral(ArrayLiteral),
    Indexing(Indexing),
    TupleLiteral(TupleLiteral),
    Return(Return),
    If(If),
    Loop(Loop),
    Break(Break),
    Match(Match),
    Block(Block),
}
