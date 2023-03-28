use std::rc::Rc;

use kaede_span::Span;
use kaede_type::{Mutability, Ty};

use crate::expr::{Expr, Ident};

#[derive(Debug, PartialEq, Eq)]
pub enum AssignKind {
    /// '='
    Simple,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assign {
    pub lhs: Expr,
    pub kind: AssignKind,
    pub rhs: Expr,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Break {
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Loop {
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Else {
    If(If),
    Block(Block),
}

#[derive(Debug, PartialEq, Eq)]
pub struct If {
    pub cond: Expr,
    pub then: Block,
    pub else_: Option<Box<Else>>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Return {
    pub val: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    pub kind: LetKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LetKind {
    NormalLet(NormalLet),
    TupleUnpack(TupleUnpack),
}

#[derive(Debug, PartialEq, Eq)]
pub struct NormalLet {
    pub name: Ident,
    pub init: Option<Rc<Expr>>,
    pub ty: Rc<Ty>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TupleUnpack {
    pub names: Vec<(Ident, Mutability)>,
    pub init: Rc<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtKind {
    Expr(Expr),
    Return(Return),
    Let(Let),
    If(If),
    Loop(Loop),
    Break(Break),
    Assign(Assign),
}

/// Statement list
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub span: Span,
}
