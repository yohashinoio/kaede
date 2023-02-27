use kaede_location::Span;
use kaede_type::Ty;

use crate::expr::Expr;

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
    pub name: String,
    pub init: Option<Expr>,
    pub ty: Ty,
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
}

/// Statement list
#[derive(Debug, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub span: Span,
}
