use kaede_type::TypeEnum;

use crate::expr::Expr;

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
