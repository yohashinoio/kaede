use kaede_location::Spanned;
use kaede_type::Ty;

use crate::expr::Expr;

#[derive(Debug, PartialEq, Eq)]
pub struct Return(pub Option<Expr>);

#[derive(Debug, PartialEq, Eq)]
pub struct Let {
    pub name: String,
    pub init: Option<Expr>,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StmtEnum {
    Expr(Expr),
    Return(Return),
    Let(Let),
}

pub type Stmt = Spanned<StmtEnum>;

pub type StmtList = Vec<Stmt>;
