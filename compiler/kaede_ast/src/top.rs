use kaede_location::Span;
use kaede_type::Ty;

use crate::stmt::StmtList;

pub type Params = Vec<(String, Ty)>;

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub params: Params,
    pub body: StmtList,
    pub return_ty: Option<Ty>,
}

pub struct Top {
    pub kind: TopKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopKind {
    Fn(Fn),
}
