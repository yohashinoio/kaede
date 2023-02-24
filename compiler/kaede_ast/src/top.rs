use kaede_location::Span;
use kaede_type::Ty;

use crate::stmt::Block;

pub type Params = Vec<(String, Ty)>;

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub params: Params,
    pub body: Block,
    pub return_ty: Option<Ty>,
}

pub struct TopLevel {
    pub kind: TopLevelKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopLevelKind {
    Fn(Fn),
}
