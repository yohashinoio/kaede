use kaede_location::Spanned;
use kaede_type::Ty;

use crate::stmt::StmtList;

pub type Params = Vec<(String, Ty)>;

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub name: String,
    pub params: Params,
    pub body: StmtList,
    pub return_ty: Option<Ty>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopEnum {
    Func(Func),
}

pub type Top = Spanned<TopEnum>;
