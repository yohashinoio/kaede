use kaede_location::Spanned;
use kaede_type::TypeEnum;

use crate::stmt::StmtList;

pub type Params = Vec<(String, TypeEnum)>;

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub name: String,
    pub params: Params,
    pub body: StmtList,
    pub return_ty: Option<TypeEnum>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopEnum {
    Func(Func),
}

pub type Top = Spanned<TopEnum>;
