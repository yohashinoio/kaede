use kaede_location::Spanned;
use kaede_type::TypeEnum;

use crate::stmt::StmtList;

#[derive(Debug, PartialEq, Eq)]
pub struct Func {
    pub name: String,
    pub body: StmtList,
    pub return_ty: Option<TypeEnum>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopEnum {
    Func(Func),
}

pub type Top = Spanned<TopEnum>;
