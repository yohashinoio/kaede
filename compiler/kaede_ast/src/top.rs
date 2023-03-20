use kaede_span::Span;
use kaede_type::Ty;

use crate::{expr::Ident, stmt::Block};

/// Accessibility
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Access {
    Public,
    Private,
}

impl Access {
    pub fn is_public(self) -> bool {
        self == Self::Public
    }

    pub fn is_private(self) -> bool {
        self == Self::Private
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: Ident,
    pub ty: Ty,
    pub access: Access,
    pub offset: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<StructField>,
    pub span: Span,
}

pub type Params = Vec<(String, Ty)>;

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: Ident,
    pub params: Params,
    pub body: Block,
    pub return_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopLevelKind {
    Fn(Fn),
    Struct(Struct),
}
