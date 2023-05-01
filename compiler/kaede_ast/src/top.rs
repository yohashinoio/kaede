use std::collections::VecDeque;

use kaede_span::Span;
use kaede_type::{Mutability, Ty};

use crate::{expr::Ident, stmt::Block};

/// Accessibility
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(self) -> bool {
        self == Self::Public
    }

    pub fn is_private(self) -> bool {
        self == Self::Private
    }
}

impl From<bool> for Visibility {
    fn from(value: bool) -> Self {
        if value {
            Visibility::Public
        } else {
            Visibility::Private
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: Ident,
    pub ty: Ty,
    pub vis: Visibility,
    pub offset: u64,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Param {
    pub name: Ident,
    pub mutability: Mutability,
    pub ty: Ty,
}

/// Deque because sometimes it is necessary to insert self (C++ style: this) at the front
#[derive(Debug, PartialEq, Eq)]
pub struct Params(pub VecDeque<Param>, pub Span);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum FnKind {
    Normal,
    Method,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub kind: FnKind,
    pub name: Ident,
    pub params: Params,
    pub body: Block,
    pub return_ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Import {
    pub module_path: Ident,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Impl {
    pub name: Ident,
    pub items: Vec<TopLevel>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TopLevel {
    pub kind: TopLevelKind,
    pub vis: Visibility,
    pub span: Span,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TopLevelKind {
    Fn(Fn),
    Struct(Struct),
    Import(Import),
    Impl(Impl),
}
