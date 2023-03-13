use std::rc::Rc;

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    pub kind: Rc<TyKind>,
    pub mutability: Mutability,
}

impl Ty {
    pub fn new(kind: Rc<TyKind>, mutability: Mutability) -> Self {
        Self { kind, mutability }
    }
}

/// Represents whether a value can be changed
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Mutability {
    Not,
    Mut,
}

impl From<bool> for Mutability {
    fn from(value: bool) -> Self {
        if value {
            Mutability::Mut
        } else {
            Mutability::Not
        }
    }
}

impl Mutability {
    /// Return `true` if self is mutable
    pub fn is_mut(self) -> bool {
        matches!(self, Self::Mut)
    }

    /// Return `true` if self is **not** mutable
    pub fn is_not(self) -> bool {
        matches!(self, Self::Not)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FundamentalTypeKind {
    I32,
    Bool,
}

pub fn make_fundamental_type(kind: FundamentalTypeKind, mutability: Mutability) -> Ty {
    Ty {
        kind: TyKind::Fundamental(FundamentalType { kind }).into(),
        mutability,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyKind {
    Fundamental(FundamentalType),
    Str,

    UDType(UDType),

    Reference(Rc<Ty>),

    /// If a type is not yet known
    Unknown,
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fundamental(fty) => write!(f, "{}", fty),

            Self::Str => write!(f, "str"),

            Self::UDType(udt) => write!(f, "{}", udt.0),

            Self::Reference(refee) => write!(f, "&{}", refee.kind),

            Self::Unknown => write!(f, "unknown"),
        }
    }
}

impl TyKind {
    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),
            Self::UDType(_) => todo!(),
            Self::Reference(ty) => ty.kind.is_signed(),

            Self::Str => panic!("Cannot get sign information of Str type!"),
            Self::Unknown => panic!("Cannot get sign information of Unknown type!"),
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FundamentalType {
    kind: FundamentalTypeKind,
}

impl std::fmt::Display for FundamentalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            FundamentalTypeKind::I32 => write!(f, "i32"),

            FundamentalTypeKind::Bool => write!(f, "bool"),
        }
    }
}

impl FundamentalType {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => context.i32_type().as_basic_type_enum(),

            Bool => context.bool_type().as_basic_type_enum(),
        }
    }

    fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => true,
            Bool => false,
        }
    }
}

/// User defined types
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UDType(pub String);
