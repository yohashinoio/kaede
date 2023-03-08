use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub mutability: Mutability,
}

impl Ty {
    pub fn new(kind: TyKind, mutability: Mutability) -> Self {
        Self { kind, mutability }
    }
}

/// Represents whether a value can be changed
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum Mutability {
    Not,
    Mut,
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
        kind: TyKind::Fundamental(FundamentalType { kind }),
        mutability,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyKind {
    Fundamental(FundamentalType),
    Str,

    UDType(UDType),

    /// If a type is not yet known
    Unknown,
}

impl TyKind {
    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),
            Self::UDType(_) => todo!(),

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
