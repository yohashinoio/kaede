use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

#[derive(Debug, PartialEq, Eq)]
pub struct Ty {
    ty: TyEnum,
    mutability: Mutability,
}

impl Ty {
    pub fn new(ty: TyEnum, mutability: Mutability) -> Self {
        Self { ty, mutability }
    }

    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        self.ty.as_llvm_type(context)
    }

    pub fn mutability(&self) -> Mutability {
        self.mutability
    }

    pub fn is_signed(&self) -> bool {
        match &self.ty {
            TyEnum::FundamentalType(fty) => fty.is_signed(),
        }
    }
}

/// Represents whether a value can be changed.
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

#[derive(Debug, PartialEq, Eq)]
pub enum FundamentalTypeKind {
    I32,
    Bool,
}

pub fn make_fundamental_type(kind: FundamentalTypeKind, mutability: Mutability) -> Ty {
    Ty {
        ty: TyEnum::FundamentalType(FundamentalType { kind }),
        mutability,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TyEnum {
    FundamentalType(FundamentalType),
}

impl TyEnum {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            TyEnum::FundamentalType(t) => t.as_llvm_type(context),
        }
    }
}

/// Interface for types.
trait Type {
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx>;
}
#[derive(Debug, PartialEq, Eq)]
pub struct FundamentalType {
    kind: FundamentalTypeKind,
}

impl FundamentalType {
    fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => true,
            Bool => false,
        }
    }
}

impl Type for FundamentalType {
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => context.i32_type().as_basic_type_enum(),

            Bool => context.bool_type().as_basic_type_enum(),
        }
    }
}
