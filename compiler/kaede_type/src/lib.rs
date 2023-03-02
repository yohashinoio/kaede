use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    ty: TyEnum,
    pub mutability: Mutability,
}

impl Ty {
    pub fn new(ty: TyEnum, mutability: Mutability) -> Self {
        Self { ty, mutability }
    }

    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        self.ty.as_llvm_type(context)
    }

    pub fn is_signed(&self) -> bool {
        match &self.ty {
            TyEnum::FundamentalType(fty) => fty.is_signed(),

            TyEnum::Str => panic!("Cannot get sign information of Str type!"),
            TyEnum::Unknown => panic!("Cannot get sign information of Unknown type!"),
        }
    }

    pub fn is_unknown(&self) -> bool {
        self.ty.is_unknown()
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyEnum {
    FundamentalType(FundamentalType),
    Str,

    /// If a type is not yet known
    Unknown,
}

impl TyEnum {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::FundamentalType(t) => t.as_llvm_type(context),

            Self::Str => context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),

            Self::Unknown => panic!("Cannot get LLVM type of Unknown type!"),
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

/// Interface for types.
trait Type {
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx>;
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
