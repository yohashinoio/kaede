use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
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

    /// If a type is not yet known
    Unknown,
}

impl TyKind {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Self::Fundamental(t) => t.as_llvm_type(context),

            Self::Str => {
                let str_ty = context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
                let len_ty = context.i64_type().as_basic_type_enum();
                // { *i8, i64 }
                context
                    .struct_type(&[str_ty, len_ty], true)
                    .as_basic_type_enum()
            }

            Self::Unknown => panic!("Cannot get LLVM type of Unknown type!"),
        }
    }

    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),

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
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
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
