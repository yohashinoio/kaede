use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

#[derive(Debug, PartialEq, Eq)]
pub enum TypeEnum {
    FundamentalType(FundamentalType),
}

impl TypeEnum {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            TypeEnum::FundamentalType(t) => t.as_llvm_type(context),
        }
    }

    pub fn new_fundamental_type(kind: FundamentalTypeKind) -> Self {
        Self::FundamentalType(FundamentalType { kind })
    }
}

trait Type {
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx>;
}

#[derive(Debug, PartialEq, Eq)]
pub enum FundamentalTypeKind {
    I32,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FundamentalType {
    kind: FundamentalTypeKind,
}

impl Type for FundamentalType {
    fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => context.i32_type().as_basic_type_enum(),
        }
    }
}
