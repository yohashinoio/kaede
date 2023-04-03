use std::rc::Rc;

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
};

/// No mutability comparisons
pub fn is_same_type(t1: &Ty, t2: &Ty) -> bool {
    if t1.kind == t2.kind {
        return true;
    }

    // True if either one is never type
    if matches!(t1.kind.as_ref(), TyKind::Never) || matches!(t2.kind.as_ref(), TyKind::Never) {
        return true;
    }

    false
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ty {
    pub kind: Rc<TyKind>,
    pub mutability: Mutability,
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
    U64,
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

    UserDefined(UserDefinedType),

    Reference(RefrenceType),

    Array((Rc<Ty> /* Element type */, u32 /* Size */)),

    Tuple(Vec<Rc<Ty>> /* Element types */),

    Unit,

    /// Same as Rust's never type
    Never,

    Inferred,
}

impl std::fmt::Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fundamental(fty) => write!(f, "{}", fty),

            Self::Str => write!(f, "str"),

            Self::UserDefined(udt) => write!(f, "{}", udt.name),

            Self::Reference(refee) => write!(f, "&{}", refee.refee_ty.kind),

            Self::Array((elem_ty, size)) => write!(f, "[{}; {}]", elem_ty.kind, size),

            Self::Tuple(elem_tys) => write!(
                f,
                "({})",
                elem_tys
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(",")
            ),

            Self::Unit => write!(f, "()"),

            Self::Never => write!(f, "!"),

            Self::Inferred => write!(f, "_"),
        }
    }
}

impl TyKind {
    pub fn is_signed(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_signed(),
            Self::UserDefined(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_signed(),

            Self::Array(_) => panic!("Cannot get sign information of array type!"),
            Self::Tuple(_) => panic!("Cannot get sign information of tuple type!"),
            Self::Str => panic!("Cannot get sign information of str type!"),
            Self::Unit => panic!("Cannot get sign information of unit type!"),
            Self::Never => panic!("Cannot get sign information of never type!"),
            Self::Inferred => panic!("Cannot get sign information of inferred type!"),
        }
    }

    pub fn is_inferred(&self) -> bool {
        matches!(self, Self::Inferred)
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
            FundamentalTypeKind::U64 => write!(f, "u32"),

            FundamentalTypeKind::Bool => write!(f, "bool"),
        }
    }
}

impl FundamentalType {
    pub fn kind(&self) -> FundamentalTypeKind {
        self.kind
    }

    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => context.i32_type().as_basic_type_enum(),
            U64 => context.i64_type().as_basic_type_enum(),

            Bool => context.bool_type().as_basic_type_enum(),
        }
    }

    fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I32 => true,
            U64 => false,
            Bool => false,
        }
    }
}

/// User defined types
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UserDefinedType {
    pub name: String,
}

#[derive(Debug, Eq, Clone)]
pub struct RefrenceType {
    pub refee_ty: Rc<Ty>,
}

impl PartialEq for RefrenceType {
    // No mutability comparisons
    fn eq(&self, other: &Self) -> bool {
        *self.refee_ty.kind == *other.refee_ty.kind
    }
}

impl RefrenceType {
    pub fn new(refee_ty: Rc<Ty>) -> Self {
        Self { refee_ty }
    }

    /// &i32 -> i32
    ///
    /// &&i32 -> i32
    pub fn get_base_type_of_reference(&self) -> Rc<Ty> {
        match self.refee_ty.kind.as_ref() {
            TyKind::Reference(rty) => rty.get_base_type_of_reference(),

            _ => self.refee_ty.clone(),
        }
    }
}
