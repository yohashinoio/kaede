use std::rc::Rc;

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

pub fn create_inferred_tuple(element_len: usize) -> TyKind {
    TyKind::Tuple({
        let mut v = Vec::with_capacity(element_len);
        (0..element_len).for_each(|_| {
            v.push(Rc::new(Ty {
                kind: Rc::new(TyKind::Inferred),
                mutability: Mutability::Not,
                external_module_name: None,
            }))
        });
        v
    })
}

pub fn wrap_in_ref(ty: Rc<Ty>, mutability: Mutability) -> Ty {
    Ty {
        external_module_name: ty.external_module_name,
        kind: TyKind::Reference(ReferenceType { refee_ty: ty }).into(),
        mutability,
    }
}

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
    pub external_module_name: Option<Symbol>,
}

impl Ty {
    pub fn new_inferred(mutability: Mutability) -> Self {
        Self {
            kind: TyKind::Inferred.into(),
            mutability,
            external_module_name: None,
        }
    }

    pub fn new_str(mutability: Mutability) -> Self {
        Self {
            kind: TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::Str,
            })
            .into(),
            mutability,
            external_module_name: None,
        }
    }

    /// Return true if it is a user-defined type
    pub fn is_udt(&self) -> bool {
        match self.kind.as_ref() {
            TyKind::Reference(rty) => matches!(rty.refee_ty.kind.as_ref(), TyKind::UserDefined(_)),
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        if let TyKind::Reference(rty) = self.kind.as_ref() {
            if let TyKind::Fundamental(fty) = rty.refee_ty.kind.as_ref() {
                return matches!(fty.kind, FundamentalTypeKind::Str);
            }
        }

        false
    }

    pub fn is_generic(&self) -> bool {
        matches!(self.kind.as_ref(), TyKind::Generic(_))
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
    I8,
    U8,
    I32,
    U32,
    I64,
    U64,
    Bool,
    Str,
}

impl std::fmt::Display for FundamentalTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FundamentalTypeKind::*;

        match self {
            I8 => write!(f, "i8"),
            U8 => write!(f, "u8"),
            I32 => write!(f, "i32"),
            U32 => write!(f, "u32"),
            I64 => write!(f, "i64"),
            U64 => write!(f, "u64"),

            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
        }
    }
}

pub fn make_fundamental_type(kind: FundamentalTypeKind, mutability: Mutability) -> Ty {
    Ty {
        kind: TyKind::Fundamental(FundamentalType { kind }).into(),
        mutability,
        external_module_name: None,
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TyKind {
    Fundamental(FundamentalType),

    UserDefined(UserDefinedType),

    Generic(GenericType),

    Reference(ReferenceType),

    Pointer(Rc<Ty> /* Pointee type */),

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
            Self::Fundamental(fty) => write!(f, "{}", fty.kind),

            Self::UserDefined(udt) => write!(f, "{}", udt.name.symbol().as_str()),

            Self::Generic(gty) => write!(f, "{}", gty.name.as_str()),

            Self::Reference(refee) => write!(f, "&{}", refee.refee_ty.kind),

            Self::Pointer(pointee) => write!(f, "*{}", pointee.kind),

            Self::Array((elem_ty, size)) => write!(f, "[{}; {}]", elem_ty.kind, size),

            Self::Tuple(elem_tys) => write!(
                f,
                "({})",
                elem_tys
                    .iter()
                    .map(|t| t.kind.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
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
            Self::Generic(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_signed(),

            Self::Pointer(_) => panic!("Cannot get sign information of pointer type!"),
            Self::Array(_) => panic!("Cannot get sign information of array type!"),
            Self::Tuple(_) => panic!("Cannot get sign information of tuple type!"),
            Self::Unit => panic!("Cannot get sign information of unit type!"),
            Self::Never => panic!("Cannot get sign information of never type!"),
            Self::Inferred => panic!("Cannot get sign information of inferred type!"),
        }
    }

    pub fn is_int_or_bool(&self) -> bool {
        match &self {
            Self::Fundamental(fty) => fty.is_int_or_bool(),
            Self::UserDefined(_) => todo!(),
            Self::Generic(_) => todo!(),
            Self::Reference(ty) => ty.refee_ty.kind.is_int_or_bool(),

            Self::Array(_)
            | Self::Tuple(_)
            | Self::Pointer(_)
            | Self::Unit
            | Self::Never
            | Self::Inferred => false,
        }
    }

    pub fn is_inferred(&self) -> bool {
        matches!(self, Self::Inferred)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FundamentalType {
    pub kind: FundamentalTypeKind,
}

impl FundamentalType {
    pub fn as_llvm_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 => context.i8_type().as_basic_type_enum(),
            U8 => context.i8_type().as_basic_type_enum(),
            I32 => context.i32_type().as_basic_type_enum(),
            U32 => context.i32_type().as_basic_type_enum(),
            I64 => context.i64_type().as_basic_type_enum(),
            U64 => context.i64_type().as_basic_type_enum(),

            Bool => context.bool_type().as_basic_type_enum(),
            Str => {
                let str_ty = context.ptr_type(AddressSpace::default());
                let len_ty = context.i64_type();
                // { *i8, i64 }
                context
                    .struct_type(&[str_ty.into(), len_ty.into()], true)
                    .into()
            }
        }
    }

    pub fn is_signed(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | I32 | I64 => true,
            U8 | U32 | U64 => false,
            Bool => false,
            Str => false,
        }
    }

    pub fn is_int_or_bool(&self) -> bool {
        use FundamentalTypeKind::*;

        match self.kind {
            I8 | U8 | I32 | U32 | I64 | U64 | Bool => true,
            Str => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GenericArgs {
    pub types: Vec<Rc<Ty>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UserDefinedType {
    pub name: Ident,
    pub generic_args: Option<GenericArgs>,
}

impl UserDefinedType {
    pub fn new(name: Ident, generic_args: Option<GenericArgs>) -> Self {
        Self { name, generic_args }
    }
}

impl PartialEq for UserDefinedType {
    fn eq(&self, other: &Self) -> bool {
        self.name.symbol() == other.name.symbol()
    }
}

impl Eq for UserDefinedType {}

#[derive(Debug, Eq, Clone)]
pub struct ReferenceType {
    pub refee_ty: Rc<Ty>,
}

impl PartialEq for ReferenceType {
    // No mutability comparisons
    fn eq(&self, other: &Self) -> bool {
        *self.refee_ty.kind == *other.refee_ty.kind
    }
}

impl ReferenceType {
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

#[derive(Debug, Clone)]
pub struct GenericType {
    pub name: Ident,
}

impl PartialEq for GenericType {
    fn eq(&self, other: &Self) -> bool {
        self.name.symbol() == other.name.symbol()
    }
}

impl Eq for GenericType {}
