use std::rc::Rc;

use inkwell::values::BasicValueEnum;
use kaede_type::Ty;

/// The type information in llvm's value is **insufficient**, so wrap it with our own type
///
/// For example, there is **no** sign information in the integer value of llvm
pub struct Value<'ctx> {
    /// None if void
    val: Option<BasicValueEnum<'ctx>>,
    /// None if void type
    ty: Option<Rc<Ty>>,
}

impl<'ctx> Value<'ctx> {
    pub fn new(val: BasicValueEnum<'ctx>, ty: Rc<Ty>) -> Self {
        Self {
            val: Some(val),
            ty: Some(ty),
        }
    }

    /// Create a `Value` representing a void value of type void
    pub fn new_void() -> Self {
        Self {
            val: None,
            ty: None,
        }
    }

    /// If the value is `void`, this function **panics**
    pub fn get_value(&self) -> BasicValueEnum<'ctx> {
        self.val.unwrap()
    }

    /// If the type is `void`, this function **panics**
    pub fn get_type(&self) -> Rc<Ty> {
        self.ty.as_ref().unwrap().clone()
    }
}
