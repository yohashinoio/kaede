use std::rc::Rc;

use inkwell::values::BasicValueEnum;
use kaede_type::Ty;

pub struct Value<'ctx> {
    /// None if void.
    val: Option<BasicValueEnum<'ctx>>,
    /// None if void type.
    ty: Option<Rc<Ty>>,
}

impl<'ctx> Value<'ctx> {
    pub fn new(val: BasicValueEnum<'ctx>, ty: Rc<Ty>) -> Self {
        Self {
            val: Some(val),
            ty: Some(ty),
        }
    }

    pub fn new_void() -> Self {
        Self {
            val: None,
            ty: None,
        }
    }

    pub fn get_value(&self) -> BasicValueEnum<'ctx> {
        self.val.unwrap()
    }

    pub fn get_type(&self) -> Rc<Ty> {
        self.ty.as_ref().unwrap().clone()
    }
}
