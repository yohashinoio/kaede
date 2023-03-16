use std::{collections::HashMap, rc::Rc};

use inkwell::{types::StructType, values::FunctionValue};
use kaede_ast::top::Access;
use kaede_type::Ty;

pub type ReturnTypeTable<'ctx> = HashMap<FunctionValue<'ctx>, Option<Rc<Ty>>>;

pub type ParamTable<'ctx> = HashMap<FunctionValue<'ctx>, Vec<Rc<Ty>>>;

pub struct StructFieldInfo {
    pub ty: Ty,
    pub access: Access,
    pub offset: u64,
}

pub struct StructInfo {
    pub fields: HashMap<String, StructFieldInfo>,
}

pub type StructTable<'ctx> = HashMap<String, (StructType<'ctx>, StructInfo)>;

/// Holds information necessary for typing
#[derive(Default)]
pub struct TypeContext<'ctx> {
    pub return_ty_table: ReturnTypeTable<'ctx>,
    pub param_table: ParamTable<'ctx>,
    pub struct_table: StructTable<'ctx>,
}
