use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::{BasicTypeEnum, StructType},
    values::{FunctionValue, PointerValue},
};
use kaede_ast::{expr::Ident, top::Visibility};
use kaede_symbol::Symbol;
use kaede_type::Ty;

use crate::error::{CodegenError, CodegenResult};

type Variable<'ctx> = (PointerValue<'ctx>, Rc<Ty> /* Variable type */);

#[derive(Debug)]
pub struct VariableTable<'ctx>(HashMap<Symbol, Variable<'ctx>>);

impl<'ctx> VariableTable<'ctx> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn lookup(&self, symbol: Symbol) -> Option<&Variable<'ctx>> {
        self.0.get(&symbol)
    }

    pub fn add(&mut self, symbol: Symbol, var: Variable<'ctx>) {
        self.0.insert(symbol, var);
    }
}

impl<'ctx> Default for VariableTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone)]
pub enum ReturnType {
    Type(Rc<Ty>),
    Void,
}

impl From<Option<Ty>> for ReturnType {
    fn from(value: Option<Ty>) -> Self {
        match value {
            Some(ty) => ReturnType::Type(ty.into()),
            None => ReturnType::Void,
        }
    }
}

pub type ReturnTypeTable<'ctx> = HashMap<FunctionValue<'ctx>, ReturnType>;

pub type FnParams = Vec<Rc<Ty>>;

pub type ParamTable<'ctx> = HashMap<FunctionValue<'ctx>, FnParams>;

pub struct StructFieldInfo {
    pub ty: Rc<Ty>,
    pub vis: Visibility,
    pub offset: u64,
}

pub struct StructInfo<'ctx> {
    pub ty: StructType<'ctx>,
    pub fields: HashMap<Symbol, StructFieldInfo>,
}

pub struct EnumVariantInfo {
    pub name: Ident,
    pub vis: Visibility,
    pub offset: u32,
    pub ty: Option<Rc<Ty>>,
}

pub struct EnumInfo<'ctx> {
    pub name: Ident,
    pub variants: HashMap<Symbol, EnumVariantInfo>,
    pub ty: BasicTypeEnum<'ctx>,
}

impl<'ctx> EnumInfo<'ctx> {
    pub fn get_variant_info_from_offset(&self, offset: u32) -> Option<&'_ EnumVariantInfo> {
        self.variants
            .values()
            .find(|&variant| variant.offset == offset)
    }
}

pub type StructTable<'ctx> = HashMap<Symbol, Rc<StructInfo<'ctx>>>;

pub type EnumTable<'ctx> = HashMap<Symbol, Rc<EnumInfo<'ctx>>>;

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    return_ty_table: ReturnTypeTable<'ctx>,
    fn_params_table: ParamTable<'ctx>,
    struct_table: StructTable<'ctx>,
    enum_table: EnumTable<'ctx>,
    variable_table_stack: Vec<VariableTable<'ctx>>,
}

impl<'ctx> TypeCtx<'ctx> {
    pub fn lookup_variable(&self, ident: &Ident) -> CodegenResult<&Variable<'ctx>> {
        for variable_table in &self.variable_table_stack {
            if let Some(var) = variable_table.lookup(ident.symbol()) {
                return Ok(var);
            }
        }

        Err(CodegenError::Undeclared {
            name: ident.symbol(),
            span: ident.span,
        })
    }

    pub fn add_variable(&mut self, symbol: Symbol, var: Variable<'ctx>) {
        self.variable_table_stack
            .last_mut()
            .unwrap()
            .add(symbol, var);
    }

    pub fn push_variable_table(&mut self, table: VariableTable<'ctx>) {
        self.variable_table_stack.push(table);
    }

    pub fn pop_variable_table(&mut self) {
        self.variable_table_stack.pop().unwrap();
    }

    pub fn add_fn_params(&mut self, fn_value: FunctionValue<'ctx>, params: FnParams) {
        self.fn_params_table.insert(fn_value, params);
    }

    pub fn get_fn_params(&self, fn_value: FunctionValue<'ctx>) -> Option<FnParams> {
        self.fn_params_table.get(&fn_value).cloned()
    }

    pub fn add_return_ty(&mut self, fn_value: FunctionValue<'ctx>, return_ty: ReturnType) {
        self.return_ty_table.insert(fn_value, return_ty);
    }

    pub fn get_return_ty(&self, fn_value: FunctionValue<'ctx>) -> Option<ReturnType> {
        self.return_ty_table.get(&fn_value).cloned()
    }

    pub fn add_struct(&mut self, name: Symbol, info: StructInfo<'ctx>) {
        self.struct_table.insert(name, info.into());
    }

    pub fn get_struct_info(&self, name: Symbol) -> Option<Rc<StructInfo<'ctx>>> {
        self.struct_table.get(&name).cloned()
    }

    pub fn add_enum(&mut self, name: Symbol, info: EnumInfo<'ctx>) {
        self.enum_table.insert(name, info.into());
    }

    pub fn get_enum_info(&self, name: Symbol) -> Option<Rc<EnumInfo<'ctx>>> {
        self.enum_table.get(&name).cloned()
    }
}
