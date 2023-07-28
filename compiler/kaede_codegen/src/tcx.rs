use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::StructType,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::{expr::Ident, top::Visibility};
use kaede_type::Ty;

use crate::error::{CodegenError, CodegenResult};

type Symbol<'ctx> = (PointerValue<'ctx>, Rc<Ty> /* Variable type */);

pub struct SymbolTable<'ctx>(HashMap<String, Symbol<'ctx>>);

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn lookup(&self, ident: &Ident) -> Option<&Symbol<'ctx>> {
        self.0.get(ident.as_str())
    }

    pub fn add(&mut self, name: String, symbol: Symbol<'ctx>) {
        self.0.insert(name, symbol);
    }
}

impl<'ctx> Default for SymbolTable<'ctx> {
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
    pub fields: HashMap<String, StructFieldInfo>,
}

pub struct EnumItemInfo {
    pub vis: Visibility,
    pub offset: u64,
}

pub struct EnumInfo {
    pub items: HashMap<String, EnumItemInfo>,
}

pub type StructTable<'ctx> = HashMap<String, Rc<StructInfo<'ctx>>>;

pub type EnumTable = HashMap<String, Rc<EnumInfo>>;

/// Holds information necessary for typing
#[derive(Default)]
pub struct TypeContext<'ctx> {
    return_ty_table: ReturnTypeTable<'ctx>,
    fn_params_table: ParamTable<'ctx>,
    struct_table: StructTable<'ctx>,
    enum_table: EnumTable,
    symbol_table_stack: Vec<SymbolTable<'ctx>>,
}

impl<'ctx> TypeContext<'ctx> {
    /// Lookup variables
    pub fn lookup_var(&self, ident: &Ident) -> CodegenResult<&Symbol<'ctx>> {
        for symbol_table in &self.symbol_table_stack {
            if let Some(symbol) = symbol_table.lookup(ident) {
                return Ok(symbol);
            }
        }

        Err(CodegenError::Undeclared {
            name: ident.name.clone(),
            span: ident.span,
        })
    }

    pub fn add_symbol(&mut self, name: String, symbol: Symbol<'ctx>) {
        self.symbol_table_stack
            .last_mut()
            .unwrap()
            .add(name, symbol);
    }

    pub fn push_symbol_table(&mut self, table: SymbolTable<'ctx>) {
        self.symbol_table_stack.push(table);
    }

    pub fn pop_symbol_table(&mut self) {
        self.symbol_table_stack.pop().unwrap();
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

    pub fn add_struct(&mut self, name: String, info: StructInfo<'ctx>) {
        self.struct_table.insert(name, info.into());
    }

    pub fn get_struct_info(&self, name: &str) -> Option<Rc<StructInfo<'ctx>>> {
        self.struct_table.get(name).cloned()
    }

    pub fn add_enum(&mut self, name: String, info: EnumInfo) {
        self.enum_table.insert(name, info.into());
    }

    pub fn get_enum_info(&self, name: &str) -> Option<Rc<EnumInfo>> {
        self.enum_table.get(name).cloned()
    }
}
