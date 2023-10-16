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

pub struct FunctionInfo {
    pub return_type: ReturnType,
    pub param_types: Vec<Rc<Ty>>,
}

pub type FunctionTable<'ctx> = HashMap<FunctionValue<'ctx>, Rc<FunctionInfo>>;

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

pub enum UDTKind<'ctx> {
    Struct(StructInfo<'ctx>),
    Enum(EnumInfo<'ctx>),
}

/// User defined type table
pub type UDTTable<'ctx> = HashMap<Symbol, Rc<UDTKind<'ctx>>>;

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    variable_table_stack: Vec<VariableTable<'ctx>>,
    fn_table: FunctionTable<'ctx>,
    udt_table: UDTTable<'ctx>,
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

    pub fn add_function_info(&mut self, fn_value: FunctionValue<'ctx>, info: FunctionInfo) {
        self.fn_table.insert(fn_value, info.into());
    }

    pub fn get_function_info(&mut self, fn_value: FunctionValue<'ctx>) -> Option<Rc<FunctionInfo>> {
        self.fn_table.get(&fn_value).cloned()
    }

    pub fn add_struct_ty(&mut self, name: Symbol, ty: StructInfo<'ctx>) {
        self.udt_table.insert(name, UDTKind::Struct(ty).into());
    }

    pub fn add_enum_ty(&mut self, name: Symbol, ty: EnumInfo<'ctx>) {
        self.udt_table.insert(name, UDTKind::Enum(ty).into());
    }

    pub fn get_udt(&self, name: Symbol) -> Option<Rc<UDTKind<'ctx>>> {
        self.udt_table.get(&name).cloned()
    }
}
