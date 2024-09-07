use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::StructType,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::top::{Enum, Fn, Struct, StructField, Visibility};
use kaede_symbol::{Ident, Symbol};
use kaede_type::Ty;

use crate::error::CodegenError;

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

#[derive(Debug)]
pub struct StructInfo<'ctx> {
    pub ty: StructType<'ctx>,
    pub fields: HashMap<Symbol, StructField>,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantInfo {
    pub name: Ident,
    pub _vis: Visibility,
    pub offset: u32,
    pub ty: Option<Rc<Ty>>,
}

#[derive(Debug)]
pub struct EnumInfo<'ctx> {
    pub ty: StructType<'ctx>,
    pub name: Symbol, // Non-mangled
    pub variants: HashMap<Symbol, EnumVariantInfo>,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug)]
pub enum UdtKind<'ctx> {
    Struct(StructInfo<'ctx>),
    Enum(EnumInfo<'ctx>),
    GenericArg(Rc<Ty>),
}

/// User defined type table
pub type UdtTable<'ctx> = HashMap<Symbol /* Mangled */, Rc<UdtKind<'ctx>>>;

#[derive(Debug)]
pub enum GenericKind {
    Struct(Struct),
    Func((Fn, Visibility)),
    Enum(Enum),
}

pub type GenericTable<'ctx> = HashMap<Symbol /* Mangled */, Rc<GenericKind>>;

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    variable_table_stack: Vec<VariableTable<'ctx>>,
    fn_table: FunctionTable<'ctx>,
    udt_table: UdtTable<'ctx>,
    generic_table: GenericTable<'ctx>,
}

impl<'ctx> TypeCtx<'ctx> {
    #[allow(dead_code)]
    pub fn dump_udt_table(&self) {
        for (name, kind) in &self.udt_table {
            eprintln!("{}: {:?}", name.as_str(), kind);
        }
    }

    pub fn lookup_variable(&self, ident: &Ident) -> anyhow::Result<&Variable<'ctx>> {
        for table in self.variable_table_stack.iter().rev() {
            if let Some(var) = table.lookup(ident.symbol()) {
                return Ok(var);
            }
        }

        Err(CodegenError::Undeclared {
            name: ident.symbol(),
            span: ident.span(),
        }
        .into())
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
        assert!(self.fn_table.insert(fn_value, info.into()).is_none());
    }

    pub fn get_function_info(&mut self, fn_value: FunctionValue<'ctx>) -> Option<Rc<FunctionInfo>> {
        self.fn_table.get(&fn_value).cloned()
    }

    pub fn add_udt(&mut self, name: Symbol, kind: UdtKind<'ctx>) {
        assert!(self.udt_table.insert(name, kind.into()).is_none());
    }

    pub fn get_udt(&self, name: Symbol) -> Option<Rc<UdtKind<'ctx>>> {
        self.udt_table.get(&name).cloned()
    }

    pub fn remove_udt(&mut self, name: Symbol) {
        self.udt_table.remove(&name);
    }

    pub fn add_generic(&mut self, name: Symbol, value: GenericKind) {
        assert!(self.generic_table.insert(name, Rc::new(value)).is_none());
    }

    pub fn get_generic_info(&self, name: Symbol) -> Option<Rc<GenericKind>> {
        self.generic_table.get(&name).cloned()
    }
}
