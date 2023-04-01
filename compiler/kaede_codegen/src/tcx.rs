use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::StructType,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::{expr::Ident, top::Visibility};
use kaede_type::Ty;

use crate::error::{CodegenError, CodegenResult};

type Symbol<'ctx> = (PointerValue<'ctx>, Rc<Ty>);

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

pub type ReturnTypeTable<'ctx> = HashMap<FunctionValue<'ctx>, Option<Rc<Ty>>>;

pub type ParamTable<'ctx> = HashMap<FunctionValue<'ctx>, Vec<Rc<Ty>>>;

pub struct StructFieldInfo {
    pub ty: Rc<Ty>,
    pub vis: Visibility,
    pub offset: u64,
}

pub struct StructInfo {
    pub fields: HashMap<String, StructFieldInfo>,
}

pub type StructTable<'ctx> = HashMap<String, (StructType<'ctx>, Rc<StructInfo>)>;

/// Holds information necessary for typing
#[derive(Default)]
pub struct TypeContext<'ctx> {
    pub return_ty_table: ReturnTypeTable<'ctx>,
    pub param_table: ParamTable<'ctx>,
    pub struct_table: StructTable<'ctx>,
    pub symbol_table_stack: Vec<SymbolTable<'ctx>>,
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
}
