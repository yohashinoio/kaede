use std::{collections::HashMap, rc::Rc};

use inkwell::{
    types::StructType,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::top::{Enum, Fn, GenericParams, Impl, Struct, StructField, Visibility};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};
use kaede_type::{GenericArgs, Ty, TyKind};

use crate::error::CodegenError;

#[derive(Debug)]
pub struct FunctionInfo<'ctx> {
    pub value: FunctionValue<'ctx>,
    pub return_type: ReturnType,
    pub param_types: Vec<Rc<Ty>>,
}

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

#[derive(Debug, Clone)]
pub struct EnumInfo<'ctx> {
    pub ty: StructType<'ctx>,
    pub name: Symbol, // Non-mangled
    pub variants: HashMap<Symbol, EnumVariantInfo>,
    pub is_external: Option<Vec<Ident>>,
}

#[derive(Debug)]
pub enum SymbolTableValue<'ctx> {
    Variable((PointerValue<'ctx>, Rc<Ty> /* Variable type */)),
    Function(FunctionInfo<'ctx>),
    Struct(StructInfo<'ctx>),
    Enum(EnumInfo<'ctx>),
    Module,
}

pub type SymbolTable<'ctx> = HashMap<Symbol, Rc<SymbolTableValue<'ctx>>>;

#[derive(Debug, Clone)]
pub enum ReturnType {
    Type(Rc<Ty>),
    Void,
}

impl From<Option<Rc<Ty>>> for ReturnType {
    fn from(value: Option<Rc<Ty>>) -> Self {
        match value {
            Some(ty) => ReturnType::Type(ty),
            None => ReturnType::Void,
        }
    }
}

#[derive(Debug)]
pub enum GenericKind {
    Struct(Struct),
    Func((Fn, Visibility)),
    Enum(Enum),
}

pub struct GenericTableValue {
    pub kind: GenericKind,
    pub is_external: Option<Vec<Ident>>,
}

pub type GenericTable = HashMap<Symbol /* Mangled */, Rc<GenericTableValue>>;

#[derive(Default)]
pub struct GenericImplTable {
    table: HashMap<Symbol /* Mangled */, Rc<(Impl, Visibility, Span)>>,

    // Using a hashset would compare even Span, etc., so Vec is used.
    already_defined: Vec<(Symbol /* Mangled */, GenericArgs)>,
}

pub struct GenericArgTable {
    map: HashMap<Symbol, Rc<Ty>>,
}

impl GenericArgTable {
    pub fn lookup(&self, symbol: Symbol) -> Option<Rc<Ty>> {
        self.map.get(&symbol).cloned()
    }
}

impl From<(GenericParams, GenericArgs)> for GenericArgTable {
    fn from((params, args): (GenericParams, GenericArgs)) -> Self {
        let mut map = HashMap::new();

        for (param, arg) in params.names.iter().zip(args.types.iter()) {
            map.insert(param.symbol(), arg.clone());
        }

        Self { map }
    }
}

#[derive(Default)]
pub struct TypeCtx<'ctx> {
    // Pushed when create a new scope.
    symbol_tables: Vec<SymbolTable<'ctx>>,

    generic_table: GenericTable,
    generic_impl_table: GenericImplTable,
    generic_arg_table_stack: Vec<GenericArgTable>,
}

impl<'ctx> TypeCtx<'ctx> {
    #[allow(dead_code)]
    pub fn dump_generic_arg_table(&self) {
        eprintln!("----------");
        for table in &self.generic_arg_table_stack {
            eprintln!("-----");
            for (name, ty) in &table.map {
                eprintln!("{} -> {:?}", name.as_str(), ty);
            }
            eprintln!("-----");
        }
        eprintln!("----------");
    }

    pub fn lookup_symbol(
        &self,
        symbol: Symbol,
        span: Span,
    ) -> anyhow::Result<Rc<SymbolTableValue<'ctx>>> {
        for table in self.symbol_tables.iter().rev() {
            if let Some(value) = table.get(&symbol) {
                return Ok(value.clone());
            }
        }

        Err(CodegenError::Undeclared { name: symbol, span }.into())
    }

    pub fn insert_symbol_to_root_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue<'ctx>,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .symbol_tables
            .first_mut()
            .unwrap()
            .insert(symbol, Rc::new(value))
            .is_some()
        {
            return Err(CodegenError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    pub fn insert_symbol_to_current_scope(
        &mut self,
        symbol: Symbol,
        value: SymbolTableValue<'ctx>,
        span: Span,
    ) -> anyhow::Result<()> {
        if self
            .symbol_tables
            .last_mut()
            .unwrap()
            .insert(symbol, Rc::new(value))
            .is_some()
        {
            return Err(CodegenError::AlreadyDeclared { name: symbol, span }.into());
        }

        Ok(())
    }

    pub fn push_symbol_table(&mut self, table: SymbolTable<'ctx>) {
        self.symbol_tables.push(table);
    }

    pub fn pop_symbol_table(&mut self) {
        self.symbol_tables.pop().unwrap();
    }

    pub fn add_generic(&mut self, name: Symbol, value: GenericTableValue) {
        assert!(self.generic_table.insert(name, Rc::new(value)).is_none());
    }

    pub fn get_generic_info(&self, name: Symbol) -> Option<Rc<GenericTableValue>> {
        self.generic_table.get(&name).cloned()
    }

    pub fn add_generic_impl(&mut self, name: Symbol, value: (Impl, Visibility, Span)) {
        assert!(self
            .generic_impl_table
            .table
            .insert(name, value.into())
            .is_none());
    }

    pub fn get_generic_impl(&self, name: Symbol) -> Option<Rc<(Impl, Visibility, Span)>> {
        self.generic_impl_table.table.get(&name).cloned()
    }

    pub fn push_generic_arg_table(&mut self, table: GenericArgTable) {
        self.generic_arg_table_stack.push(table);
    }

    pub fn pop_generic_arg_table(&mut self) {
        self.generic_arg_table_stack.pop().unwrap();
    }

    // Returns the actual type in preference to generic types.
    pub fn lookup_generic_arg(&self, symbol: Symbol) -> Option<Rc<Ty>> {
        for table in self.generic_arg_table_stack.iter().rev() {
            if let Some(ty) = table.lookup(symbol) {
                match ty.kind.as_ref() {
                    TyKind::Generic(gty) => {
                        if gty.name.symbol() == symbol {
                            continue;
                        } else {
                            return self.lookup_generic_arg(gty.name.symbol());
                        }
                    }
                    _ => return Some(ty),
                }
            }
        }

        None
    }

    pub fn register_defined_generic_impl(&mut self, name: Symbol, generic_args: GenericArgs) {
        self.generic_impl_table
            .already_defined
            .push((name, generic_args));
    }

    pub fn is_defined_generic_impl(&self, name: Symbol, generic_args: &GenericArgs) -> bool {
        for (n, args) in &self.generic_impl_table.already_defined {
            if n == &name && args.types == generic_args.types {
                return true;
            }
        }

        false
    }
}
