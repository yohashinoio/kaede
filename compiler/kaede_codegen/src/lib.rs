use std::{collections::HashMap, rc::Rc};

use error::{CodegenError, CodegenResult};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::TranslationUnit;
use kaede_type::Ty;
use top::build_top_level;

mod error;
mod expr;
mod stmt;
mod top;
mod value;

#[cfg(test)]
mod tests;

pub type SymbolTable<'ctx> = HashMap<String, (PointerValue<'ctx>, Rc<Ty>)>;

pub type ReturnTypeTable<'ctx> = HashMap<FunctionValue<'ctx>, Option<Rc<Ty>>>;

pub type ParamTable<'ctx> = HashMap<FunctionValue<'ctx>, Vec<Rc<Ty>>>;

pub fn codegen<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    ast: TranslationUnit,
) -> CodegenResult<()> {
    CGCtx::new(context, module).generate(ast)?;

    Ok(())
}

/// Codegen context.
/// Instantiate per translation unit.
pub struct CGCtx<'ctx, 'module> {
    pub context: &'ctx Context,

    pub module: &'module Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub return_ty_table: ReturnTypeTable<'ctx>,
    pub param_table: ParamTable<'ctx>,
}

impl<'ctx, 'module> CGCtx<'ctx, 'module> {
    pub fn new(context: &'ctx Context, module: &'module Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            return_ty_table: ReturnTypeTable::new(),
            param_table: ParamTable::new(),
        }
    }

    pub fn generate(&mut self, ast: TranslationUnit) -> CodegenResult<()> {
        for top in ast {
            build_top_level(self, top)?;
        }

        self.module.verify().map_err(|e| CodegenError::LLVMError {
            what: e.to_string(),
        })?;

        Ok(())
    }
}
