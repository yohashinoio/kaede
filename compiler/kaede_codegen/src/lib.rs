use std::{collections::HashMap, rc::Rc};

use error::{CodegenError, CodegenResult};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, PointerValue},
};
use kaede_ast::TranslationUnit;
use kaede_type::TypeEnum;
use top::build_top;

mod error;
mod expr;
mod stmt;
mod top;
mod value;

#[cfg(test)]
mod tests;

pub type Symbols<'ctx> = HashMap<String, (PointerValue<'ctx>, Rc<TypeEnum>)>;

pub type ReturnTypeTable<'ctx> = HashMap<FunctionValue<'ctx>, Option<Rc<TypeEnum>>>;

pub fn codegen<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    ast: TranslationUnit,
) -> CodegenResult<()> {
    CodeGen::new(context, module).generate(ast)?;

    Ok(())
}

// Instantiate per translation unit.
pub struct CodeGen<'ctx, 'module> {
    pub context: &'ctx Context,

    pub module: &'module Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub return_ty_table: ReturnTypeTable<'ctx>,
}

impl<'ctx, 'module> CodeGen<'ctx, 'module> {
    pub fn new(context: &'ctx Context, module: &'module Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            return_ty_table: ReturnTypeTable::new(),
        }
    }

    pub fn generate(&mut self, ast: TranslationUnit) -> CodegenResult<()> {
        for top in ast {
            build_top(self, top)?;
        }

        self.module.verify().map_err(|e| CodegenError::LLVMError {
            what: e.to_string(),
        })?;

        Ok(())
    }
}
