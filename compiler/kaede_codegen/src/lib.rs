use std::collections::HashMap;

use error::{CodegenError, CodegenResult};
use inkwell::{builder::Builder, context::Context, module::Module, values::PointerValue};
use kaede_ast::TranslationUnit;
use top::build_top;

mod error;
mod expr;
mod stmt;
mod top;

#[cfg(test)]
mod tests;

pub type Symbols<'ctx> = HashMap<String, PointerValue<'ctx>>;

pub fn codegen<'ctx, 'module>(
    context: &'ctx Context,
    module: &'module Module<'ctx>,
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
}

impl<'ctx, 'module> CodeGen<'ctx, 'module> {
    pub fn new(context: &'ctx Context, module: &'module Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
        }
    }

    pub fn generate(&self, ast: TranslationUnit) -> CodegenResult<()> {
        for top in ast {
            build_top(self, top)?;
        }

        self.module.verify().map_err(|e| CodegenError::LLVMError {
            what: e.to_string(),
        })?;

        Ok(())
    }
}
