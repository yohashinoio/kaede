use inkwell::{builder::Builder, context::Context, module::Module};

use crate::ast::ast::TranslationUnit;

// Instantiate per translation unit.
pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,

    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, mod_name: &str) -> Self {
        Self {
            context: context,
            module: context.create_module(mod_name),
            builder: context.create_builder(),
        }
    }

    pub fn gen(&self, ast: &TranslationUnit) -> &Module<'ctx> {
        for top in ast {
            self.gen_top(top);
        }

        &self.module
    }
}
