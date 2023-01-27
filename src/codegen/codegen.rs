use inkwell::{builder::Builder, context::Context, module::Module};

use crate::ast::ast::TranslationUnit;

pub fn codegen<'ctx, 'module>(
    context: &'ctx Context,
    module: &'module Module<'ctx>,
    ast: &TranslationUnit,
) {
    CodeGen::new(context, module).gen(ast);
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

    pub fn gen(&self, ast: &TranslationUnit) {
        for top in ast {
            self.top(top);
        }
    }
}
