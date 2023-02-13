use inkwell::{builder::Builder, context::Context, module::Module};
use kaede_ast::TranslationUnit;
use top::build_top;

mod expr;
mod stmt;
mod top;

#[cfg(test)]
mod tests;

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
            build_top(self, top);
        }
    }
}
