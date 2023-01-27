use crate::ast::ast::{Function, Top};

use super::codegen::CodeGen;

impl CodeGen<'_> {
    pub fn gen_top(&self, top_ast: &Top) {
        match top_ast {
            Top::Function(fn_ast) => self.gen_fn(fn_ast),
        }
    }

    fn gen_fn(&self, fn_ast: &Function) {
        let fn_type = self.context.i32_type().fn_type(&[], false);

        let fn_value = self.module.add_function(&fn_ast.name, fn_type, None);

        let basic_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(basic_block);

        self.builder
            .build_return(Some(&self.gen_expr(&fn_ast.body)));
    }
}
