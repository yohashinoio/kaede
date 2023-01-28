use kaede_ast::{Expr, Top};

use crate::CodeGen;

impl CodeGen<'_, '_> {
    pub fn top(&self, top_ast: &Top) {
        match top_ast {
            Top::Function { name, body } => self.func(name, body),
        }
    }

    fn func(&self, name: &str, body: &Expr) {
        let fn_type = self.context.i32_type().fn_type(&[], false);

        let fn_value = self.module.add_function(name, fn_type, None);

        let basic_block = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(basic_block);

        self.builder.build_return(Some(&self.expr(body)));
    }
}
