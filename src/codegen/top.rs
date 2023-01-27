use crate::ast::ast::{Expr, Function, Top};

use super::codegen::CodeGen;

impl CodeGen<'_> {
    pub fn gen_top(&self, top: &Top) {
        match top {
            Top::Function(func) => self.gen_fn(func),
        }
    }

    fn gen_fn(&self, func: &Function) {
        let fn_type = self.context.i32_type().fn_type(&[], false);

        let function = self.module.add_function(&func.name, fn_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        match func.body {
            Expr::Integer(int) => {
                let return_value = self.context.i32_type().const_int(int, false);
                self.builder.build_return(Some(&return_value));
            }

            _ => todo!(),
        }
    }
}
