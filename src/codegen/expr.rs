use inkwell::values::{BasicValue, BasicValueEnum};

use crate::ast::ast::Expr;

use super::codegen::CodeGen;

impl CodeGen<'_> {
    pub fn gen_expr(&self, expr_ast: &Expr) -> BasicValueEnum {
        match expr_ast {
            Expr::Integer(int) => self
                .context
                .i32_type()
                .const_int(*int, false)
                .as_basic_value_enum(),

            _ => todo!(),
        }
    }
}
