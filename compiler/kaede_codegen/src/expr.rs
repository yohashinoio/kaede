use inkwell::values::{BasicValue, BasicValueEnum, IntValue};
use kaede_ast::{BinOpKind, Expr};

use crate::CodeGen;

impl CodeGen<'_, '_> {
    pub fn expr(&self, expr_ast: &Expr) -> BasicValueEnum {
        match expr_ast {
            Expr::Integer(int) => self
                .context
                .i32_type()
                .const_int(*int, false)
                .as_basic_value_enum(),

            Expr::BinOp(lhs, op, rhs) => self.binop(lhs, op, rhs).as_basic_value_enum(),
        }
    }

    fn binop(&self, lhs: &Expr, op: &BinOpKind, rhs: &Expr) -> IntValue {
        use BinOpKind::*;

        let lhs = self.expr(lhs).into_int_value();
        let rhs = self.expr(rhs).into_int_value();

        match op {
            Add => self.builder.build_int_add(lhs, rhs, ""),
            Sub => self.builder.build_int_sub(lhs, rhs, ""),
            Mul => self.builder.build_int_mul(lhs, rhs, ""),
            Div => self.builder.build_int_signed_div(lhs, rhs, ""), // TODO: unsigned
        }
    }
}
