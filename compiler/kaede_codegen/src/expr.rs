use inkwell::values::{BasicValue, BasicValueEnum, IntValue};
use kaede_ast::{BinOpKind, Expr};

use crate::CodeGen;

pub fn build_expression<'a>(ctx: &'a CodeGen, node: &'a Expr) -> BasicValueEnum<'a> {
    let builder = ExprBuilder::new(ctx);

    builder.build(node)
}

struct ExprBuilder<'a, 'ctx, 'c> {
    ctx: &'a CodeGen<'ctx, 'c>,
}

impl<'a, 'ctx, 'c> ExprBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CodeGen<'ctx, 'c>) -> Self {
        Self { ctx }
    }

    fn build(&self, node: &Expr) -> BasicValueEnum<'ctx> {
        match node {
            Expr::Integer(int) => self
                .ctx
                .context
                .i32_type()
                .const_int(*int, false)
                .as_basic_value_enum(),

            Expr::BinOp(lhs, op, rhs) => self.binary_op(lhs, op, rhs).as_basic_value_enum(),
        }
    }

    fn binary_op(&self, lhs: &Expr, op: &BinOpKind, rhs: &Expr) -> IntValue<'ctx> {
        use BinOpKind::*;

        let lhs = self.build(lhs).into_int_value();
        let rhs = self.build(rhs).into_int_value();

        match op {
            Add => self.ctx.builder.build_int_add(lhs, rhs, ""),
            Sub => self.ctx.builder.build_int_sub(lhs, rhs, ""),
            Mul => self.ctx.builder.build_int_mul(lhs, rhs, ""),
            Div => self.ctx.builder.build_int_signed_div(lhs, rhs, ""), // TODO: unsigned
        }
    }
}
