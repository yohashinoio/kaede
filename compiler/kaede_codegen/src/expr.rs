use inkwell::values::{BasicValue, BasicValueEnum, IntValue};
use kaede_ast::{BinOpKind, Expr, FuncCall};

use crate::{CodeGen, Symbols};

pub fn build_expression<'a, 'ctx, 'c>(
    ctx: &'a CodeGen<'ctx, 'c>,
    node: &'a Expr,
    scope: &'a Symbols<'ctx>,
) -> BasicValueEnum<'a> {
    let builder = ExprBuilder::new(ctx, scope);

    builder.build(node)
}

struct ExprBuilder<'a, 'ctx, 'c> {
    ctx: &'a CodeGen<'ctx, 'c>,
    scope: &'a Symbols<'ctx>,
}

impl<'a, 'ctx, 'c> ExprBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CodeGen<'ctx, 'c>, scope: &'a Symbols<'ctx>) -> Self {
        Self { ctx, scope }
    }

    fn build(&self, node: &Expr) -> BasicValueEnum<'ctx> {
        match node {
            Expr::Integer(int) => self
                .ctx
                .context
                .i32_type()
                .const_int(*int, false)
                .as_basic_value_enum(),

            Expr::Ident(ident) => {
                // TODO: 変数が見つからなかった時の処理
                self.ctx.builder.build_load(self.scope[ident], "")
            }

            Expr::BinOp(lhs, op, rhs) => self.binary_op(lhs, op, rhs).as_basic_value_enum(),

            Expr::FuncCall(fcall) => self.call_func(fcall),
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

    fn call_func(&self, node: &FuncCall) -> BasicValueEnum<'ctx> {
        let func = self.ctx.module.get_function(&node.name);

        // TODO: 関数が見つからなかった時の処理

        self.ctx
            .builder
            .build_call(func.unwrap(), &[], "")
            .try_as_basic_value()
            .left()
            .unwrap()
    }
}
