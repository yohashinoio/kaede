use inkwell::values::{BasicValue, BasicValueEnum, IntValue};
use kaede_ast::expr::{BinOp, BinOpKind, Expr, ExprEnum, FuncCall};
use kaede_location::Span;

use crate::{
    error::{CodegenError, CodegenResult},
    CodeGen, Symbols,
};

pub fn build_expression<'a, 'ctx, 'c>(
    ctx: &'a CodeGen<'ctx, 'c>,
    node: Expr,
    scope: &'a Symbols<'ctx>,
) -> CodegenResult<BasicValueEnum<'a>> {
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

    fn build(&self, node: Expr) -> CodegenResult<BasicValueEnum<'ctx>> {
        Ok(match node.val {
            ExprEnum::Int(int) => int.as_llvm_int(self.ctx.context).as_basic_value_enum(),

            ExprEnum::Ident(name) => self.expr_ident(name, node.span)?,

            ExprEnum::BinOp(binop) => self.binary_op(binop)?.as_basic_value_enum(),

            ExprEnum::FuncCall(fcall) => self.call_func(fcall, node.span)?,
        })
    }

    fn expr_ident(&self, name: String, span: Span) -> CodegenResult<BasicValueEnum<'ctx>> {
        match self.scope.get(&name) {
            Some(ptr) => Ok(self.ctx.builder.build_load(*ptr, "")),

            None => Err(CodegenError::Undeclared { name, span }),
        }
    }

    fn binary_op(&self, node: BinOp) -> CodegenResult<IntValue<'ctx>> {
        use BinOpKind::*;

        let lhs = self.build(*node.lhs)?.into_int_value();
        let rhs = self.build(*node.rhs)?.into_int_value();

        Ok(match node.op {
            Add => self.ctx.builder.build_int_add(lhs, rhs, ""),
            Sub => self.ctx.builder.build_int_sub(lhs, rhs, ""),
            Mul => self.ctx.builder.build_int_mul(lhs, rhs, ""),
            Div => self.ctx.builder.build_int_signed_div(lhs, rhs, ""), // TODO: unsigned
        })
    }

    fn call_func(&self, node: FuncCall, span: Span) -> CodegenResult<BasicValueEnum<'ctx>> {
        let func = self.ctx.module.get_function(&node.name);

        match func {
            Some(func) => Ok(self
                .ctx
                .builder
                .build_call(func, &[], "")
                .try_as_basic_value()
                .left()
                .unwrap()),

            None => Err(CodegenError::Undeclared {
                name: node.name,
                span,
            }),
        }
    }
}
