use std::rc::Rc;

use crate::{
    error::{CodegenError, CodegenResult},
    value::Value,
    CGCtx, SymbolTable,
};
use inkwell::values::BasicValue;
use kaede_ast::expr::{BinOp, BinOpKind, Expr, ExprKind, FnCall, Ident};

pub fn build_expression<'a, 'ctx>(
    ctx: &'a CGCtx<'ctx, '_>,
    node: Expr,
    scope: &'a SymbolTable<'ctx>,
) -> CodegenResult<Value<'ctx>> {
    let builder = ExprBuilder::new(ctx, scope);

    builder.build(node)
}

struct ExprBuilder<'a, 'ctx, 'c> {
    ctx: &'a CGCtx<'ctx, 'c>,
    scope: &'a SymbolTable<'ctx>,
}

impl<'a, 'ctx, 'c> ExprBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CGCtx<'ctx, 'c>, scope: &'a SymbolTable<'ctx>) -> Self {
        Self { ctx, scope }
    }

    fn build(&self, node: Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match node.kind {
            ExprKind::Int(int) => Value::new(
                int.kind.as_llvm_int(self.ctx.context).as_basic_value_enum(),
                Rc::new(int.kind.get_type()),
            ),

            ExprKind::Ident(name) => self.expr_ident(name)?,

            ExprKind::BinOp(binop) => self.binary_op(binop)?,

            ExprKind::FnCall(fcall) => self.call_fn(fcall)?,
        })
    }

    fn expr_ident(&self, ident: Ident) -> CodegenResult<Value<'ctx>> {
        match self.scope.get(&ident.name) {
            Some((ptr, ty)) => Ok(Value::new(
                self.ctx.builder.build_load(*ptr, ""),
                ty.clone(),
            )),

            None => Err(CodegenError::Undeclared {
                name: ident.name,
                span: ident.span,
            }),
        }
    }

    fn binary_op(&self, node: BinOp) -> CodegenResult<Value<'ctx>> {
        use BinOpKind::*;

        let lhs = self.build(*node.lhs)?;
        let rhs = self.build(*node.rhs)?;

        let lhs_val = lhs.get_value().into_int_value();
        let rhs_val = rhs.get_value().into_int_value();

        Ok(Value::new(
            match node.op {
                Add => self.ctx.builder.build_int_add(lhs_val, rhs_val, ""),
                Sub => self.ctx.builder.build_int_sub(lhs_val, rhs_val, ""),
                Mul => self.ctx.builder.build_int_mul(lhs_val, rhs_val, ""),
                Div => self.ctx.builder.build_int_signed_div(lhs_val, rhs_val, ""), // TODO: unsigned
            }
            .as_basic_value_enum(),
            lhs.get_type(),
        ))
    }

    fn call_fn(&self, node: FnCall) -> CodegenResult<Value<'ctx>> {
        let func = self.ctx.module.get_function(&node.name.name);

        let args = {
            let mut args = Vec::new();

            for arg in node.args {
                args.push(self.build(arg)?);
            }

            args
        };

        match func {
            Some(func) => {
                let return_value = self
                    .ctx
                    .builder
                    .build_call(
                        func,
                        args.iter()
                            .map(|a| a.get_value().into())
                            .collect::<Vec<_>>()
                            .as_slice(),
                        "",
                    )
                    .try_as_basic_value()
                    .left();

                Ok(match return_value {
                    Some(val) => Value::new(val, self.ctx.return_ty_table[&func].clone().unwrap()),
                    None => Value::new_void(),
                })
            }

            None => Err(CodegenError::Undeclared {
                name: node.name.name,
                span: node.span,
            }),
        }
    }
}
