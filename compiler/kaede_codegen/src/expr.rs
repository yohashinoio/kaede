use std::rc::Rc;

use crate::{
    error::{CodegenError, CodegenResult},
    value::Value,
    CGCtx, SymbolTable,
};

use inkwell::{values::BasicValue, IntPredicate};
use kaede_ast::expr::{Binary, BinaryKind, Expr, ExprKind, FnCall, Ident};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability};

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

    // Variables
    scope: &'a SymbolTable<'ctx>,
}

impl<'a, 'ctx, 'c> ExprBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CGCtx<'ctx, 'c>, scope: &'a SymbolTable<'ctx>) -> Self {
        Self { ctx, scope }
    }

    /// Generate expression code.
    fn build(&self, node: Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match node.kind {
            ExprKind::Int(int) => Value::new(
                int.kind.as_llvm_int(self.ctx.context).as_basic_value_enum(),
                Rc::new(int.kind.get_type()),
            ),

            ExprKind::StirngLiteral(_) => todo!(),

            ExprKind::Ident(name) => self.expr_ident(name)?,

            ExprKind::BinOp(binop) => self.binary_op(binop)?,

            ExprKind::FnCall(fcall) => self.call_fn(fcall)?,
        })
    }

    fn expr_ident(&self, ident: Ident) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = self.scope.find(&ident)?;

        Ok(Value::new(
            self.ctx.builder.build_load(*ptr, ""),
            ty.clone(),
        ))
    }

    fn binary_op(&self, node: Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        let lhs = self.build(*node.lhs)?;
        let rhs = self.build(*node.rhs)?;

        let lhs_val = lhs.get_value().into_int_value();
        let rhs_val = rhs.get_value().into_int_value();

        Ok(match node.op {
            Add => Value::new(
                self.ctx
                    .builder
                    .build_int_add(lhs_val, rhs_val, "")
                    .as_basic_value_enum(),
                lhs.get_type(),
            ),

            Sub => Value::new(
                self.ctx
                    .builder
                    .build_int_sub(lhs_val, rhs_val, "")
                    .as_basic_value_enum(),
                lhs.get_type(),
            ),

            Mul => Value::new(
                self.ctx
                    .builder
                    .build_int_mul(lhs_val, rhs_val, "")
                    .as_basic_value_enum(),
                lhs.get_type(),
            ),

            Div => {
                if lhs.get_type().is_signed() || rhs.get_type().is_signed() {
                    Value::new(
                        self.ctx
                            .builder
                            .build_int_signed_div(lhs_val, rhs_val, "")
                            .as_basic_value_enum(),
                        lhs.get_type(),
                    )
                } else {
                    Value::new(
                        self.ctx
                            .builder
                            .build_int_unsigned_div(lhs_val, rhs_val, "")
                            .as_basic_value_enum(),
                        lhs.get_type(),
                    )
                }
            }

            Eq => Value::new(
                self.ctx
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs_val, rhs_val, "")
                    .as_basic_value_enum(),
                Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )),
            ),
        })
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
