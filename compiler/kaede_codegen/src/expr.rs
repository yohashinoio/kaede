use std::rc::Rc;

use crate::{
    as_llvm_type,
    error::{CodegenError, CodegenResult},
    value::Value,
    CGCtx, SymbolTable,
};

use inkwell::{values::BasicValue, IntPredicate};
use kaede_ast::expr::{Binary, BinaryKind, Expr, ExprKind, FnCall, Ident, StructLiteral};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind, UDType};

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

    /// Generate expression code
    fn build(&self, node: Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match node.kind {
            ExprKind::Int(int) => Value::new(
                int.kind.as_llvm_int(self.ctx.context).into(),
                Rc::new(int.kind.get_type()),
            ),

            ExprKind::StirngLiteral(s) => self.string_literal(&s),

            ExprKind::Ident(name) => self.expr_ident(name)?,

            ExprKind::Binary(b) => self.binary_op(b)?,

            ExprKind::FnCall(fcall) => self.call_fn(fcall)?,

            ExprKind::StructLiteral(node) => self.struct_literal(node)?,

            // Boolean literals
            ExprKind::True => self.boolean_literal(true),
            ExprKind::False => self.boolean_literal(false),
        })
    }

    fn boolean_literal(&self, value: bool) -> Value<'ctx> {
        Value::new(
            self.ctx
                .context
                .bool_type()
                .const_int(value as u64, false)
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        )
    }

    fn struct_literal(&self, node: StructLiteral) -> CodegenResult<Value<'ctx>> {
        let (_ty, info) = match self.ctx.struct_table.get(node.struct_name.as_str()) {
            Some(value) => value,

            None => {
                return Err(CodegenError::Undeclared {
                    span: node.struct_name.span,
                    name: node.struct_name.name,
                })
            }
        };

        let mut values = Vec::new();

        for value in node.values {
            let field_info = &info.fields[value.0.as_str()];

            // To sort by offset, store offset
            values.push((
                field_info.offset,
                build_expression(self.ctx, value.1, self.scope)?.get_value(),
            ));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));

        // Remove offsets
        let inits: Vec<_> = values.iter().map(|e| e.1).collect();

        Ok(Value::new(
            self.ctx.context.const_struct(&inits, true).into(),
            Rc::new(Ty::new(
                TyKind::UserDefinedType(UDType(node.struct_name.name)),
                Mutability::Not,
            )),
        ))
    }

    fn string_literal(&self, s: &str) -> Value<'ctx> {
        let global_s = self.ctx.builder.build_global_string_ptr(s, "str");

        Value::new(
            self.ctx
                .context
                .const_struct(
                    &[
                        global_s.as_basic_value_enum(),
                        self.ctx
                            .context
                            .i64_type()
                            .const_int(s.len() as u64, false)
                            .into(),
                    ],
                    true,
                )
                .into(),
            Rc::new(Ty::new(TyKind::Str, Mutability::Not)),
        )
    }

    fn expr_ident(&self, ident: Ident) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = self.scope.find(&ident)?;

        Ok(Value::new(
            self.ctx
                .builder
                .build_load(as_llvm_type(self.ctx, ty), *ptr, ""),
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
                self.ctx.builder.build_int_add(lhs_val, rhs_val, "").into(),
                lhs.get_type(),
            ),

            Sub => Value::new(
                self.ctx.builder.build_int_sub(lhs_val, rhs_val, "").into(),
                lhs.get_type(),
            ),

            Mul => Value::new(
                self.ctx.builder.build_int_mul(lhs_val, rhs_val, "").into(),
                lhs.get_type(),
            ),

            Div => {
                if lhs.get_type().kind.is_signed() || rhs.get_type().kind.is_signed() {
                    Value::new(
                        self.ctx
                            .builder
                            .build_int_signed_div(lhs_val, rhs_val, "")
                            .into(),
                        lhs.get_type(),
                    )
                } else {
                    Value::new(
                        self.ctx
                            .builder
                            .build_int_unsigned_div(lhs_val, rhs_val, "")
                            .into(),
                        lhs.get_type(),
                    )
                }
            }

            Eq => Value::new(
                self.ctx
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs_val, rhs_val, "")
                    .into(),
                Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )),
            ),
        })
    }

    fn call_fn(&self, node: FnCall) -> CodegenResult<Value<'ctx>> {
        let func = self.ctx.module.get_function(node.name.as_str());

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
