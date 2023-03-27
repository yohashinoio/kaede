use std::rc::Rc;

use crate::{
    error::{CodegenError, CodegenResult},
    get_loaded_pointer,
    mangle::mangle_name,
    to_llvm_type,
    value::{has_signed, Value},
    CompileUnitContext,
};

use inkwell::{
    values::{BasicValue, IntValue},
    IntPredicate,
};
use kaede_ast::expr::{
    ArrayLiteral, Binary, BinaryKind, Borrow, Deref, Expr, ExprKind, FnCall, Ident, Index,
    LogicalNot, StructLiteral, TupleLiteral,
};
use kaede_type::{make_fundamental_type, FundamentalTypeKind, Mutability, Ty, TyKind, UDType};

pub fn build_expression<'ctx>(
    cucx: &CompileUnitContext<'ctx, '_, '_>,
    node: &Expr,
) -> CodegenResult<Value<'ctx>> {
    let builder = ExprBuilder::new(cucx);

    builder.build(node)
}

struct ExprBuilder<'a, 'ctx, 'm, 'c> {
    cucx: &'a CompileUnitContext<'ctx, 'm, 'c>,
}

impl<'a, 'ctx, 'm, 'c> ExprBuilder<'a, 'ctx, 'm, 'c> {
    fn new(cucx: &'a CompileUnitContext<'ctx, 'm, 'c>) -> Self {
        Self { cucx }
    }

    /// Generate expression code
    fn build(&self, node: &Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match &node.kind {
            ExprKind::Int(int) => Value::new(
                int.kind.as_llvm_int(self.cucx.context()).into(),
                Rc::new(int.kind.get_type()),
            ),

            ExprKind::StringLiteral(s) => self.string_literal(s),

            ExprKind::StructLiteral(node) => self.struct_literal(node)?,

            ExprKind::ArrayLiteral(node) => self.array_literal(node)?,

            ExprKind::TupleLiteral(node) => self.tuple_literal(node)?,

            ExprKind::Ident(name) => self.expr_ident(name)?,

            ExprKind::Binary(node) => self.binary_op(node)?,

            ExprKind::LogicalNot(node) => self.logical_not(node)?,

            ExprKind::FnCall(node) => self.call_fn(node)?,

            // Boolean literals
            ExprKind::True => self.boolean_literal(true),
            ExprKind::False => self.boolean_literal(false),

            ExprKind::Borrow(node) => self.borrow(node)?,

            ExprKind::Deref(node) => self.deref(node)?,

            ExprKind::Index(node) => self.index_expr(node)?,
        })
    }

    fn tuple_literal(&self, node: &TupleLiteral) -> CodegenResult<Value<'ctx>> {
        let element_values = {
            let mut v = Vec::new();
            for e in node.elements.iter() {
                v.push(build_expression(self.cucx, e)?);
            }
            v
        };

        Ok(Value::new(
            self.cucx
                .context()
                .const_struct(
                    element_values
                        .iter()
                        .map(|v| v.get_value())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    true,
                )
                .into(),
            Ty {
                kind: TyKind::Tuple(element_values.iter().map(|v| v.get_type()).collect()).into(),
                mutability: Mutability::Not,
            }
            .into(),
        ))
    }

    fn index_expr(&self, node: &Index) -> CodegenResult<Value<'ctx>> {
        let array = build_expression(self.cucx, &node.operand)?;
        let array_ty = array.get_type();

        let elem_ty = match array_ty.kind.as_ref() {
            TyKind::Array((elem_ty, _)) => elem_ty,
            _ => todo!(), /* Error */
        };

        let array_ty_llvm = to_llvm_type(self.cucx, &array_ty).into_array_type();

        let load_array_inst = array.get_value().as_instruction_value().unwrap();

        let ptr_to_array = get_loaded_pointer(&load_array_inst).unwrap();

        // Remove load instruction as it is not needed
        load_array_inst.erase_from_basic_block();

        let index = build_expression(self.cucx, &node.index)?;

        // Calculate the address of the index-th element
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                array_ty_llvm,
                ptr_to_array,
                &[
                    self.cucx.context().i32_type().const_zero(),
                    index.get_value().into_int_value(),
                ],
                "",
            )
        };

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(array_ty_llvm.get_element_type(), gep, ""),
            elem_ty.clone(),
        ))
    }

    fn array_literal(&self, node: &ArrayLiteral) -> CodegenResult<Value<'ctx>> {
        assert!(!node.elements.is_empty());

        let mut elems = Vec::new();

        // Compile elements
        for elem in node.elements.iter() {
            elems.push(build_expression(self.cucx, elem)?);
        }

        let array_ty = Rc::new(Ty {
            kind: TyKind::Array((elems[0].get_type(), elems.len() as u32)).into(),
            mutability: Mutability::Not,
        });

        let alloca = self.cucx.create_entry_block_alloca("arrtmp", &array_ty);

        let array_ty_llvm = to_llvm_type(self.cucx, &array_ty);

        for (idx, elem) in elems.iter().enumerate() {
            let gep = unsafe {
                self.cucx.builder.build_in_bounds_gep(
                    array_ty_llvm,
                    alloca,
                    &[
                        self.cucx.context().i32_type().const_zero(),
                        self.cucx.context().i32_type().const_int(idx as u64, false),
                    ],
                    "",
                )
            };

            self.cucx.builder.build_store(gep, elem.get_value());
        }

        Ok(Value::new(
            self.cucx.builder.build_load(array_ty_llvm, alloca, ""),
            array_ty,
        ))
    }

    fn logical_not(&self, node: &LogicalNot) -> CodegenResult<Value<'ctx>> {
        let operand = build_expression(self.cucx, &node.operand)?;

        let zero = to_llvm_type(self.cucx, &operand.get_type()).const_zero();

        // Compared to zero, it would be equivalent to 'logical not'
        Ok(Value::new(
            self.cucx
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    operand.get_value().into_int_value(),
                    zero.into_int_value(),
                    "",
                )
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        ))
    }

    fn deref(&self, node: &Deref) -> CodegenResult<Value<'ctx>> {
        let operand = build_expression(self.cucx, &node.operand)?;

        let (pointee_ty, mutability) = match operand.get_type().kind.as_ref() {
            TyKind::Reference((pointee_ty, m)) => (pointee_ty.clone(), *m),

            kind => {
                return Err(CodegenError::CannotDeref {
                    ty: kind.to_string(),
                    span: node.span,
                })
            }
        };

        let loaded_operand = self.cucx.builder.build_load(
            to_llvm_type(self.cucx, &pointee_ty),
            operand.get_value().into_pointer_value(),
            "",
        );

        Ok(Value::new(
            loaded_operand,
            Rc::new(Ty {
                kind: pointee_ty.kind.clone(),
                mutability,
            }),
        ))
    }

    fn borrow(&self, node: &Borrow) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = match &node.operand.kind {
            ExprKind::Ident(ident) => {
                let var = self.cucx.tcx.lookup_var(ident)?;

                if var.1.mutability.is_not() && node.mutability.is_mut() {
                    return Err(CodegenError::MutableBorrowingFromImmutable {
                        immutable_var: ident.name.clone(),
                        span: node.span,
                    });
                }

                (var.0, var.1.clone())
            }

            // Create a variable to take an address since the reference is to a temporary value
            _ => {
                let operand = build_expression(self.cucx, &node.operand)?;

                let ty = operand.get_type();

                let alloca = self.cucx.create_entry_block_alloca("temp", &ty);
                self.cucx.builder.build_store(alloca, operand.get_value());

                (alloca, ty)
            }
        };

        Ok(Value::new(
            ptr.as_basic_value_enum(),
            Rc::new(Ty {
                kind: TyKind::Reference((ty, node.mutability)).into(),
                // Pointers to references are always immutable!
                mutability: Mutability::Not,
            }),
        ))
    }

    fn boolean_literal(&self, value: bool) -> Value<'ctx> {
        Value::new(
            self.cucx
                .context()
                .bool_type()
                .const_int(value as u64, false)
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        )
    }

    fn struct_literal(&self, node: &StructLiteral) -> CodegenResult<Value<'ctx>> {
        let (_ty, info) = match self.cucx.tcx.struct_table.get(node.struct_name.as_str()) {
            Some(value) => value,

            None => {
                return Err(CodegenError::Undeclared {
                    span: node.struct_name.span,
                    name: node.struct_name.name.clone(),
                })
            }
        };

        let mut values = Vec::new();

        for value in node.values.iter() {
            let field_info = &info.fields[value.0.as_str()];

            // To sort by offset, store offset
            values.push((
                field_info.offset,
                build_expression(self.cucx, &value.1)?.get_value(),
            ));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));

        // Remove offsets
        let inits: Vec<_> = values.iter().map(|e| e.1).collect();

        Ok(Value::new(
            self.cucx.context().const_struct(&inits, true).into(),
            Rc::new(Ty::new(
                TyKind::UDType(UDType(node.struct_name.name.clone())).into(),
                Mutability::Not,
            )),
        ))
    }

    fn string_literal(&self, s: &str) -> Value<'ctx> {
        let global_s = self.cucx.builder.build_global_string_ptr(s, "str");

        Value::new(
            self.cucx
                .context()
                .const_struct(
                    &[
                        global_s.as_basic_value_enum(),
                        self.cucx
                            .context()
                            .i64_type()
                            .const_int(s.len() as u64, false)
                            .into(),
                    ],
                    true,
                )
                .into(),
            Rc::new(Ty::new(TyKind::Str.into(), Mutability::Not)),
        )
    }

    fn expr_ident(&self, ident: &Ident) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = self.cucx.tcx.lookup_var(ident)?;

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(to_llvm_type(self.cucx, ty), *ptr, ""),
            ty.clone(),
        ))
    }

    fn binary_op(&self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        match node.kind {
            Access => self.access(node),

            _ => self.binary_arithmetic_op(node),
        }
    }

    fn build_logical_or(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> Value<'ctx> {
        let bool_type = self.cucx.context().bool_type();

        let result = self.cucx.builder.build_or(b1, b2, "");

        let zero = bool_type.const_zero();
        let cmp = self
            .cucx
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "");
        let result = self
            .cucx
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "");

        Value::new(
            result,
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        )
    }

    fn build_logical_and(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> Value<'ctx> {
        let bool_type = self.cucx.context().bool_type();

        let result = self.cucx.builder.build_and(b1, b2, "");

        let zero = bool_type.const_zero();
        let cmp = self
            .cucx
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "");
        let result = self
            .cucx
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "");

        Value::new(
            result,
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        )
    }

    fn binary_arithmetic_op(&self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        let left = self.build(node.lhs.as_ref())?;
        let right = self.build(node.rhs.as_ref())?;

        let left_int = left.get_value().into_int_value();
        let right_int = right.get_value().into_int_value();

        Ok(match node.kind {
            LogicalOr => self.build_logical_or(left_int, right_int),

            LogicalAnd => self.build_logical_and(left_int, right_int),

            Add => Value::new(
                self.cucx
                    .builder
                    .build_int_add(left_int, right_int, "")
                    .into(),
                left.get_type(),
            ),

            Sub => Value::new(
                self.cucx
                    .builder
                    .build_int_sub(left_int, right_int, "")
                    .into(),
                left.get_type(),
            ),

            Mul => Value::new(
                self.cucx
                    .builder
                    .build_int_mul(left_int, right_int, "")
                    .into(),
                left.get_type(),
            ),

            Rem => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_signed_rem(left_int, right_int, "")
                            .into(),
                        left.get_type(),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_unsigned_rem(left_int, right_int, "")
                            .into(),
                        left.get_type(),
                    )
                }
            }

            Div => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_signed_div(left_int, right_int, "")
                            .into(),
                        left.get_type(),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_unsigned_div(left_int, right_int, "")
                            .into(),
                        left.get_type(),
                    )
                }
            }

            Eq => Value::new(
                self.cucx
                    .builder
                    .build_int_compare(IntPredicate::EQ, left_int, right_int, "")
                    .into(),
                Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )),
            ),

            Ne => Value::new(
                self.cucx
                    .builder
                    .build_int_compare(IntPredicate::NE, left_int, right_int, "")
                    .into(),
                Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )),
            ),

            Lt => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SLT, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::ULT, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Le => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SLE, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::ULE, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Gt => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SGT, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::UGT, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Ge => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SGE, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::UGE, left_int, right_int, "")
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Access => unreachable!(),
        })
    }

    /// Field access or module item access
    fn access(&self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::Access));

        if let ExprKind::Ident(modname) = &node.lhs.kind {
            if self.cucx.imported_modules.contains(modname.as_str()) {
                // --- Module item access ---
                self.cucx.module.set_name(modname.as_str());

                let value = build_expression(self.cucx, &node.rhs);

                // Revert to the current module name
                self.cucx.module.set_name(&self.cucx.modname);
                return value;
            }
        }

        // --- Field access ---

        // Pointer to left value (struct)
        let (p, struct_ty) = {
            match &node.lhs.kind {
                ExprKind::Ident(name) => self.cucx.tcx.lookup_var(name)?,

                ExprKind::FnCall(_) => todo!(),

                _ => {
                    return Err(CodegenError::HasNoFields {
                        span: node.lhs.span,
                    })
                }
            }
        };

        let struct_name = match struct_ty.kind.as_ref() {
            TyKind::UDType(n) => &n.0,
            _ => todo!(),
        };

        let field_name = match &node.rhs.kind {
            ExprKind::Ident(s) => s.as_str(),
            kind => unreachable!("{:?}", kind),
        };

        let (_, struct_info) = &self.cucx.tcx.struct_table[struct_name];

        let field_info = &struct_info.fields[field_name];
        let field_llvm_ty = to_llvm_type(self.cucx, &field_info.ty);

        let offset = field_info.offset;

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                to_llvm_type(self.cucx, struct_ty),
                *p,
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(offset, false),
                ],
                "",
            )
        };

        Ok(Value::new(
            self.cucx.builder.build_load(field_llvm_ty, gep, ""),
            struct_ty.clone(),
        ))
    }

    fn call_fn(&self, node: &FnCall) -> CodegenResult<Value<'ctx>> {
        let func = self
            .cucx
            .module
            .get_function(&mangle_name(self.cucx, node.name.as_str()));

        let args = {
            let mut args = Vec::new();

            for arg in node.args.iter() {
                args.push(self.build(arg)?);
            }

            args
        };

        match func {
            Some(func) => {
                let return_value = self
                    .cucx
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
                    Some(val) => {
                        Value::new(val, self.cucx.tcx.return_ty_table[&func].clone().unwrap())
                    }
                    None => Value::new_void(),
                })
            }

            None => Err(CodegenError::Undeclared {
                name: node.name.name.clone(),
                span: node.span,
            }),
        }
    }
}
