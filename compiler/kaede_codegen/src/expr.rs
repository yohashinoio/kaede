use std::rc::Rc;

use crate::{
    error::{CodegenError, CodegenResult},
    get_loaded_pointer,
    mangle::mangle_name,
    stmt::{build_block, build_statement},
    tcx::SymbolTable,
    value::{has_signed, Value},
    CompileUnitContext,
};

use inkwell::{
    values::{BasicValue, IntValue, PointerValue},
    IntPredicate,
};
use kaede_ast::{
    expr::{
        ArrayLiteral, Binary, BinaryKind, Borrow, Break, Deref, Else, Expr, ExprKind, FnCall,
        Ident, If, Indexing, LogicalNot, Loop, Return, StructLiteral, TupleLiteral,
    },
    stmt::{Block, StmtKind},
};
use kaede_span::Span;
use kaede_type::{
    is_same_type, make_fundamental_type, FundamentalTypeKind, Mutability, RefrenceType, Ty, TyKind,
    UDType,
};

/// Unit value if the end of the block is not an expression
pub fn build_block_expression<'ctx>(
    cucx: &mut CompileUnitContext<'ctx, '_, '_>,
    block: &Block,
) -> CodegenResult<Value<'ctx>> {
    if block.body.is_empty() {
        return Ok(Value::new_unit());
    }

    cucx.tcx.push_symbol_table(SymbolTable::new());

    let mut idx: usize = 0;

    let last_stmt = loop {
        if idx + 1 == block.body.len() {
            // Last element
            break &block.body[idx];
        }

        build_statement(cucx, &block.body[idx])?;

        idx += 1;
    };

    let value = match &last_stmt.kind {
        StmtKind::Expr(e) => build_expression(cucx, e)?,

        // The end of the block is not an expression
        _ => Value::new_unit(),
    };

    cucx.tcx.pop_symbol_table();

    Ok(value)
}

pub fn build_expression<'ctx>(
    cucx: &mut CompileUnitContext<'ctx, '_, '_>,
    node: &Expr,
) -> CodegenResult<Value<'ctx>> {
    let mut builder = ExprBuilder::new(cucx);

    builder.build(node)
}

pub fn build_tuple_indexing<'ctx>(
    cucx: &CompileUnitContext<'ctx, '_, '_>,
    tuple: PointerValue<'ctx>,
    index: u64,
    tuple_ty: &Rc<Ty>,
    span: Span,
) -> CodegenResult<Value<'ctx>> {
    let gep = unsafe {
        cucx.builder.build_in_bounds_gep(
            cucx.to_llvm_type(tuple_ty),
            tuple,
            &[
                cucx.context().i32_type().const_zero(),
                cucx.context().i32_type().const_int(index, false),
            ],
            "",
        )
    };

    let elem_ty = match tuple_ty.kind.as_ref() {
        TyKind::Tuple(types) => match types.get(index as usize) {
            Some(ty) => ty,

            None => return Err(CodegenError::IndexOutOfRange { index, span }),
        },
        kind => unreachable!("{:?}", kind),
    };

    Ok(Value::new(
        cucx.builder.build_load(cucx.to_llvm_type(elem_ty), gep, ""),
        Ty {
            kind: elem_ty.kind.clone(),
            mutability: tuple_ty.mutability,
        }
        .into(),
    ))
}

struct ExprBuilder<'a, 'ctx, 'm, 'c> {
    cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>,
}

impl<'a, 'ctx, 'm, 'c> ExprBuilder<'a, 'ctx, 'm, 'c> {
    fn new(cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>) -> Self {
        Self { cucx }
    }

    /// Generate expression code
    fn build(&mut self, node: &Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match &node.kind {
            ExprKind::Int(int) => Value::new(
                int.as_llvm_int(self.cucx.context()).into(),
                Rc::new(int.get_type()),
            ),

            ExprKind::StringLiteral(s) => self.string_literal(s),

            ExprKind::StructLiteral(node) => self.struct_literal(node)?,

            ExprKind::ArrayLiteral(node) => self.array_literal(node)?,

            ExprKind::TupleLiteral(node) => self.tuple_literal(node)?,

            ExprKind::Ident(name) => self.ident_expr(name)?,

            ExprKind::Binary(node) => self.binary_op(node)?,

            ExprKind::LogicalNot(node) => self.logical_not(node)?,

            ExprKind::FnCall(node) => self.call_fn(node)?,

            // Boolean literals
            ExprKind::True => self.boolean_literal(true),
            ExprKind::False => self.boolean_literal(false),

            ExprKind::Borrow(node) => self.borrow(node)?,

            ExprKind::Deref(node) => self.deref(node)?,

            ExprKind::Indexing(node) => self.indexing(node)?,

            ExprKind::Break(node) => self.break_(node)?,

            ExprKind::Return(node) => self.return_(node)?,

            ExprKind::Loop(node) => self.loop_(node)?,

            ExprKind::If(node) => self.if_(node)?,
        })
    }

    fn break_(&self, node: &Break) -> CodegenResult<Value<'ctx>> {
        match self.cucx.loop_break_bb_stk.last() {
            Some(bb) => {
                self.cucx.builder.build_unconditional_branch(*bb);
                Ok(Value::new_never())
            }

            None => Err(CodegenError::BreakOutsideOfLoop { span: node.span }),
        }
    }

    fn return_(&mut self, node: &Return) -> CodegenResult<Value<'ctx>> {
        match &node.val {
            Some(val) => {
                let value = build_expression(self.cucx, val)?;
                self.cucx.builder.build_return(Some(&value.get_value()))
            }

            None => self.cucx.builder.build_return(None),
        };

        Ok(Value::new_never())
    }

    fn loop_(&mut self, node: &Loop) -> CodegenResult<Value<'ctx>> {
        let parent = self.cucx.get_current_fn();

        let body_bb = self.cucx.context().append_basic_block(parent, "body");

        let cont_bb = self.cucx.context().append_basic_block(parent, "loopcont");

        // Setup for break statement
        self.cucx.loop_break_bb_stk.push(cont_bb);

        // Build body block
        self.cucx.builder.build_unconditional_branch(body_bb);
        self.cucx.builder.position_at_end(body_bb);
        build_block(self.cucx, &node.body)?;

        // Loop!
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(body_bb);
        }

        self.cucx.builder.position_at_end(cont_bb);

        Ok(Value::new_never())
    }

    fn if_(&mut self, node: &If) -> CodegenResult<Value<'ctx>> {
        let parent = self.cucx.get_current_fn();
        let zero_const = self.cucx.context().bool_type().const_zero();

        let cond = build_expression(self.cucx, &node.cond)?;
        let cond = self.cucx.builder.build_int_compare(
            IntPredicate::NE,
            cond.get_value().into_int_value(),
            zero_const,
            "ifcond",
        );

        let then_bb = self.cucx.context().append_basic_block(parent, "then");
        let else_bb = self.cucx.context().append_basic_block(parent, "else");
        let cont_bb = self.cucx.context().append_basic_block(parent, "ifcont");

        self.cucx
            .builder
            .build_conditional_branch(cond, then_bb, else_bb);

        // Build then block
        self.cucx.builder.position_at_end(then_bb);
        let then_val = build_block_expression(self.cucx, &node.then)?;
        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        let then_bb = self.cucx.builder.get_insert_block().unwrap();

        // Build else block
        self.cucx.builder.position_at_end(else_bb);

        let else_val = match &node.else_ {
            Some(else_) => match else_.as_ref() {
                Else::If(if_) => self.if_(if_)?,
                Else::Block(block) => build_block_expression(self.cucx, block)?,
            },

            _ => {
                // Check if it is used as an expression
                if self.cucx.is_if_statement {
                    Value::new_unit()
                } else {
                    return Err(CodegenError::IfMustHaveElseUsedAsExpr { span: node.span });
                }
            }
        };

        let else_bb = self.cucx.builder.get_insert_block().unwrap();

        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        self.cucx.builder.position_at_end(cont_bb);

        if self.cucx.is_if_statement {
            // Not used as a expression
            return Ok(Value::new_unit());
        }

        if !is_same_type(&then_val.get_type(), &else_val.get_type()) {
            return Err(CodegenError::IfAndElseHaveIncompatibleTypes {
                types: (
                    then_val.get_type().kind.to_string(),
                    else_val.get_type().kind.to_string(),
                ),
                span: node.span,
            });
        }

        // Either then_val or else_val could be never
        let ty = if matches!(then_val.get_type().kind.as_ref(), TyKind::Never) {
            return Ok(else_val);
        } else if matches!(else_val.get_type().kind.as_ref(), TyKind::Never) {
            return Ok(then_val);
        } else {
            then_val.get_type()
        };

        let phi = self
            .cucx
            .builder
            .build_phi(self.cucx.to_llvm_type(&ty), "iftmp");

        phi.add_incoming(&[
            (&then_val.get_value(), then_bb),
            (&else_val.get_value(), else_bb),
        ]);

        Ok(Value::new(phi.as_basic_value(), ty))
    }

    fn tuple_literal(&mut self, node: &TupleLiteral) -> CodegenResult<Value<'ctx>> {
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

    fn indexing(&mut self, node: &Indexing) -> CodegenResult<Value<'ctx>> {
        let array = build_expression(self.cucx, &node.operand)?;
        let array_ty = array.get_type();

        let elem_ty = match array_ty.kind.as_ref() {
            TyKind::Array((elem_ty, _)) => elem_ty,
            _ => todo!(), /* Error */
        };

        let array_ty_llvm = self.cucx.to_llvm_type(&array_ty).into_array_type();

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
            Ty {
                kind: elem_ty.kind.clone(),
                mutability: array_ty.mutability,
            }
            .into(),
        ))
    }

    fn array_literal(&mut self, node: &ArrayLiteral) -> CodegenResult<Value<'ctx>> {
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

        let array_ty_llvm = self.cucx.to_llvm_type(&array_ty);

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

    fn logical_not(&mut self, node: &LogicalNot) -> CodegenResult<Value<'ctx>> {
        let operand = build_expression(self.cucx, &node.operand)?;

        let zero = self.cucx.to_llvm_type(&operand.get_type()).const_zero();

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

    fn deref_internal(&self, operand: &Value<'ctx>, span: Span) -> CodegenResult<Value<'ctx>> {
        let (refee_ty, mutability) = match operand.get_type().kind.as_ref() {
            TyKind::Reference(rty) => (rty.refee_ty.clone(), rty.mutability),

            kind => {
                return Err(CodegenError::CannotDeref {
                    ty: kind.to_string(),
                    span,
                })
            }
        };

        let loaded_operand = self.cucx.builder.build_load(
            self.cucx.to_llvm_type(&refee_ty),
            operand.get_value().into_pointer_value(),
            "",
        );

        Ok(Value::new(
            loaded_operand,
            Rc::new(Ty {
                kind: refee_ty.kind.clone(),
                mutability,
            }),
        ))
    }

    fn deref(&mut self, node: &Deref) -> CodegenResult<Value<'ctx>> {
        let operand = build_expression(self.cucx, &node.operand)?;

        self.deref_internal(&operand, node.span)
    }

    fn borrow(&mut self, node: &Borrow) -> CodegenResult<Value<'ctx>> {
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
                kind: TyKind::Reference(RefrenceType::new(ty, node.mutability)).into(),
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

    fn struct_literal(&mut self, node: &StructLiteral) -> CodegenResult<Value<'ctx>> {
        let (_ty, info) = match self.cucx.tcx.struct_table.get(node.struct_name.as_str()) {
            Some(x) => (x.0, x.1.clone()),

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

            let value = build_expression(self.cucx, &value.1)?;

            // To sort by offset, store offset
            values.push((field_info.offset, value.get_value()));
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

    fn ident_expr(&self, ident: &Ident) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = self.cucx.tcx.lookup_var(ident)?;

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(self.cucx.to_llvm_type(ty), *ptr, ""),
            ty.clone(),
        ))
    }

    fn binary_op(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        match node.kind {
            Access => self.access(node),

            _ => self.binary_arithmetic_op(node),
        }
    }

    fn logical_or(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> Value<'ctx> {
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

    fn logical_and(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> Value<'ctx> {
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

    fn binary_arithmetic_op(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        let left = self.build(node.lhs.as_ref())?;
        let right = self.build(node.rhs.as_ref())?;

        let left_int = left.get_value().into_int_value();
        let right_int = right.get_value().into_int_value();

        Ok(match node.kind {
            LogicalOr => self.logical_or(left_int, right_int),

            LogicalAnd => self.logical_and(left_int, right_int),

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
    fn access(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
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

        let left = build_expression(self.cucx, &node.lhs)?;

        let left_ty = &left.get_type();

        if let TyKind::Reference(rty) = left_ty.kind.as_ref() {
            // ---  Field access or tuple indexing --- (Reference)
            if matches!(
                rty.get_base_type_of_reference().kind.as_ref(),
                TyKind::UDType(_) | TyKind::Tuple(_)
            ) {
                return self.field_access_or_tuple_indexing_ref(&left, node.lhs.span, &node.rhs);
            } else {
                return Err(CodegenError::HasNoFields {
                    span: node.lhs.span,
                });
            }
        }

        // --- Field access or tuple indexing --- (Not reference)
        self.field_access_or_tuple_indexing(&left, node.lhs.span, &node.rhs)
    }

    /// Expect that left hand side is a reference
    fn field_access_or_tuple_indexing_ref(
        &self,
        left: &Value<'ctx>,
        left_span: Span,
        right: &Expr,
    ) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(
            left.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let mutability = if let TyKind::Reference(rty) = left.get_type().kind.as_ref() {
            rty.mutability
        } else {
            unreachable!()
        };

        let accessed_value = self.field_access_or_tuple_indexing(
            &self.deref_internal(left, left_span)?,
            left_span,
            right,
        )?;

        let inst = accessed_value.get_value().as_instruction_value().unwrap();

        let ptr_to_accessed = get_loaded_pointer(&inst).unwrap();

        Ok(Value::new(
            ptr_to_accessed.into(),
            Ty {
                kind: TyKind::Reference(RefrenceType {
                    refee_ty: accessed_value.get_type(),
                    mutability,
                })
                .into(),
                mutability: Mutability::Not,
            }
            .into(),
        ))
    }

    /// Expect that left hand side is not a reference
    fn field_access_or_tuple_indexing(
        &self,
        left: &Value<'ctx>,
        left_span: Span,
        right: &Expr,
    ) -> CodegenResult<Value<'ctx>> {
        assert!(!matches!(
            left.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let left_ty = &left.get_type();

        let left_inst = match left.get_value().as_instruction_value() {
            Some(inst) => inst,
            None => return Err(CodegenError::HasNoFields { span: left_span }),
        };

        let ptr_to_left = match get_loaded_pointer(&left_inst) {
            Some(p) => p,
            None => return Err(CodegenError::HasNoFields { span: left_span }),
        };

        match left_ty.kind.as_ref() {
            TyKind::UDType(_) => self.struct_field_access(ptr_to_left, right, left_ty),

            TyKind::Tuple(_) => self.tuple_indexing(ptr_to_left, right, left_ty),

            _ => Err(CodegenError::HasNoFields { span: left_span }),
        }
    }

    fn struct_field_access(
        &self,
        left: PointerValue<'ctx>,
        right: &Expr,
        struct_type: &Rc<Ty>,
    ) -> CodegenResult<Value<'ctx>> {
        let struct_name = if let TyKind::UDType(ty) = struct_type.kind.as_ref() {
            &ty.0
        } else {
            unreachable!()
        };

        let field_name = match &right.kind {
            ExprKind::Ident(s) => s.as_str(),
            kind => unreachable!("{:?}", kind),
        };

        let (struct_ty_llvm, struct_info) = &self.cucx.tcx.struct_table[struct_name];

        let field_info = &struct_info.fields[field_name];

        let offset = field_info.offset;

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                *struct_ty_llvm,
                left,
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(offset, false),
                ],
                "",
            )
        };

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(self.cucx.to_llvm_type(&field_info.ty), gep, ""),
            Ty {
                kind: field_info.ty.kind.clone(),
                mutability: struct_type.mutability,
            }
            .into(),
        ))
    }

    fn tuple_indexing(
        &self,
        left: PointerValue<'ctx>,
        right: &Expr,
        tuple_ty: &Rc<Ty>,
    ) -> CodegenResult<Value<'ctx>> {
        let index = match &right.kind {
            ExprKind::Int(i) => i.as_u64(),
            _ => return Err(CodegenError::TupleRequireAccessByIndex { span: right.span }),
        };

        build_tuple_indexing(self.cucx, left, index, tuple_ty, right.span)
    }

    fn call_fn(&mut self, node: &FnCall) -> CodegenResult<Value<'ctx>> {
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
