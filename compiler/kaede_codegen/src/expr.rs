use std::{collections::VecDeque, rc::Rc};

use crate::{
    error::{CodegenError, CodegenResult},
    mangle::mangle_name,
    stmt::{build_block, build_statement},
    tcx::{ReturnType, SymbolTable},
    value::{has_signed, Value},
    CompileUnitContext,
};

use inkwell::{
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
    IntPredicate,
};
use kaede_ast::{
    expr::{
        ArrayLiteral, Binary, BinaryKind, Break, Else, Expr, ExprKind, FnCall, Ident, If, Indexing,
        LogicalNot, Loop, Return, StructLiteral, TupleLiteral,
    },
    stmt::{Block, StmtKind},
};
use kaede_span::Span;
use kaede_type::{
    is_same_type, make_fundamental_type, FundamentalTypeKind, Mutability, RefrenceType, Ty, TyKind,
    UserDefinedType,
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
        _ => {
            build_statement(cucx, last_stmt)?;
            Value::new_unit()
        }
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

pub fn create_struct_alloca<'ctx>(
    cucx: &mut CompileUnitContext<'ctx, '_, '_>,
    struct_ty: &Ty,
    inits: &[BasicValueEnum<'ctx>],
) -> PointerValue<'ctx> {
    let mallocd = cucx.gc_malloc(struct_ty);

    let struct_llvm_ty = cucx.to_llvm_type(struct_ty);

    for (index, init) in inits.iter().enumerate() {
        let gep = unsafe {
            cucx.builder.build_in_bounds_gep(
                struct_llvm_ty,
                mallocd,
                &[
                    cucx.context().i32_type().const_zero(),
                    cucx.context().i32_type().const_int(index as u64, false),
                ],
                "",
            )
        };

        cucx.builder.build_store(gep, *init);
    }

    mallocd
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

            ExprKind::Indexing(node) => self.array_indexing(node)?,

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

        let body_bb = self.cucx.context().append_basic_block(parent, "loopbody");

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

    fn struct_literal(&mut self, node: &StructLiteral) -> CodegenResult<Value<'ctx>> {
        let struct_info = match self.cucx.tcx.get_struct_info(node.struct_name.as_str()) {
            Some(x) => x.clone(),

            None => {
                return Err(CodegenError::Undeclared {
                    span: node.struct_name.span,
                    name: node.struct_name.name.clone(),
                })
            }
        };

        let struct_ty = Ty {
            kind: TyKind::UserDefined(UserDefinedType {
                name: node.struct_name.name.clone(),
            })
            .into(),
            mutability: Mutability::Not,
        };

        let mut values = Vec::new();

        for value in node.values.iter() {
            let field_info = &struct_info.fields[value.0.as_str()];

            let value = build_expression(self.cucx, &value.1)?;

            // To sort by offset, store offset
            values.push((field_info.offset, value.get_value()));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));

        // Remove offsets
        let inits: Vec<_> = values.iter().map(|e| e.1).collect();

        let alloca = create_struct_alloca(self.cucx, &struct_ty, &inits).into();

        let struct_ref_ty = Ty {
            kind: TyKind::Reference(RefrenceType {
                refee_ty: struct_ty.into(),
            })
            .into(),
            mutability: Mutability::Mut,
        };

        Ok(Value::new(alloca, struct_ref_ty.into()))
    }

    fn tuple_literal(&mut self, node: &TupleLiteral) -> CodegenResult<Value<'ctx>> {
        let element_values = {
            let mut v = Vec::new();
            for e in node.elements.iter() {
                v.push(build_expression(self.cucx, e)?);
            }
            v
        };

        let tuple_ty = Ty {
            kind: TyKind::Tuple(element_values.iter().map(|v| v.get_type()).collect()).into(),
            mutability: Mutability::Not,
        };

        let alloca = create_struct_alloca(
            self.cucx,
            &tuple_ty,
            element_values
                .iter()
                .map(|v| v.get_value())
                .collect::<Vec<_>>()
                .as_slice(),
        );

        Ok(Value::new(
            alloca.into(),
            Ty {
                kind: TyKind::Reference(RefrenceType {
                    refee_ty: tuple_ty.into(),
                })
                .into(),
                mutability: Mutability::Mut,
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

        let array_ty_llvm = self.cucx.to_llvm_type(&array_ty);

        let mallocd = self.cucx.gc_malloc(&array_ty);

        for (idx, elem) in elems.iter().enumerate() {
            let gep = unsafe {
                self.cucx.builder.build_in_bounds_gep(
                    array_ty_llvm,
                    mallocd,
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
            mallocd.into(),
            Ty {
                kind: TyKind::Reference(RefrenceType {
                    refee_ty: array_ty.clone(),
                })
                .into(),
                mutability: Mutability::Mut,
            }
            .into(),
        ))
    }

    fn array_indexing(&mut self, node: &Indexing) -> CodegenResult<Value<'ctx>> {
        // A raw array cannot be passed, but a pointer(reference) to an array
        let array_ref = build_expression(self.cucx, &node.operand)?;

        let array_ref_ty = array_ref.get_type();

        let (array_ty, elem_ty) = match array_ref_ty.kind.as_ref() {
            TyKind::Reference(rty) => match rty.refee_ty.kind.as_ref() {
                TyKind::Array((elem_ty, _)) => (rty.refee_ty.clone(), elem_ty),

                _ => todo!("ERROR"),
            },

            _ => unreachable!(),
        };

        let array_llvm_ty = self.cucx.to_llvm_type(&array_ty).into_array_type();

        let ptr_to_array = array_ref.get_value().into_pointer_value();

        let index = build_expression(self.cucx, &node.index)?;

        // Calculate the address of the index-th element
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                array_llvm_ty,
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
                .build_load(array_llvm_ty.get_element_type(), gep, ""),
            Ty {
                kind: elem_ty.kind.clone(),
                mutability: array_ref_ty.mutability,
            }
            .into(),
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
            Ty {
                kind: TyKind::Str.into(),
                mutability: Mutability::Not,
            }
            .into(),
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

    /// Struct access or module item access
    fn access(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::Access));

        if let ExprKind::Ident(modname) = &node.lhs.kind {
            if self.cucx.imported_modules.contains(modname.as_str()) {
                // --- Module item access ---
                self.cucx.module.set_name(modname.as_str());

                let value = build_expression(self.cucx, &node.rhs);

                // Revert to the current module name
                self.cucx.module.set_name(&self.cucx.module_name);
                return value;
            }
        }

        let left = build_expression(self.cucx, &node.lhs)?;

        let left_ty = &left.get_type();

        if let TyKind::Reference(rty) = left_ty.kind.as_ref() {
            // ---  Struct access or tuple indexing ---
            if matches!(
                rty.get_base_type_of_reference().kind.as_ref(),
                TyKind::UserDefined(_) | TyKind::Tuple(_)
            ) {
                return self.struct_access_or_tuple_indexing(&left, node.lhs.span, &node.rhs);
            } else {
                Err(CodegenError::HasNoFields {
                    span: node.lhs.span,
                })
            }
        } else {
            // No possibility of raw tuple or struct

            Err(CodegenError::HasNoFields {
                span: node.lhs.span,
            })
        }
    }

    fn struct_access_or_tuple_indexing(
        &mut self,
        left: &Value<'ctx>,
        left_span: Span,
        right: &Expr,
    ) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(
            left.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let left_ty = &left.get_type();

        let refee_ty = match left_ty.kind.as_ref() {
            TyKind::Reference(rty) => &rty.refee_ty,
            _ => unreachable!(),
        };

        match refee_ty.kind.as_ref() {
            TyKind::UserDefined(_) => self.struct_access(left, right, refee_ty),

            TyKind::Tuple(_) => self.tuple_indexing(left, right, refee_ty),

            _ => Err(CodegenError::HasNoFields { span: left_span }),
        }
    }

    /// Field access or method access
    fn struct_access(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        struct_ty: &Rc<Ty>,
    ) -> CodegenResult<Value<'ctx>> {
        let struct_name = if let TyKind::UserDefined(ty) = struct_ty.kind.as_ref() {
            &ty.name
        } else {
            unreachable!()
        };

        match &right.kind {
            // Field
            ExprKind::Ident(field_name) => {
                self.struct_field_access(left, struct_name, struct_ty, field_name.as_str())
            }

            // Method
            ExprKind::FnCall(node) => self.struct_method_access(left, struct_name, node),

            kind => unreachable!("{:?}", kind),
        }
    }

    fn struct_field_access(
        &self,
        struct_value: &Value<'ctx>,
        struct_name: &str,
        struct_ty: &Rc<Ty>,
        field_name: &str,
    ) -> CodegenResult<Value<'ctx>> {
        let struct_info = self.cucx.tcx.get_struct_info(struct_name).unwrap();

        let field_info = &struct_info.fields[field_name];

        let offset = field_info.offset;

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                self.cucx.to_llvm_type(struct_ty),
                struct_value.get_value().into_pointer_value(),
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
                mutability: struct_ty.mutability,
            }
            .into(),
        ))
    }

    fn struct_method_access(
        &mut self,
        struct_value: &Value<'ctx>,
        struct_name: &str,
        call_node: &FnCall,
    ) -> CodegenResult<Value<'ctx>> {
        // For 'get_age' method of 'Person' structure
        // Person.get_age
        let actual_method_name = format!("{}.{}", struct_name, call_node.name.as_str());

        // Convert arguments(exprs) to values
        let mut args = {
            let mut args = VecDeque::new();

            for arg in call_node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        // Push self to front
        args.push_front((struct_value.clone(), call_node.args.1));

        self.call_fn_internal(&actual_method_name, args, call_node.span)
    }

    fn tuple_indexing(
        &self,
        left: &Value<'ctx>,
        right: &Expr,
        tuple_ty: &Rc<Ty>,
    ) -> CodegenResult<Value<'ctx>> {
        let index = match &right.kind {
            ExprKind::Int(i) => i.as_u64(),
            _ => return Err(CodegenError::TupleRequireAccessByIndex { span: right.span }),
        };

        let left_value = left.get_value().into_pointer_value();

        build_tuple_indexing(self.cucx, left_value, index, tuple_ty, right.span)
    }

    fn call_fn(&mut self, node: &FnCall) -> CodegenResult<Value<'ctx>> {
        let args = {
            let mut args = VecDeque::new();

            for arg in node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        self.call_fn_internal(node.name.as_str(), args, node.span)
    }

    fn call_fn_internal(
        &mut self,
        name: &str,
        args: VecDeque<(Value<'ctx>, Span)>,
        span: Span,
    ) -> CodegenResult<Value<'ctx>> {
        let func = self.cucx.module.get_function(&mangle_name(self.cucx, name));

        let func = match func {
            Some(func) => func,

            None => {
                return Err(CodegenError::Undeclared {
                    name: name.to_string(),
                    span,
                })
            }
        };

        let param_types = self.cucx.tcx.get_fn_params(func).unwrap();

        self.verify_args(&args, &param_types)?;

        let return_value = self
            .cucx
            .builder
            .build_call(
                func,
                args.iter()
                    .map(|a| a.0.get_value().into())
                    .collect::<Vec<_>>()
                    .as_slice(),
                "",
            )
            .try_as_basic_value()
            .left();

        Ok(match return_value {
            // With return value
            Some(val) => {
                let return_ty = self.cucx.tcx.get_return_ty(func).unwrap();
                Value::new(
                    val,
                    match return_ty {
                        ReturnType::Type(ty) => ty,
                        ReturnType::Void => unreachable!(),
                    },
                )
            }

            // Without return value (void function)
            None => Value::new_void(),
        })
    }

    fn verify_args(
        &self,
        args: &VecDeque<(Value<'ctx>, Span)>,
        params: &[Rc<Ty>],
    ) -> CodegenResult<()> {
        for (idx, arg) in args.iter().enumerate() {
            let param = &params[idx];

            if !is_same_type(&arg.0.get_type(), param) {
                return Err(CodegenError::MismatchedTypes {
                    types: (
                        arg.0.get_type().kind.to_string(),
                        params[idx].kind.to_string(),
                    ),
                    span: arg.1,
                });
            }

            // Check mutability
            if arg.0.get_type().mutability.is_not() && param.mutability.is_mut() {
                return Err(CodegenError::CannotAssignImmutableToMutable { span: arg.1 });
            }
        }

        Ok(())
    }
}
