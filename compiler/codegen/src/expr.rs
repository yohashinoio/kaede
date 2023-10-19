use std::{
    collections::{BTreeSet, VecDeque},
    rc::Rc,
    slice::Iter,
};

use crate::{
    error::{CodegenError, CodegenResult},
    mangle::{mangle_name, mangle_udt_name},
    stmt::{build_block, build_normal_let, build_statement},
    tcx::{EnumInfo, EnumVariantInfo, ReturnType, UDTKind, VariableTable},
    value::{has_signed, Value},
    CompileUnitCtx,
};

use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};
use kaede_ast::{
    expr::{
        Args, ArrayLiteral, Binary, BinaryKind, Break, Else, Expr, ExprKind, FnCall, If, Indexing,
        LogicalNot, Loop, Match, MatchArm, MatchArmList, Return, StructLiteral, TupleLiteral,
    },
    stmt::{Block, Stmt, StmtKind},
};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};
use kaede_type::{
    is_same_type, make_fundamental_type, wrap_in_ref, FundamentalType, FundamentalTypeKind,
    Mutability, RefrenceType, Ty, TyKind, UserDefinedType,
};

struct EnumUnpack<'ctx> {
    /// A::B(x)
    ///      ^
    name: Ident,
    /// match x
    ///       ^
    enum_value: Value<'ctx>,
    variant_ty: Rc<Ty>,
    span: Span,
}

/// `If` where `Expr` is changed to `Value`
struct ValuedIf<'ctx> {
    cond: Value<'ctx>,
    then: Rc<Block>,
    else_: Option<Box<ValuedElse<'ctx>>>,
    span: Span,

    /// Used to initialize variant parameters of match
    enum_unpack: Option<EnumUnpack<'ctx>>,
}

impl<'ctx> ValuedIf<'ctx> {
    fn add_else_at_last(&mut self, node: ValuedElse<'ctx>) {
        if let Some(else_) = self.else_.as_mut() {
            if let ValuedElse::If(if_) = else_.as_mut() {
                return if_.add_else_at_last(node);
            }
        }

        self.else_ = Some(Box::new(node));
    }
}

enum ValuedElse<'ctx> {
    If(ValuedIf<'ctx>),
    Block(Rc<Block>),
}

/// Unit value if the end of the block is not an expression
pub fn build_block_expression<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    block: &Block,
) -> CodegenResult<Value<'ctx>> {
    if block.body.is_empty() {
        return Ok(Value::new_unit());
    }

    cucx.tcx.push_variable_table(VariableTable::new());

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

    cucx.tcx.pop_variable_table();

    Ok(value)
}

pub fn build_expression<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    node: &Expr,
) -> CodegenResult<Value<'ctx>> {
    let mut builder = ExprBuilder::new(cucx);

    builder.build(node)
}

pub fn build_tuple_indexing<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    tuple: PointerValue<'ctx>,
    index: u32,
    tuple_ty: &Rc<Ty>,
    span: Span,
) -> CodegenResult<Value<'ctx>> {
    let llvm_tuple_ty = cucx.conv_to_llvm_type(tuple_ty)?;

    let gep = unsafe {
        cucx.builder.build_in_bounds_gep(
            llvm_tuple_ty,
            tuple,
            &[
                cucx.context().i32_type().const_zero(),
                cucx.context().i32_type().const_int(index as u64, false),
            ],
            "",
        )
    };

    let elem_ty = match tuple_ty.kind.as_ref() {
        TyKind::Tuple(types) => match types.get(index as usize) {
            Some(ty) => ty,

            None => {
                return Err(CodegenError::IndexOutOfRange {
                    index: index as u64,
                    span,
                })
            }
        },
        kind => unreachable!("{:?}", kind),
    };

    let llvm_elem_ty = cucx.conv_to_llvm_type(elem_ty)?;

    Ok(Value::new(
        cucx.builder.build_load(llvm_elem_ty, gep, ""),
        Ty {
            kind: elem_ty.kind.clone(),
            mutability: tuple_ty.mutability,
        }
        .into(),
    ))
}

pub fn create_gc_struct<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    struct_llvm_ty: BasicTypeEnum<'ctx>,
    inits: &[BasicValueEnum<'ctx>],
) -> CodegenResult<PointerValue<'ctx>> {
    let mallocd = cucx.gc_malloc(struct_llvm_ty)?;

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

    Ok(mallocd)
}

struct ExprBuilder<'a, 'ctx> {
    cucx: &'a mut CompileUnitCtx<'ctx>,
}

impl<'a, 'ctx> ExprBuilder<'a, 'ctx> {
    fn new(cucx: &'a mut CompileUnitCtx<'ctx>) -> Self {
        Self { cucx }
    }

    /// Generate expression code
    fn build(&mut self, node: &Expr) -> CodegenResult<Value<'ctx>> {
        Ok(match &node.kind {
            ExprKind::Block(block) => build_block_expression(self.cucx, block)?,

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

            ExprKind::Match(node) => self.match_(node)?,
        })
    }

    /// A::B to ("A", "B", None)
    /// A::B(a, b, c) to ("A", "B", Some(["a", "b", "c"]))
    fn dismantle_enum_variant_pattern<'pat>(
        &self,
        pattern: &'pat Expr,
    ) -> (&'pat Ident, &'pat Ident, Option<&'pat Args>) {
        let (enum_name, variant_name_and_param) = match &pattern.kind {
            ExprKind::Binary(b) => match b.kind {
                BinaryKind::ScopeResolution => (&b.lhs, &b.rhs),
                _ => unreachable!(),
            },

            _ => unreachable!(),
        };

        let enum_name = match &enum_name.kind {
            ExprKind::Ident(s) => s,
            _ => unreachable!(),
        };

        let (variant_name, param) = match &variant_name_and_param.kind {
            ExprKind::Ident(ident) => (ident, None),
            ExprKind::FnCall(fncall) => (&fncall.name, Some(&fncall.args)),
            _ => unreachable!(),
        };

        (enum_name, variant_name, param)
    }

    fn match_(&mut self, node: &Match) -> CodegenResult<Value<'ctx>> {
        let value = self.build(&node.target)?;

        let value_ty = value.get_type();

        match value_ty.kind.as_ref() {
            TyKind::Reference(refty) => self.build_match_on_reference(node, &value, refty),
            TyKind::Fundamental(fty) => self.build_match_on_fundamental_value(node, &value, fty),
            _ => todo!("Unsupported enum target"),
        }
    }

    fn check_exhaustiveness_for_match_on_int(
        &mut self,
        target_type: &FundamentalType,
        arms: &MatchArmList,
        span: Span,
    ) -> CodegenResult<()> {
        if arms.wildcard.is_some() {
            return Ok(());
        }

        assert!(target_type.is_int_or_bool());

        if target_type.kind == FundamentalTypeKind::Bool {
            let mut has_true = false;
            let mut has_false = false;

            for arm in arms.non_wildcard_iter() {
                match arm.pattern.kind {
                    ExprKind::True => has_true = true,
                    ExprKind::False => has_false = true,
                    _ => {
                        return Err(CodegenError::MismatchedTypes {
                            types: (
                                target_type.kind.to_string(),
                                self.build(&arm.pattern)?.get_type().kind.to_string(),
                            ),
                            span: arm.pattern.span,
                        })
                    }
                }
            }

            if has_true && has_false {
                return Ok(());
            }

            if has_true {
                return Err(CodegenError::NonExhaustivePatterns {
                    non_exhaustive_patterns: "`false`".to_owned(),
                    span,
                });
            }

            if has_false {
                return Err(CodegenError::NonExhaustivePatterns {
                    non_exhaustive_patterns: "`true`".to_owned(),
                    span,
                });
            }

            return Err(CodegenError::NonExhaustivePatterns {
                non_exhaustive_patterns: "`true` and `false`".to_owned(),
                span,
            });
        }

        // Non-bool integer
        Err(CodegenError::NonExhaustivePatterns {
            non_exhaustive_patterns: "`_`".to_owned(),
            span,
        })
    }

    fn add_wildcard(&self, if_: &mut ValuedIf<'ctx>, wildcard: &MatchArm) {
        assert!(wildcard.is_wildcard());

        let wildcard = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(wildcard.code.clone()),
                span: wildcard.code.span,
            }],
            span: wildcard.code.span,
        };

        if_.add_else_at_last(ValuedElse::Block(Rc::new(wildcard)));
    }

    fn build_match_on_fundamental_value(
        &mut self,
        node: &Match,
        target: &Value<'ctx>,
        fty: &FundamentalType,
    ) -> CodegenResult<Value<'ctx>> {
        if fty.is_int_or_bool() {
            self.check_exhaustiveness_for_match_on_int(fty, &node.arms, node.span)?;

            let mut valued_if = self
                .conv_match_arms_on_int_to_if(target, node.arms.non_wildcard_iter(), node.span)?
                .unwrap();

            if let Some(wc) = &node.arms.wildcard {
                self.add_wildcard(&mut valued_if, wc);
            }

            return self.build_if(&valued_if, true);
        }

        todo!()
    }

    /// The return value is `Option` because of the recursion termination condition, so there is no problem if the caller `unwrap`s it
    fn conv_match_arms_on_int_to_if(
        &mut self,
        target: &Value<'ctx>,
        mut arms: Iter<MatchArm>,
        span: Span,
    ) -> CodegenResult<Option<ValuedIf<'ctx>>> {
        let current_arm = match arms.next() {
            Some(c) => c,
            None => return Ok(None),
        };

        let cond = {
            let arm_value = self.build(&current_arm.pattern)?;

            if !arm_value.get_type().kind.is_int_or_bool() {
                return Err(CodegenError::MismatchedTypes {
                    types: (
                        target.get_type().kind.to_string(),
                        arm_value.get_type().kind.to_string(),
                    ),
                    span: current_arm.pattern.span,
                });
            }

            self.build_int_equal(
                target.get_value().into_int_value(),
                arm_value.get_value().into_int_value(),
            )
        };

        let then = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(current_arm.code.clone()),
                span: current_arm.code.span,
            }],
            span: current_arm.code.span,
        };

        let else_ = self
            .conv_match_arms_on_int_to_if(target, arms, span)?
            .map(|if_| Box::new(ValuedElse::If(if_)));

        Ok(Some(ValuedIf {
            cond,
            enum_unpack: None,
            then: then.into(),
            else_,
            span,
        }))
    }

    fn check_exhaustiveness_for_match_on_enum(
        &self,
        enum_info: &EnumInfo,
        arms: &MatchArmList,
        span: Span,
    ) -> CodegenResult<()> {
        if arms.wildcard.is_some() {
            return Ok(());
        }

        let variants = &enum_info.variants;

        let mut pattern_variant_names = BTreeSet::new();

        for arm in arms.non_wildcard_iter() {
            let (enum_name, variant_name, _) = self.dismantle_enum_variant_pattern(&arm.pattern);

            if enum_info.name.symbol() != enum_name.symbol() {
                todo!("Error");
            }

            if !variants.contains_key(&variant_name.symbol()) {
                return Err(CodegenError::NoVariant {
                    variant_name: variant_name.symbol(),
                    parent_name: enum_name.symbol(),
                    span: variant_name.span(),
                });
            }

            if !pattern_variant_names.insert(variant_name.as_str()) {
                // There were multiple identical patterns
                return Err(CodegenError::UnreachablePattern {
                    span: enum_name.span(),
                });
            }
        }

        let variant_names = variants.keys().map(|k| k.as_str()).collect::<BTreeSet<_>>();

        let dif = variant_names
            .difference(&pattern_variant_names)
            .collect::<Vec<_>>();

        if !dif.is_empty() {
            return Err(CodegenError::NonExhaustivePatterns {
                non_exhaustive_patterns: dif
                    .into_iter()
                    .map(|p| format!("`{}::{}`", enum_info.name.as_str(), p))
                    .collect::<Vec<_>>()
                    .join(" and "),
                span,
            });
        }

        Ok(())
    }

    fn build_match_on_reference(
        &mut self,
        node: &Match,
        target: &Value<'ctx>,
        refty: &RefrenceType,
    ) -> CodegenResult<Value<'ctx>> {
        let refee_ty = &refty.refee_ty;

        let udt = match refee_ty.kind.as_ref() {
            TyKind::UserDefined(udt) => udt,
            _ => unimplemented!(),
        };

        let udt_kind = match self.cucx.tcx.get_udt(udt.name.symbol()) {
            Some(udt) => udt,
            None => {
                return Err(CodegenError::Undeclared {
                    name: udt.name.symbol(),
                    span: udt.name.span(),
                })
            }
        };

        let enum_info = match udt_kind.as_ref() {
            UDTKind::Enum(enum_info) => enum_info,
            _ => {
                return Err(CodegenError::Undeclared {
                    name: udt.name.symbol(),
                    span: udt.name.span(),
                })
            }
        };

        self.check_exhaustiveness_for_match_on_enum(enum_info, &node.arms, node.span)?;

        self.build_match_on_enum(enum_info, target, &node.arms, node.span)
    }

    fn build_match_on_enum(
        &mut self,
        enum_info: &EnumInfo<'ctx>,
        target: &Value<'ctx>,
        arms: &MatchArmList,
        span: Span,
    ) -> CodegenResult<Value<'ctx>> {
        let target_offset = self.load_enum_variant_offset_from_value(target);

        let mut valued_if = self
            .conv_match_arms_on_enum_to_if(
                enum_info,
                target,
                &target_offset,
                arms.non_wildcard_iter(),
                span,
            )?
            .unwrap();

        if let Some(wc) = &arms.wildcard {
            self.add_wildcard(&mut valued_if, wc);
        }

        self.build_if(&valued_if, true)
    }

    /// The return value is `Option` because of the recursion termination condition, so there is no problem if the caller `unwrap`s it
    fn conv_match_arms_on_enum_to_if(
        &mut self,
        enum_info: &EnumInfo<'ctx>,
        target: &Value<'ctx>,
        target_offset: &Value<'ctx>,
        mut arms: Iter<MatchArm>,
        span: Span,
    ) -> CodegenResult<Option<ValuedIf<'ctx>>> {
        let current_arm = match arms.next() {
            Some(c) => c,
            None => return Ok(None),
        };

        let (cond, param_name, pattern_variant_info) = {
            let (pattern_variant_info, params) =
                self.derive_variant_from_enum_variant_pattern(enum_info, &current_arm.pattern)?;

            let param_name = params.map(|x| match &x.0.front().unwrap().kind {
                ExprKind::Ident(ident) => ident,
                _ => todo!("Error"),
            });

            (
                self.build_int_equal(
                    target_offset.get_value().into_int_value(),
                    self.cucx
                        .context()
                        .i32_type()
                        .const_int(pattern_variant_info.offset as u64, false),
                ),
                param_name,
                pattern_variant_info,
            )
        };

        let then = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(current_arm.code.clone()),
                span: current_arm.code.span,
            }],
            span: current_arm.code.span,
        };

        let else_ = self
            .conv_match_arms_on_enum_to_if(enum_info, target, target_offset, arms, span)?
            .map(|if_| Box::new(ValuedElse::If(if_)));

        let enum_unpack = if let Some(param_name) = param_name {
            match &pattern_variant_info.ty {
                Some(ty) => Some(EnumUnpack {
                    name: *param_name,
                    enum_value: target.clone(),
                    variant_ty: ty.clone(),
                    span,
                }),
                None => {
                    return Err(CodegenError::UnitVariantCannotUnpack {
                        unit_variant_name: format!(
                            "{}::{}",
                            enum_info.name.as_str(),
                            pattern_variant_info.name.as_str()
                        )
                        .into(),
                        span: current_arm.pattern.span,
                    })
                }
            }
        } else {
            None
        };

        Ok(Some(ValuedIf {
            cond,
            enum_unpack,
            then: then.into(),
            else_,
            span,
        }))
    }

    fn load_enum_variant_offset_from_value(&self, value: &Value<'ctx>) -> Value<'ctx> {
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                self.cucx.context().i32_type(),
                value.get_value().into_pointer_value(),
                &[self.cucx.context().i32_type().const_zero()],
                "",
            )
        };

        Value::new(
            self.cucx
                .builder
                .build_load(self.cucx.context().i32_type(), gep, ""),
            make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not).into(),
        )
    }

    fn derive_variant_from_enum_variant_pattern<'pat, 'e>(
        &self,
        enum_info: &'e EnumInfo,
        pattern: &'pat Expr,
    ) -> CodegenResult<(&'e EnumVariantInfo, Option<&'pat Args>)> {
        let (enum_name, variant_name, param) = self.dismantle_enum_variant_pattern(pattern);

        if enum_info.name.symbol() != enum_name.symbol() {
            todo!("Error");
        }

        Ok((
            self.get_enum_variant_info_from_name(
                enum_info,
                (variant_name.symbol(), variant_name.span()),
            )?,
            param,
        ))
    }

    fn get_enum_variant_info_from_name<'e>(
        &self,
        enum_info: &'e EnumInfo,
        variant_name: (Symbol, Span),
    ) -> CodegenResult<&'e EnumVariantInfo> {
        let variant = match enum_info.variants.get(&variant_name.0) {
            Some(item) => item,
            None => {
                return Err(CodegenError::NoVariant {
                    variant_name: variant_name.0,
                    parent_name: enum_info.name.symbol(),
                    span: variant_name.1,
                })
            }
        };

        Ok(variant)
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

    fn conv_to_valued_if(&mut self, orig: &If) -> CodegenResult<ValuedIf<'ctx>> {
        let cond = build_expression(self.cucx, &orig.cond)?;

        let else_ = match &orig.else_ {
            Some(els) => Some(match els.as_ref() {
                Else::If(if_) => ValuedElse::If(self.conv_to_valued_if(if_)?).into(),
                Else::Block(block) => ValuedElse::Block(block.clone()).into(),
            }),
            None => None,
        };

        Ok(ValuedIf {
            cond,
            enum_unpack: None,
            then: orig.then.clone(),
            else_,
            span: orig.span,
        })
    }

    fn if_(&mut self, node: &If) -> CodegenResult<Value<'ctx>> {
        let valued_if = self.conv_to_valued_if(node)?;
        self.build_if(&valued_if, false)
    }

    /// If unreachable_else is true and there is no else, build unreachable
    fn build_if(&mut self, node: &ValuedIf, unreachable_else: bool) -> CodegenResult<Value<'ctx>> {
        let if_value = self.unsafe_build_if(node, unreachable_else)?;

        // All control paths will be 'never'
        if if_value.is_never_ty() {
            return Err(CodegenError::NeverIfExpr { span: node.span });
        }

        Ok(if_value)
    }

    /// Do not use this function! (Use `build_if` instead)
    ///
    /// Because this function is recursive, it is difficult to handle some errors, so it is wrapped in `build_if`
    ///
    /// Therefore, 'unsafe' in the function name is not memory unsafe!
    fn unsafe_build_if(
        &mut self,
        node: &ValuedIf,
        unreachable_else: bool,
    ) -> CodegenResult<Value<'ctx>> {
        let span = node.span;

        let parent = self.cucx.get_current_fn();
        let zero_const = self.cucx.context().bool_type().const_zero();

        let cond = self.cucx.builder.build_int_compare(
            IntPredicate::NE,
            node.cond.get_value().into_int_value(),
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

        // Enum unpack (optional)
        if let Some(enum_unpack) = &node.enum_unpack {
            self.build_enum_unpack(enum_unpack)?;
        }

        // Build then code
        let then_val = build_block_expression(self.cucx, &node.then)?;

        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        let then_bb = self.cucx.builder.get_insert_block().unwrap();

        // Build else block
        self.cucx.builder.position_at_end(else_bb);

        // Build else code
        let else_val = match &node.else_ {
            Some(else_) => match else_.as_ref() {
                ValuedElse::If(if_) => self.unsafe_build_if(if_, unreachable_else)?,
                ValuedElse::Block(block) => build_block_expression(self.cucx, block)?,
            },

            _ => {
                // Check if it is used as an expression
                if self.cucx.is_ifmatch_stmt {
                    Value::new_unit()
                } else if unreachable_else {
                    // Build unreachable
                    self.cucx.builder.build_unreachable();
                    Value::new_never()
                } else {
                    return Err(CodegenError::IfMustHaveElseUsedAsExpr { span });
                }
            }
        };

        let else_bb = self.cucx.builder.get_insert_block().unwrap();

        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        self.cucx.builder.position_at_end(cont_bb);

        if self.cucx.is_ifmatch_stmt {
            // Not used as a expression
            return Ok(Value::new_unit());
        }

        if !is_same_type(&then_val.get_type(), &else_val.get_type()) {
            return Err(CodegenError::IfAndElseHaveIncompatibleTypes {
                types: (
                    then_val.get_type().kind.to_string(),
                    else_val.get_type().kind.to_string(),
                ),
                span,
            });
        }

        // Either then_val or else_val could be never
        let phi_ty = if then_val.is_never_ty() {
            return Ok(else_val);
        } else if else_val.is_never_ty() {
            return Ok(then_val);
        } else {
            then_val.get_type()
        };

        let llvm_phi_ty = self.cucx.conv_to_llvm_type(&phi_ty)?;
        let phi = self.cucx.builder.build_phi(llvm_phi_ty, "iftmp");

        phi.add_incoming(&[
            (&then_val.get_value(), then_bb),
            (&else_val.get_value(), else_bb),
        ]);

        Ok(Value::new(phi.as_basic_value(), phi_ty))
    }

    fn build_enum_unpack(&mut self, enum_unpack: &EnumUnpack) -> CodegenResult<()> {
        let variant_ty_llvm = match enum_unpack.variant_ty.kind.as_ref() {
            TyKind::Reference(refty) => self.cucx.conv_to_llvm_type(&refty.refee_ty),
            _ => self.cucx.conv_to_llvm_type(&enum_unpack.variant_ty),
        }?;

        let bitcast = self.cucx.builder.build_bitcast(
            enum_unpack.enum_value.get_value(),
            self.cucx
                .context()
                .struct_type(
                    &[self.cucx.context().i32_type().into(), variant_ty_llvm],
                    true,
                )
                .ptr_type(AddressSpace::default()),
            "",
        );

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                variant_ty_llvm,
                bitcast.into_pointer_value(),
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(1_u64, false),
                ],
                "",
            )
        };

        let variant_value = Value::new(
            self.cucx.builder.build_load(variant_ty_llvm, gep, ""),
            enum_unpack.variant_ty.clone(),
        );

        build_normal_let(
            self.cucx,
            &enum_unpack.name,
            Mutability::Not,
            Some(variant_value),
            Rc::new(Ty::new_inferred(Mutability::Not)),
            enum_unpack.span,
        )?;

        Ok(())
    }

    fn struct_literal(&mut self, node: &StructLiteral) -> CodegenResult<Value<'ctx>> {
        let mangled_name = mangle_udt_name(self.cucx, &node.struct_ty);

        let struct_ty = Ty {
            kind: TyKind::UserDefined(node.struct_ty.clone()).into(),
            mutability: Mutability::Not,
        };

        let struct_llvm_ty = self.cucx.conv_to_llvm_type(&struct_ty)?;

        let udt_kind = match self.cucx.tcx.get_udt(mangled_name) {
            Some(udt) => udt.clone(),

            None => {
                return Err(CodegenError::Undeclared {
                    name: mangled_name,
                    span: node.struct_ty.name.span(),
                });
            }
        };

        let struct_info = match udt_kind.as_ref() {
            UDTKind::Struct(info) => info,

            _ => {
                return Err(CodegenError::Undeclared {
                    name: mangled_name,
                    span: node.struct_ty.name.span(),
                });
            }
        };

        let mut values = Vec::new();

        for value in node.values.iter() {
            let field_info = &struct_info.fields[&value.0.symbol()];

            let value = build_expression(self.cucx, &value.1)?;

            // To sort by offset, store offset
            values.push((field_info.offset, value.get_value()));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));

        // Remove offsets
        let inits: Vec<_> = values.iter().map(|e| e.1).collect();

        Ok(Value::new(
            create_gc_struct(self.cucx, struct_llvm_ty, &inits)?.into(),
            wrap_in_ref(struct_ty.into(), Mutability::Mut).into(),
        ))
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

        let tuple_llvm_ty = self.cucx.conv_to_llvm_type(&tuple_ty)?;

        Ok(Value::new(
            create_gc_struct(
                self.cucx,
                tuple_llvm_ty,
                element_values
                    .iter()
                    .map(|v| v.get_value())
                    .collect::<Vec<_>>()
                    .as_slice(),
            )?
            .into(),
            wrap_in_ref(tuple_ty.into(), Mutability::Mut).into(),
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

        let array_llvm_ty = self.cucx.conv_to_llvm_type(&array_ty)?;

        let mallocd = self.cucx.gc_malloc(array_llvm_ty)?;

        for (idx, elem) in elems.iter().enumerate() {
            let gep = unsafe {
                self.cucx.builder.build_in_bounds_gep(
                    array_llvm_ty,
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

        let array_llvm_ty = self.cucx.conv_to_llvm_type(&array_ty)?.into_array_type();

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

        let zero = self
            .cucx
            .conv_to_llvm_type(&operand.get_type())?
            .const_zero();

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

    fn ident_expr(&mut self, ident: &Ident) -> CodegenResult<Value<'ctx>> {
        let (ptr, ty) = self.cucx.tcx.lookup_variable(ident)?.clone();

        let llvm_ty = self.cucx.conv_to_llvm_type(&ty)?;

        Ok(Value::new(
            self.cucx.builder.build_load(llvm_ty, ptr, ""),
            ty.clone(),
        ))
    }

    fn binary_op(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        use BinaryKind::*;

        match node.kind {
            Access => self.access(node),

            ScopeResolution => self.scope_resolution(node),

            _ => self.binary_arithmetic_op(node),
        }
    }

    fn scope_resolution(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::ScopeResolution));

        let left = match &node.lhs.kind {
            ExprKind::Ident(ident) => ident,
            _ => todo!(),
        };

        // Create enum variant with value
        if let ExprKind::FnCall(right) = &node.rhs.kind {
            if right.args.0.len() != 1 {
                todo!("Error");
            }

            let value = right.args.0.front().unwrap();

            return self.create_enum_variant(left, &right.name, Some(value));
        }

        // Create enum variant
        if let ExprKind::Ident(right) = &node.rhs.kind {
            return self.create_enum_variant(left, right, None);
        }

        todo!()
    }

    fn create_enum_variant(
        &mut self,
        enum_name: &Ident,
        variant_name: &Ident,
        value: Option<&Expr>,
    ) -> CodegenResult<Value<'ctx>> {
        let udt_kind = match self.cucx.tcx.get_udt(enum_name.symbol()) {
            Some(udt) => udt,
            None => {
                return Err(CodegenError::Undeclared {
                    name: enum_name.symbol(),
                    span: enum_name.span(),
                })
            }
        };

        let enum_info = match udt_kind.as_ref() {
            UDTKind::Enum(enum_info) => enum_info,
            _ => {
                return Err(CodegenError::Undeclared {
                    name: enum_name.symbol(),
                    span: enum_name.span(),
                })
            }
        };

        let variant_offset = self
            .get_enum_variant_info_from_name(
                enum_info,
                (variant_name.symbol(), variant_name.span()),
            )?
            .offset;

        let enum_ty = Ty {
            kind: TyKind::UserDefined(UserDefinedType::new(*enum_name, None)).into(),
            mutability: Mutability::Not,
        };

        let enum_llvm_ty = self.cucx.conv_to_llvm_type(&enum_ty)?;

        let offset_in_llvm = self
            .cucx
            .context()
            .i32_type()
            .const_int(variant_offset as u64, true)
            .into();

        let inits = match value {
            Some(v) => vec![offset_in_llvm, self.build(v)?.get_value()],
            None => vec![offset_in_llvm],
        };

        Ok(Value::new(
            create_gc_struct(self.cucx, enum_llvm_ty, &inits)?.into(),
            wrap_in_ref(enum_ty.into(), Mutability::Mut).into(),
        ))
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

    fn build_int_equal(&self, left: IntValue<'ctx>, right: IntValue<'ctx>) -> Value<'ctx> {
        Value::new(
            self.cucx
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "")
                .into(),
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

        let left_value = left.get_value();
        let right_value = right.get_value();

        // If operands are not int
        if !(left_value.is_int_value() && right_value.is_int_value()) {
            todo!("Error");
        }

        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

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

            Eq => self.build_int_equal(left_int, right_int),

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
            ScopeResolution => unreachable!(),
        })
    }

    /// Struct access or module item access
    fn access(&mut self, node: &Binary) -> CodegenResult<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::Access));

        if let ExprKind::Ident(modname) = &node.lhs.kind {
            if self.cucx.imported_modules.contains(&modname.symbol()) {
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
            // All tuples and structs in this language are used via reference

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
        let (udt, mangled_struct_name) = if let TyKind::UserDefined(udt) = struct_ty.kind.as_ref() {
            (udt, mangle_udt_name(self.cucx, udt))
        } else {
            unreachable!()
        };

        match &right.kind {
            // Field
            ExprKind::Ident(field_name) => self.struct_field_access(
                left,
                udt.name.symbol(),
                mangled_struct_name,
                struct_ty,
                field_name,
            ),

            // Method
            ExprKind::FnCall(node) => self.struct_method_access(left, mangled_struct_name, node),

            kind => unreachable!("{:?}", kind),
        }
    }

    fn struct_field_access(
        &mut self,
        struct_value: &Value<'ctx>,
        struct_name: Symbol,
        mangled_struct_name: Symbol,
        struct_ty: &Rc<Ty>,
        field_name: &Ident,
    ) -> CodegenResult<Value<'ctx>> {
        let llvm_struct_ty = self.cucx.conv_to_llvm_type(struct_ty)?;

        let udt_kind = self.cucx.tcx.get_udt(mangled_struct_name).unwrap();

        let struct_info = match udt_kind.as_ref() {
            UDTKind::Struct(t) => t,
            _ => unreachable!(),
        };

        let field_info = match struct_info.fields.get(&field_name.symbol()) {
            Some(field) => field,
            None => {
                return Err(CodegenError::NoMember {
                    member_name: field_name.symbol(),
                    parent_name: struct_name,
                    span: field_name.span(),
                });
            }
        };

        let offset = field_info.offset;

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                llvm_struct_ty,
                struct_value.get_value().into_pointer_value(),
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(offset, false),
                ],
                "",
            )
        };

        let llvm_field_ty = self.cucx.conv_to_llvm_type(&field_info.ty)?;

        Ok(Value::new(
            self.cucx.builder.build_load(llvm_field_ty, gep, ""),
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
        struct_name: Symbol,
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

        self.build_call_fn(actual_method_name.into(), args, call_node.span)
    }

    fn tuple_indexing(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        tuple_ty: &Rc<Ty>,
    ) -> CodegenResult<Value<'ctx>> {
        let index = match &right.kind {
            ExprKind::Int(i) => i.as_u64(),
            _ => return Err(CodegenError::TupleRequireAccessByIndex { span: right.span }),
        };

        let left_value = left.get_value().into_pointer_value();

        build_tuple_indexing(self.cucx, left_value, index as u32, tuple_ty, right.span)
    }

    fn call_fn(&mut self, node: &FnCall) -> CodegenResult<Value<'ctx>> {
        let args = {
            let mut args = VecDeque::new();

            for arg in node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        self.build_call_fn(node.name.symbol(), args, node.span)
    }

    fn build_call_fn(
        &mut self,
        name: Symbol,
        args: VecDeque<(Value<'ctx>, Span)>,
        span: Span,
    ) -> CodegenResult<Value<'ctx>> {
        let func = self.cucx.module.get_function(&mangle_name(self.cucx, name));

        let func = match func {
            Some(func) => func,

            None => return Err(CodegenError::Undeclared { name, span }),
        };

        let function_info = self.cucx.tcx.get_function_info(func).unwrap();

        self.verify_args(&args, &function_info.param_types)?;

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
            Some(val) => Value::new(
                val,
                match &function_info.return_type {
                    ReturnType::Type(ty) => ty.clone(),
                    ReturnType::Void => unreachable!(),
                },
            ),

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
