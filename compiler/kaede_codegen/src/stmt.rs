use std::rc::Rc;

use inkwell::{basic_block::BasicBlock, values::BasicValue, IntPredicate};
use kaede_ast::expr::Ident;
use kaede_ast::stmt::{
    Assign, AssignKind, Block, Break, Else, If, Let, LetKind, Loop, NormalLet, Return, Stmt,
    StmtKind, TupleUnpack,
};
use kaede_span::Span;
use kaede_type::{Mutability, RefrenceType, Ty, TyKind};

use crate::expr::build_tuple_indexing;
use crate::value::Value;
use crate::{
    error::{CodegenError, CodegenResult},
    expr::build_expression,
    get_loaded_pointer,
    tcx::SymbolTable,
    CompileUnitContext,
};

pub fn build_block<'a, 'ctx>(
    cucx: &'a mut CompileUnitContext<'ctx, '_, '_>,
    scx: &'a mut StmtContext<'ctx>,
    block: Block,
) -> CodegenResult<()> {
    cucx.tcx.push_symbol_table(SymbolTable::new());

    for stmt in block.body {
        build_statement(cucx, scx, stmt)?;
    }

    cucx.tcx.pop_symbol_table();

    Ok(())
}

pub fn build_statement<'a, 'ctx>(
    cucx: &'a mut CompileUnitContext<'ctx, '_, '_>,
    scx: &'a mut StmtContext<'ctx>,
    node: Stmt,
) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(cucx, scx);

    builder.build(node)?;

    Ok(())
}

pub struct StmtContext<'ctx> {
    /// Block to jump to when a `break` is executed
    ///
    /// None if **not** in a loop
    pub loop_break_bb: Option<BasicBlock<'ctx>>,
}

impl StmtContext<'_> {
    /// Fields of type `Option<T>` will be `None`
    pub fn new() -> Self {
        Self {
            loop_break_bb: None,
        }
    }
}

struct StmtBuilder<'a, 'ctx, 'm, 'c> {
    cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>,
    scx: &'a mut StmtContext<'ctx>,
}

impl<'a, 'ctx, 'm, 'c> StmtBuilder<'a, 'ctx, 'm, 'c> {
    fn new(cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>, scx: &'a mut StmtContext<'ctx>) -> Self {
        Self { cucx, scx }
    }

    /// Generate statement code
    fn build(&mut self, stmt: Stmt) -> CodegenResult<()> {
        match stmt.kind {
            StmtKind::Expr(e) => {
                build_expression(self.cucx, &e)?;
            }

            StmtKind::Return(node) => self.return_(node)?,

            StmtKind::Let(node) => self.let_(node)?,

            StmtKind::If(node) => self.if_(node)?,

            StmtKind::Loop(node) => self.loop_(node)?,

            StmtKind::Break(node) => self.break_(node)?,

            StmtKind::Assign(node) => self.assign(node)?,
        }

        Ok(())
    }

    fn assign(&mut self, node: Assign) -> CodegenResult<()> {
        let left = build_expression(self.cucx, &node.lhs)?;

        if left.get_type().mutability.is_not() {
            return Err(CodegenError::CannotAssignTwiceToImutable { span: node.span });
        }

        let ptr_to_left =
            match get_loaded_pointer(&left.get_value().as_instruction_value().unwrap()) {
                Some(p) => p,
                None => return Err(CodegenError::InvalidLeftOfAssignment { span: node.span }),
            };

        let right = build_expression(self.cucx, &node.rhs)?;

        match node.kind {
            AssignKind::Simple => self
                .cucx
                .builder
                .build_store(ptr_to_left, right.get_value()),
        };

        Ok(())
    }

    fn break_(&mut self, node: Break) -> CodegenResult<()> {
        match self.scx.loop_break_bb {
            Some(bb) => {
                self.cucx.builder.build_unconditional_branch(bb);
                Ok(())
            }

            None => Err(CodegenError::BreakOutsideOfLoop { span: node.span }),
        }
    }

    fn loop_(&mut self, node: Loop) -> CodegenResult<()> {
        let parent = self.cucx.get_current_fn();

        let body_bb = self.cucx.context().append_basic_block(parent, "body");

        let cont_bb = self.cucx.context().append_basic_block(parent, "loopcont");

        // Setup for break statement
        self.scx.loop_break_bb = Some(cont_bb);

        // Build body block
        self.cucx.builder.build_unconditional_branch(body_bb);
        self.cucx.builder.position_at_end(body_bb);
        build_block(self.cucx, self.scx, node.body)?;

        // Loop!
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(body_bb);
        }

        self.cucx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn if_(&mut self, node: If) -> CodegenResult<()> {
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
        build_block(self.cucx, self.scx, node.then)?;
        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        // Build else block
        self.cucx.builder.position_at_end(else_bb);
        if let Some(else_) = node.else_ {
            match *else_ {
                Else::If(if_) => self.if_(if_)?,
                Else::Block(block) => build_block(self.cucx, self.scx, block)?,
            }
        }
        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb);
        }

        // Build continue block
        self.cucx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn return_(&mut self, node: Return) -> CodegenResult<()> {
        match node.val {
            Some(val) => self
                .cucx
                .builder
                .build_return(Some(&build_expression(self.cucx, &val)?.get_value())),

            None => self.cucx.builder.build_return(None),
        };

        Ok(())
    }

    fn let_(&mut self, node: Let) -> CodegenResult<()> {
        match node.kind {
            LetKind::NormalLet(node) => self.normal_let(node),
            LetKind::TupleUnpack(node) => self.tuple_unpacking(node),
        }
    }

    fn normal_let_internal(
        &mut self,
        name: Ident,
        mutability: Mutability,
        init: Option<Value<'ctx>>,
        specified_ty: Rc<Ty>,
        span: Span,
    ) -> CodegenResult<()> {
        if let Some(init) = init {
            let alloca = if specified_ty.kind.is_inferred() {
                // No type information was available, so infer from an initializer

                if let TyKind::Reference(rty) = init.get_type().kind.as_ref() {
                    // Example: let mut x = &y
                    if mutability.is_mut() && rty.mutability.is_not() {
                        return Err(CodegenError::CannotAssignImmutableReferencesToMut { span });
                    }
                }

                let mut ty = (*init.get_type()).clone();
                ty.mutability = mutability;

                let alloca = self.cucx.create_entry_block_alloca(name.as_str(), &ty);

                self.cucx.tcx.add_symbol(name.name, (alloca, ty.into()));

                alloca
            } else {
                // Type information is available

                // Check if an initializer type and type match
                if specified_ty != init.get_type() {
                    return Err(CodegenError::MismatchedTypes { span });
                }

                let alloca = self
                    .cucx
                    .create_entry_block_alloca(name.as_str(), &specified_ty);

                self.cucx.tcx.add_symbol(name.name, (alloca, specified_ty));

                alloca
            };

            // Initialization
            self.cucx.builder.build_store(alloca, init.get_value());

            return Ok(());
        }

        todo!()
    }

    fn normal_let(&mut self, node: NormalLet) -> CodegenResult<()> {
        self.normal_let_internal(
            node.name,
            node.mutability,
            match node.init {
                Some(e) => Some(build_expression(self.cucx, &e)?),
                None => None,
            },
            node.ty,
            node.span,
        )
    }

    fn tuple_unpacking(&mut self, node: TupleUnpack) -> CodegenResult<()> {
        let tuple = build_expression(self.cucx, &node.init)?;

        // The tuple is a temporary object, allocate it in memory
        let tuple = if tuple.get_value().as_instruction_value().is_none() {
            let alloca = self
                .cucx
                .create_entry_block_alloca("tupletmp", &tuple.get_type());

            self.cucx.builder.build_store(alloca, tuple.get_value());

            Value::new(
                self.cucx
                    .builder
                    .build_load(self.cucx.to_llvm_type(&tuple.get_type()), alloca, ""),
                tuple.get_type(),
            )
        } else {
            tuple
        };

        let tuple_ty = tuple.get_type();

        let (tuple_field_len, tuple_mutability) = match tuple_ty.kind.as_ref() {
            TyKind::Tuple(ts) => (ts.len(), Mutability::Not),

            TyKind::Reference(rty) => {
                let tuple_ty = rty.get_base_type_of_reference();

                match tuple_ty.kind.as_ref() {
                    TyKind::Tuple(ts) => (ts.len(), tuple_ty.mutability),

                    _ => {
                        return Err(CodegenError::InitializerTupleUnpackingMustBeTuple {
                            span: node.init.span,
                        })
                    }
                }
            }

            _ => {
                return Err(CodegenError::InitializerTupleUnpackingMustBeTuple {
                    span: node.init.span,
                })
            }
        };

        if node.names.len() != tuple_field_len {
            return Err(CodegenError::NumberOfTupleFieldsDoesNotMatch {
                lens: (node.names.len(), tuple_field_len),
                span: node.span,
            });
        }

        // Unpacking
        for (index, name_and_mutability) in node.names.into_iter().enumerate() {
            let (name, mutability) = match name_and_mutability {
                Some(nam) => (nam.0, nam.1),

                // Ignore field
                None => continue,
            };

            if mutability.is_mut() && tuple_mutability.is_not() {
                todo!("ERROR")
            }

            self.tuple_unpacking_internal(&tuple, index as u64, name, mutability, node.span)?;
        }

        Ok(())
    }

    fn tuple_unpacking_internal(
        &mut self,
        tuple: &Value<'ctx>,
        index: u64,
        unpacked_name: Ident,
        unpacked_mutability: Mutability,
        span: Span,
    ) -> CodegenResult<()> {
        if matches!(tuple.get_type().kind.as_ref(), TyKind::Reference(_)) {
            return self.reference_tuple_unpacking(
                tuple,
                index,
                unpacked_name,
                unpacked_mutability,
                span,
            );
        }

        let ptr_to_tuple =
            get_loaded_pointer(&tuple.get_value().as_instruction_value().unwrap()).unwrap();

        let unpacked_value =
            build_tuple_indexing(self.cucx, ptr_to_tuple, index, &tuple.get_type(), span)?;

        self.normal_let_internal(
            unpacked_name,
            unpacked_mutability,
            Some(unpacked_value),
            Ty {
                kind: TyKind::Inferred.into(),
                mutability: unpacked_mutability,
            }
            .into(),
            span,
        )?;

        Ok(())
    }

    fn reference_tuple_unpacking(
        &mut self,
        tuple: &Value<'ctx>,
        index: u64,
        unpacked_name: Ident,
        unpacked_mutability: Mutability,
        span: Span,
    ) -> CodegenResult<()> {
        assert!(matches!(
            tuple.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let ref_tuple_ty = tuple.get_type();
        let ref_tuple_ty = if let TyKind::Reference(rty) = ref_tuple_ty.kind.as_ref() {
            rty
        } else {
            unreachable!();
        };

        let ptr_to_tuple = tuple.get_value().into_pointer_value();

        let unpacked_value =
            build_tuple_indexing(self.cucx, ptr_to_tuple, index, &ref_tuple_ty.refee_ty, span)?;

        let ptr_to_unpacked_value =
            get_loaded_pointer(&unpacked_value.get_value().as_instruction_value().unwrap())
                .unwrap();

        let unpacked_value_reference = Value::new(
            ptr_to_unpacked_value.into(),
            Ty {
                kind: TyKind::Reference(RefrenceType {
                    refee_ty: unpacked_value.get_type(),
                    mutability: ref_tuple_ty.mutability,
                })
                .into(),
                mutability: Mutability::Not,
            }
            .into(),
        );

        self.normal_let_internal(
            unpacked_name,
            unpacked_mutability,
            Some(unpacked_value_reference),
            Ty {
                kind: TyKind::Inferred.into(),
                mutability: unpacked_mutability,
            }
            .into(),
            span,
        )?;

        Ok(())
    }
}
