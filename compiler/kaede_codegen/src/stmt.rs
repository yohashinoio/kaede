use std::rc::Rc;

use inkwell::{basic_block::BasicBlock, values::BasicValue, IntPredicate};
use kaede_ast::{
    expr::ExprKind,
    stmt::{Assign, AssignKind, Block, Break, Else, If, Let, Loop, Return, Stmt, StmtKind},
};

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
        if !matches!(node.lhs.kind, ExprKind::Ident(_) | ExprKind::Deref(_)) {
            return Err(CodegenError::InvalidLeftOfAssignment { span: node.span });
        }

        let left = build_expression(self.cucx, &node.lhs)?;

        if left.get_type().mutability.is_not() {
            return Err(CodegenError::CannotAssignTwiceToImutable { span: node.span });
        }

        let ptr_to_left =
            get_loaded_pointer(&left.get_value().as_instruction_value().unwrap()).unwrap();

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
        if let Some(init) = node.init {
            let init = build_expression(self.cucx, &init)?;

            let alloca = if node.ty.kind.is_inferred() {
                // No type information was available, so infer from an initializer
                let mut ty = (*init.get_type()).clone();
                ty.mutability = node.ty.mutability;

                let alloca = self.cucx.create_entry_block_alloca(node.name.as_str(), &ty);

                self.cucx
                    .tcx
                    .add_symbol(node.name.name, (alloca, Rc::new(ty)));

                alloca
            } else {
                // Type information is available

                // Check if an initializer type and type match
                if node.ty != *init.get_type() {
                    return Err(CodegenError::MismatchedTypes { span: node.span });
                }

                let alloca = self
                    .cucx
                    .create_entry_block_alloca(node.name.as_str(), &node.ty);

                self.cucx
                    .tcx
                    .add_symbol(node.name.name, (alloca, Rc::new(node.ty)));

                alloca
            };

            // Initialization
            self.cucx.builder.build_store(alloca, init.get_value());

            return Ok(());
        }

        unreachable!();
    }
}
