use inkwell::{basic_block::BasicBlock, IntPredicate};
use kaede_ast::stmt::{Block, Break, Else, If, Let, Loop, Return, Stmt, StmtKind};

use crate::{
    error::{CodegenError, CodegenResult},
    expr::build_expression,
    CGCtx, SymbolTable,
};

pub fn build_block<'a, 'ctx>(
    ctx: &'a CGCtx<'ctx, '_>,
    stmt_ctx: &'a mut StmtCtx<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
    block: Block,
) -> CodegenResult<()> {
    for stmt in block.body {
        build_statement(ctx, stmt_ctx, scope, stmt)?;
    }

    Ok(())
}

pub fn build_statement<'a, 'ctx>(
    ctx: &'a CGCtx<'ctx, '_>,
    stmt_ctx: &'a mut StmtCtx<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
    node: Stmt,
) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(ctx, stmt_ctx, scope);

    builder.build(node)?;

    Ok(())
}

pub struct StmtCtx<'ctx> {
    /// Jump at break (statement).
    /// None if not in a loop.
    pub loop_break_bb: Option<BasicBlock<'ctx>>,
}

impl StmtCtx<'_> {
    /// Fields of type `Option<T>` will be `None`.
    pub fn new() -> Self {
        Self {
            loop_break_bb: None,
        }
    }
}

struct StmtBuilder<'a, 'ctx, 'c> {
    ctx: &'a CGCtx<'ctx, 'c>,
    stmt_ctx: &'a mut StmtCtx<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
}

impl<'a, 'ctx, 'c> StmtBuilder<'a, 'ctx, 'c> {
    fn new(
        ctx: &'a CGCtx<'ctx, 'c>,
        stmt_ctx: &'a mut StmtCtx<'ctx>,
        scope: &'a mut SymbolTable<'ctx>,
    ) -> Self {
        Self {
            ctx,
            stmt_ctx,
            scope,
        }
    }

    /// Generate statement code.
    fn build(&mut self, stmt: Stmt) -> CodegenResult<()> {
        match stmt.kind {
            StmtKind::Expr(e) => {
                build_expression(self.ctx, e, self.scope)?;
            }

            StmtKind::Return(node) => self.return_(node)?,

            StmtKind::Let(node) => self.let_(node)?,

            StmtKind::If(node) => self.if_(node)?,

            StmtKind::Loop(node) => self.loop_(node)?,

            StmtKind::Break(node) => self.break_(node)?,
        }

        Ok(())
    }

    fn break_(&mut self, node: Break) -> CodegenResult<()> {
        match self.stmt_ctx.loop_break_bb {
            Some(bb) => {
                self.ctx.builder.build_unconditional_branch(bb);
                Ok(())
            }

            None => Err(CodegenError::BreakOutsideOfLoop { span: node.span }),
        }
    }

    fn loop_(&mut self, node: Loop) -> CodegenResult<()> {
        let parent = self.ctx.get_current_fn();

        let body_bb = self.ctx.context.append_basic_block(parent, "body");

        let cont_bb = self.ctx.context.append_basic_block(parent, "cont");

        // Setup for break statement
        self.stmt_ctx.loop_break_bb = Some(cont_bb);

        // Build body block
        self.ctx.builder.build_unconditional_branch(body_bb);
        self.ctx.builder.position_at_end(body_bb);
        build_block(self.ctx, self.stmt_ctx, self.scope, node.body)?;

        // Loop!
        if self.ctx.no_terminator() {
            self.ctx.builder.build_unconditional_branch(body_bb);
        }

        self.ctx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn if_(&mut self, node: If) -> CodegenResult<()> {
        let parent = self.ctx.get_current_fn();
        let zero_const = self.ctx.context.bool_type().const_zero();

        let cond = build_expression(self.ctx, node.cond, self.scope)?;
        let cond = self.ctx.builder.build_int_compare(
            IntPredicate::NE,
            cond.get_value().into_int_value(),
            zero_const,
            "ifcond",
        );

        let then_bb = self.ctx.context.append_basic_block(parent, "then");
        let else_bb = self.ctx.context.append_basic_block(parent, "else");
        let cont_bb = self.ctx.context.append_basic_block(parent, "ifcont");

        self.ctx
            .builder
            .build_conditional_branch(cond, then_bb, else_bb);

        // Build then block
        self.ctx.builder.position_at_end(then_bb);
        build_block(self.ctx, self.stmt_ctx, self.scope, node.then)?;
        // Since there can be no more than one terminator per block.
        if self.ctx.no_terminator() {
            self.ctx.builder.build_unconditional_branch(cont_bb);
        }

        // Build else block
        self.ctx.builder.position_at_end(else_bb);
        if let Some(else_) = node.else_ {
            match *else_ {
                Else::If(if_) => self.if_(if_)?,
                Else::Block(block) => build_block(self.ctx, self.stmt_ctx, self.scope, block)?,
            }
        }
        // Since there can be no more than one terminator per block.
        if self.ctx.no_terminator() {
            self.ctx.builder.build_unconditional_branch(cont_bb);
        }

        // Build continue block
        self.ctx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn return_(&mut self, node: Return) -> CodegenResult<()> {
        match node.val {
            Some(val) => self.ctx.builder.build_return(Some(
                &build_expression(self.ctx, val, self.scope)?.get_value(),
            )),

            None => self.ctx.builder.build_return(None),
        };

        Ok(())
    }

    fn let_(&mut self, node: Let) -> CodegenResult<()> {
        let alloca = self.ctx.create_entry_block_alloca(&node.name, &node.ty);

        if let Some(init) = node.init {
            let init = build_expression(self.ctx, init, self.scope)?;

            // Initialization
            self.ctx.builder.build_store(alloca, init.get_value());

            self.scope.insert(node.name, (alloca, init.get_type()));

            return Ok(());
        }

        unreachable!();
    }
}
