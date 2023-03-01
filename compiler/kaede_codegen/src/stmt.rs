use std::rc::Rc;

use inkwell::{basic_block::BasicBlock, values::BasicValue, IntPredicate};
use kaede_ast::{
    expr::{Expr, ExprKind},
    stmt::{Assign, AssignKind, Block, Break, Else, If, Let, Loop, Return, Stmt, StmtKind},
};

use crate::{
    error::{CodegenError, CodegenResult},
    expr::build_expression,
    value::Value,
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

            StmtKind::Assign(node) => self.assign(node)?,
        }

        Ok(())
    }

    /// Create a value for the side to be assigned.
    /// Return `Err` if the expression is inappropriate as the one to be assigned.
    fn build_assignable(&mut self, node: Expr) -> CodegenResult<Value<'ctx>> {
        match node.kind {
            ExprKind::Ident(ident) => {
                let (ptr, ty) = self.scope.find(&ident)?;

                if ty.mutability.is_not() {
                    return Err(CodegenError::CannotAssignTwiceToImutable {
                        name: ident.name,
                        span: ident.span,
                    });
                }

                Ok(Value::new(ptr.as_basic_value_enum(), ty.clone()))
            }

            ExprKind::FnCall(_) => unimplemented!(),

            _ => Err(CodegenError::InvalidLeftOfAssignment { span: node.span }),
        }
    }

    fn assign(&mut self, node: Assign) -> CodegenResult<()> {
        let lhs = self.build_assignable(node.lhs)?;

        let rhs = build_expression(self.ctx, node.rhs, self.scope)?;

        match node.kind {
            AssignKind::Simple => self
                .ctx
                .builder
                .build_store(lhs.get_value().into_pointer_value(), rhs.get_value()),
        };

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

        let cont_bb = self.ctx.context.append_basic_block(parent, "loopcont");

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
        if let Some(init) = node.init {
            let init = build_expression(self.ctx, init, self.scope)?;

            let alloca = if node.ty.is_unknown() {
                // No type information was available, so infer from an initializer
                let mut ty = (*init.get_type()).clone();
                ty.mutability = node.ty.mutability;

                let alloca = self.ctx.create_entry_block_alloca(&node.name, &ty);

                self.scope.0.insert(node.name, (alloca, Rc::new(ty)));

                alloca
            } else {
                // Type information is available

                // Check if an initializer type and type match
                if node.ty != *init.get_type() {
                    return Err(CodegenError::MismatchedTypes { span: node.span });
                }

                let alloca = self.ctx.create_entry_block_alloca(&node.name, &node.ty);

                self.scope.0.insert(node.name, (alloca, Rc::new(node.ty)));

                alloca
            };

            // Initialization
            self.ctx.builder.build_store(alloca, init.get_value());

            return Ok(());
        }

        unreachable!();
    }
}
