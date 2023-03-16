use std::rc::Rc;

use inkwell::{basic_block::BasicBlock, IntPredicate};
use kaede_ast::{
    expr::{Expr, ExprKind},
    stmt::{Assign, AssignKind, Block, Break, Else, If, Let, Loop, Return, Stmt, StmtKind},
};
use kaede_type::TyKind;

use crate::{
    error::{CodegenError, CodegenResult},
    expr::build_expression,
    value::Value,
    SymbolTable, TranslUnitContext,
};

pub fn build_block<'a, 'ctx>(
    ctx: &'a TranslUnitContext<'ctx, '_, '_>,
    scx: &'a mut StmtContext<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
    block: Block,
) -> CodegenResult<()> {
    for stmt in block.body {
        build_statement(ctx, scx, scope, stmt)?;
    }

    Ok(())
}

pub fn build_statement<'a, 'ctx>(
    ctx: &'a TranslUnitContext<'ctx, '_, '_>,
    scx: &'a mut StmtContext<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
    node: Stmt,
) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(ctx, scx, scope);

    builder.build(node)?;

    Ok(())
}

pub struct StmtContext<'ctx> {
    /// Block to jump to when a `break` is executed
    ///
    /// None if not in a loop
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

struct StmtBuilder<'a, 'ctx, 'modl, 'cgcx> {
    tucx: &'a TranslUnitContext<'ctx, 'modl, 'cgcx>,
    scx: &'a mut StmtContext<'ctx>,
    scope: &'a mut SymbolTable<'ctx>,
}

impl<'a, 'ctx, 'modl, 'cgcx> StmtBuilder<'a, 'ctx, 'modl, 'cgcx> {
    fn new(
        tucx: &'a TranslUnitContext<'ctx, 'modl, 'cgcx>,
        scx: &'a mut StmtContext<'ctx>,
        scope: &'a mut SymbolTable<'ctx>,
    ) -> Self {
        Self { tucx, scx, scope }
    }

    /// Generate statement code
    fn build(&mut self, stmt: Stmt) -> CodegenResult<()> {
        match stmt.kind {
            StmtKind::Expr(e) => {
                build_expression(self.tucx, &e, self.scope)?;
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

    /// Create a value for the side to be assigned
    /// Return `Err` if the expression is inappropriate as the one to be assigned
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

                Ok(Value::new((*ptr).into(), ty.clone()))
            }

            ExprKind::Deref(deref) => {
                let opr = build_expression(self.tucx, &deref.operand, self.scope)?;

                // Check if mutable
                assert!(matches!(opr.get_type().kind.as_ref(), TyKind::Reference(_)));
                if let TyKind::Reference((_, mutability)) = opr.get_type().kind.as_ref() {
                    if mutability.is_not() {
                        return Err(CodegenError::CannotAssignToImutableRef { span: deref.span });
                    }
                }

                Ok(opr)
            }

            ExprKind::FnCall(_) => unimplemented!(),

            _ => Err(CodegenError::InvalidLeftOfAssignment { span: node.span }),
        }
    }

    fn assign(&mut self, node: Assign) -> CodegenResult<()> {
        let lhs = self.build_assignable(node.lhs)?;

        let rhs = build_expression(self.tucx, &node.rhs, self.scope)?;

        match node.kind {
            AssignKind::Simple => self
                .tucx
                .builder
                .build_store(lhs.get_value().into_pointer_value(), rhs.get_value()),
        };

        Ok(())
    }

    fn break_(&mut self, node: Break) -> CodegenResult<()> {
        match self.scx.loop_break_bb {
            Some(bb) => {
                self.tucx.builder.build_unconditional_branch(bb);
                Ok(())
            }

            None => Err(CodegenError::BreakOutsideOfLoop { span: node.span }),
        }
    }

    fn loop_(&mut self, node: Loop) -> CodegenResult<()> {
        let parent = self.tucx.get_current_fn();

        let body_bb = self.tucx.context().append_basic_block(parent, "body");

        let cont_bb = self.tucx.context().append_basic_block(parent, "loopcont");

        // Setup for break statement
        self.scx.loop_break_bb = Some(cont_bb);

        // Build body block
        self.tucx.builder.build_unconditional_branch(body_bb);
        self.tucx.builder.position_at_end(body_bb);
        build_block(self.tucx, self.scx, self.scope, node.body)?;

        // Loop!
        if self.tucx.no_terminator() {
            self.tucx.builder.build_unconditional_branch(body_bb);
        }

        self.tucx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn if_(&mut self, node: If) -> CodegenResult<()> {
        let parent = self.tucx.get_current_fn();
        let zero_const = self.tucx.context().bool_type().const_zero();

        let cond = build_expression(self.tucx, &node.cond, self.scope)?;
        let cond = self.tucx.builder.build_int_compare(
            IntPredicate::NE,
            cond.get_value().into_int_value(),
            zero_const,
            "ifcond",
        );

        let then_bb = self.tucx.context().append_basic_block(parent, "then");
        let else_bb = self.tucx.context().append_basic_block(parent, "else");
        let cont_bb = self.tucx.context().append_basic_block(parent, "ifcont");

        self.tucx
            .builder
            .build_conditional_branch(cond, then_bb, else_bb);

        // Build then block
        self.tucx.builder.position_at_end(then_bb);
        build_block(self.tucx, self.scx, self.scope, node.then)?;
        // Since there can be no more than one terminator per block
        if self.tucx.no_terminator() {
            self.tucx.builder.build_unconditional_branch(cont_bb);
        }

        // Build else block
        self.tucx.builder.position_at_end(else_bb);
        if let Some(else_) = node.else_ {
            match *else_ {
                Else::If(if_) => self.if_(if_)?,
                Else::Block(block) => build_block(self.tucx, self.scx, self.scope, block)?,
            }
        }
        // Since there can be no more than one terminator per block
        if self.tucx.no_terminator() {
            self.tucx.builder.build_unconditional_branch(cont_bb);
        }

        // Build continue block
        self.tucx.builder.position_at_end(cont_bb);

        Ok(())
    }

    fn return_(&mut self, node: Return) -> CodegenResult<()> {
        match node.val {
            Some(val) => self.tucx.builder.build_return(Some(
                &build_expression(self.tucx, &val, self.scope)?.get_value(),
            )),

            None => self.tucx.builder.build_return(None),
        };

        Ok(())
    }

    fn let_(&mut self, node: Let) -> CodegenResult<()> {
        if let Some(init) = node.init {
            let init = build_expression(self.tucx, &init, self.scope)?;

            let alloca = if node.ty.kind.is_unknown() {
                // No type information was available, so infer from an initializer
                let mut ty = (*init.get_type()).clone();
                ty.mutability = node.ty.mutability;

                let alloca = self.tucx.create_entry_block_alloca(node.name.as_str(), &ty);

                self.scope.0.insert(node.name.name, (alloca, Rc::new(ty)));

                alloca
            } else {
                // Type information is available

                // Check if an initializer type and type match
                if node.ty != *init.get_type() {
                    return Err(CodegenError::MismatchedTypes { span: node.span });
                }

                let alloca = self
                    .tucx
                    .create_entry_block_alloca(node.name.as_str(), &node.ty);

                self.scope
                    .0
                    .insert(node.name.name, (alloca, Rc::new(node.ty)));

                alloca
            };

            // Initialization
            self.tucx.builder.build_store(alloca, init.get_value());

            return Ok(());
        }

        unreachable!();
    }
}
