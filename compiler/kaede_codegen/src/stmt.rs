use inkwell::types::BasicTypeEnum;
use kaede_ast::stmt::{Block, Let, Return, Stmt, StmtKind};

use crate::{error::CodegenResult, expr::build_expression, CGCtx, SymbolTable};

pub fn build_block<'a, 'ctx>(
    ctx: &'a CGCtx<'ctx, '_>,
    block: Block,
    scope: &'a mut SymbolTable<'ctx>,
) -> CodegenResult<()> {
    for stmt in block.body {
        build_statement(ctx, stmt, scope)?;
    }

    Ok(())
}

pub fn build_statement<'a, 'ctx>(
    ctx: &'a CGCtx<'ctx, '_>,
    node: Stmt,
    scope: &'a mut SymbolTable<'ctx>,
) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(ctx, scope);

    builder.build(node)?;

    Ok(())
}

struct StmtBuilder<'a, 'ctx, 'c> {
    ctx: &'a CGCtx<'ctx, 'c>,
    scope: &'a mut SymbolTable<'ctx>,
}

impl<'a, 'ctx, 'c> StmtBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CGCtx<'ctx, 'c>, scope: &'a mut SymbolTable<'ctx>) -> Self {
        Self { ctx, scope }
    }

    fn build(&mut self, stmt: Stmt) -> CodegenResult<()> {
        match stmt.kind {
            StmtKind::Expr(e) => {
                build_expression(self.ctx, e, self.scope)?;
            }

            StmtKind::Return(node) => self.return_(node)?,

            StmtKind::Let(node) => self.let_(node)?,

            StmtKind::If(_node) => unimplemented!(),
        }

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
        let ty: BasicTypeEnum<'ctx> = node.ty.as_llvm_type(self.ctx.context);

        let alloca = self.ctx.builder.build_alloca(ty, &node.name);

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
