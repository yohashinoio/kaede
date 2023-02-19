use kaede_ast::stmt::{Let, Return, Stmt, StmtEnum, StmtList};

use crate::{error::CodegenResult, expr::build_expression, CodeGen, Symbols};

pub fn build_statement_list(ctx: &CodeGen, list: StmtList) -> CodegenResult<()> {
    let mut scope = Symbols::new();

    for stmt in list {
        build_statement(ctx, stmt, &mut scope)?;
    }

    Ok(())
}

pub fn build_statement<'a, 'ctx, 'c>(
    ctx: &'a CodeGen<'ctx, 'c>,
    node: Stmt,
    scope: &'a mut Symbols<'ctx>,
) -> CodegenResult<()> {
    let mut builder = StmtBuilder::new(ctx, scope);

    builder.build(node)?;

    Ok(())
}

struct StmtBuilder<'a, 'ctx, 'c> {
    ctx: &'a CodeGen<'ctx, 'c>,
    scope: &'a mut Symbols<'ctx>,
}

impl<'a, 'ctx, 'c> StmtBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CodeGen<'ctx, 'c>, scope: &'a mut Symbols<'ctx>) -> Self {
        Self { ctx, scope }
    }

    fn build(&mut self, stmt: Stmt) -> CodegenResult<()> {
        match stmt.val {
            StmtEnum::Expr(e) => {
                build_expression(self.ctx, e, self.scope)?;
            }

            StmtEnum::Return(node) => self.return_(node)?,

            StmtEnum::Let(node) => self.let_(node)?,
        }

        Ok(())
    }

    fn return_(&mut self, node: Return) -> CodegenResult<()> {
        match node.0 {
            Some(val) => self
                .ctx
                .builder
                .build_return(Some(&build_expression(self.ctx, val, self.scope)?)),

            None => self.ctx.builder.build_return(None),
        };

        Ok(())
    }

    fn let_(&mut self, node: Let) -> CodegenResult<()> {
        let alloca = self
            .ctx
            .builder
            .build_alloca(node.ty.as_llvm_type(self.ctx.context), &node.name);

        if let Some(init) = node.init {
            // Initialization
            self.ctx
                .builder
                .build_store(alloca, build_expression(self.ctx, init, self.scope)?);
        }

        self.scope.insert(node.name, alloca);

        Ok(())
    }
}
