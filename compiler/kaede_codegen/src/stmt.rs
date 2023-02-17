use kaede_ast::stmt::{Let, Return, Stmt, StmtList};

use crate::{expr::build_expression, CodeGen, Symbols};

pub fn build_statement_list(ctx: &CodeGen, list: StmtList) {
    let mut scope = Symbols::new();

    for stmt in list {
        build_statement(ctx, stmt, &mut scope);
    }
}

pub fn build_statement<'a, 'ctx, 'c>(
    ctx: &'a CodeGen<'ctx, 'c>,
    node: Stmt,
    scope: &'a mut Symbols<'ctx>,
) {
    let mut builder = StmtBuilder::new(ctx, scope);

    builder.build(node);
}

struct StmtBuilder<'a, 'ctx, 'c> {
    ctx: &'a CodeGen<'ctx, 'c>,
    scope: &'a mut Symbols<'ctx>,
}

impl<'a, 'ctx, 'c> StmtBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a CodeGen<'ctx, 'c>, scope: &'a mut Symbols<'ctx>) -> Self {
        Self { ctx, scope }
    }

    fn build(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                build_expression(self.ctx, &e, self.scope);
            }

            Stmt::Return(node) => self.return_(node),

            Stmt::Let(node) => self.let_(node),
        }
    }

    fn return_(&mut self, node: Return) {
        match &node.0 {
            Some(val) => self
                .ctx
                .builder
                .build_return(Some(&build_expression(self.ctx, val, self.scope))),

            None => self.ctx.builder.build_return(None),
        };
    }

    fn let_(&mut self, node: Let) {
        let alloca = self
            .ctx
            .builder
            .build_alloca(node.ty.as_llvm_type(self.ctx.context), &node.name);

        if let Some(init) = node.init.as_ref() {
            // Initialization
            self.ctx
                .builder
                .build_store(alloca, build_expression(self.ctx, init, self.scope));
        }

        self.scope.insert(node.name, alloca);
    }
}
