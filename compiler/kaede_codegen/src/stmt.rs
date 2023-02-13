use kaede_ast::{Let, Return, Stmt, StmtList};

use crate::{expr::build_expression, CodeGen};

pub fn build_statement_list(ctx: &CodeGen, list: &StmtList) {
    for stmt in list {
        build_statement(ctx, stmt);
    }
}

pub fn build_statement(ctx: &CodeGen, node: &Stmt) {
    let builder = StmtBuilder::new(ctx);

    builder.build(node);
}

struct StmtBuilder<'a, 'b, 'c> {
    ctx: &'a CodeGen<'b, 'c>,
}

impl<'a, 'b, 'c> StmtBuilder<'a, 'b, 'c> {
    fn new(ctx: &'a CodeGen<'b, 'c>) -> Self {
        Self { ctx }
    }

    fn build(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                build_expression(self.ctx, e);
            }

            Stmt::Return(node) => self.return_(node),

            Stmt::Let(node) => self.let_(node),
        }
    }

    fn return_(&self, node: &Return) {
        match &node.0 {
            Some(val) => self
                .ctx
                .builder
                .build_return(Some(&build_expression(self.ctx, val))),

            None => self.ctx.builder.build_return(None),
        };
    }

    fn let_(&self, node: &Let) {
        let alloca = self
            .ctx
            .builder
            .build_alloca(self.ctx.context.i32_type(), &node.name);

        if let Some(init) = node.init.as_ref() {
            // Initialization
            self.ctx
                .builder
                .build_store(alloca, build_expression(self.ctx, init));
        }
    }
}
