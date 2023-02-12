use kaede_ast::{Let, Return, Stmt, StmtList};

use crate::CodeGen;

impl CodeGen<'_, '_> {
    pub fn stmt_list(&self, stmt_list: &StmtList) {
        for stmt in stmt_list {
            self.stmt(stmt);
        }
    }

    pub fn stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(e) => {
                self.expr(e);
            }

            Stmt::Return(node) => self.return_(node),

            Stmt::Let(node) => self.let_(node),
        }
    }

    fn return_(&self, node: &Return) {
        match &node.0 {
            Some(val) => self.builder.build_return(Some(&self.expr(val))),
            None => self.builder.build_return(None),
        };
    }

    fn let_(&self, node: &Let) {
        self.builder
            .build_alloca(self.context.i32_type(), &node.name);
    }
}
