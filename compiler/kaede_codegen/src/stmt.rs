use kaede_ast::{Return, Stmt, StmtList};

use crate::CodeGen;

impl CodeGen<'_, '_> {
    pub fn stmt_list(&self, stmt_list: &StmtList) {
        for stmt in stmt_list {
            self.stmt(stmt);
        }
    }

    pub fn stmt(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Return(r) => self.return_(r),
            Stmt::Expr(e) => {
                self.expr(e);
            }
        }
    }

    fn return_(&self, node: &Return) {
        match &node.0 {
            Some(val) => self.builder.build_return(Some(&self.expr(val))),
            None => self.builder.build_return(None),
        };
    }
}
