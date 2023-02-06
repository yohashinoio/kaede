use kaede_ast::{Return, Stmt};

use crate::CodeGen;

impl CodeGen<'_, '_> {
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
