use std::rc::Rc;

use inkwell::types::BasicType;
use kaede_ast::top::{Func, Top, TopEnum};

use crate::{error::CodegenResult, stmt::build_statement_list, CodeGen};

pub fn build_top(ctx: &mut CodeGen, node: Top) -> CodegenResult<()> {
    let mut builder = TopBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

struct TopBuilder<'a, 'b, 'c> {
    ctx: &'a mut CodeGen<'b, 'c>,
}

impl<'a, 'b, 'c> TopBuilder<'a, 'b, 'c> {
    fn new(ctx: &'a mut CodeGen<'b, 'c>) -> Self {
        Self { ctx }
    }

    fn build(&mut self, node: Top) -> CodegenResult<()> {
        match node.val {
            TopEnum::Func(func) => self.func(func)?,
        }

        Ok(())
    }

    fn func(&mut self, node: Func) -> CodegenResult<()> {
        let fn_type = match &node.return_ty {
            Some(ty) => ty.as_llvm_type(self.ctx.context).fn_type(&[], false),
            None => self.ctx.context.void_type().fn_type(&[], false),
        };

        let fn_value = self.ctx.module.add_function(&node.name, fn_type, None);

        let basic_block = self.ctx.context.append_basic_block(fn_value, "entry");
        self.ctx.builder.position_at_end(basic_block);

        self.ctx
            .return_ty_table
            .insert(node.name, node.return_ty.map(Rc::new));

        if node.body.is_empty() {
            self.ctx.builder.build_return(None);
        } else {
            build_statement_list(self.ctx, node.body)?;
        }

        Ok(())
    }
}
