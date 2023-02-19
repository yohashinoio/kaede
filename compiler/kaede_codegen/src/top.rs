use kaede_ast::{stmt::StmtList, Top, TopEnum};

use crate::{error::CodegenResult, stmt::build_statement_list, CodeGen};

pub fn build_top(ctx: &CodeGen, node: Top) -> CodegenResult<()> {
    let builder = TopBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

struct TopBuilder<'a, 'b, 'c> {
    ctx: &'a CodeGen<'b, 'c>,
}

impl<'a, 'b, 'c> TopBuilder<'a, 'b, 'c> {
    fn new(ctx: &'a CodeGen<'b, 'c>) -> Self {
        Self { ctx }
    }

    fn build(&self, node: Top) -> CodegenResult<()> {
        match node.val {
            TopEnum::Function { name, body } => self.func(&name, body)?,
        }

        Ok(())
    }

    fn func(&self, name: &str, body: StmtList) -> CodegenResult<()> {
        let fn_type = self.ctx.context.i32_type().fn_type(&[], false);

        let fn_value = self.ctx.module.add_function(name, fn_type, None);

        let basic_block = self.ctx.context.append_basic_block(fn_value, "entry");
        self.ctx.builder.position_at_end(basic_block);

        if body.is_empty() {
            self.ctx.builder.build_return(None);
        } else {
            build_statement_list(self.ctx, body)?;
        }

        Ok(())
    }
}
