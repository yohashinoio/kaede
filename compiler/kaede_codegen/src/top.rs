use std::rc::Rc;

use inkwell::{types::BasicType, values::FunctionValue};
use kaede_ast::top::{Fn, TopLevel, TopLevelKind};
use kaede_type::Ty;

use crate::{
    error::CodegenResult,
    stmt::{build_block, StmtCtx},
    CGCtx, SymbolTable,
};

pub fn build_top_level(ctx: &mut CGCtx, node: TopLevel) -> CodegenResult<()> {
    let mut builder = TopLevelBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

struct TopLevelBuilder<'a, 'ctx, 'c> {
    ctx: &'a mut CGCtx<'ctx, 'c>,
}

impl<'a, 'ctx, 'c> TopLevelBuilder<'a, 'ctx, 'c> {
    fn new(ctx: &'a mut CGCtx<'ctx, 'c>) -> Self {
        Self { ctx }
    }

    /// Generate top-level code.
    fn build(&mut self, node: TopLevel) -> CodegenResult<()> {
        match node.kind {
            TopLevelKind::Fn(func) => self.func(func)?,
        }

        Ok(())
    }

    fn func(&mut self, node: Fn) -> CodegenResult<()> {
        let param_llvm_tys = node
            .params
            .iter()
            .map(|e| e.1.kind.as_llvm_type(self.ctx.context).into())
            .collect::<Vec<_>>();

        let fn_type = match &node.return_ty {
            Some(ty) => ty
                .kind
                .as_llvm_type(self.ctx.context)
                .fn_type(param_llvm_tys.as_slice(), false),

            None => self
                .ctx
                .context
                .void_type()
                .fn_type(param_llvm_tys.as_slice(), false),
        };

        let fn_value = self.ctx.module.add_function(&node.name, fn_type, None);

        let basic_block = self.ctx.context.append_basic_block(fn_value, "entry");
        self.ctx.builder.position_at_end(basic_block);

        let param_info = node
            .params
            .into_iter()
            .map(|e| (e.0, Rc::new(e.1)))
            .collect::<Vec<_>>();

        // Store return type information in table.
        self.ctx
            .return_ty_table
            .insert(fn_value, node.return_ty.map(Rc::new));

        // Store parameter information in table.
        self.ctx
            .param_table
            .insert(fn_value, param_info.iter().map(|e| e.1.clone()).collect());

        // Allocate parameters.
        let mut param_table = self.create_param_table(param_info, fn_value);

        if node.body.body.is_empty() {
            self.ctx.builder.build_return(None);
        } else {
            build_block(self.ctx, &mut StmtCtx::new(), &mut param_table, node.body)?;
        }

        Ok(())
    }

    /// Expand function parameters into a symbol table.
    fn create_param_table(
        &self,
        param_info: Vec<(String, Rc<Ty>)>,
        fn_value: FunctionValue<'ctx>,
    ) -> SymbolTable<'ctx> {
        let mut params = SymbolTable::new();

        assert_eq!(fn_value.count_params(), param_info.len() as u32);

        for (idx, (name, ty)) in param_info.into_iter().enumerate() {
            let alloca = self
                .ctx
                .builder
                .build_alloca(ty.kind.as_llvm_type(self.ctx.context), &name);

            self.ctx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap());

            params.0.insert(name, (alloca, ty));
        }

        params
    }
}
