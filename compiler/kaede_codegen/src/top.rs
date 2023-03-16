use std::rc::Rc;

use inkwell::{types::BasicType, values::FunctionValue};
use kaede_ast::top::{Fn, Struct, TopLevel, TopLevelKind};
use kaede_type::Ty;

use crate::{
    as_llvm_type,
    error::CodegenResult,
    stmt::{build_block, StmtContext},
    tcx::{StructFieldInfo, StructInfo},
    SymbolTable, TranslUnitContext,
};

pub fn build_top_level(ctx: &mut TranslUnitContext, node: TopLevel) -> CodegenResult<()> {
    let mut builder = TopLevelBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

struct TopLevelBuilder<'a, 'ctx, 'modl, 'cgcx> {
    tucx: &'a mut TranslUnitContext<'ctx, 'modl, 'cgcx>,
}

impl<'a, 'ctx, 'modl, 'cgcx> TopLevelBuilder<'a, 'ctx, 'modl, 'cgcx> {
    fn new(ctx: &'a mut TranslUnitContext<'ctx, 'modl, 'cgcx>) -> Self {
        Self { tucx: ctx }
    }

    /// Generate top-level code.
    fn build(&mut self, node: TopLevel) -> CodegenResult<()> {
        match node.kind {
            TopLevelKind::Fn(func) => self.define_func(func)?,

            TopLevelKind::Struct(node) => self.define_struct(node),
        }

        Ok(())
    }

    fn define_func(&mut self, node: Fn) -> CodegenResult<()> {
        let param_llvm_tys = node
            .params
            .iter()
            .map(|e| as_llvm_type(self.tucx, &e.1).into())
            .collect::<Vec<_>>();

        let fn_type = match &node.return_ty {
            Some(ty) => as_llvm_type(self.tucx, ty).fn_type(param_llvm_tys.as_slice(), false),

            None => self
                .tucx
                .context()
                .void_type()
                .fn_type(param_llvm_tys.as_slice(), false),
        };

        let fn_value = self
            .tucx
            .module
            .add_function(node.name.as_str(), fn_type, None);

        let basic_block = self.tucx.context().append_basic_block(fn_value, "entry");
        self.tucx.builder.position_at_end(basic_block);

        let param_info = node
            .params
            .into_iter()
            .map(|e| (e.0, Rc::new(e.1)))
            .collect::<Vec<_>>();

        // Store return type information in table
        self.tucx
            .tcx
            .return_ty_table
            .insert(fn_value, node.return_ty.map(Rc::new));

        // Store parameter information in table
        self.tucx
            .tcx
            .param_table
            .insert(fn_value, param_info.iter().map(|e| e.1.clone()).collect());

        // Allocate parameters
        let mut param_table = self.tabling_fn_params(param_info, fn_value);

        build_block(
            self.tucx,
            &mut StmtContext::new(),
            &mut param_table,
            node.body,
        )?;

        if fn_type.get_return_type().is_none() && self.tucx.no_terminator() {
            // If return type is void and there is no termination, insert return
            self.tucx.builder.build_return(None);
        }

        Ok(())
    }

    /// Expand function parameters into a symbol table for easier handling
    fn tabling_fn_params(
        &self,
        param_info: Vec<(String, Rc<Ty>)>,
        fn_value: FunctionValue<'ctx>,
    ) -> SymbolTable<'ctx> {
        let mut params = SymbolTable::new();

        assert_eq!(fn_value.count_params(), param_info.len() as u32);

        for (idx, (name, ty)) in param_info.into_iter().enumerate() {
            let alloca = self
                .tucx
                .builder
                .build_alloca(as_llvm_type(self.tucx, &ty), &name);

            self.tucx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap());

            params.0.insert(name, (alloca, ty));
        }

        params
    }

    fn define_struct(&mut self, node: Struct) {
        let field_tys: Vec<_> = node
            .fields
            .iter()
            .map(|f| as_llvm_type(self.tucx, &f.ty))
            .collect();

        let ty = self.tucx.context().opaque_struct_type(node.name.as_str());

        ty.set_body(&field_tys, true);

        let fields = node
            .fields
            .into_iter()
            .map(|f| {
                (
                    f.name.name,
                    StructFieldInfo {
                        ty: f.ty,
                        access: f.access,
                        offset: f.offset,
                    },
                )
            })
            .collect();

        self.tucx
            .tcx
            .struct_table
            .insert(node.name.name, (ty, StructInfo { fields }));
    }
}
