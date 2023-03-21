use std::rc::Rc;

use inkwell::{types::BasicType, values::FunctionValue};
use kaede_ast::top::{Fn, Module, Struct, TopLevel, TopLevelKind};
use kaede_type::Ty;

use crate::{
    as_llvm_type,
    error::CodegenResult,
    mangle::mangle_name,
    stmt::{build_block, StmtContext},
    tcx::{StructFieldInfo, StructInfo, SymbolTable},
    CompileUnitContext,
};

pub fn build_top_level(ctx: &mut CompileUnitContext, node: TopLevel) -> CodegenResult<()> {
    let mut builder = TopLevelBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

struct TopLevelBuilder<'a, 'ctx, 'm, 'c> {
    cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>,
}

impl<'a, 'ctx, 'm, 'c> TopLevelBuilder<'a, 'ctx, 'm, 'c> {
    fn new(cucx: &'a mut CompileUnitContext<'ctx, 'm, 'c>) -> Self {
        Self { cucx }
    }

    /// Generate top-level code.
    fn build(&mut self, node: TopLevel) -> CodegenResult<()> {
        match node.kind {
            TopLevelKind::Module(node) => self.module(node),

            TopLevelKind::Fn(node) => self.define_fn(node)?,

            TopLevelKind::Struct(node) => self.define_struct(node),
        }

        Ok(())
    }

    fn module(&mut self, _node: Module) {
        unimplemented!()
    }

    fn define_fn(&mut self, node: Fn) -> CodegenResult<()> {
        let mangled_name = mangle_name(self.cucx, node.name.as_str());

        let param_llvm_tys = node
            .params
            .iter()
            .map(|e| as_llvm_type(self.cucx, &e.1).into())
            .collect::<Vec<_>>();

        let fn_type = match &node.return_ty {
            Some(ty) => as_llvm_type(self.cucx, ty).fn_type(param_llvm_tys.as_slice(), false),

            None => self
                .cucx
                .context()
                .void_type()
                .fn_type(param_llvm_tys.as_slice(), false),
        };

        let fn_value = self.cucx.module.add_function(&mangled_name, fn_type, None);

        let basic_block = self.cucx.context().append_basic_block(fn_value, "entry");
        self.cucx.builder.position_at_end(basic_block);

        let param_info = node
            .params
            .into_iter()
            .map(|e| (e.0, Rc::new(e.1)))
            .collect::<Vec<_>>();

        // Store return type information in table
        self.cucx
            .tcx
            .return_ty_table
            .insert(fn_value, node.return_ty.map(Rc::new));

        // Store parameter information in table
        self.cucx
            .tcx
            .param_table
            .insert(fn_value, param_info.iter().map(|e| e.1.clone()).collect());

        // Allocate parameters
        let param_table = self.tabling_fn_params(param_info, fn_value);

        // Push parameter table
        self.cucx.tcx.push_symbol_table(param_table);

        build_block(self.cucx, &mut StmtContext::new(), node.body)?;

        self.cucx.tcx.pop_symbol_table();

        if fn_type.get_return_type().is_none() && self.cucx.no_terminator() {
            // If return type is void and there is no termination, insert return
            self.cucx.builder.build_return(None);
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
                .cucx
                .builder
                .build_alloca(as_llvm_type(self.cucx, &ty), &name);

            self.cucx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap());

            params.add(name, (alloca, ty));
        }

        params
    }

    fn define_struct(&mut self, node: Struct) {
        let mangled_name = mangle_name(self.cucx, node.name.as_str());

        let field_tys: Vec<_> = node
            .fields
            .iter()
            .map(|f| as_llvm_type(self.cucx, &f.ty))
            .collect();

        let ty = self.cucx.context().opaque_struct_type(&mangled_name);

        ty.set_body(&field_tys, true);

        let fields = node
            .fields
            .into_iter()
            .map(|f| {
                (
                    f.name.s,
                    StructFieldInfo {
                        ty: f.ty,
                        access: f.access,
                        offset: f.offset,
                    },
                )
            })
            .collect();

        self.cucx
            .tcx
            .struct_table
            .insert(node.name.s, (ty, StructInfo { fields }));
    }
}
