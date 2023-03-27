use std::{fs, rc::Rc};

use inkwell::{
    module::Linkage,
    types::{BasicType, FunctionType},
    values::FunctionValue,
};
use kaede_ast::top::{Fn, Import, Params, Struct, TopLevel, TopLevelKind};
use kaede_lex::lex;
use kaede_parse::parse;
use kaede_type::Ty;

use crate::{
    error::{CodegenError, CodegenResult},
    mangle::{mangle_external_name, mangle_name},
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

    /// Generate top-level code
    fn build(&mut self, node: TopLevel) -> CodegenResult<()> {
        match node.kind {
            TopLevelKind::Import(node) => self.import_module(node)?,

            TopLevelKind::Fn(node) => self.define_fn(node)?,

            TopLevelKind::Struct(node) => self.define_struct(node),
        }

        Ok(())
    }

    fn import_module(&mut self, node: Import) -> CodegenResult<()> {
        let import_module_name = node.modpath.as_str();

        let path = self
            .cucx
            .file_path
            .parent()
            .unwrap()
            .join(import_module_name)
            .with_extension("kae");

        if !path.exists() {
            return Err(CodegenError::FileNotFoundForModule {
                span: node.span,
                mod_name: node.modpath.name,
            });
        }

        // TODO: Optimize
        let psd_module = parse(lex(&fs::read_to_string(&path).unwrap())).unwrap();

        for top_level in psd_module.top_levels {
            // Do not import if private
            if top_level.vis.is_private() {
                continue;
            }

            match top_level.kind {
                TopLevelKind::Fn(func) => {
                    self.declare_fn(
                        &mangle_external_name(import_module_name, func.name.as_str()),
                        &func.params,
                        func.return_ty,
                        Linkage::External,
                    );
                }

                TopLevelKind::Struct(_) => todo!(),

                TopLevelKind::Import(_) => todo!(),
            }
        }

        self.cucx.imported_modules.insert(node.modpath.name);

        Ok(())
    }

    // If return_ty is `None`, treat as void
    fn create_fn_type(&mut self, params: &Params, return_ty: &Option<Ty>) -> FunctionType<'ctx> {
        let param_types = params
            .iter()
            .map(|e| self.cucx.to_llvm_type(&e.1).into())
            .collect::<Vec<_>>();

        match &return_ty {
            Some(ty) => self
                .cucx
                .to_llvm_type(ty)
                .fn_type(param_types.as_slice(), false),

            None => self
                .cucx
                .context()
                .void_type()
                .fn_type(param_types.as_slice(), false),
        }
    }

    fn declare_fn(
        &mut self,
        mangled_name: &str,
        params: &Params,
        return_ty: Option<Ty>,
        linkage: Linkage,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.create_fn_type(params, &return_ty);

        // Declaration
        let value = self
            .cucx
            .module
            .add_function(mangled_name, fn_type, Some(linkage));

        // Store return type information in table
        self.cucx
            .tcx
            .return_ty_table
            .insert(value, return_ty.map(Rc::new));

        value
    }

    fn define_fn(&mut self, node: Fn) -> CodegenResult<()> {
        // Suppress mangling of main function
        let mangled_name = if node.name.as_str() == "main" {
            node.name.name
        } else {
            mangle_name(self.cucx, node.name.as_str())
        };

        let fn_value = self.declare_fn(
            mangled_name.as_str(),
            &node.params,
            node.return_ty,
            Linkage::External,
        );

        let basic_block = self.cucx.context().append_basic_block(fn_value, "entry");
        self.cucx.builder.position_at_end(basic_block);

        let param_info = node
            .params
            .into_iter()
            .map(|e| (e.0, Rc::new(e.1)))
            .collect::<Vec<_>>();

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

        if fn_value.get_type().get_return_type().is_none() && self.cucx.no_terminator() {
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
                .build_alloca(self.cucx.to_llvm_type(&ty), &name);

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
            .map(|f| self.cucx.to_llvm_type(&f.ty))
            .collect();

        let ty = self.cucx.context().opaque_struct_type(&mangled_name);

        ty.set_body(&field_tys, true);

        let fields = node
            .fields
            .into_iter()
            .map(|f| {
                (
                    f.name.name,
                    StructFieldInfo {
                        ty: f.ty.into(),
                        vis: f.vis,
                        offset: f.offset,
                    },
                )
            })
            .collect();

        self.cucx
            .tcx
            .struct_table
            .insert(node.name.name, (ty, StructInfo { fields }));
    }
}
