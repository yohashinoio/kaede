use std::{fs, rc::Rc};

use inkwell::{module::Linkage, types::StructType, values::FunctionValue};
use kaede_ast::top::{
    Enum, EnumVariant, Extern, Fn, Impl, Import, Param, Params, Struct, StructField, TopLevel,
    TopLevelKind,
};
use kaede_parse::Parser;
use kaede_symbol::{Ident, Symbol};
use kaede_type::{Mutability, Ty, TyKind};

use crate::{
    error::CodegenError,
    mangle::{mangle_external_name, mangle_method, mangle_name, mangle_static_method},
    stmt::{build_block, change_mutability_dup},
    tcx::{EnumInfo, EnumVariantInfo, GenericKind, ReturnType, StructInfo, UDTKind, VariableTable},
    CompileUnitCtx,
};

type FnParams = Vec<(Ident, Rc<Ty>)>;

type FnValueParamsPair<'ctx> = (FunctionValue<'ctx>, FnParams);

pub fn create_struct_type<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    mangled_name: Symbol,
    fields: &[StructField],
) -> anyhow::Result<StructType<'ctx>> {
    let mut field_tys = Vec::new();
    for field in fields.iter() {
        field_tys.push(cucx.conv_to_llvm_type(&field.ty)?);
    }

    let ty = cucx.context().opaque_struct_type(mangled_name.as_str());

    ty.set_body(&field_tys, true);

    Ok(ty)
}

pub fn build_top_level(cucx: &mut CompileUnitCtx, node: TopLevel) -> anyhow::Result<()> {
    let mut builder = TopLevelBuilder::new(cucx);

    builder.build(node)?;

    Ok(())
}

pub fn push_self_to_front(params: &mut Params, impl_for_ty: Rc<Ty>, mutability: Mutability) {
    params.v.push_front(Param {
        name: Ident::new("self".to_string().into(), params.span),
        mutability,
        ty: impl_for_ty,
    });
}

struct TopLevelBuilder<'a, 'ctx> {
    cucx: &'a mut CompileUnitCtx<'ctx>,
}

impl<'a, 'ctx> TopLevelBuilder<'a, 'ctx> {
    fn new(cucx: &'a mut CompileUnitCtx<'ctx>) -> Self {
        Self { cucx }
    }

    /// Generate top-level code
    fn build(&mut self, node: TopLevel) -> anyhow::Result<()> {
        match node.kind {
            TopLevelKind::Import(node) => self.import_(node)?,

            TopLevelKind::Fn(node) => self.func(node)?,

            TopLevelKind::Struct(node) => self.struct_(node)?,

            TopLevelKind::Impl(node) => self.impl_(node)?,

            TopLevelKind::Enum(node) => self.enum_(node),

            TopLevelKind::Extern(node) => self.extern_(node)?,
        }

        Ok(())
    }

    fn extern_(&mut self, node: Extern) -> anyhow::Result<()> {
        if node.fn_decl.self_.is_some() {
            todo!("error")
        }

        match node.lang_linkage {
            Some(lang_linkage) => match lang_linkage.syb.as_str() {
                "C" => {
                    let is_var_args = node.fn_decl.params.is_var_args;

                    self.declare_fn(
                        node.fn_decl.name.as_str(),
                        node.fn_decl.params,
                        node.fn_decl.return_ty.into(),
                        Linkage::External,
                        is_var_args,
                    )
                    .unwrap();
                }

                _ => {
                    return Err(CodegenError::UnsupportedLanguageLinkage {
                        span: lang_linkage.span,
                        lang_linkage: lang_linkage.syb,
                    }
                    .into());
                }
            },

            _ => unimplemented!(),
        };

        Ok(())
    }

    fn enum_(&mut self, node: Enum) {
        let largest_type_size = self.get_largest_type_size_of_enum(&node.variants);

        let items = node
            .variants
            .into_iter()
            .map(|e| {
                (
                    e.name.symbol(),
                    EnumVariantInfo {
                        name: e.name,
                        vis: e.vis,
                        offset: e.offset,
                        ty: e.ty.map(Rc::new),
                    },
                )
            })
            .collect();

        // If there is an item with a specified type
        // Specified: { i32, [i8; LARGEST_TYPE_SIZE_IN_BYTES] }
        // Not specified: { i32 }
        match largest_type_size {
            Some(size) => {
                let ty = self.cucx.context().opaque_struct_type(node.name.as_str());

                ty.set_body(
                    &[
                        self.cucx.context().i32_type().into(),
                        self.cucx
                            .context()
                            .i8_type()
                            .array_type((size / 8) as u32)
                            .into(),
                    ],
                    true,
                );

                self.cucx.tcx.add_udt(
                    node.name.symbol(),
                    UDTKind::Enum(EnumInfo {
                        name: node.name,
                        ty: ty.into(),
                        variants: items,
                    }),
                );
            }

            None => {
                let ty = self.cucx.context().opaque_struct_type(node.name.as_str());

                ty.set_body(&[self.cucx.context().i32_type().into()], true);

                self.cucx.tcx.add_udt(
                    node.name.symbol(),
                    UDTKind::Enum(EnumInfo {
                        name: node.name,
                        ty: ty.into(),
                        variants: items,
                    }),
                );
            }
        }
    }

    /// Return None if type is not specified for all (like C's enum)
    /// The size is returned in bits
    fn get_largest_type_size_of_enum(&mut self, enum_items: &[EnumVariant]) -> Option<u64> {
        let mut largest = 0;

        for item in enum_items.iter() {
            if let Some(ty) = &item.ty {
                let llvm_ty = self.cucx.conv_to_llvm_type(ty).unwrap();
                let size = self.cucx.get_size_in_bits(&llvm_ty);
                largest = std::cmp::max(size, largest);
            }
        }

        match largest {
            0 => None,
            _ => Some(largest),
        }
    }

    fn impl_(&mut self, node: Impl) -> anyhow::Result<()> {
        let ty = Rc::new(node.ty);

        for item in node.items {
            match item.kind {
                TopLevelKind::Fn(fn_) => self.define_method(ty.clone(), fn_)?,
                _ => todo!("Error"),
            }
        }

        Ok(())
    }

    fn func(&mut self, node: Fn) -> anyhow::Result<()> {
        assert_eq!(node.decl.self_, None);

        // Suppress mangling of main function
        if node.decl.name.as_str() == "main" {
            self.build_fn("kdmain", node)
        } else {
            let mangled_name = mangle_name(self.cucx, node.decl.name.symbol());
            self.build_fn(&mangled_name, node)
        }
    }

    fn mangle_method(&mut self, impl_for_ty: &Ty, node: &Fn) -> String {
        let impl_for_ty_s = match impl_for_ty.kind.as_ref() {
            TyKind::Reference(refty) => refty.refee_ty.kind.to_string(),
            _ => impl_for_ty.kind.to_string(),
        };

        if impl_for_ty.is_udt() {
            if node.decl.self_.is_none() {
                return mangle_static_method(
                    self.cucx,
                    Symbol::from(impl_for_ty_s),
                    node.decl.name.symbol(),
                );
            };

            mangle_method(
                self.cucx,
                Symbol::from(impl_for_ty_s),
                node.decl.name.symbol(),
            )
        } else {
            // Builtin types

            if node.decl.self_.is_none() {
                // Static method
                // Without module name
                return format!("{}::{}", impl_for_ty_s, node.decl.name.symbol().as_str());
            };

            // Without module name
            format!("{}.{}", impl_for_ty_s, node.decl.name.symbol().as_str())
        }
    }

    /// Static method can also be handled by this function
    ///
    /// If kind is `Normal`, it becomes a static method (said in C++ style)
    fn define_method(&mut self, impl_for_ty: Rc<Ty>, node: Fn) -> anyhow::Result<()> {
        let mangled_name = self.mangle_method(&impl_for_ty, &node);
        self.define_method_internal(&mangled_name, impl_for_ty, node)
    }

    fn define_method_internal(
        &mut self,
        mangled_name: &str,
        impl_for_ty: Rc<Ty>,
        mut node: Fn,
    ) -> anyhow::Result<()> {
        match node.decl.self_ {
            Some(mutability) => {
                // Method
                push_self_to_front(&mut node.decl.params, impl_for_ty, mutability);
                self.build_fn(mangled_name, node)?;
            }

            None => {
                // Static method
                self.build_fn(mangled_name, node)?;
            }
        }

        Ok(())
    }

    fn declare_method(
        &mut self,
        mangled_name: &str,
        impl_for_ty: Rc<Ty>,
        mut node: Fn,
    ) -> anyhow::Result<()> {
        match node.decl.self_ {
            Some(mutability) => {
                // Method
                push_self_to_front(&mut node.decl.params, impl_for_ty, mutability);
                self.declare_fn(
                    mangled_name,
                    node.decl.params,
                    node.decl.return_ty.into(),
                    Linkage::External,
                    false,
                )?;
            }

            None => {
                // Static method
                self.declare_fn(
                    mangled_name,
                    node.decl.params,
                    node.decl.return_ty.into(),
                    Linkage::External,
                    false,
                )?;
            }
        }

        Ok(())
    }

    // Return function value and parameter information reflecting mutability for the types
    fn declare_fn(
        &mut self,
        mangled_name: &str,
        params: Params,
        return_ty: ReturnType,
        linkage: Linkage,
        is_var_args: bool,
    ) -> anyhow::Result<FnValueParamsPair<'ctx>> {
        let params = params
            .v
            .into_iter()
            .map(|e| (e.name, change_mutability_dup(e.ty, e.mutability)))
            .collect::<Vec<_>>();

        let fn_value = self.cucx.declare_fn(
            mangled_name,
            params.iter().map(|e| e.1.clone()).collect(),
            return_ty,
            Some(linkage),
            is_var_args,
        )?;

        Ok((fn_value, params))
    }

    fn build_fn(&mut self, mangled_name: &str, node: Fn) -> anyhow::Result<()> {
        let fn_value_and_params = self.declare_fn(
            mangled_name,
            node.decl.params,
            node.decl.return_ty.into(),
            Linkage::External,
            false,
        )?;

        let fn_value = fn_value_and_params.0;
        let params = fn_value_and_params.1;

        let basic_block = self.cucx.context().append_basic_block(fn_value, "entry");
        self.cucx.builder.position_at_end(basic_block);

        // Allocate parameters
        let param_table = self.fn_params_to_variable_table(fn_value, params)?;

        // Push parameter table
        self.cucx.tcx.push_variable_table(param_table);

        build_block(self.cucx, &node.body)?;

        self.cucx.tcx.pop_variable_table();

        if fn_value.get_type().get_return_type().is_none() && self.cucx.no_terminator() {
            // If return type is void and there is no termination, insert return
            self.cucx.builder.build_return(None);
        }

        Ok(())
    }

    fn fn_params_to_variable_table(
        &mut self,
        fn_value: FunctionValue<'ctx>,
        params: FnParams,
    ) -> anyhow::Result<VariableTable<'ctx>> {
        let mut var_table = VariableTable::new();

        assert_eq!(fn_value.count_params(), params.len() as u32);

        for (idx, (name, param_ty)) in params.into_iter().enumerate() {
            let llvm_param_ty = self.cucx.conv_to_llvm_type(&param_ty)?;
            let alloca = self.cucx.builder.build_alloca(llvm_param_ty, name.as_str());

            self.cucx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap());

            var_table.add(name.symbol(), (alloca, param_ty));
        }

        Ok(var_table)
    }

    fn import_(&mut self, node: Import) -> anyhow::Result<()> {
        self.import_module(node.module_path)
    }

    fn import_module(&mut self, module_path: Ident) -> anyhow::Result<()> {
        let import_module_name = module_path.as_str();

        let path = self
            .cucx
            .file_path
            .parent()
            .unwrap()
            .join(import_module_name)
            .with_extension("kd");

        if !path.exists() {
            return Err(CodegenError::FileNotFoundForModule {
                span: module_path.span(),
                mod_name: module_path.symbol(),
            }
            .into());
        }

        // TODO: Optimize
        let psd_module = Parser::new(&fs::read_to_string(&path).unwrap())
            .run()
            .unwrap();

        for top_level in psd_module.top_levels {
            match top_level.kind {
                TopLevelKind::Fn(func) => {
                    if top_level.vis.is_private() {
                        continue;
                    }

                    self.declare_fn(
                        &mangle_external_name(
                            import_module_name.to_owned().into(),
                            func.decl.name.symbol(),
                        ),
                        func.decl.params,
                        func.decl.return_ty.into(),
                        Linkage::External,
                        false,
                    )?;
                }

                TopLevelKind::Impl(impl_) => {
                    self.import_impl(impl_)?;
                }

                TopLevelKind::Struct(_) => todo!(),

                TopLevelKind::Import(_) => todo!(),

                TopLevelKind::Enum(_) => todo!(),

                TopLevelKind::Extern(_) => todo!(),
            }
        }

        self.cucx.imported_modules.insert(module_path.symbol());

        Ok(())
    }

    fn import_impl(&mut self, impl_: Impl) -> anyhow::Result<()> {
        let impl_for_ty = Rc::new(impl_.ty);

        for item in impl_.items {
            match item.kind {
                TopLevelKind::Fn(func) => {
                    if item.vis.is_private() {
                        continue;
                    }

                    let mangled_name = self.mangle_method(&impl_for_ty, &func);

                    self.declare_method(&mangled_name, impl_for_ty.clone(), func)?;
                }

                _ => todo!(),
            }
        }

        Ok(())
    }

    fn struct_(&mut self, node: Struct) -> anyhow::Result<()> {
        if node.generic_params.is_some() {
            // Generic
            self.cucx
                .tcx
                .add_generic(node.name.symbol(), GenericKind::Struct(node));

            return Ok(());
        }

        let ty = create_struct_type(
            self.cucx,
            Symbol::from(mangle_name(self.cucx, node.name.symbol())),
            &node.fields,
        )?;

        let fields = node
            .fields
            .into_iter()
            .map(|field| (field.name.symbol(), field))
            .collect();

        self.cucx.tcx.add_udt(
            node.name.symbol(),
            UDTKind::Struct(StructInfo { ty, fields }),
        );

        Ok(())
    }
}
