use std::{fs, rc::Rc};

use inkwell::{module::Linkage, values::FunctionValue};
use kaede_ast::{
    expr::Ident,
    top::{
        Enum, EnumVariant, Fn, FnKind, Impl, Import, Param, Params, Struct, TopLevel, TopLevelKind,
    },
};
use kaede_parse::Parser;
use kaede_symbol::Symbol;
use kaede_type::{Mutability, RefrenceType, Ty, TyKind, UserDefinedType};

use crate::{
    error::{CodegenError, CodegenResult},
    mangle::{mangle_external_name, mangle_method, mangle_name},
    stmt::{build_block, change_mutability_dup},
    tcx::{EnumInfo, EnumVariantInfo, ReturnType, StructFieldInfo, StructInfo, VariableTable},
    CompileUnitCtx,
};

type FnParams = Vec<(Ident, Rc<Ty>)>;

type FnValueParamsPair<'ctx> = (FunctionValue<'ctx>, FnParams);

pub fn build_top_level(ctx: &mut CompileUnitCtx, node: TopLevel) -> CodegenResult<()> {
    let mut builder = TopLevelBuilder::new(ctx);

    builder.build(node)?;

    Ok(())
}

pub fn push_self_to_front(v: &mut Params, struct_name: String, mutability: Mutability) {
    let span = v.1;

    let self_ident = Ident::new("self".to_string().into(), span);

    let struct_ty = Ty {
        kind: TyKind::UserDefined(UserDefinedType::new(Symbol::from(struct_name), span)).into(),
        mutability,
    }
    .into();

    v.0.push_front(Param {
        name: self_ident,
        mutability,
        ty: Ty {
            kind: TyKind::Reference(RefrenceType {
                refee_ty: struct_ty,
            })
            .into(),
            mutability,
        },
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
    fn build(&mut self, node: TopLevel) -> CodegenResult<()> {
        match node.kind {
            TopLevelKind::Import(node) => self.import_(node)?,

            TopLevelKind::Fn(node) => self.func(node)?,

            TopLevelKind::Struct(node) => self.struct_(node)?,

            TopLevelKind::Impl(node) => self.impl_(node)?,

            TopLevelKind::Enum(node) => self.enum_(node),
        }

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

                self.cucx.tcx.add_enum_ty(
                    node.name.symbol(),
                    EnumInfo {
                        name: node.name,
                        ty: ty.into(),
                        variants: items,
                    },
                );
            }

            None => {
                let ty = self.cucx.context().opaque_struct_type(node.name.as_str());

                ty.set_body(&[self.cucx.context().i32_type().into()], true);

                self.cucx.tcx.add_enum_ty(
                    node.name.symbol(),
                    EnumInfo {
                        name: node.name,
                        ty: ty.into(),
                        variants: items,
                    },
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
                let size = self
                    .cucx
                    .get_size_in_bits(&self.cucx.to_llvm_type(ty).unwrap());
                largest = std::cmp::max(size, largest);
            }
        }

        match largest {
            0 => None,
            _ => Some(largest),
        }
    }

    fn impl_(&mut self, node: Impl) -> CodegenResult<()> {
        for item in node.items {
            match item.kind {
                TopLevelKind::Fn(fn_) => self.method(node.name.symbol(), fn_)?,

                _ => todo!("Error"),
            }
        }

        Ok(())
    }

    /// Static method can also be handled by this function
    ///
    /// If kind is `Normal`, it becomes a static method (said in C++ style)
    fn method(&mut self, impl_for: Symbol, mut node: Fn) -> CodegenResult<()> {
        let mangled_name = mangle_method(self.cucx, impl_for, node.name.symbol());

        match node.kind {
            FnKind::Method => {
                push_self_to_front(&mut node.params, impl_for.to_string(), node.self_mutability);
                self.build_fn(&mangled_name, node)?;
            }

            // Static method
            FnKind::Normal => self.build_fn(&mangled_name, node)?,
        }

        Ok(())
    }

    fn import_(&mut self, node: Import) -> CodegenResult<()> {
        self.import_module(node.module_path)
    }

    fn import_module(&mut self, module_path: Ident) -> CodegenResult<()> {
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
                span: module_path.span,
                mod_name: module_path.symbol(),
            });
        }

        // TODO: Optimize
        let psd_module = Parser::new(&fs::read_to_string(&path).unwrap())
            .run()
            .unwrap();

        for top_level in psd_module.top_levels {
            // Do not import if private
            if top_level.vis.is_private() {
                continue;
            }

            match top_level.kind {
                TopLevelKind::Fn(func) => {
                    self.declare_fn(
                        &mangle_external_name(
                            import_module_name.to_owned().into(),
                            func.name.symbol(),
                        ),
                        func.params,
                        func.return_ty.into(),
                        Linkage::External,
                    )?;
                }

                TopLevelKind::Struct(_) => todo!(),

                TopLevelKind::Import(_) => todo!(),

                TopLevelKind::Impl(_) => todo!(),

                TopLevelKind::Enum(_) => todo!(),
            }
        }

        self.cucx.imported_modules.insert(module_path.symbol());

        Ok(())
    }

    fn func(&mut self, node: Fn) -> CodegenResult<()> {
        assert_eq!(node.kind, FnKind::Normal);

        // Suppress mangling of main function
        if node.name.as_str() == "main" {
            self.build_fn("kdmain", node)
        } else {
            let mangled_name = mangle_name(self.cucx, node.name.symbol());
            self.build_fn(&mangled_name, node)
        }
    }

    // Return function value and parameter information reflecting mutability for the types
    fn declare_fn(
        &mut self,
        mangled_name: &str,
        params: Params,
        return_ty: ReturnType,
        linkage: Linkage,
    ) -> CodegenResult<FnValueParamsPair<'ctx>> {
        let params = params
            .0
            .into_iter()
            .map(|e| (e.name, change_mutability_dup(e.ty.into(), e.mutability)))
            .collect::<Vec<_>>();

        let fn_value = self.cucx.declare_fn(
            mangled_name,
            params.iter().map(|e| e.1.clone()).collect(),
            return_ty,
            Some(linkage),
        )?;

        Ok((fn_value, params))
    }

    fn build_fn(&mut self, mangled_name: &str, node: Fn) -> CodegenResult<()> {
        let fn_value_and_params = self.declare_fn(
            mangled_name,
            node.params,
            node.return_ty.into(),
            Linkage::External,
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
        &self,
        fn_value: FunctionValue<'ctx>,
        params: FnParams,
    ) -> CodegenResult<VariableTable<'ctx>> {
        let mut var_table = VariableTable::new();

        assert_eq!(fn_value.count_params(), params.len() as u32);

        for (idx, (name, param_ty)) in params.into_iter().enumerate() {
            let alloca = self
                .cucx
                .builder
                .build_alloca(self.cucx.to_llvm_type(&param_ty)?, name.as_str());

            self.cucx
                .builder
                .build_store(alloca, fn_value.get_nth_param(idx as u32).unwrap());

            var_table.add(name.symbol(), (alloca, param_ty));
        }

        Ok(var_table)
    }

    fn struct_(&mut self, node: Struct) -> CodegenResult<()> {
        if node.generic_params.is_some() {
            // self.cucx.tcx.
            todo!()
        }

        let mangled_name = mangle_name(self.cucx, node.name.symbol());

        let mut field_tys = Vec::new();
        for field in node.fields.iter() {
            field_tys.push(self.cucx.to_llvm_type(&field.ty)?);
        }

        let ty = self.cucx.context().opaque_struct_type(&mangled_name);

        ty.set_body(&field_tys, true);

        let fields = node
            .fields
            .into_iter()
            .map(|f| {
                (
                    f.name.symbol(),
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
            .add_struct_ty(node.name.symbol(), StructInfo { ty, fields });

        Ok(())
    }
}
