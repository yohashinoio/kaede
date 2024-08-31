use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use error::CodegenError;
use generic::{def_generic_args, undef_generic_args};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use kaede_ast::{
    top::{Enum, GenericParams, Import, Struct, StructField, TopLevel, TopLevelKind, Visibility},
    CompileUnit,
};
use kaede_common::kaede_dir;
use kaede_span::{file::FilePath, Span};
use kaede_symbol::{Ident, Symbol};
use kaede_type::{
    FundamentalType, FundamentalTypeKind, GenericArgs, Mutability, ReferenceType, Ty, TyKind,
    UserDefinedType,
};
use mangle::mangle_udt_name;
use tcx::{EnumInfo, EnumVariantInfo, FunctionInfo, GenericKind, ReturnType, StructInfo, TypeCtx};
use top::{build_top_level, create_enum_type, create_struct_type};

use crate::tcx::UdtKind;

pub mod error;
mod expr;
mod generic;
mod mangle;
mod stmt;
mod tcx;
mod top;
mod value;

#[cfg(test)]
mod tests;

/// Used when you want to get a pointer from the evaluated value after evaluating an expression
fn get_loaded_pointer<'ctx>(load_instr: &InstructionValue<'ctx>) -> Option<PointerValue<'ctx>> {
    if let Some(loaded_value) = load_instr.get_operand(0) {
        // Check if the loaded value is a pointer
        if let BasicValueEnum::PointerValue(pointer_value) = loaded_value.left().unwrap() {
            // Return the pointer value as an InstructionValue
            return Some(pointer_value);
        }
    }

    // If the loaded value is not a pointer, return None
    None
}

fn generic_types_to_actual_in_struct(
    cucx: &CompileUnitCtx,
    fields: &[StructField],
) -> Vec<StructField> {
    let mut actual = Vec::new();

    for field in fields.iter() {
        let field_ty_name = match field.ty.kind.as_ref() {
            TyKind::Generic(gty) => gty.name,
            _ => {
                actual.push(field.clone());
                continue;
            }
        };

        let udt_kind = match cucx.tcx.get_udt(field_ty_name.symbol()) {
            Some(udt_kind) => udt_kind,
            None => {
                actual.push(field.clone());
                continue;
            }
        };

        match udt_kind.as_ref() {
            UdtKind::GenericArg(ty) => {
                // Replace with actual type
                actual.push(StructField {
                    ty: ty.clone(),
                    ..field.clone()
                });
                continue;
            }
            _ => {
                actual.push(field.clone());
                continue;
            }
        }
    }

    actual
}

fn generic_types_to_actual_in_enum(
    cucx: &CompileUnitCtx,
    variants: &HashMap<Symbol, EnumVariantInfo>,
) -> HashMap<Symbol, EnumVariantInfo> {
    let mut actual = HashMap::new();

    for variant in variants.iter() {
        let variant_ty = match variant.1.ty.as_ref() {
            Some(ty) => ty.clone(),
            None => {
                actual.insert(*variant.0, variant.1.clone());
                continue;
            }
        };

        let variant_ty_name = match variant_ty.kind.as_ref() {
            TyKind::Generic(gty) => gty.name,
            _ => {
                actual.insert(*variant.0, variant.1.clone());
                continue;
            }
        };

        let udt_kind = match cucx.tcx.get_udt(variant_ty_name.symbol()) {
            Some(udt_kind) => udt_kind,
            None => {
                actual.insert(*variant.0, variant.1.clone());
                continue;
            }
        };

        match udt_kind.as_ref() {
            UdtKind::GenericArg(ty) => {
                // Replace with actual type
                actual.insert(
                    *variant.0,
                    EnumVariantInfo {
                        ty: Some(ty.clone()),
                        ..*variant.1
                    },
                );
                continue;
            }
            _ => {
                actual.insert(*variant.0, variant.1.clone());
                continue;
            }
        }
    }

    actual
}

pub fn codegen_compile_unit<'ctx>(
    cgcx: &'ctx CodegenCtx<'ctx>,
    file_path: FilePath,
    cu: CompileUnit,
    no_autoload: bool,
) -> anyhow::Result<Module<'ctx>> {
    let cucx = CompileUnitCtx::new(cgcx, file_path)?;

    cucx.codegen(cu.top_levels, no_autoload)
}

/// Do **not** create this struct multiple times!
pub struct CodegenCtx<'ctx> {
    _target_machine: TargetMachine,
    target_data: TargetData,

    context: &'ctx Context,
}

impl<'ctx> CodegenCtx<'ctx> {
    fn create_target_machine() -> anyhow::Result<TargetMachine> {
        let triple = TargetMachine::get_default_triple();

        let target = match Target::from_triple(&triple) {
            Ok(tgt) => tgt,
            Err(what) => {
                return Err(CodegenError::FailedToLookupTarget {
                    triple: triple.as_str().to_str().unwrap().to_string(),
                    what: what.to_string(),
                }
                .into())
            }
        };

        match target.create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        ) {
            Some(m) => Ok(m),
            None => Err(CodegenError::FailedToCreateTargetMachine.into()),
        }
    }

    pub fn new(context: &'ctx Context) -> anyhow::Result<Self> {
        // Without initialization, target creation will always fail
        Target::initialize_all(&InitializationConfig::default());

        let machine = Self::create_target_machine()?;

        Ok(Self {
            target_data: machine.get_target_data(),
            _target_machine: machine,
            context,
        })
    }
}

enum LazyDefinedFn {
    GenericFn((TopLevel, GenericParams, GenericArgs)),
}

struct ModulesForMangle {
    names: Vec<Ident>,
}

impl ModulesForMangle {
    fn new(names: &[Ident]) -> Self {
        Self {
            names: names.to_vec(),
        }
    }

    fn get(&self) -> Vec<Ident> {
        // TODO: Optimize
        self.names.clone()
    }

    fn create_mangle_prefix(&self) -> String {
        self.names
            .iter()
            .map(|name| name.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn drain_and_append(&mut self, mut v: Vec<Ident>) -> Vec<Ident> {
        let drained = self.names.drain(..).collect();

        self.names.append(&mut v);

        drained
    }

    fn replace(&mut self, backup: Vec<Ident>) {
        self.names = backup;
    }
}

pub struct CompileUnitCtx<'ctx> {
    cgcx: &'ctx CodegenCtx<'ctx>,
    tcx: TypeCtx<'ctx>,

    module: Module<'ctx>,
    builder: Builder<'ctx>,

    file_path: FilePath,

    imported_modules: HashSet<Symbol>,

    modules_for_mangle: ModulesForMangle,

    /// Block to jump to when a `break` is executed
    ///
    /// Empty if **not** in a loop
    ///
    /// Each time a loop is nested, a basicblock is pushed
    loop_break_bb_stk: Vec<BasicBlock<'ctx>>,

    is_ifmatch_stmt: bool,

    // The elements of this array will begin to be generated sequentially after the module's code generation is finished.
    lazy_define_fns: Vec<LazyDefinedFn>,
}

impl<'ctx> CompileUnitCtx<'ctx> {
    pub fn new(cgcx: &'ctx CodegenCtx<'ctx>, file_path: FilePath) -> anyhow::Result<Self> {
        let module_name = file_path
            .path()
            .file_stem()
            .unwrap()
            .to_owned()
            .into_string()
            .unwrap();

        let module = cgcx.context.create_module(&module_name);
        module.set_source_file_name(file_path.path().to_str().unwrap());

        Ok(Self {
            cgcx,
            tcx: Default::default(),
            module,
            builder: cgcx.context.create_builder(),
            file_path,
            modules_for_mangle: ModulesForMangle::new(&[Ident::new(
                module_name.into(),
                Span::dummy(),
            )]),
            imported_modules: HashSet::new(),
            loop_break_bb_stk: Vec::new(),
            is_ifmatch_stmt: false,
            lazy_define_fns: Vec::new(),
        })
    }

    fn context(&self) -> &'ctx Context {
        self.cgcx.context
    }

    pub fn get_size_in_bits(&self, type_: &dyn AnyType) -> u64 {
        self.cgcx.target_data.get_bit_size(type_)
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(
        &mut self,
        name: &str,
        ty: &Ty,
        span: Span,
    ) -> anyhow::Result<PointerValue<'ctx>> {
        let builder = self.context().create_builder();

        let entry = self.get_current_fn().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        Ok(builder.build_alloca(self.conv_to_llvm_type(ty, span)?, name)?)
    }

    // If return_ty is `None`, treat as void
    fn create_fn_type(
        &mut self,
        params: &[Rc<Ty>],
        return_ty: &ReturnType,
        is_var_args: bool,
        span: Span,
    ) -> anyhow::Result<FunctionType<'ctx>> {
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.conv_to_llvm_type(param, span)?.into());
        }

        Ok(match return_ty {
            ReturnType::Type(ty) => self
                .conv_to_llvm_type(ty, span)?
                .fn_type(param_types.as_slice(), is_var_args),

            ReturnType::Void => self
                .context()
                .void_type()
                .fn_type(param_types.as_slice(), is_var_args),
        })
    }

    fn declare_fn(
        &mut self,
        name: &str,
        param_types: Vec<Rc<Ty>>,
        return_type: ReturnType,
        linkage: Option<Linkage>,
        is_var_args: bool,
        span: Span,
    ) -> anyhow::Result<FunctionValue<'ctx>> {
        let fn_type = self.create_fn_type(&param_types, &return_type, is_var_args, span)?;

        let fn_value = self.module.add_function(name, fn_type, linkage);

        // Store function information in table
        self.tcx.add_function_info(
            fn_value,
            FunctionInfo {
                return_type,
                param_types,
            },
        );

        Ok(fn_value)
    }

    fn gc_init(&mut self) -> anyhow::Result<()> {
        // Declare GC_malloc in boehm-gc
        let return_ty = Ty {
            kind: TyKind::Reference(ReferenceType {
                refee_ty: Ty {
                    kind: TyKind::Fundamental(FundamentalType {
                        kind: FundamentalTypeKind::I8,
                    })
                    .into(),
                    mutability: Mutability::Mut,
                }
                .into(),
            })
            .into(),
            mutability: Mutability::Mut,
        }
        .into();

        let param_types = vec![Ty {
            kind: TyKind::Fundamental(FundamentalType {
                kind: FundamentalTypeKind::U64,
            })
            .into(),
            mutability: Mutability::Not,
        }
        .into()];

        self.declare_fn(
            "GC_malloc",
            param_types,
            ReturnType::Type(return_ty),
            None,
            false,
            Span::dummy(),
        )
        .map(|_| ())
    }

    fn gc_malloc(&mut self, ty: BasicTypeEnum<'ctx>) -> anyhow::Result<PointerValue<'ctx>> {
        let gc_mallocd = self.module.get_function("GC_malloc").unwrap();

        let size = ty.size_of().unwrap().into();

        let addr = self
            .builder
            .build_call(gc_mallocd, &[size], "")?
            .try_as_basic_value()
            .left()
            .unwrap();

        Ok(addr.into_pointer_value())
    }

    fn get_current_fn(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    /// `True` if there is **not** a terminator in the current block
    fn no_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    /// If already created, this function is not created anew
    fn create_generic_struct_type(
        &mut self,
        mangled_name: Symbol,
        ast: &Struct,
        generic_args: &GenericArgs,
    ) -> anyhow::Result<StructType<'ctx>> {
        // Check if already created.
        if let Some(udt_kind) = self.tcx.get_udt(mangled_name) {
            match udt_kind.as_ref() {
                UdtKind::Struct(info) => return Ok(info.ty),
                _ => unreachable!(),
            }
        }

        def_generic_args(self, ast.generic_params.as_ref().unwrap(), generic_args)?;

        let ty = create_struct_type(self, mangled_name, &ast.fields)?;
        // To resolve generics types when using this struct.
        let actual_fields = generic_types_to_actual_in_struct(self, &ast.fields);

        undef_generic_args(self, ast.generic_params.as_ref().unwrap());

        let fields = actual_fields
            .iter()
            .map(|field| (field.name.symbol(), field.clone()))
            .collect();

        self.tcx.add_udt(
            mangled_name,
            UdtKind::Struct(StructInfo {
                ty,
                fields,
                is_external: None,
            }),
        );

        return Ok(ty);
    }

    /// If already created, this function is not created anew
    fn create_generic_enum_type(
        &mut self,
        mangled_name: Symbol,
        ast: &Enum,
        generic_args: &GenericArgs,
    ) -> anyhow::Result<StructType<'ctx>> {
        // Check if already created.
        if let Some(udt_kind) = self.tcx.get_udt(mangled_name) {
            match udt_kind.as_ref() {
                UdtKind::Enum(info) => return Ok(info.ty),
                _ => unreachable!(),
            }
        }

        def_generic_args(self, ast.generic_params.as_ref().unwrap(), generic_args)?;

        let (variants, ty, _) = create_enum_type(self, ast, mangled_name, false);
        // To resolve generics types when using this struct.
        let variants = generic_types_to_actual_in_enum(self, &variants);

        undef_generic_args(self, ast.generic_params.as_ref().unwrap());

        self.tcx.add_udt(
            mangled_name,
            UdtKind::Enum(EnumInfo {
                name: ast.name.symbol(),
                ty,
                variants,
                is_external: None,
            }),
        );

        return Ok(ty);
    }

    fn create_generic_type(&mut self, udt: &UserDefinedType) -> anyhow::Result<StructType<'ctx>> {
        let kind = self
            .tcx
            .get_generic_info(mangle_udt_name(
                self,
                &UserDefinedType {
                    name: udt.name,
                    generic_args: None, // Generic table's key doesn't include generic arg types.
                },
            ))
            .unwrap();

        let generic_args = udt.generic_args.as_ref().unwrap();

        let mangled_generic_name = mangle_udt_name(self, udt);

        match kind.as_ref() {
            GenericKind::Struct(ast) => {
                self.create_generic_struct_type(mangled_generic_name, ast, generic_args)
            }
            GenericKind::Enum(ast) => {
                self.create_generic_enum_type(mangled_generic_name, ast, generic_args)
            }
            _ => unimplemented!(),
        }
    }

    fn conv_to_llvm_type(&mut self, ty: &Ty, span: Span) -> anyhow::Result<BasicTypeEnum<'ctx>> {
        let context = self.context();

        Ok(match ty.kind.as_ref() {
            TyKind::External(ety) => {
                let bkup = self
                    .modules_for_mangle
                    .drain_and_append(vec![ety.module_name]);

                let ty = self.conv_to_llvm_type(&ety.ty, span)?;

                self.modules_for_mangle.replace(bkup);

                ty
            }

            TyKind::Fundamental(t) => t.as_llvm_type(self.context()),

            TyKind::UserDefined(udt) => {
                if udt.generic_args.is_some() {
                    return self.create_generic_type(udt).map(|ty| ty.into());
                }

                let mangled_name = mangle_udt_name(self, udt);

                let udt_kind = match self.tcx.get_udt(mangled_name) {
                    Some(udt) => udt,
                    None => {
                        return Err(CodegenError::Undeclared {
                            name: udt.name.symbol(),
                            span,
                        }
                        .into());
                    }
                };

                match udt_kind.as_ref() {
                    UdtKind::Struct(sty) => sty.ty.into(),
                    UdtKind::Enum(ety) => ety.ty.as_basic_type_enum(),
                    UdtKind::GenericArg(ty) => self.conv_to_llvm_type(ty, span)?,
                }
            }

            TyKind::Generic(gty) => {
                let udt_kind = match self.tcx.get_udt(gty.name.symbol()) {
                    Some(udt) => udt,
                    None => {
                        return Err(CodegenError::Undeclared {
                            name: gty.name.symbol(),
                            span,
                        }
                        .into());
                    }
                };

                match udt_kind.as_ref() {
                    UdtKind::GenericArg(ty) => self.conv_to_llvm_type(ty, span)?,
                    _ => unreachable!(),
                }
            }

            TyKind::Reference(rty) => {
                // If the reference includes generic types, create a generic type.
                // But return a pointer type.
                if let TyKind::UserDefined(udt) = rty.get_base_type().kind.as_ref() {
                    if udt.generic_args.is_some() {
                        self.create_generic_type(udt)?;
                    }
                }

                self.context().ptr_type(AddressSpace::default()).into()
            }

            TyKind::Pointer(pty) => {
                // If the reference includes generic types, create a generic type.
                // But return a pointer type.
                if let TyKind::UserDefined(udt) = pty.get_base_type().kind.as_ref() {
                    if udt.generic_args.is_some() {
                        self.create_generic_type(udt)?;
                    }
                }

                self.context().ptr_type(AddressSpace::default()).into()
            }

            TyKind::Array((elem_ty, size)) => self
                .conv_to_llvm_type(elem_ty, span)?
                .array_type(*size)
                .into(),

            TyKind::Tuple(types) => {
                let mut llvm_types = Vec::new();
                for ty in types {
                    llvm_types.push(self.conv_to_llvm_type(ty, span)?);
                }
                context.struct_type(llvm_types.as_slice(), true).into()
            }

            TyKind::Unit => panic!("Cannot get LLVM type of unit type!"),
            TyKind::Never => panic!("Cannot get LLVM type of never type!"),
            TyKind::Inferred => panic!("Cannot get LLVM type of inferred type!"),
        })
    }

    fn verify_module(&self) -> anyhow::Result<()> {
        self.module.verify().map_err(|e| {
            self.module.print_to_stderr();
            CodegenError::LLVMError {
                what: e.to_string(),
            }
            .into()
        })
    }

    fn import_autoloads(&mut self) -> anyhow::Result<()> {
        let autoload_libs = std::fs::read_dir(format!("{}/lib/src/autoload/", kaede_dir()))?
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.is_file() && path.extension().is_some_and(|e| e == "kd")) // Exclude non-source files
            .collect::<Vec<_>>();

        for lib in autoload_libs {
            build_top_level(
                self,
                TopLevel {
                    kind: TopLevelKind::Import(Import {
                        module_path: Ident::new(
                            Symbol::from(lib.to_str().unwrap().to_string()),
                            Span::dummy(),
                        ),
                        span: Span::dummy(),
                    }),
                    vis: Visibility::Private,
                    span: Span::dummy(),
                },
            )?;
        }

        Ok(())
    }

    /// If there was no user-defined main in the module, do nothing
    fn build_main_fn(&mut self) -> anyhow::Result<()> {
        let main_internal = match self.module.get_function("kdmain") {
            Some(fn_v) => fn_v,
            None => return Ok(()),
        };

        let main =
            self.module
                .add_function("main", self.context().i32_type().fn_type(&[], false), None);
        self.builder
            .position_at_end(self.context().append_basic_block(main, "entry"));

        let exit_status = self
            .builder
            .build_call(main_internal, &[], "")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.builder.build_return(Some(&exit_status))?;

        Ok(())
    }

    fn handle_lazy_define_fns(&mut self) {
        let lazy_fns: Vec<_> = self.lazy_define_fns.drain(..).collect();

        for lazy in lazy_fns {
            match lazy {
                LazyDefinedFn::GenericFn((top, generic_params, generic_args)) => {
                    def_generic_args(self, &generic_params, &generic_args).unwrap();
                    build_top_level(self, top).unwrap();
                    undef_generic_args(self, &generic_params);
                }
            }
        }
    }

    // This function doesn't optimize modules.
    // Please execute 'opt' command to optimize the module.
    fn codegen(
        mut self,
        top_levels: Vec<TopLevel>,
        no_autoload: bool,
    ) -> anyhow::Result<Module<'ctx>> {
        self.gc_init()?;

        if !no_autoload {
            self.import_autoloads()?;
        }

        for top in top_levels {
            build_top_level(&mut self, top)?;
        }

        self.build_main_fn()?;

        self.handle_lazy_define_fns();

        self.verify_module()?;

        Ok(self.module)
    }
}
