use std::{collections::HashSet, path::PathBuf, rc::Rc};

use error::{CodegenError, CodegenResult};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::{PassManager, PassManagerBuilder},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{AnyType, BasicType, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use kaede_ast::{top::TopLevel, CompileUnit};
use kaede_symbol::Symbol;
use kaede_type::{FundamentalType, FundamentalTypeKind, Mutability, RefrenceType, Ty, TyKind};
use tcx::{FunctionInfo, ReturnType, TypeCtx};
use top::build_top_level;

use crate::tcx::UDTKind;

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

pub fn codegen_compile_unit<'ctx>(
    cgcx: &'ctx CodegenCtx<'ctx>,
    file_path: PathBuf,
    cu: CompileUnit,
    opt_level: OptimizationLevel,
) -> CodegenResult<Module<'ctx>> {
    let cucx = CompileUnitCtx::new(cgcx, file_path)?;

    cucx.codegen(cu.top_levels, opt_level)
}

/// Do **not** create this struct multiple times!
pub struct CodegenCtx<'ctx> {
    _target_machine: TargetMachine,
    target_data: TargetData,

    context: &'ctx Context,
}

impl<'ctx> CodegenCtx<'ctx> {
    fn create_target_machine() -> CodegenResult<TargetMachine> {
        let triple = TargetMachine::get_default_triple();

        let target = match Target::from_triple(&triple) {
            Ok(tgt) => tgt,
            Err(what) => {
                return Err(CodegenError::FailedToLookupTarget {
                    triple: triple.as_str().to_str().unwrap().to_string(),
                    what: what.to_string(),
                })
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
            None => Err(CodegenError::FailedToCreateTargetMachine),
        }
    }

    pub fn new(context: &'ctx Context) -> CodegenResult<Self> {
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

pub struct CompileUnitCtx<'ctx> {
    cgcx: &'ctx CodegenCtx<'ctx>,
    tcx: TypeCtx<'ctx>,

    module: Module<'ctx>,
    builder: Builder<'ctx>,

    file_path: PathBuf,

    module_name: String,
    imported_modules: HashSet<Symbol>,

    /// Block to jump to when a `break` is executed
    ///
    /// Empty if **not** in a loop
    ///
    /// Each time a loop is nested, a basicblock is pushed
    loop_break_bb_stk: Vec<BasicBlock<'ctx>>,

    is_ifmatch_stmt: bool,
}

impl<'ctx> CompileUnitCtx<'ctx> {
    pub fn new(cgcx: &'ctx CodegenCtx<'ctx>, file_path: PathBuf) -> CodegenResult<Self> {
        let module_name = file_path
            .file_stem()
            .unwrap()
            .to_owned()
            .into_string()
            .unwrap();

        let module = cgcx.context.create_module(&module_name);
        module.set_source_file_name(file_path.to_str().unwrap());

        Ok(Self {
            cgcx,
            tcx: Default::default(),
            module,
            builder: cgcx.context.create_builder(),
            file_path,
            module_name,
            imported_modules: HashSet::new(),
            loop_break_bb_stk: Vec::new(),
            is_ifmatch_stmt: false,
        })
    }

    fn context(&self) -> &'ctx Context {
        self.cgcx.context
    }

    pub fn get_size_in_bits(&self, type_: &dyn AnyType) -> u64 {
        self.cgcx.target_data.get_bit_size(type_)
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str, ty: &Ty) -> CodegenResult<PointerValue<'ctx>> {
        let builder = self.context().create_builder();

        let entry = self.get_current_fn().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        Ok(builder.build_alloca(self.to_llvm_type(ty)?, name))
    }

    // If return_ty is `None`, treat as void
    fn create_fn_type(
        &mut self,
        params: &[Rc<Ty>],
        return_ty: &ReturnType,
    ) -> CodegenResult<FunctionType<'ctx>> {
        let mut param_types = Vec::new();
        for param in params {
            param_types.push(self.to_llvm_type(param)?.into());
        }

        Ok(match return_ty {
            ReturnType::Type(ty) => self
                .to_llvm_type(ty)?
                .fn_type(param_types.as_slice(), false),

            ReturnType::Void => self
                .context()
                .void_type()
                .fn_type(param_types.as_slice(), false),
        })
    }

    fn declare_fn(
        &mut self,
        name: &str,
        param_types: Vec<Rc<Ty>>,
        return_type: ReturnType,
        linkage: Option<Linkage>,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let fn_type = self.create_fn_type(&param_types, &return_type)?;

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

    fn gc_init(&mut self) -> CodegenResult<()> {
        // Declare GC_malloc in boehm-gc
        let return_ty = Ty {
            kind: TyKind::Reference(RefrenceType {
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

        self.declare_fn("GC_malloc", param_types, ReturnType::Type(return_ty), None)
            .map(|_| ())
    }

    fn gc_malloc(&self, ty: &Ty) -> CodegenResult<PointerValue<'ctx>> {
        let gc_mallocd = self.module.get_function("GC_malloc").unwrap();

        let addr = self
            .builder
            .build_call(
                gc_mallocd,
                &[self.to_llvm_type(ty)?.size_of().unwrap().into()],
                "",
            )
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

    fn to_llvm_type(&self, ty: &Ty) -> CodegenResult<BasicTypeEnum<'ctx>> {
        let context = self.context();

        Ok(match ty.kind.as_ref() {
            TyKind::Fundamental(t) => t.as_llvm_type(self.context()),

            TyKind::Str => {
                let str_ty = context.i8_type().ptr_type(AddressSpace::default());
                let len_ty = context.i64_type();
                // { *i8, i64 }
                context
                    .struct_type(&[str_ty.into(), len_ty.into()], true)
                    .into()
            }

            TyKind::UserDefined(udt) => {
                let udt_kind = match self.tcx.get_udt(udt.symbol()) {
                    Some(udt) => udt,
                    None => {
                        return Err(CodegenError::Undeclared {
                            name: udt.symbol(),
                            span: udt.span(),
                        })
                    }
                };

                match udt_kind.as_ref() {
                    UDTKind::Struct(sty) => sty.ty.into(),
                    UDTKind::Enum(ety) => ety.ty,
                }
            }

            TyKind::Reference(rty) => self
                .to_llvm_type(&rty.refee_ty)?
                .ptr_type(AddressSpace::default())
                .into(),

            TyKind::Array((elem_ty, size)) => self.to_llvm_type(elem_ty)?.array_type(*size).into(),

            TyKind::Tuple(types) => {
                let mut llvm_types = Vec::new();
                for ty in types {
                    llvm_types.push(self.to_llvm_type(ty)?);
                }
                context.struct_type(llvm_types.as_slice(), true).into()
            }

            TyKind::Unit => panic!("Cannot get LLVM type of unit type!"),
            TyKind::Never => panic!("Cannot get LLVM type of never type!"),
            TyKind::Inferred => panic!("Cannot get LLVM type of inferred type!"),
        })
    }

    fn opt_module(&self, opt_level: OptimizationLevel) {
        let pm_builder = PassManagerBuilder::create();
        pm_builder.set_optimization_level(opt_level);

        let pm = PassManager::create(());
        pm_builder.populate_module_pass_manager(&pm);

        pm.run_on(&self.module);
    }

    fn verify_module(&self) -> CodegenResult<()> {
        self.module.verify().map_err(|e| {
            self.module.print_to_stderr();
            CodegenError::LLVMError {
                what: e.to_string(),
            }
        })
    }

    /// If there was no user-defined main in the module, do nothing
    fn build_main_fn(&mut self) {
        let main_internal = match self.module.get_function("kdmain") {
            Some(fn_v) => fn_v,
            None => return,
        };

        let main =
            self.module
                .add_function("main", self.context().i32_type().fn_type(&[], false), None);
        self.builder
            .position_at_end(self.context().append_basic_block(main, "entry"));

        let exit_status = self
            .builder
            .build_call(main_internal, &[], "")
            .try_as_basic_value()
            .left()
            .unwrap();

        self.builder.build_return(Some(&exit_status));
    }

    fn codegen(
        mut self,
        top_levels: Vec<TopLevel>,
        opt_level: OptimizationLevel,
    ) -> CodegenResult<Module<'ctx>> {
        self.gc_init()?;

        for top in top_levels {
            build_top_level(&mut self, top)?;
        }

        self.build_main_fn();

        self.verify_module()?;

        self.opt_module(opt_level);

        Ok(self.module)
    }
}