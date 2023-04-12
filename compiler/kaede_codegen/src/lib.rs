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
use kaede_type::{FundamentalType, FundamentalTypeKind, Mutability, RefrenceType, Ty, TyKind};
use tcx::TypeContext;
use top::build_top_level;

mod error;
mod expr;
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

pub fn codegen<'ctx>(
    ctx: &CodegenContext<'ctx>,
    module: &Module<'ctx>,
    file_path: PathBuf,
    cu: CompileUnit,
    opt_level: OptimizationLevel,
) -> CodegenResult<()> {
    let mut cucx = CompileUnitContext::new(ctx, module, file_path)?;

    cucx.init_gc();

    cucx.codegen(cu.top_levels, opt_level)?;

    Ok(())
}

/// Do **not** create this struct multiple times!
pub struct CodegenContext<'ctx> {
    _target_machine: TargetMachine,
    target_data: TargetData,

    pub context: &'ctx Context,
}

impl<'ctx> CodegenContext<'ctx> {
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

pub struct CompileUnitContext<'ctx, 'm, 'c> {
    pub cgcx: &'c CodegenContext<'ctx>,

    pub module: &'m Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub tcx: TypeContext<'ctx>,

    pub file_path: PathBuf,

    pub modname: String,
    pub imported_modules: HashSet<String>,

    /// Block to jump to when a `break` is executed
    ///
    /// Empty if **not** in a loop
    ///
    /// Each time a loop is nested, a basicblock is pushed
    pub loop_break_bb_stk: Vec<BasicBlock<'ctx>>,

    // Flag to be used if 'if' is not used as an expression
    pub is_if_statement: bool,
}

impl<'ctx, 'm, 'c> CompileUnitContext<'ctx, 'm, 'c> {
    pub fn new(
        ctx: &'c CodegenContext<'ctx>,
        module: &'m Module<'ctx>,
        file_path: PathBuf,
    ) -> CodegenResult<Self> {
        let modname = module.get_name().to_str().unwrap().to_string();

        Ok(Self {
            builder: ctx.context.create_builder(),
            cgcx: ctx,
            module,
            tcx: Default::default(),
            file_path,
            modname,
            imported_modules: HashSet::new(),
            loop_break_bb_stk: Vec::new(),
            is_if_statement: false,
        })
    }

    fn context(&self) -> &'ctx Context {
        self.cgcx.context
    }

    pub fn get_size_in_bits(&self, type_: &dyn AnyType) -> u64 {
        self.cgcx.target_data.get_bit_size(type_)
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str, ty: &Ty) -> PointerValue<'ctx> {
        let builder = self.context().create_builder();

        let entry = self.get_current_fn().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.to_llvm_type(ty), name)
    }

    // If return_ty is `None`, treat as void
    fn create_fn_type(
        &mut self,
        params: &[Rc<Ty>],
        return_ty: &Option<Rc<Ty>>,
    ) -> FunctionType<'ctx> {
        let param_types = params
            .iter()
            .map(|t| self.to_llvm_type(t).into())
            .collect::<Vec<_>>();

        match &return_ty {
            Some(ty) => self.to_llvm_type(ty).fn_type(param_types.as_slice(), false),

            None => self
                .context()
                .void_type()
                .fn_type(param_types.as_slice(), false),
        }
    }

    fn decl_fn(
        &mut self,
        name: &str,
        param_types: Vec<Rc<Ty>>,
        return_ty: Option<Rc<Ty>>,
        linkage: Option<Linkage>,
    ) -> FunctionValue<'ctx> {
        let fn_type = self.create_fn_type(&param_types, &return_ty);

        let fn_value = self.module.add_function(name, fn_type, linkage);

        // Store return type information in table
        self.tcx.return_ty_table.insert(fn_value, return_ty);

        // Store parameter information in table
        self.tcx.param_table.insert(fn_value, param_types);

        fn_value
    }

    // Initialize garbage collector
    fn init_gc(&mut self) {
        // Init GC_malloc (boehm-gc)
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
        self.decl_fn("GC_malloc", param_types, Some(return_ty), None);
    }

    fn gc_malloc(&self, ty: &Ty) -> PointerValue<'ctx> {
        let gc_mallocd = self.module.get_function("GC_malloc").unwrap();

        let addr = self
            .builder
            .build_call(
                gc_mallocd,
                &[self.to_llvm_type(ty).size_of().unwrap().into()],
                "",
            )
            .try_as_basic_value()
            .left()
            .unwrap();

        addr.into_pointer_value()
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

    fn to_llvm_type(&self, ty: &Ty) -> BasicTypeEnum<'ctx> {
        let context = self.context();

        match ty.kind.as_ref() {
            TyKind::Fundamental(t) => t.as_llvm_type(self.context()),

            TyKind::Str => {
                let str_ty = context.i8_type().ptr_type(AddressSpace::default());
                let len_ty = context.i64_type();
                // { *i8, i64 }
                context
                    .struct_type(&[str_ty.into(), len_ty.into()], true)
                    .into()
            }

            TyKind::UserDefined(udt) => self.tcx.struct_table[&udt.name].0.into(),

            TyKind::Reference(rty) => self
                .to_llvm_type(&rty.refee_ty)
                .ptr_type(AddressSpace::default())
                .into(),

            TyKind::Array((elem_ty, size)) => self.to_llvm_type(elem_ty).array_type(*size).into(),

            TyKind::Tuple(types) => {
                let types: Vec<_> = types.iter().map(|t| self.to_llvm_type(t)).collect();
                context.struct_type(types.as_slice(), true).into()
            }

            TyKind::Unit => panic!("Cannot get LLVM type of unit type!"),
            TyKind::Never => panic!("Cannot get LLVM type of never type!"),
            TyKind::Inferred => panic!("Cannot get LLVM type of inferred type!"),
        }
    }

    fn opt_module(&self, opt_level: OptimizationLevel) {
        let pm_builder = PassManagerBuilder::create();
        pm_builder.set_optimization_level(opt_level);

        let pm = PassManager::create(());
        pm_builder.populate_module_pass_manager(&pm);

        pm.run_on(self.module);
    }

    fn verify_module(&self) -> CodegenResult<()> {
        self.module.verify().map_err(|e| {
            self.module.print_to_stderr();
            CodegenError::LLVMError {
                what: e.to_string(),
            }
        })
    }

    fn codegen(
        &mut self,
        top_levels: Vec<TopLevel>,
        opt_level: OptimizationLevel,
    ) -> CodegenResult<()> {
        for top in top_levels {
            build_top_level(self, top)?;
        }

        self.verify_module()?;

        self.opt_module(opt_level);

        Ok(())
    }
}
