use error::{CodegenError, CodegenResult};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetData, TargetMachine},
    types::{AnyType, BasicType, BasicTypeEnum},
    values::{FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use kaede_ast::CompileUnit;
use kaede_type::{Ty, TyKind};
use tcx::TypeContext;
use top::build_top_level;

mod error;
mod expr;
mod stmt;
mod tcx;
mod top;
mod value;

#[cfg(test)]
mod tests;

pub fn as_llvm_type<'ctx>(cucx: &CompileUnitContext<'ctx, '_, '_>, ty: &Ty) -> BasicTypeEnum<'ctx> {
    let context = cucx.context();

    match ty.kind.as_ref() {
        TyKind::Fundamental(t) => t.as_llvm_type(cucx.context()),

        TyKind::Str => {
            let str_ty = context.i8_type().ptr_type(AddressSpace::default());
            let len_ty = context.i64_type();
            // { *i8, i64 }
            context
                .struct_type(&[str_ty.into(), len_ty.into()], true)
                .into()
        }

        TyKind::UDType(name) => cucx.tcx.struct_table[&name.0].0.into(),

        TyKind::Reference((refee_ty, _)) => as_llvm_type(cucx, refee_ty)
            .ptr_type(AddressSpace::default())
            .into(),

        TyKind::Unknown => panic!("Cannot get LLVM type of Unknown type!"),
    }
}

pub fn codegen<'ctx>(
    ctx: &CodegenContext<'ctx>,
    module: &Module<'ctx>,
    ast: CompileUnit,
) -> CodegenResult<()> {
    CompileUnitContext::new(ctx, module)?.codegen(ast)?;

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
}

impl<'ctx, 'm, 'c> CompileUnitContext<'ctx, 'm, 'c> {
    pub fn new(ctx: &'c CodegenContext<'ctx>, module: &'m Module<'ctx>) -> CodegenResult<Self> {
        Ok(Self {
            builder: ctx.context.create_builder(),
            cgcx: ctx,
            module,
            tcx: Default::default(),
        })
    }

    pub fn context(&self) -> &'ctx Context {
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

        builder.build_alloca(as_llvm_type(self, ty), name)
    }

    pub fn get_current_fn(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    /// `True` if there is **not** a terminator in the current block
    pub fn no_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    pub fn codegen(&mut self, ast: CompileUnit) -> CodegenResult<()> {
        for top in ast {
            build_top_level(self, top)?;
        }

        self.module.verify().map_err(|e| {
            self.module.print_to_stderr();
            CodegenError::LLVMError {
                what: e.to_string(),
            }
        })?;

        Ok(())
    }
}
