use std::{collections::HashMap, rc::Rc};

use error::{CodegenError, CodegenResult};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum},
    values::{FunctionValue, PointerValue},
    AddressSpace,
};
use kaede_ast::{expr::Ident, TranslationUnit};
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

type Symbol<'ctx> = (PointerValue<'ctx>, Rc<Ty>);

pub struct SymbolTable<'ctx>(pub HashMap<String, Symbol<'ctx>>);

impl<'ctx> SymbolTable<'ctx> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn find(&self, ident: &Ident) -> CodegenResult<&Symbol<'ctx>> {
        match self.0.get(ident.as_str()) {
            Some(result) => Ok(result),

            None => Err(CodegenError::Undeclared {
                name: ident.name.clone(),
                span: ident.span,
            }),
        }
    }
}

impl<'ctx> Default for SymbolTable<'ctx> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn as_llvm_type<'ctx>(ctx: &CodegenContext<'ctx, '_>, ty: &Ty) -> BasicTypeEnum<'ctx> {
    let context = ctx.context;

    match ty.kind.as_ref() {
        TyKind::Fundamental(t) => t.as_llvm_type(ctx.context),

        TyKind::Str => {
            let str_ty = context.i8_type().ptr_type(AddressSpace::default());
            let len_ty = context.i64_type();
            // { *i8, i64 }
            context
                .struct_type(&[str_ty.into(), len_ty.into()], true)
                .into()
        }

        TyKind::UDType(name) => ctx.tcx.struct_table[&name.0].0.into(),

        TyKind::Reference((refee_ty, _)) => as_llvm_type(ctx, refee_ty)
            .ptr_type(AddressSpace::default())
            .into(),

        TyKind::Unknown => panic!("Cannot get LLVM type of Unknown type!"),
    }
}

pub fn codegen<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    ast: TranslationUnit,
) -> CodegenResult<()> {
    CodegenContext::new(context, module).codegen(ast)?;

    Ok(())
}

/// Per translation unit
pub struct CodegenContext<'ctx, 'module> {
    pub context: &'ctx Context,

    pub module: &'module Module<'ctx>,
    pub builder: Builder<'ctx>,

    pub tcx: TypeContext<'ctx>,
}

impl<'ctx, 'module> CodegenContext<'ctx, 'module> {
    pub fn new(context: &'ctx Context, module: &'module Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            tcx: Default::default(),
        }
    }

    /// Create a new stack allocation instruction in the entry block of the function
    fn create_entry_block_alloca(&self, name: &str, ty: &Ty) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

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

    pub fn codegen(&mut self, ast: TranslationUnit) -> CodegenResult<()> {
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
