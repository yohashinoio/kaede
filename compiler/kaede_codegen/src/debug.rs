use std::{collections::HashMap, rc::Rc};

use inkwell::debug_info::{
    DICompileUnit, DIFlagsConstants, DIType, DebugInfoBuilder, LLVMDWARFTypeEncoding,
};
use kaede_type::{FundamentalTypeKind, Ty, TyKind};

use crate::{as_llvm_type, error::CodegenResult, TranslUnitContext};

/// From libdwarf
///
/// Please refer to the DWARF documentation for the meaning of each
///
/// https://dwarfstd.org/doc/DWARF5.pdf
/// Chapter 5. Type Entries
/// Table 5.1: Encoding attribute values
#[allow(non_camel_case_types)]
#[allow(clippy::upper_case_acronyms)]
enum DW_ATE {
    address = 0x01,
    boolean = 0x02,
    complex_float = 0x03,
    float = 0x04,
    signed = 0x05,
    signed_char = 0x06,
    unsigned = 0x07,
    unsigned_char = 0x08,
    imaginary_float = 0x09, /* DWARF3 */
    packed_decimal = 0x0a,  /* DWARF3f */
    numeric_string = 0x0b,  /* DWARF3f */
    edited = 0x0c,          /* DWARF3f */
    signed_fixed = 0x0d,    /* DWARF3f */
    unsigned_fixed = 0x0e,  /* DWARF3f */
    decimal_float = 0x0f,   /* DWARF3f */
    UTF = 0x10,             /* DWARF4 */
    UCS = 0x11,             /* DWARF5 */
    ASCII = 0x12,           /* DWARF5 */
}

fn get_dwarf_encoding(fty_k: &FundamentalTypeKind) -> LLVMDWARFTypeEncoding {
    (match fty_k {
        FundamentalTypeKind::I32 => DW_ATE::signed,
        FundamentalTypeKind::Bool => DW_ATE::boolean,
    }) as LLVMDWARFTypeEncoding
}

pub struct DebugInfo<'ctx, 'module, 'a, 'cgcx> {
    ctx: &'a TranslUnitContext<'ctx, 'module, 'cgcx>,
    builder: &'a DebugInfoBuilder<'ctx>,
    cu: DICompileUnit<'ctx>,

    type_cache: HashMap<Rc<Ty>, DIType<'ctx>>,
}

impl<'ctx, 'module, 'a, 'cgcx> DebugInfo<'ctx, 'module, 'a, 'cgcx> {
    pub fn get_type(&self, ty: &Rc<Ty>) -> CodegenResult<DIType<'ctx>> {
        let size_in_bits = self.ctx.get_size_in_bits(&as_llvm_type(self.ctx, ty));

        Ok(match ty.kind.as_ref() {
            TyKind::Fundamental(fty) => self
                .builder
                .create_basic_type(
                    &ty.kind.to_string(),
                    size_in_bits,
                    get_dwarf_encoding(&fty.kind()),
                    DIFlagsConstants::PUBLIC,
                )
                .unwrap()
                .as_type(),

            _ => unimplemented!(),
        })
    }
}
