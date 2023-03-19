use std::{collections::HashMap, rc::Rc};

use inkwell::{
    debug_info::{
        DICompileUnit, DIFlagsConstants, DIType, DWARFEmissionKind, DWARFSourceLanguage,
        DebugInfoBuilder, LLVMDWARFTypeEncoding,
    },
    module::Module,
};
use kaede_type::{FundamentalTypeKind, Ty, TyKind};

use crate::{as_llvm_type, error::CodegenResult, CompileUnitContext};

/// From libdwarf
///
/// Please refer to the DWARF documentation for the meaning of each
///
/// https://dwarfstd.org/doc/DWARF5.pdf
/// Chapter 5. Type Entries
/// Table 5.1: Encoding attribute values
#[allow(non_camel_case_types)]
#[allow(dead_code)]
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

pub struct DebugInfo<'ctx> {
    dibuilder: DebugInfoBuilder<'ctx>,
    _cu: DICompileUnit<'ctx>,

    // Type cache
    ty_cache: HashMap<String, DIType<'ctx>>,
}

impl<'ctx> DebugInfo<'ctx> {
    pub fn new(module: &Module<'ctx>) -> Self {
        let (dibuilder, cu) = module.create_debug_info_builder(
            true,
            DWARFSourceLanguage::C,
            module.get_source_file_name().to_str().unwrap(),
            ".",
            "Kaede Compiler",
            false,
            "",
            0,
            "",
            DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        Self {
            dibuilder,
            _cu: cu,
            ty_cache: HashMap::new(),
        }
    }

    pub fn get_type(
        &mut self,
        cucx: &CompileUnitContext,
        ty: &Rc<Ty>,
    ) -> CodegenResult<DIType<'ctx>> {
        let size_in_bits = cucx.get_size_in_bits(&as_llvm_type(cucx, ty));

        let ty_str = ty.kind.to_string();

        // Check if the type is cached
        if let Some(di_ty) = self.ty_cache.get(&ty_str) {
            return Ok(*di_ty);
        }

        // Create debug info type
        let di_ty = match ty.kind.as_ref() {
            TyKind::Fundamental(fty) => self
                .dibuilder
                .create_basic_type(
                    &ty_str,
                    size_in_bits,
                    get_dwarf_encoding(&fty.kind()),
                    DIFlagsConstants::PUBLIC,
                )
                .unwrap()
                .as_type(),

            _ => unimplemented!(),
        };

        // Cache type
        self.ty_cache.insert(ty_str, di_ty);

        Ok(di_ty)
    }
}
