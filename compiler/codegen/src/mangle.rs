use kaede_symbol::Symbol;
use kaede_type::UserDefinedType;

use crate::CompileUnitCtx;

/// Mangled by the current module name
pub fn mangle_name(cucx: &CompileUnitCtx, name: Symbol) -> String {
    format!(
        "{}.{}",
        cucx.modules_for_mangle.create_mangle_prefix(),
        name
    )
}

pub fn mangle_method(cucx: &CompileUnitCtx, impl_for: Symbol, name: Symbol) -> String {
    format!(
        "{}.{}.{}",
        cucx.modules_for_mangle.create_mangle_prefix(),
        impl_for,
        name
    )
}

pub fn mangle_static_method(cucx: &CompileUnitCtx, impl_for: Symbol, name: Symbol) -> String {
    format!(
        "{}.{}::{}",
        cucx.modules_for_mangle.create_mangle_prefix(),
        impl_for,
        name
    )
}

pub fn mangle_udt_name(cucx: &CompileUnitCtx, udt: &UserDefinedType) -> Symbol {
    if let Some(generic_args) = &udt.generic_args {
        format!(
            "{}-{}",
            mangle_name(cucx, udt.name.symbol()),
            generic_args
                .types
                .iter()
                .map(|t| t.kind.to_string())
                .collect::<Vec<_>>()
                .join("_")
        )
    } else {
        mangle_name(cucx, udt.name.symbol())
    }
    .into()
}
