use kaede_symbol::Symbol;
use kaede_type::UserDefinedType;

use crate::CompileUnitCtx;

/// Mangled by the current module name
pub fn mangle_name(cucx: &CompileUnitCtx, name: Symbol) -> String {
    format!("{}.{}", cucx.module.get_name().to_str().unwrap(), name)
}

pub fn mangle_method(cucx: &CompileUnitCtx, impl_for: Symbol, name: Symbol) -> String {
    format!(
        "{}.{}.{}",
        cucx.module.get_name().to_str().unwrap(),
        impl_for,
        name
    )
}

pub fn mangle_udt_name(cucx: &CompileUnitCtx, udt: &UserDefinedType) -> Symbol {
    if let Some(generic_args) = &udt.generic_args {
        Symbol::from(format!(
            "{}-{}",
            mangle_name(cucx, udt.name.symbol()),
            generic_args
                .types
                .iter()
                .map(|t| t.kind.to_string())
                .collect::<Vec<_>>()
                .join("_")
        ))
    } else {
        udt.name.symbol()
    }
}

/// Used to mangle symbols outside the current module
pub fn mangle_external_name(module_name: Symbol, name: Symbol) -> String {
    format!("{}.{}", module_name, name)
}
