use kaede_symbol::Symbol;

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

/// Used to mangle symbols outside the current module
pub fn mangle_external_name(module_name: Symbol, name: Symbol) -> String {
    format!("{}.{}", module_name, name)
}
