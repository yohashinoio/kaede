use crate::CompileUnitContext;

/// Mangled by the current module name
pub fn mangle_name(cucx: &CompileUnitContext, name: &str) -> String {
    format!("{}.{}", cucx.module.get_name().to_str().unwrap(), name)
}

/// Used to mangle symbols outside the current module
pub fn mangle_external_name(module_name: &str, name: &str) -> String {
    format!("{}.{}", module_name, name)
}