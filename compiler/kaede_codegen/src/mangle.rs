use crate::CompileUnitContext;

pub fn mangle_name(cucx: &CompileUnitContext, name: &str) -> String {
    format!("{}.{}", cucx.module_name, name)
}
