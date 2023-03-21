use crate::CompileUnitContext;

pub fn mangle_name(cucx: &CompileUnitContext, name: &str) -> String {
    format!("{}.{}", cucx.module.get_name().to_str().unwrap(), name)
}
