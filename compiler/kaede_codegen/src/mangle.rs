use crate::CompileUnitContext;

pub fn mangle_name(cucx: &CompileUnitContext, name: &str) -> String {
    format!("{}.{}", cucx.package_name.as_str(), name)
}
