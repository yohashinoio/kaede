use kaede_symbol::Symbol;
use kaede_type::UserDefinedType;

use crate::CompileUnitCtx;

pub enum ModuleLocation {
    External(Symbol /* External module name */),
    Internal,
}

/// Mangled by the current module name
pub fn mangle_name(cucx: &CompileUnitCtx, name: Symbol) -> String {
    format!("{}.{}", cucx.module.get_name().to_str().unwrap(), name)
}

/// Used to mangle symbols outside the current module
pub fn mangle_external_name(external_module_name: Symbol, name: Symbol) -> String {
    format!("{}.{}", external_module_name, name)
}

pub fn mangle_method(
    cucx: &CompileUnitCtx,
    impl_for: Symbol,
    name: Symbol,
    loc: ModuleLocation,
) -> String {
    match loc {
        ModuleLocation::Internal => format!(
            "{}.{}.{}",
            cucx.module.get_name().to_str().unwrap(),
            impl_for,
            name
        ),
        ModuleLocation::External(external_module_name) => {
            format!("{}.{}.{}", external_module_name, impl_for, name)
        }
    }
}

pub fn mangle_static_method(
    cucx: &CompileUnitCtx,
    impl_for: Symbol,
    name: Symbol,
    loc: ModuleLocation,
) -> String {
    match loc {
        ModuleLocation::Internal => format!(
            "{}.{}::{}",
            cucx.module.get_name().to_str().unwrap(),
            impl_for,
            name
        ),
        ModuleLocation::External(external_module_name) => {
            format!("{}.{}::{}", external_module_name, impl_for, name)
        }
    }
}

pub fn mangle_udt_name(
    cucx: &CompileUnitCtx,
    udt: &UserDefinedType,
    loc: ModuleLocation,
) -> Symbol {
    if let Some(generic_args) = &udt.generic_args {
        Symbol::from(format!(
            "{}-{}",
            match loc {
                ModuleLocation::Internal => mangle_name(cucx, udt.name.symbol()),
                ModuleLocation::External(external_module_name) =>
                    mangle_external_name(external_module_name, udt.name.symbol()),
            },
            generic_args
                .types
                .iter()
                .map(|t| t.kind.to_string())
                .collect::<Vec<_>>()
                .join("_")
        ))
    } else {
        match loc {
            ModuleLocation::Internal => mangle_name(cucx, udt.name.symbol()),
            ModuleLocation::External(external_module_name) => {
                mangle_external_name(external_module_name, udt.name.symbol())
            }
        }
        .into()
    }
}
