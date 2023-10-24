use kaede_ast::top::GenericParams;
use kaede_type::GenericArgs;

use crate::{tcx::UDTKind, CompileUnitCtx};

pub fn def_generic_args(
    cucx: &mut CompileUnitCtx<'_>,
    params: &GenericParams,
    args: &GenericArgs,
) -> anyhow::Result<()> {
    assert_eq!(params.names.len(), args.types.len());

    for idx in 0..args.types.len() {
        cucx.tcx.add_udt(
            params.names[idx].symbol(),
            UDTKind::GenericArg(args.types[idx].clone()),
        );
    }

    Ok(())
}

pub fn undef_generic_args(cucx: &mut CompileUnitCtx<'_>, params: &GenericParams) {
    for param in params.names.iter() {
        cucx.tcx.remove_udt(param.symbol());
    }
}
