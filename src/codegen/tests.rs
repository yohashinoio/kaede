use inkwell::{context::Context, module::Module, OptimizationLevel};

use crate::{lex, parse};

use super::*;

type TestFunc = unsafe extern "C" fn() -> u64;

// Expects that a test function of type TestFunc is defined in the module.
fn jit_compile_test(module: &Module) -> u64 {
    let engine = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    unsafe { engine.get_function::<TestFunc>("test").unwrap().call() }
}

#[test]
fn simple() -> anyhow::Result<()> {
    let context = Context::create();
    let module = context.create_module("test");

    codegen(
        &context,
        &module,
        &parse(lex(r" fn test() { (48*2 +10 * 2) / 2}"))?,
    );

    assert_eq!(jit_compile_test(&module), 58);

    Ok(())
}
