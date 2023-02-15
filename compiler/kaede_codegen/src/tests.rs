use inkwell::{context::Context, module::Module, OptimizationLevel};

use kaede_lex::lex;
use kaede_parse::parse;

use super::*;

type TestFunc = unsafe extern "C" fn() -> u64;

// Expects that a test function of type TestFunc is defined in the module.
fn jit_compile_test(module: &Module) -> u64 {
    let engine = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    unsafe { engine.get_function::<TestFunc>("test").unwrap().call() }
}

fn cg_test(program: &str) -> anyhow::Result<u64> {
    let context = Context::create();
    let module = context.create_module("test");

    codegen(&context, &module, parse(lex(program))?);

    Ok(jit_compile_test(&module))
}

#[test]
fn return_stmt() -> anyhow::Result<()> {
    assert_eq!(cg_test("fn test() { return (48*2 +10 * 2) / 2}")?, 58);

    Ok(())
}

#[test]
fn variable() -> anyhow::Result<()> {
    let program = r"fn test() {
        let yoha = 48
        let io = 10
        return yoha + io
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}

#[test]
fn call_func() -> anyhow::Result<()> {
    let program = r"fn f1() {
        return 48
    }

    fn f2() {
        return 10
    }

    fn test() {
        return f1() + f2()
    }";

    assert_eq!(cg_test(program)?, 58);

    Ok(())
}
